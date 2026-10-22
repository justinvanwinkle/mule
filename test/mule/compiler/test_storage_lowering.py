"""Physical storage lowering preserves dictionary probes and sequence behavior."""

import re
import subprocess

import pytest

from mule.compiler import emit_llvm
from mule.compiler.artifacts import Graph
from mule.compiler.driver import Build, Toolchain
from test.mule.compiler.test_exception_groups import compare

WORD_COUNT = """
def count_words(tokens: list[str]) -> dict[str, int]:
    counts: dict[str, int] = {}
    for token in tokens:
        counts[token] = counts.get(token, 0) + 1
    return counts

counts = count_words(['alpha', 'beta', 'alpha', 'gamma', 'beta', 'alpha'])
print(counts['alpha'], counts['beta'], counts['gamma'], len(counts))
"""

_SYMBOL = r'"[^"\n]+"|[-\w.$:]+'
_CALL = re.compile(rf"\b(?:call|invoke)\b[^\n]*?@({_SYMBOL})\(")
_OPAQUE_STORAGE = frozenset(
    {
        "mule_storage_read",
        "mule_storage_write",
        "mule_storage_length",
        "mule_storage_view",
    }
)


def _llvm_functions(llvm):
    pattern = rf"^define[^\n]*@({_SYMBOL})\([^\n]*\)[^\n]*\{{\n(.*?)^\}}"
    return {
        name.strip('"'): body
        for name, body in re.findall(pattern, llvm, re.MULTILINE | re.DOTALL)
    }


def _reachable_functions(root, functions):
    reached = set()
    pending = [root]
    while pending:
        name = pending.pop()
        if name in reached:
            continue
        reached.add(name)
        pending.extend(
            target.strip('"') for target in _CALL.findall(functions.get(name, ""))
        )
    return reached


def _llvm_blocks(body):
    blocks = {}
    current = None
    for line in body.splitlines():
        label = re.match(r"^([-\w.$]+):", line)
        if label:
            current = label.group(1)
            blocks[current] = []
        elif current is not None:
            blocks[current].append(line)
    return {name: "\n".join(lines) for name, lines in blocks.items()}


def _storage_query_dependencies(definitions, loop_labels, functions):
    """Track loop-local queries through SSA copies, phis, selects, and loads."""
    dependencies = {variable: set() for variable in definitions}
    callees = {}
    for variable, (label, instruction) in definitions.items():
        called = _CALL.search(instruction)
        if label not in loop_labels or called is None:
            continue
        target = called.group(1).strip('"')
        if target not in callees:
            callees[target] = _reachable_functions(target, functions) & _OPAQUE_STORAGE
        dependencies[variable].update(callees[target])

    changed = True
    while changed:
        changed = False
        for variable, (_, instruction) in definitions.items():
            previous = dependencies[variable].copy()
            for operand in re.findall(r"%[-\w.$]+", instruction):
                dependencies[variable].update(dependencies.get(operand, ()))
            changed |= previous != dependencies[variable]
    return dependencies


@pytest.mark.integration
def test_word_counting_probe_loops_do_not_query_storage_for_each_native_access(
    tmp_path,
):
    compare(tmp_path, WORD_COUNT)
    toolchain = Toolchain(optimize=2)
    with Graph(1) as graph:
        builtins = Build(graph, toolchain).builtins()
        graph.run(builtins)
        builtins_llvm = builtins.path("unit.ll")

    # Keep a runtime-input kernel reachable without retaining program startup.
    # The dictionary probe bodies come from the actual compiled builtins unit.
    source = tmp_path / "word_count.ll"
    source.write_text(
        emit_llvm(WORD_COUNT)
        + """
define ptr @word_count_ir_probe(ptr %globals, ptr %tokens) {
entry:
  %counts = call ptr @"mule_fn_count_words.body"(ptr %globals, ptr %tokens)
  ret ptr %counts
}
"""
    )
    main_optimized = tmp_path / "word_count.main.optimized.ll"
    builtins_optimized = tmp_path / "word_count.builtins.optimized.ll"
    optimized = tmp_path / "word_count.optimized.ll"
    # Optimize each unit separately, as the measured -O2 build does without
    # LTO. Linking afterward is only for call-graph and loop inspection.
    for command in (
        [
            toolchain.clang,
            "-O2",
            "-S",
            "-emit-llvm",
            source,
            "-o",
            main_optimized,
        ],
        [
            toolchain.clang,
            "-O2",
            "-S",
            "-emit-llvm",
            builtins_llvm,
            "-o",
            builtins_optimized,
        ],
        [
            toolchain.llvm_tool("llvm-link"),
            "-S",
            main_optimized,
            builtins_optimized,
            "-o",
            optimized,
        ],
    ):
        result = subprocess.run(
            command, check=False, capture_output=True, text=True, timeout=30
        )
        assert result.returncode == 0, result.stderr

    llvm = optimized.read_text()
    functions = _llvm_functions(llvm)
    reachable = _reachable_functions("word_count_ir_probe", functions)

    analysis = subprocess.run(
        [
            toolchain.llvm_tool("opt"),
            "-passes=print<loops>",
            "-disable-output",
            optimized,
        ],
        check=False,
        capture_output=True,
        text=True,
        timeout=30,
    )
    assert analysis.returncode == 0, analysis.stderr
    accesses, blocked_accesses = _probe_storage_accesses(
        functions, reachable, analysis.stderr
    )
    assert accesses, (
        "word-count probe loops contain no native indexed object-slot accesses"
    )
    # A view acquired before a loop is allowed. An unavoidable query that
    # supplies each indexed access is not, even through a renamed wrapper.
    assert not blocked_accesses, (
        "opaque storage queries still supply native accesses inside word-count probe loops:\n"
        + "\n".join(blocked_accesses)
    )


def _probe_storage_accesses(functions, reachable, loop_analysis):
    loop_blocks = {}
    current = None
    for line in loop_analysis.splitlines():
        header = re.match(r"Loop info for function '(.+)':", line)
        if header:
            current = header.group(1)
        elif (
            "Loop at depth" in line
            and current is not None
            and current in reachable
            and any(
                operation in current
                for operation in ("mule_fn__table_lookup", "mule_fn__table_insert")
            )
        ):
            # Check the actual probe loops, not cold exception, formatting,
            # or generic hashing algorithms reachable through their fallbacks.
            loop_blocks.setdefault(current, set()).update(
                re.findall(r"%([-\w.$]+)", line)
            )

    accesses = []
    blocked_accesses = []
    for name, labels in loop_blocks.items():
        blocks = _llvm_blocks(functions[name])
        definitions = {
            variable: (label, instruction)
            for label, body in blocks.items()
            for variable, instruction in re.findall(r"(%[-\w.$]+) = ([^\n]+)", body)
        }
        dependencies = _storage_query_dependencies(definitions, labels, functions)
        for label in labels:
            # A variable-index object slot is a real vector access, not an
            # argument buffer, constant field offset, or helper declaration.
            for base in re.findall(
                r"getelementptr(?: inbounds)? ptr, ptr (%[-\w.$]+), i64 %[-\w.$]+",
                blocks[label],
            ):
                accesses.append((name, label))
                opaque = dependencies.get(base, ())
                if opaque:
                    blocked_accesses.append(f"{name}:{label}: {sorted(opaque)}")

    return accesses, blocked_accesses


def test_dictionary_collision_probes_survive_growth_updates_and_deletion(tmp_path):
    compare(
        tmp_path,
        """
class Key:
    def __init__(self, value):
        self.value = value
    def __hash__(self):
        return 7
    def __eq__(self, other):
        return self.value == other.value
table = {}
for index in range(80):
    table[Key(index)] = index * 3
for index in range(0, 80, 2):
    table[Key(index)] += 1
for index in range(0, 80, 3):
    del table[Key(index)]
for index in range(80, 160):
    table[Key(index)] = index * 5
print(len(table), sum(table.values()))
print([table.get(Key(index), -1) for index in (0, 1, 2, 79, 80, 159, 160)])
print([key.value for key in table][:8])
""",
    )


def test_storage_reads_and_writes_preserve_aliases_and_fallback_sequences(tmp_path):
    compare(
        tmp_path,
        """
first = ['original']
second = ['replacement']
values = [first, first]
values[0] = second
print(values[0] is second, values[1] is first, first)
values[:] = values[::-1]
print(values[0] is first, values[1] is second)
for sequence in ([], (), b'', '', [2, 3, 5], (7, 11), b'ABC'):
    print(len(sequence), all(sequence), any(sequence))
print(sum([2, 3, 5]), sum((7, 11)), sum(b'ABC'))
print(list('abc'), list(b'xyz'))
for sequence in ([1], (1,), b'x', 'x'):
    for index in (-2, 1, 1 << 70):
        try:
            print(sequence[index])
        except (IndexError, OverflowError) as error:
            print(type(error).__name__)
""",
    )

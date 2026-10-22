"""Exact-list element borrowing keeps its owner alive through the consumer."""

import re

import pytest

from test.mule.compiler.rewrite_support import build_case

pytestmark = pytest.mark.integration


SOURCE = """
def record_widths(rows: list[str]):
    total = 0
    empty = 0
    for position in range(len(rows)):
        total += len(rows[position])
        empty += rows[position] == ""
    return total, empty, len(rows)


def replace_rows(rows):
    rows.clear()
    rows.append("replacement")
    return "replacement"


def mutation_between_operands(rows: list[str], index: int):
    same = rows[index & 3] == replace_rows(rows)
    return same, rows


def reassign_between_operands(rows: list[str], replacement: list[str], index: int):
    same = rows[index & 3] == (rows := replacement)[0]
    return same, rows


def escaping_element(rows: list[str], index: int):
    return rows[index & 3]


def repeated_element(rows: list[str], index: int):
    row = rows[index & 3]
    return len(row), row


def crossing_branch(rows: list[str], index: int, clear: bool):
    row = rows[index & 3]
    if clear:
        rows.clear()
    return len(row), row, len(rows)


def read_generic(rows, index):
    return rows[index]


class Index:
    def __init__(self, value):
        self.value = value

    def __index__(self):
        print("index", self.value)
        return self.value


class CustomRows(list):
    def __getitem__(self, index):
        print("override", index)
        return "overridden"


seed = input()
index = int(input())
clear = bool(input())
rows = [seed + "alpha", seed + "β", seed, seed + "🚀"]
for extra in range((index & 7) + 1):
    rows.append(seed)
print("widths", record_widths(rows))
print("mutate", mutation_between_operands(list(rows), index))
print("reassign", reassign_between_operands(list(rows), ["replacement"], index))
saved = escaping_element(list(rows), index)
rows.clear()
print("escaped", saved)
rows = [seed + "first", seed + "second", seed + "third", seed + "last"]
print("repeated", repeated_element(rows, index))
print("branch", crossing_branch(list(rows), index, clear))
for probe in (index, -1, -5, True, 1 << 100, Index(1)):
    try:
        print("generic", read_generic(rows, probe))
    except (IndexError, TypeError) as error:
        print(type(error).__name__)
print("subclass", read_generic(CustomRows(rows), 0))
"""


@pytest.fixture(scope="module")
def borrowed_list_case(tmp_path_factory):
    return build_case(tmp_path_factory.mktemp("borrowed-list"), SOURCE, optimize=3)


def _loop_block_bodies(llvm):
    """Find cyclic blocks without depending on generated label spelling."""
    label_pattern = r'(?m)^("[^"\n]+"|[-a-zA-Z$._0-9]+):[^\n]*\n'
    target_pattern = r'label %("[^"\n]+"|[-a-zA-Z$._0-9]+)'
    for function in re.split(r"(?m)(?=^define\b)", llvm):
        parts = re.split(label_pattern, function)
        blocks = dict(zip(parts[1::2], parts[2::2], strict=True))
        successors = {
            label: set(re.findall(target_pattern, body))
            for label, body in blocks.items()
        }
        for label, body in blocks.items():
            pending = set(successors[label])
            visited = set()
            while pending:
                following = pending.pop()
                if following == label:
                    yield body
                    break
                if following not in visited:
                    visited.add(following)
                    pending.update(successors.get(following, ()))


def _assert_no_borrowed_element_reference_traffic(llvm):
    identifier = r"%[-a-zA-Z$._0-9]+"
    inspected = False
    for function in re.split(r"(?m)(?=^define\b)", llvm):
        aliases = set(
            re.findall(
                rf"({identifier}) = (?:tail )?call ptr @mule_list_getitem_borrow\(",
                function,
            )
        )
        if not aliases:
            continue
        inspected = True
        lines = function.splitlines()
        # Inlined retain/release operations address the object's reference
        # count through a derived pointer rather than calling a named helper.
        while True:
            previous = set(aliases)
            for line in lines:
                pointer = re.search(
                    rf"({identifier}) = (?:getelementptr|bitcast|addrspacecast|phi ptr|select )",
                    line,
                )
                if pointer and aliases.intersection(
                    re.findall(identifier, line.split("=", 1)[1])
                ):
                    aliases.add(pointer[1])
            if aliases == previous:
                break
        for line in lines:
            if not aliases.intersection(re.findall(identifier, line)):
                continue
            assert not re.search(r"\b(?:atomicrmw|cmpxchg)\b", line), line
            assert not re.search(r"\bcall\b[^\n]*@[^\s(]*(?:retain|release)", line), (
                line
            )
            store = re.search(rf"\bstore\b.*?, ptr ({identifier})(?:,|$)", line)
            assert store is None or store[1] not in aliases, line
    assert inspected, "No borrowed element result was inspected"


@pytest.mark.parametrize(
    "stdin",
    ["\n0\n\n", "payload\n3\nyes\n", "héllo🚀\n-1\nyes\n", "x\n8\n\n"],
    ids=["empty-text", "last-slot-clear", "unicode-negative", "out-of-range"],
)
def test_list_borrowing_preserves_scanning_and_ownership_boundaries(
    borrowed_list_case, stdin
):
    borrowed_list_case.compare(stdin=stdin)


def test_record_scan_borrows_heap_elements_without_reference_count_work(
    borrowed_list_case,
):
    body = borrowed_list_case.function_ir(r"mule_fn_record_widths\.")
    # Both list length and payloads depend on input. Require acquisition in
    # the actual scan loop, not an unrelated call elsewhere in the function.
    loop_bodies = "\n".join(_loop_block_bodies(body))
    assert re.search(r"\bcall ptr @mule_list_getitem_borrow\(", loop_bodies), body
    _assert_no_borrowed_element_reference_traffic(body)

    # Follow the selected Rust primitive in the executable, rather than credit
    # helper selection while an element retain remains hidden inside it.
    primitive = borrowed_list_case.function_assembly(r"^mule_list_getitem_borrow$")
    assert re.search(r"\bmov[a-z]*\s+[^\n]*\([^\n)]*,[^\n)]*,8\)", primitive), primitive
    assert not re.search(r"\b(?:lock|xadd|cmpxchg)\b", primitive), primitive
    assert not re.search(r"<[^>]*(?:retain|release)[^>]*>", primitive), primitive
    assert not re.search(r"<mule_list_getitem(?:\+[^>]*)?>", primitive), primitive


@pytest.mark.parametrize(
    "function",
    [
        "mutation_between_operands",
        "reassign_between_operands",
        "escaping_element",
        "repeated_element",
        "crossing_branch",
    ],
)
def test_unsafe_list_element_lifetimes_acquire_an_owned_reference(
    borrowed_list_case, function
):
    body = borrowed_list_case.function_ir(rf"mule_fn_{function}\.")
    acquisitions = re.findall(r"\bcall ptr @(mule_list_getitem(?:_borrow)?)\(", body)
    assert acquisitions, body
    # The first access must survive the mutation, reassignment, escaping use,
    # local copy, or control-flow transfer in the corresponding source case.
    assert acquisitions[0] == "mule_list_getitem", body


def test_unknown_receiver_and_index_keep_python_indexing_dispatch(borrowed_list_case):
    body = borrowed_list_case.function_ir(r"mule_fn_read_generic\.body$")
    assert "@mule_list_getitem_borrow(" not in body, body
    assert "@mule_list_getitem(" not in body, body

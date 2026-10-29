"""Typed byte paths retain Python behavior and expose byte-sized loop operations."""

import re
import subprocess

import pytest

from mule.compiler import build
from test.mule.compiler.test_native import run_native

BYTEARRAY_ITERATION_SOURCE = """\
data = bytearray([128, 255])
iterator = iter(data)
print(next(iterator))
data.extend(bytes(range(8)))
data[1] = 129
print(list(iterator))
print(list(iterator))
iterator = iter(data)
print(next(iterator))
data.__init__([7, 8, 9])
print(list(iterator))
iterator = iter(data)
print(list(iterator))
data.append(10)
print(list(iterator))
"""

BYTEARRAY_ITERATION_OUTPUT = """\
128
[129, 0, 1, 2, 3, 4, 5, 6, 7]
[]
128
[8, 9]
[7, 8, 9]
[]
"""


BYTEARRAY_EXPORT_SOURCE = """\
data = bytearray(b'abc')
view = memoryview(data)
data[0] = 65
print(view[0])
try:
    data.append(100)
except BufferError:
    print('exported')
view[1] = 66
print(bytes(data))
view.release()
data.append(100)
data.extend(data)
print(bytes(data))
held = None
class ExportingIndex:
    def __index__(self):
        global held
        data.__init__(b'new')
        held = memoryview(data)
        return 101
try:
    data.append(ExportingIndex())
except BufferError:
    print('exported during conversion')
print(bytes(data))
held.release()
"""

BYTEARRAY_EXPORT_OUTPUT = """\
65
exported
b'ABc'
b'ABcdABcd'
exported during conversion
b'new'
"""


BYTE_LOOP_SOURCE = """\
from mule_types import optimize

@optimize(speed=3, safety=0, debug=0)
def encode(data{annotation}):
    output = bytearray()
    for byte in data:
        output.append(byte & 127)
    return output

data = bytes.fromhex(input())
print(bytes(encode(data)).hex())
"""


def test_bytearray_iteration_observes_resize_and_item_mutation(tmp_path):
    result = run_native(tmp_path, BYTEARRAY_ITERATION_SOURCE)
    assert result.returncode == 0, result.stderr
    assert result.stdout == BYTEARRAY_ITERATION_OUTPUT


def test_bytearray_export_blocks_growth_but_preserves_live_writes(tmp_path):
    result = run_native(tmp_path, BYTEARRAY_EXPORT_SOURCE)
    assert result.returncode == 0, result.stderr
    assert result.stdout == BYTEARRAY_EXPORT_OUTPUT


def _loop_regions(llvm, without_calls=()):
    """Find cycles that can repeat without taking a protocol fallback."""
    for function in re.finditer(
        r"^define [^\n]+\{\n(.*?)^}", llvm, re.MULTILINE | re.DOTALL
    ):
        blocks = {}
        current = "entry"
        for line in function.group(1).splitlines():
            label = re.match(r"^([\w.$-]+):", line)
            if label:
                current = label.group(1)
            blocks.setdefault(current, []).append(line)
        blocks = {
            name: lines
            for name, lines in blocks.items()
            if not any(
                f"@{callee}(" in line for callee in without_calls for line in lines
            )
        }
        edges = {
            name: {
                target
                for target in re.findall(r"label %([\w.$-]+)", "\n".join(lines))
                if target in blocks
            }
            for name, lines in blocks.items()
        }
        reachable = {}
        for start in blocks:
            visited = set()
            pending = list(edges[start])
            while pending:
                node = pending.pop()
                if node not in visited:
                    visited.add(node)
                    pending.extend(edges.get(node, ()))
            reachable[start] = visited
        regions = {
            frozenset(node for node in reachable[start] if start in reachable[node])
            for start in blocks
            if start in reachable[start]
        }
        for region in regions:
            yield "\n".join(line for name in region for line in blocks[name])


@pytest.mark.integration
@pytest.mark.parametrize("annotation", ["", ": bytes"], ids=["inferred", "annotated"])
def test_known_byte_loop_emits_direct_byte_load_and_store(tmp_path, annotation):
    source = tmp_path / "byte_loop.py"
    source.write_text(BYTE_LOOP_SOURCE.format(annotation=annotation))
    executable = tmp_path / "byte_loop"
    original_ir = tmp_path / "byte_loop.ll"
    build(
        source,
        executable,
        llvm_output=original_ir,
        optimize=3,
        arch="native",
        workers=1,
    )
    for data, expected in [("", "\n"), ("007f80ff", "007f007f\n")]:
        result = subprocess.run(
            [executable],
            input=data + "\n",
            capture_output=True,
            text=True,
            check=False,
            timeout=10,
        )
        assert result.returncode == 0, result.stderr
        assert result.stdout == expected
    optimized_ir = tmp_path / "byte_loop.optimized.ll"
    subprocess.run(
        [
            "clang",
            "-O3",
            "-march=native",
            "-S",
            "-emit-llvm",
            str(original_ir),
            "-o",
            str(optimized_ir),
        ],
        check=True,
        capture_output=True,
        text=True,
    )
    loops = list(
        _loop_regions(
            optimized_ir.read_text(),
            without_calls=(
                "mule_fast_next_item",
                "mule_bytearray_append_fast",
                "mule_method_entry",
            ),
        )
    )
    byte_loops = [
        loop
        for loop in loops
        if re.search(r"\bload (?:i8|<\d+ x i8>),", loop)
        and re.search(r"\bstore (?:i8|<\d+ x i8>)(?=\s)", loop)
    ]
    assert byte_loops, (
        "known bytes must reach a cycle with byte loads and stores "
        "without per-byte protocol calls"
    )
    assert all("atomicrmw" not in loop for loop in byte_loops), (
        "byte loops must borrow their stable owners instead of retaining per byte"
    )

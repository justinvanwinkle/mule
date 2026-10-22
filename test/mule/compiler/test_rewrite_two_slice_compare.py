"""Record-span comparisons must avoid materializing either substring."""

import re
import struct

import pytest

from test.mule.compiler.rewrite_support import build_case

SOURCE = """
def direct_record(left: str, right: str, a: int, b: int, c: int, d: int):
    return left[a:b] == right[c:d]


def copied_record(left: str, right: str, a: int, b: int, c: int, d: int):
    first = left[a:b]
    second = right[c:d]
    return first != second


def open_record(left: str, right: str, a: int, b: int):
    return left[a:] == right[:b]


def reused_record(left: str, right: str, a: int, b: int, c: int, d: int):
    first = left[a:b]
    second = right[c:d]
    equal = first == second
    return equal, len(first), len(second), first, second


def interfering_record(left: str, right: str, a: int, b: int, c: int, d: int):
    first = left[a:b]
    print("between", a)
    second = right[c:d]
    return first == second


def overwritten_record(left: str, right: str, a: int, b: int, c: int, d: int):
    first = left[a:b]
    a = c
    b = d
    second = right[a:b]
    return first == second


def stepped_record(left: str, right: str, a: int, b: int, c: int, d: int):
    return left[a:b:2] == right[c:d:2]


def zero_step_record(left: str, right: str, a: int, b: int, c: int, d: int):
    try:
        return left[a:b:0] == right[c:d]
    except ValueError:
        return "zero-step"


left = input()
right = input()
a = int(input())
b = int(input())
c = int(input())
d = int(input())
print(direct_record(left, right, a, b, c, d))
print(copied_record(left, right, a, b, c, d))
print(open_record(left, right, a, d))
print(reused_record(left, right, a, b, c, d))
print(interfering_record(left, right, a, b, c, d))
print(overwritten_record(left, right, a, b, c, d))
print(stepped_record(left, right, a, b, c, d))
print(zero_step_record(left, right, a, b, c, d))
# Physical overlap is valid for immutable strings. Reusing one source must not
# make the fusion confuse the independently evaluated bounds.
print(direct_record(left, left, a, b, c, d))
"""


@pytest.fixture(scope="module")
def record_case(tmp_path_factory):
    return build_case(tmp_path_factory.mktemp("two-slice-records"), SOURCE)


@pytest.mark.integration
@pytest.mark.parametrize(
    ("left", "right", "bounds"),
    [
        ("id:alpha|alpha", "tag:alpha|omega", (3, 8, 4, 9)),
        ("id:alpha|alpha", "tag:alpHa|omega", (3, 8, 4, 9)),
        ("xxhé🚀中yy", "abché🚀中zz", (2, 6, 3, 7)),
        ("xxhé🚀中yy", "abche🚀中zz", (2, 6, 3, 7)),
        ("aaaaaa", "aaaa", (0, 4, 2, 6)),
        ("abcdef", "abcdef", (-100, 100, 0, 6)),
        ("abcdef", "abcdef", (5, 2, 30, 40)),
        ("abcdef", "bcde", (-5, -1, 0, 4)),
        ("", "", (0, 4, -10, 10)),
        # The helper's bigint-bound fallback must preserve clipping semantics.
        ("hé🚀", "hé🚀", (-(2**100), 2**100, 0, 3)),
    ],
)
def test_two_slice_records_preserve_unicode_bounds_and_effects(
    record_case, left, right, bounds
):
    stdin = "\n".join((left, right, *(str(value) for value in bounds))) + "\n"
    record_case.compare(stdin)


@pytest.mark.integration
@pytest.mark.parametrize("function", ["direct_record", "copied_record", "open_record"])
def test_two_slice_record_comparison_removes_both_substrings(record_case, function):
    # Select specialized entries only. Generic callable entries must keep their
    # protocol fallback for objects that do not satisfy the entry contract.
    body = record_case.function_ir(rf"^mule_fn_{function}\.spec\.")
    assert "@mule_text_slice2_eq(" in body
    assert not re.search(r"@(?:mule_fast_getslice|mule\.fast\.getslice)\(", body)
    assert "@mule_text_slice_eq(" not in body


@pytest.mark.integration
@pytest.mark.parametrize(
    "function", ["reused_record", "interfering_record", "stepped_record"]
)
def test_two_slice_record_near_misses_keep_materialized_values(record_case, function):
    body = record_case.function_ir(rf"^mule_fn_{function}\.spec\.")
    assert "@mule_text_slice2_eq(" not in body
    assert re.search(r"@(?:mule_fast_getslice|mule\.fast\.getslice)\(", body)


class _TextComparisonImage:
    """Resolve this executable's static GOT and Text::Kind jump tables."""

    def __init__(self, case):
        self.data = case.executable.read_bytes()
        assert self.data[:6] == b"\x7fELF\x02\x01", "Requires little-endian ELF64"
        offset = struct.unpack_from("<Q", self.data, 32)[0]
        size, count = struct.unpack_from("<HH", self.data, 54)
        self.segments = []
        dynamic = None
        for index in range(count):
            kind, _, file_offset, address, _, length, _, _ = struct.unpack_from(
                "<IIQQQQQQ", self.data, offset + index * size
            )
            if kind == 1:
                self.segments.append((address, length, file_offset))
            elif kind == 2:
                dynamic = (address, length)
        assert dynamic, "Requires ELF dynamic relocation metadata"
        tags = dict(
            self.read(address, "<QQ") for address in range(dynamic[0], sum(dynamic), 16)
        )
        assert tags[9] == 24, "Requires Elf64_Rela entries"
        self.relocations = {}
        for address in range(tags[7], tags[7] + tags[8], tags[9]):
            slot, info, addend = self.read(address, "<QQq")
            if info & 0xFFFFFFFF == 8:  # R_X86_64_RELATIVE
                self.relocations[slot] = addend
        self.symbols = {
            int(match[1], 16): match[2]
            for match in re.finditer(
                r"^([0-9a-f]+) <([^>]+)>:", case.assembly, re.MULTILINE
            )
        }

    def read(self, address, format_):
        size = struct.calcsize(format_)
        for start, length, offset in self.segments:
            if start <= address and address + size <= start + length:
                return struct.unpack_from(format_, self.data, offset + address - start)
        raise AssertionError(f"Unmapped executable address: {address:x}")

    def call_operand(self, operands):
        if "(%rip)" not in operands:
            return operands
        slot = re.search(r"# ([0-9a-f]+)", operands)
        assert slot, f"Unresolved GOT slot: {operands}"
        address = int(slot[1], 16)
        pointer = self.relocations.get(address, self.read(address, "<Q")[0])
        # Dynamic libc relocations already carry their external symbol in the
        # disassembly. Static GOT entries contain the local function address.
        if pointer:
            assert pointer in self.symbols, f"Unresolved GOT target: {operands}"
            return f"<{self.symbols[pointer]}>"
        assert re.search(r"<(?:bcmp|memcmp)@", operands), operands
        return operands

    def kind_targets(self, instructions, index):
        # Rust Kind has InlineLatin1, Latin1, Ucs2 and Ucs4. Its loaded tag
        # indexes a signed-i32 relative table, not an arbitrary callable value.
        register = re.fullmatch(r"\*(%\w+)", instructions[index][2])
        assert register, instructions[index]
        register = register[1]
        previous = instructions[:index]
        add = max(
            i
            for i, (_, op, args) in enumerate(previous)
            if op == "add" and args.endswith(f",{register}")
        )
        base = re.fullmatch(rf"(%\w+),{register}", previous[add][2])
        assert base, previous[add]
        assert previous[add - 1][1] == "movslq"
        assert re.fullmatch(
            rf"(?:0x0)?\({base[1]},%\w+,4\),{register}", previous[add - 1][2]
        ), previous[add - 1]
        # The codepoint loop hoists the left dispatch destination and the
        # right table base outside its loop. Neither destination is a call.
        assert not any(
            args.split(" #")[0].rstrip().endswith(f",{register}")
            for _, _, args in previous[add + 1 :]
        ), f"Jump destination overwritten: {instructions[index]}"
        table_load = next(
            instruction
            for instruction in reversed(previous[: add - 1])
            if instruction[1] == "lea" and f",{base[1]} " in instruction[2]
        )
        address = re.search(r"# ([0-9a-f]+)", table_load[2])
        assert address, table_load
        table = int(address[1], 16)
        return [table + relative for relative in self.read(table, "<4i")]


def _before_slice_fallback(assembly, image):
    """Follow x86 instructions until the two-slice helper's generic fallback.

    This is intentionally local to this ABI helper. Its exact-text/fixnum arm
    returns before mule_fast_getslice. Bigint bounds and non-exact text enter
    that call instead. Do not use arbitrary allocation-free paths as evidence.
    """
    instructions = [
        (int(match[1], 16), match[2], match[3])
        for match in re.finditer(
            r"^[ \t]*([0-9a-f]+):[ \t]+(\S+)[ \t]*(.*?)$", assembly, re.MULTILINE
        )
    ]
    assert instructions, assembly
    positions = {address: index for index, (address, _, _) in enumerate(instructions)}
    pending = [0]
    visited = set()
    while pending:
        index = pending.pop()
        if index in visited or index >= len(instructions):
            continue
        visited.add(index)
        _, opcode, operands = instructions[index]
        if opcode.startswith("call"):
            operands = image.call_operand(operands)
        if "<mule_fast_getslice>" in operands:
            continue
        # Panic branches do not return to the successful text algorithm.
        if opcode.startswith("call") and re.search(r"panic|unwrap_failed", operands):
            continue
        if opcode == "jmp" and operands.startswith("*"):
            for address in image.kind_targets(instructions, index):
                assert address in positions, f"Nonlocal Kind destination: {address:x}"
                pending.append(positions[address])
            continue
        yield opcode, operands
        if opcode.startswith("ret") or opcode == "ud2":
            continue
        if opcode.startswith("j"):
            target = re.match(r"([0-9a-f]+)\s", operands)
            assert target, f"Unsupported indirect jump: {opcode} {operands}"
            address = int(target[1], 16)
            if address in positions:
                pending.append(positions[address])
            else:
                assert opcode == "jmp", operands
            if opcode == "jmp":
                continue
        pending.append(index + 1)


@pytest.mark.integration
def test_two_slice_runtime_compares_storage_without_substring_allocations(record_case):
    # Follow actual linked Rust code, including outlined range/codepoint helpers.
    # Cold generic slicing remains required for the bigint-bound oracle case.
    pending = ["mule_text_slice2_eq"]
    image = _TextComparisonImage(record_case)
    visited = set()
    native_compare = False
    native_load = False
    while pending:
        symbol = pending.pop()
        if symbol in visited:
            continue
        visited.add(symbol)
        assert len(visited) < 64, "Unexpectedly broad text-comparison helper graph"
        assembly = record_case.function_assembly(rf"^{re.escape(symbol)}$")
        for opcode, operands in _before_slice_fallback(assembly, image):
            native_load |= opcode.startswith("mov") and "(" in operands
            if not opcode.startswith("call") and opcode != "jmp":
                continue
            target = re.search(r"<([^>]+)>", operands)
            if opcode == "jmp" and target and "+0x" in target[1]:
                continue
            assert target, f"Unresolved helper call: {opcode} {operands}"
            callee = target[1]
            assert not re.search(
                r"alloc|realloc|getslice|retain|release|from_codepoints|from_latin1",
                callee,
            ), f"Unexpected substring/ownership work in {symbol}: {operands}"
            if re.match(r"(?:memcmp|bcmp)(?:@|$)", callee):
                native_compare = True
                continue
            pending.append(callee)
    assert native_load, "The actual range algorithm must read string storage"
    assert native_compare, "The Latin-1 range path must compare existing byte storage"

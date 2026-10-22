"""Length rewrites through source inference, entry specialization and real storage."""

import re

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

_RECORDS = (
    pytest.param("str(raw)", "str", "_text_length", id="text"),
    pytest.param("b'AB' * int(count)", "bytes", "_storage_length", id="bytes"),
    pytest.param("[raw] * int(count)", "list", "_storage_length", id="list"),
    pytest.param("tuple(raw)", "tuple", "_storage_length", id="tuple"),
)


@pytest.mark.parametrize(("expression", "annotation", "primitive"), _RECORDS)
def test_length_ir1_infers_runtime_record_storage(expression, annotation, primitive):
    source = (
        f"def record_size(raw, count):\n value = {expression}\n return len(value)\n"
    )
    compilation = compile_source(source)
    function = compilation.lambdas[1]
    assert list(known_calls(function, primitive))
    check_ir1_consistency(compilation.components)


def test_length_tuple_repetition_propagates_known_storage():
    compilation = compile_source(
        "def record_size(raw, count):\n"
        " value = (raw,) * int(count)\n"
        " return len(value)\n"
    )
    function = compilation.lambdas[1]
    assert list(known_calls(function, "_storage_length"))
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    "source",
    (
        "__safety__ = 3\ndef measure():\n return len('abc')\n",
        "def measure(len):\n return len('abc')\n",
        "len = None\ndef measure():\n return len('abc')\n",
        "len = None\ndel len\ndef measure():\n return len('abc')\n",
        "def measure():\n return len()\n",
        "def measure():\n return len('a', 'b')\n",
        "def measure():\n return len(obj='abc')\n",
        "def measure(flag):\n value = 'abc' if flag else None\n return len(value)\n",
        "def measure(flag):\n if flag:\n  value = 'abc'\n return len(value)\n",
        "def measure():\n value = 'abc'\n del value\n return len(value)\n",
    ),
)
def test_length_ir1_near_misses_preserve_checked_call(source):
    compilation = compile_source(source)
    function = next(fn for fn in compilation.lambdas if fn.name == "mule_fn_measure")
    assert not list(known_calls(function, "_text_length"))
    assert not list(known_calls(function, "_storage_length"))
    check_ir1_consistency(compilation.components)


def _instructions(assembly):
    return "\n".join(re.findall(r"^\s*[0-9a-f]+:\s+(.+)$", assembly, re.MULTILINE))


def _assert_text_length_return(assembly):
    instructions = _instructions(assembly)
    # These offsets are the runtime's physical Text layout. RDI is the receiver
    # and RAX is the integer return under the x86-64 System V ABI. Both storage
    # forms must feed their stored character count directly into fixnum tagging.
    for load in (r"mov\s+0x30\(%rdi\),%rax", r"movzbl\s+0x19\(%rdi\),%eax"):
        assert re.search(
            load + r"\nlea\s+0x1\(,%rax,2\),%rax\n(?:pop\s+%\w+\n)*ret",
            instructions,
        ), assembly


def _inline_length_values(optimized):
    # ValueBuffer is repr(C): the data pointer is at 24, the element count at
    # 32. Each SSA dependency must remain in this straight-line fast block.
    return list(
        re.finditer(
            r"(?P<address>%[\w.]+) = getelementptr[^\n]*i8, ptr (?P<value>%[\w.]+), i64 32\n\s*"
            r"(?P<count>%[\w.]+) = load i64, ptr (?P=address)[^\n]*\n\s*"
            r"(?P<shifted>%[\w.]+) = shl i64 (?P=count), 1\n\s*"
            r"(?P<tagged>%[\w.]+) = or(?: disjoint)? i64 (?P=shifted), 1\n\s*"
            r"(?P<result>%[\w.]+) = inttoptr i64 (?P=tagged) to ptr",
            optimized,
        )
    )


def _assert_physical_length_algorithm(case, primitive, annotation):
    """Follow receiver length data into the returned integer, including outlining."""
    if annotation in ("list", "tuple"):
        # These types use compiler-inlined ValueBuffer loads. Do not inspect the
        # Rust fallback's bytes arm as evidence for a list or tuple operation.
        return
    symbol = (
        "mule_text_length" if primitive == "_text_length" else "mule_storage_length"
    )
    assembly = case.function_assembly(rf"^{symbol}$")
    assert not re.search(
        r"<(?:mule_fast_len|mule_call|mule_call_resolved|mule_resolve_method|strlen)(?:>|\+)",
        assembly,
    ), assembly
    if primitive == "_text_length":
        _assert_text_length_return(assembly)
        return
    # Bytes retains Vec storage. Its count at 40 becomes integer_from_usize's ABI
    # argument. Stack restoration must not replace it before the tail transfer.
    assert re.search(
        r"mov\s+0x28\(%rdi\),%rdi\n"
        r"(?:add[^\n]+\n|pop[^\n]+\n)*jmp\s+[^\n]*<[^>]*integer_from_usize>",
        _instructions(assembly),
    ), assembly
    integer = case.function_assembly(r"numeric.*integer_from_usize$")
    # Follow that argument through its saved register to RAX. The ordinary-size
    # return does not call a boxer or dispatcher. Bigint allocation remains a
    # legitimate cold branch for sizes outside the fixnum range.
    assert re.search(
        r"mov\s+%rdi,%(?P<count>\w+)\n"
        r"(?:(?!(?:call|jmp)\s)(?![^\n]*,%(?P=count)\n)[^\n]+\n)*?"
        r"lea\s+0x1\(,%(?P=count),2\),%rax\n"
        r"(?:add[^\n]+\n|pop[^\n]+\n)*ret",
        _instructions(integer),
    ), integer


@pytest.mark.integration
@pytest.mark.parametrize(("expression", "annotation", "primitive"), _RECORDS)
def test_length_runtime_record_processing_uses_physical_length(
    tmp_path, expression, annotation, primitive
):
    source = (
        "def record_size(raw, count):\n"
        f" value = {expression}\n"
        " before = len(value)\n"
        " return before, value\n"
        "raw = input()\n"
        "count = int(input())\n"
        "size, value = record_size(raw, count)\n"
        "print(size, value)\n"
    )
    case = build_case(tmp_path, source)
    symbol = (
        "mule_text_length"
        if primitive == "_text_length"
        else "mule.fast.storage.length"
    )
    body = case.function_ir(r"mule_fn_record_size\.body$", optimized=False)
    assert f"@{symbol}(" in body
    assert "@mule_fast_len(" not in body
    optimized = case.function_ir(r"mule_fn_record_size\.body$")
    if annotation in ("list", "tuple"):
        lengths = _inline_length_values(optimized)
        assert lengths, optimized
        length = lengths[0]
    else:
        runtime_symbol = (
            "mule_text_length" if annotation == "str" else "mule_storage_length"
        )
        length = re.search(
            rf"(?P<result>%[\w.]+) = (?:tail )?call ptr @{runtime_symbol}"
            rf"\(ptr (?:nonnull )?(?P<value>%[\w.]+)\)",
            optimized,
        )
        assert length, optimized
    result = length["result"]
    joined = re.search(
        rf"(?P<result>%[\w.]+) = phi ptr [^\n]*\[ {re.escape(result)},",
        optimized,
    )
    if joined is not None:
        result = joined["result"]
    # The optimized record path returns both the length and the same receiver,
    # so a helper call from an unrelated function cannot satisfy this proof.
    stored = re.search(
        rf"store ptr {re.escape(result)}, ptr (?P<buffer>%[\w.]+),",
        optimized,
    )
    assert stored, optimized
    buffer = re.escape(stored["buffer"])
    second = re.search(
        rf"(?P<slot>%[\w.]+) = getelementptr[^\n]*i8, ptr {buffer}, i64 8",
        optimized,
    )
    assert second, optimized
    assert re.search(
        rf"store ptr {re.escape(length['value'])}, ptr {re.escape(second['slot'])},",
        optimized,
    ), optimized
    assert re.search(
        rf"call ptr @mule_sequence_tuple_owned\(i64 2, ptr (?:nonnull )?{buffer}\)",
        optimized,
    ), optimized
    assert "@mule_fast_len(" not in optimized
    _assert_physical_length_algorithm(case, primitive, annotation)
    for raw, count in (("", 0), ("plain", 3), ("caféλ😀", 7), ("x" * 80, 1)):
        case.compare(f"{raw}\n{count}\n")


@pytest.mark.integration
@pytest.mark.parametrize(("expression", "annotation", "primitive"), _RECORDS)
def test_length_entry_context_specializes_without_changing_open_entry(
    tmp_path, expression, annotation, primitive
):
    source = (
        f"def measure(value: {annotation}):\n"
        " return len(value)\n"
        "raw = input()\n"
        "count = int(input())\n"
        f"value = {expression}\n"
        "print(measure(value))\n"
    )
    compilation = compile_source(source)
    function = next(fn for fn in compilation.lambdas if fn.name == "mule_fn_measure")
    # The open IR1 entry cannot reinterpret arbitrary receivers as physical storage.
    assert not list(known_calls(function, primitive))
    assert compilation.facts is not None
    assert compilation.facts[function].variants, (
        f"No specialized entry facts for runtime {annotation} records. "
        "This is an entry-inference gap, not an LLVM symbol-selection mismatch."
    )
    case = build_case(tmp_path, source)
    specialized = case.function_ir(r"mule_fn_measure\.spec\.", optimized=False)
    general = case.function_ir(r"mule_fn_measure\.body$", optimized=False)
    symbol = (
        "mule_text_length"
        if primitive == "_text_length"
        else "mule.fast.storage.length"
    )
    assert f"@{symbol}(" in specialized
    assert "@mule_fast_len(" not in specialized
    assert "@mule_fast_len(" in general
    if annotation in ("list", "tuple"):
        assert _inline_length_values(case.optimized_llvm)
    else:
        assert re.search(rf"\bcall\s+ptr\s+@{symbol}\(", case.optimized_llvm)
    _assert_physical_length_algorithm(case, primitive, annotation)
    case.compare("caféλ😀\n4\n")


@pytest.mark.integration
def test_length_reloads_mutated_list_storage_and_preserves_element_identity(tmp_path):
    source = """
class Row:
 def __init__(self, text):
  self.text = text
def process(text):
 row = Row(text)
 rows = [row]
 alias = rows
 first = len(rows)
 alias.append(row)
 second = len(rows)
 removed = alias.pop()
 third = len(rows)
 print(first, second, third, removed is row, rows[0] is row, removed.text)
process(input())
"""
    case = build_case(tmp_path, source)
    body = case.function_ir(r"mule_fn_process\.body$", optimized=False)
    assert "@mule.fast.storage.length(" in body
    assert "@mule_fast_len(" not in body
    assert _inline_length_values(case.function_ir(r"mule_fn_process\.body$"))
    case.compare("owned-λ😀\n")


@pytest.mark.integration
def test_length_fallback_observes_override_errors_unbound_and_argument_effects(
    tmp_path,
):
    source = """
class Sized:
 def __len__(self):
  print('custom-length')
  return 13
class Rows(list):
 def __len__(self):
  print('overridden-length')
  return 19
def measure(value):
 return len(value)
def argument(label):
 print(label)
 return input()
def nullable(flag):
 value = input() if flag else None
 return len(value)
def unbound(flag):
 if flag:
  value = input()
 return len(value)
def deleted():
 value = input()
 del value
 return len(value)
print(measure(Sized()), measure(Rows([input()])))
try:
 nullable(False)
except TypeError:
 print('nullable-error')
try:
 unbound(False)
except UnboundLocalError:
 print('unbound-error')
try:
 deleted()
except UnboundLocalError:
 print('deleted-error')
try:
 len(argument('first'), argument('second'))
except TypeError:
 print('arity-error')
try:
 len(obj=argument('keyword'))
except TypeError:
 print('keyword-error')
try:
 len()
except TypeError:
 print('missing-error')
"""
    case = build_case(tmp_path, source)
    case.compare("row\ndeleted\nfirst-value\nsecond-value\nkeyword-value\n")


@pytest.mark.integration
def test_length_shadowed_deleted_and_safety_bindings_preserve_lookup(tmp_path):
    source = """
__safety__ = 3
def checked(value):
 return len(value)
def replacement(value):
 print('replacement', value)
 return 37
def shadowed(len, value):
 return len(value)
def local_deleted(value):
 len = replacement
 del len
 return len(value)
raw = input()
print(checked(raw), shadowed(replacement, raw))
try:
 local_deleted(raw)
except UnboundLocalError:
 print('deleted-callable')
len = replacement
print(checked(raw))
del len
print(checked(raw))
"""
    case = build_case(tmp_path, source)
    case.compare("λ😀\n")

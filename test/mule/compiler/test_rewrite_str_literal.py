"""Literal str rewrites with live record consumers and dynamic exclusions."""

import re

import pytest

from mule import ast
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import combinations
from mule.compiler.main import compile_source
from mule.compiler.node import Combination, Constant, GlobalVar, Ref
from test.mule.compiler.rewrite_support import build_case

LITERALS = [
    ("empty", "str()", ""),
    ("integer", "str(42)", "42"),
    ("negative", "str(-17)", "-17"),
    ("true", "str(True)", "True"),
    ("false", "str(False)", "False"),
    ("none", "str(None)", "None"),
    ("unicode", "str('café🐍')", "café🐍"),
    ("nul", r"str('a\0b')", "a\0b"),
]


def _function(compilation, name):
    return next(fn for fn in compilation.lambdas if fn.source_name == name)


def _pooled_text_values(llvm, body, text):
    """Follow concrete UTF-8 storage through its constructor and literal cache."""
    encoded = "".join(
        chr(byte) if 32 <= byte < 127 and byte not in (34, 92) else f"\\{byte:02X}"
        for byte in text.encode()
    )
    symbol = re.search(
        rf'^(@[\w.]+) = [^\n]*constant \[\d+ x i8\] c"{re.escape(encoded)}\\00"',
        llvm,
        re.MULTILINE,
    )
    assert symbol, text
    value = r"%[\w.$-]+"
    built = re.search(
        rf"({value}) = (?:tail )?call ptr @mule_literal_text\("
        rf"ptr(?: nonnull)? {re.escape(symbol.group(1))}, i64 {len(text.encode())}\)",
        body,
    )
    assert built, body
    values = {built.group(1)}
    slot = re.search(rf"store ptr {re.escape(built.group(1))}, ptr (@[\w.]+)", body)
    assert slot, body
    values.update(
        re.findall(rf"({value}) = load ptr, ptr {re.escape(slot.group(1))},", body)
    )
    for result, incoming in re.findall(rf"({value}) = phi ptr([^\n]+)", body):
        operands = re.findall(rf"\[ ({value}),", incoming)
        if operands and all(operand in values for operand in operands):
            values.add(result)
    return values


@pytest.mark.parametrize(("name", "expression", "expected"), LITERALS)
def test_str_literal_fold_remains_live_in_runtime_record(name, expression, expected):
    compilation = compile_source(
        f"def record_{name}(row):\n return row, {expression}\n"
    )
    fn = _function(compilation, f"record_{name}")
    result = fn.return_.result.use()
    assert isinstance(result, Combination)
    # The folded value must reach the tuple consumer, not just disappear with
    # an unused call. The other field remains an unknown runtime parameter.
    assert len(result.args) == 2
    label = result.args[1].use()
    assert isinstance(label, Ref)
    assert isinstance(label.leaf, Constant)
    assert isinstance(label.leaf.value, ast.TextLiteral)
    assert label.leaf.value.value == expected
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    ("prologue", "signature", "expression"),
    [
        ("", "record(str)", "str(42)"),
        ("str = replacement\n", "record()", "str(42)"),
        ("", "record()", "str(object=42)"),
        ("", "record()", "str('abc', 'utf-8')"),
        ("", "record()", "str(1, 2, 3, 4)"),
        ("__safety__ = 3\n", "record()", "str(42)"),
        ("", "record(value)", "str(value)"),
        ("", "record()", "str(1.25)"),
        ("", "record()", f"str({1 << 129})"),
    ],
)
def test_str_literal_near_misses_keep_conversion(prologue, signature, expression):
    compilation = compile_source(prologue + f"def {signature}:\n return {expression}\n")
    fn = _function(compilation, signature.partition("(")[0])
    conversion = fn.return_.result.use()
    assert isinstance(conversion, Combination)
    assert conversion in list(combinations(fn))
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    "prefix",
    [
        "mule_sequences_",
        "mule_format_",
        "mule_arguments_",
        "mule_native_class_",
        "mule_print_",
    ],
)
def test_str_literal_protocol_compilation_keeps_conversion(prefix):
    # Protocol compilation supplies the internal prefix. A user function whose
    # source identifier begins with it still receives the ordinary mule_fn_ prefix.
    compilation = compile_source("def record():\n return str(42)\n", prefix=prefix)
    fn = _function(compilation, "record")
    assert isinstance(fn.return_.result.use(), Combination)
    check_ir1_consistency(compilation.components)


def test_str_literal_module_initialization_keeps_builtin_lookup():
    compilation = compile_source("label = str(42)\nprint(label)\n")
    module = compilation.lambdas[0]
    assert any(
        isinstance(use, Ref)
        and isinstance(use.leaf, GlobalVar)
        and use.leaf.name == "str"
        for call in combinations(module)
        for use in call.fun.uses
    )
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
def test_str_literal_record_formatting_removes_conversion_not_runtime_consumer(
    tmp_path,
):
    definitions = "\n".join(
        f"def record_{name}(row):\n return f'{{row}}:{{{expression}}}'\n"
        for name, expression, _expected in LITERALS
    )
    source = (
        definitions
        + "\nrow = input()\n"
        + "\n".join(
            f"print(record_{name}(row))" for name, _expression, _expected in LITERALS
        )
    )
    case = build_case(tmp_path, source, optimize=3, lto=False)
    for row in ("record-19", "café🐍:42", ""):
        case.compare(stdin=row + "\n")
    for name, _expression, expected in LITERALS:
        body = case.function_ir(rf"^mule_fn_record_{name}\.body$")
        # Formatting the unknown row remains necessary. The label must instead
        # come directly from folded UTF-8 storage, not any conversion's result.
        assert not re.search(r'@"?mule\.call\.[01]"?\(', body)
        assert not re.search(r"@mule_call(?:_owned)?\(", body)
        assert "_decimal_protocol" not in body
        value = r"%[\w.$-]+"
        formatted = re.search(
            rf"({value}) = [^\n]*call [^\n]*@mule\.compiled\."
            rf"mule_format_format_value\.\d+\(ptr %globals, ptr ({value}),",
            body,
        )
        assert formatted, body
        additions = re.findall(
            rf"({value}) = (?:tail )?call ptr @mule_binary_reuse\("
            rf"i32 0, ptr(?: nonnull)? ({value}), ptr(?: nonnull)? ({value}),",
            body,
        )
        colon_values = _pooled_text_values(case.optimized_llvm, body, ":")
        prefixes = {
            result
            for result, left, right in additions
            if left == formatted.group(1) and right in colon_values
        }
        assert prefixes, body
        # The backend can merge a generic binary result with its speculative
        # fixnum arm. Follow that merge to the next concatenation operand.
        for result, incoming in re.findall(rf"({value}) = phi ptr([^\n]+)", body):
            operands = re.findall(rf"\[ ({value}),", incoming)
            if any(operand in prefixes for operand in operands):
                prefixes.add(result)
        if expected:
            label_values = _pooled_text_values(case.optimized_llvm, body, expected)
            completed = {
                result
                for result, left, right in additions
                if left in prefixes and right in label_values
            }
            assert completed, body
        else:
            completed = prefixes.copy()
        # For str(), adding its folded empty result disappears. The live
        # runtime row plus colon must still supply the returned record.
        for result, incoming in re.findall(rf"({value}) = phi ptr([^\n]+)", body):
            operands = re.findall(rf"\[ ({value}),", incoming)
            if any(operand in completed for operand in operands):
                completed.add(result)
        assert any(
            returned in completed
            for returned in re.findall(rf"ret ptr ({value})", body)
        ), body

    # Follow the actual concatenation consumer into its text-storage algorithm.
    # Other dynamic operand cases in binary remain valid cold fallbacks.
    reuse = case.function_assembly(r"^mule_binary_reuse$")
    binary_symbol = re.search(
        r"(?:call|jmp)\s+[0-9a-f]+ <([^>]*6object6binary[^>]*)>", reuse
    )
    assert binary_symbol, reuse
    binary = case.function_assembly(rf"^{re.escape(binary_symbol.group(1))}$")
    join_symbol = re.search(
        r"call\s+[0-9a-f]+ <([^>]*4text[^>]*4Text4join[^>]*)>", binary
    )
    assert join_symbol, binary
    join = case.function_assembly(rf"^{re.escape(join_symbol.group(1))}$")
    assert "<memcpy@" in join


@pytest.mark.integration
def test_str_fallback_preserves_hooks_errors_shadowing_and_argument_order(tmp_path):
    source = """
events = []

class Record:
    def __init__(self, text):
        self.text = text
    def __str__(self):
        events.append(self.text)
        return self.text

class InvalidRecord:
    def __str__(self):
        events.append('invalid')
        return 42

class BrokenRecord:
    def __str__(self):
        events.append('broken')
        raise ValueError('conversion')

def mark(text):
    events.append(text)
    return text

def render(record):
    return str(record)

def shadow(str, row):
    return str(row)

def replacement(row):
    events.append('replacement')
    return row + '!'

def mule_format_record(row):
    return row + ':' + str(42)

row = input()
record = Record(row)
original = render(record)
print(original)
record.text = row + ':changed'
print(render(record))
print(original)
print(shadow(replacement, row))
print(mule_format_record(row))
for record in [InvalidRecord(), BrokenRecord()]:
    try:
        print(render(record))
    except TypeError:
        print('TypeError')
    except ValueError:
        print('ValueError')
try:
    print(str(object=mark('keyword')))
except TypeError:
    print('keyword TypeError')
try:
    print(str(mark('first'), mark('second'), mark('third'), mark('fourth')))
except TypeError:
    print('arity TypeError')
builtin_str = str
str = replacement
print(str(row))
str = builtin_str
print(events)
"""
    case = build_case(tmp_path, source, optimize=3, lto=False)
    case.compare(stdin="runtime record\n")


@pytest.mark.integration
def test_str_literal_safety_three_keeps_dynamic_lookup(tmp_path):
    source = """
__safety__ = 3

def record(row):
    return row + ':' + str(42)

print(record(input()))
"""
    case = build_case(tmp_path, source, optimize=3, lto=False)
    case.compare(stdin="checked\n")
    body = case.function_ir(r"^mule_fn_record\.body$")
    # O3 inlines mule.call.1 and the live-global wrapper. Follow the binding
    # lookup's result into the remaining generic call instead of requiring
    # either wrapper to survive in optimized IR.
    value = r"%[\w.$-]+"
    lookup = re.search(
        rf"({value}) = (?:tail )?call ptr @mule_load_global\("
        r'ptr(?: nonnull)? @"mule.cell:__main__:str",',
        body,
    )
    assert lookup, body
    conversion = re.search(
        rf"({value}) = (?:tail )?call ptr @mule_call_owned\("
        rf"ptr(?: nonnull)? ({value}), i64 1, ptr(?: nonnull)? ({value})\)",
        body,
    )
    assert conversion, body
    callee = conversion.group(2)
    if callee != lookup.group(1):
        assert re.search(
            rf"{re.escape(callee)} = phi ptr[^\n]*"
            rf"\[ {re.escape(lookup.group(1))},",
            body,
        ), body
    # The generic conversion must supply the record concatenation's operand.
    assert re.search(
        rf"@mule_binary_reuse\([^\n]*ptr(?: nonnull)? "
        rf"{re.escape(conversion.group(1))},",
        body,
    ), body

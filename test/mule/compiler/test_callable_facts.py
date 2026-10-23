"""Callable information must survive normal arguments, results, and captures."""

import re

import pytest

from mule.compiler.ctype import FUNCTION_TYPE, PYTHON_INTEGER, FunctionType
from mule.compiler.main import compile_source
from mule.compiler.node import Combination
from test.mule.compiler.rewrite_support import build_case
from test.mule.compiler.test_modules import execute

CALLBACK_REDUCTION = """\
def transform(value):
    return (value & 255) * 3 + 1

def accumulate(callback, count):
    total = 0
    for index in range(count):
        total += callback(index & 255)
    return total

print(accumulate(transform, int(input())))
"""


def _function(compilation, name):
    return next(item for item in compilation.lambdas if item.source_name == name)


def _contexts(facts):
    return (facts, *facts.variants)


def test_callable_domains_keep_only_compatible_code_identity():
    compilation = compile_source(
        "def first(value):\n    return value\ndef second(value):\n    return value\n"
    )
    first = FunctionType(target=_function(compilation, "first"))
    second = FunctionType(target=_function(compilation, "second"))
    assert first.join(first) == first
    assert first.join(second) == FUNCTION_TYPE
    assert second.join(first) == FUNCTION_TYPE
    assert first.join(FUNCTION_TYPE) == FUNCTION_TYPE
    assert first.fits(FUNCTION_TYPE)
    assert not FUNCTION_TYPE.fits(first)
    assert not first.fits(second)


def test_callback_argument_retains_target_and_integer_result():
    compilation = compile_source(CALLBACK_REDUCTION)
    transform = _function(compilation, "transform")
    accumulate = _function(compilation, "accumulate")
    assert any(
        isinstance(node, Combination)
        and node.fun is not None
        and len(node.fun.uses) == 1
        and context.targets.get(node.fun.use()) is transform
        and result.fits(PYTHON_INTEGER)
        for context in _contexts(compilation.facts[accumulate])
        for node, result in context.types.items()
    ), "The native range branch must resolve its callback and derive its integer result"


def test_returned_callback_retains_target_through_forwarder():
    compilation = compile_source(
        """
def transform(value):
    return (value & 255) + 1
def invoke(callback, value):
    def forward(selected):
        return selected
    selected = forward(callback)
    if type(value) is int:
        return selected(value & 255)
    return 0
print(invoke(transform, int(input())))
"""
    )
    transform = _function(compilation, "transform")
    invoke = _function(compilation, "invoke")
    assert any(
        transform in context.targets.values()
        and context.return_type.fits(PYTHON_INTEGER)
        for context in _contexts(compilation.facts[invoke])
    ), "Returning a callback must preserve its callable identity"


def test_imported_callback_uses_its_actual_globals(tmp_path):
    result = execute(
        tmp_path,
        {
            "callback.py": """
offset = 7
def transform(value):
    return value + offset
""",
        },
        """
import callback
offset = 100
def invoke(function, value):
    return function(value)
print(invoke(callback.transform, 3))
callback.offset = 11
print(invoke(callback.transform, 3))
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "10\n14\n"


def test_closed_closure_derives_captured_parameter_from_callers():
    compilation = compile_source(
        """
def run(value):
    if type(value) is int:
        def calculate(offset):
            def add(step):
                return offset + step
            return add(1)
        return calculate(value & 255)
    return 0
print(run(int(input())))
"""
    )
    add = _function(compilation, "add")
    assert compilation.facts[add].return_type.fits(PYTHON_INTEGER)


def test_escaping_callback_cell_observes_external_replacement(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
def factory():
    def initial(value):
        return value + 1
    def replacement(value):
        return value + 100
    callback = initial
    def run(value):
        return callback(value)
    # Assignment makes this an actual shared cell, not a copied capture.
    callback = initial
    return run, replacement
run, replacement = factory()
print(run(3))
run.__closure__[0].cell_contents = replacement
print(run(3))
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "4\n103\n"


def _assert_native_callback_path(llvm):
    # main passes the module entry to the runtime. Follow that actual entry,
    # including inlining, rather than accepting an unused numeric helper.
    definitions = {}
    for function in re.findall(r"^define .*?^}", llvm, re.MULTILINE | re.DOTALL):
        header = re.search(r'@(?:"([^"\n]+)"|([^\s(]+))\(', function)
        definitions[header[1] or header[2]] = function
    assert re.search(
        r"@mule_run_protocols\(ptr [^\n]*@mule_module.fn", definitions["main"]
    )
    pending, reached = ["mule_module.fn"], set()
    while pending:
        symbol = pending.pop()
        if symbol in reached:
            continue
        reached.add(symbol)
        function = definitions[symbol]
        for quoted, plain in re.findall(
            r'\bcall\b[^\n]*@(?:"([^"\n]+)"|([\w.$]+))\(', function
        ):
            callee = quoted or plain
            if callee in definitions:
                pending.append(callee)

    for symbol in reached:
        function = definitions[symbol]
        sections = re.split(r"^([\w.$-]+):[^\n]*\n", function, flags=re.MULTILINE)
        blocks = dict(zip(sections[1::2], sections[2::2]))
        for label, block in blocks.items():
            masked = re.search(r"(%[\w.$-]+) = and i64 [^\n]+, 255\b", block)
            if masked is None:
                continue
            scaled = re.compile(rf"\b(?:mul|shl)\b[^\n]*\bi64 {re.escape(masked[1])},")
            pending, seen = [label], set()
            while pending:
                current = pending.pop()
                if current in seen:
                    continue
                seen.add(current)
                instructions = blocks[current]
                multiplication = scaled.search(instructions)
                prefix = (
                    instructions[: multiplication.start()]
                    if multiplication is not None
                    else instructions
                )
                if re.search(r"@mule_(?:call_owned|multiply|add)\(", prefix):
                    continue
                if multiplication is not None:
                    return
                pending.extend(re.findall(r"label %([\w.$-]+)", instructions))
    raise AssertionError(
        "The reachable callback path must scale the masked value with native "
        "arithmetic, without generic callback or arithmetic dispatch on that path"
    )


@pytest.mark.integration
def test_callback_reduction_uses_specialized_path_and_matches_python(tmp_path):
    case = build_case(tmp_path, CALLBACK_REDUCTION)
    for count in (0, 1, 257):
        case.compare(stdin=f"{count}\n")
    _assert_native_callback_path(case.optimized_llvm)


@pytest.mark.integration
def test_callback_environments_reassignment_and_dynamic_targets_match_python(tmp_path):
    case = build_case(
        tmp_path,
        """
def make_offsetter(offset):
    def apply(value=offset):
        return value + offset
    return apply
def invoke(callback, value):
    return callback(value)
def forward(callback):
    return callback
def invoke_default(callback):
    return callback()
def replace(callback, value):
    callback = make_offsetter(100)
    return callback(value)
def make_counter(start):
    value = start
    def advance(step):
        nonlocal value
        value += step
        return value
    return advance
value = int(input())
left = make_offsetter(7)
right = make_offsetter(11)
print(invoke(left, value), invoke(right, value))
print(forward(left)(value), forward(right)(value))
text = make_offsetter('tail')
print(invoke(text, 'head'))
print(left(), right(value=value))
print(invoke_default(left))
print(replace(left, value))
counter = make_counter(value)
print(invoke(counter, 1), invoke(counter, 3))
selected = left
if value < 0:
    selected = right
print(invoke(selected, value))
def original(amount):
    return amount + 1
def update_code_metadata(callback, amount):
    callback.__code__ = callback.__code__.replace(
        co_flags=callback.__code__.co_flags | 0x100
    )
    return callback(amount)
print(update_code_metadata(original, value))
selected = left
if value < 0:
    selected = original
print(invoke(selected, value))
class Adder:
    def __init__(self, offset):
        self.offset = offset
    def __call__(self, amount):
        return amount + self.offset
print(invoke(Adder(13), value))
""",
    )
    for value in (-3, 5):
        case.compare(stdin=f"{value}\n")

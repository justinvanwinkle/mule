"""Literal unary folds retain numeric domains and binary64 sign bits."""

import re
import struct

import pytest

from mule import ast
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import constant_literal
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import _llvm_functions, build_case


def _native_path_definitions(case, function):
    """Follow native calls from this source function, not unrelated native entries."""
    root = case.function_ir(
        rf"^(?:mule_fn_|mule[.]direct[.]mule_fn_){function}(?![.]__annotate__)(?:[._]|$)"
    )
    definitions = dict(_llvm_functions(root))
    pending = list(definitions.values())
    while pending:
        body = pending.pop()
        callees = re.findall(
            r'\b(?:call|invoke)\b[^\n]*?@"?(mule[.]native[.][^\s"(]+)', body
        )
        for callee in callees:
            if callee not in definitions:
                native = case.function_ir(rf"^{re.escape(callee)}$")
                definitions[callee] = native
                pending.append(native)
    return list(definitions.values())


def _returned_arithmetic(
    body, opcode, constant, predecessor=None, constant_position=None
):
    """Find arithmetic that consumes runtime data and contributes to the result."""
    value = r"%[-a-zA-Z$._0-9]+"
    parameters = set(re.findall(value, body.split("{", 1)[0]))
    assignments = dict(re.findall(rf"^\s*({value}) = (.+)$", body, re.MULTILINE))
    dependencies = {
        result: set(re.findall(value, instruction))
        for result, instruction in assignments.items()
    }
    # Returned boxed numbers carry their value in an explicitly stored payload.
    # Follow that store to its allocation, not merely to the allocation address.
    for stored, address in re.findall(
        rf"\bstore\s+(?:double|i64|ptr)\s+({value}),\s+ptr\s+({value})", body
    ):
        owner = address
        while assignments.get(owner, "").startswith("getelementptr"):
            owner = re.findall(value, assignments[owner])[0]
        dependencies.setdefault(owner, set()).add(stored)
    # The native integer adapter writes the scalar argument through an out slot.
    for argument, output in re.findall(
        rf"@mule_unbox_(?:i64|f64)\(ptr\s+({value}),\s+ptr\s+(?:nonnull\s+)?({value})\)",
        body,
    ):
        dependencies.setdefault(output, set()).add(argument)

    def depends_on(result, inputs):
        pending = [result]
        visited = set()
        while pending:
            operand = pending.pop()
            if operand in inputs:
                return True
            if operand not in visited:
                visited.add(operand)
                pending.extend(dependencies.get(operand, ()))
        return False

    returned = re.findall(rf"\bret\s+(?:ptr|double|i64)\s+({value})", body)
    for result, instruction in assignments.items():
        operation = re.fullmatch(
            rf"{opcode}\s+(?:double|i64)\s+([^,]+),\s*(\S+)", instruction
        )
        if operation is None:
            continue
        operands = operation.groups()
        positions = range(2) if constant_position is None else (constant_position,)
        if not any(
            not operands[position].startswith("%")
            and float(operands[position]) == constant
            for position in positions
        ):
            continue
        inputs = {predecessor} if predecessor is not None else parameters
        if depends_on(result, inputs) and any(
            depends_on(returned_value, {result}) for returned_value in returned
        ):
            return result
    return None


@pytest.mark.parametrize(
    ("expression", "expected"),
    [
        ("+17", 17),
        ("-17", -17),
        ("~17", -18),
        ("~0", -1),
        ("+True", 1),
        ("-True", -1),
        ("~True", -2),
        ("+False", 0),
        ("-False", 0),
        ("~False", -1),
        (f"+{2**128 - 1}", 2**128 - 1),
        # Integer unary + also reduces by identity, independently of fold limits.
        (f"+{2**128}", 2**128),
        (f"-{2**128 - 1}", -(2**128 - 1)),
        (f"~{2**127 - 1}", -(2**127)),
        ("+2.5", 2.5),
        ("-2.5", -2.5),
        ("+0.0", 0.0),
        ("-0.0", -0.0),
        ("+(-0.0)", -0.0),
        ("-(-0.0)", 0.0),
        ("+2.5j", complex(0.0, 2.5)),
        ("-2.5j", complex(-0.0, -2.5)),
        ("+0j", complex(0.0, 0.0)),
        ("-0j", complex(-0.0, -0.0)),
        ("+(-0j)", complex(-0.0, -0.0)),
        ("-(-0j)", complex(0.0, 0.0)),
    ],
)
def test_literal_unary_fold_reaches_runtime_calibration_operand(expression, expected):
    compilation = compile_source(
        f"def calibrate(sample):\n return sample + ({expression})\n"
    )
    function = next(
        function
        for function in compilation.lambdas
        if function.source_name == "calibrate"
    )
    for operation in ("positive", "negative", "invert"):
        assert not list(known_calls(function, f"%unary_{operation}"))
    addition = list(known_calls(function, "%add"))
    assert len(addition) == 1, "The runtime sample must still consume the coefficient"
    coefficient = constant_literal(addition[0].args[1])
    assert coefficient is not None
    assert type(coefficient.value) is type(expected)
    if isinstance(expected, complex):
        assert isinstance(coefficient, ast.ComplexLiteral)
        assert struct.pack(">dd", coefficient.value.real, coefficient.value.imag) == (
            struct.pack(">dd", expected.real, expected.imag)
        )
    elif isinstance(expected, float):
        assert isinstance(coefficient, ast.FloatLiteral)
        assert struct.pack(">d", coefficient.value) == struct.pack(">d", expected)
    else:
        # Unary bool operations produce int, not bool, even when the value is zero.
        assert isinstance(coefficient, ast.IntegerLiteral)
        assert coefficient.value == expected
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    ("expression", "operation"),
    [
        ("~1.5", "invert"),
        ("~0j", "invert"),
        ("+'12'", "positive"),
        ("-'12'", "negative"),
        ("~'12'", "invert"),
        ("+b'12'", "positive"),
        ("-None", "negative"),
        ("~(1, 2)", "invert"),
        (f"-{2**128}", "negative"),
        (f"~{2**128 - 1}", "invert"),
    ],
)
def test_invalid_domains_and_oversized_results_retain_unary_operation(
    expression, operation
):
    compilation = compile_source(
        f"def calibrate(sample):\n return sample + ({expression})\n"
    )
    function = next(
        function
        for function in compilation.lambdas
        if function.source_name == "calibrate"
    )
    assert list(known_calls(function, f"%unary_{operation}"))
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
def test_runtime_calibration_uses_folded_coefficients_without_unary_work(tmp_path):
    case = build_case(
        tmp_path,
        """
from mule_types import float64, int64

def calibrate(sample: float64) -> float64:
    return sample * (-2.5) + (+0.25)

def align(sample: int64) -> int64:
    return sample & (~15)

def rotate(sample):
    return sample * (-2j)

def signed_zero(index):
    # Select calibration defaults at runtime so each sign reaches an observer.
    return [-0.0, +(-0.0), -(-0.0), -0j, +(-0j), -(-0j)][index]

raw = input()
for text in raw.split(','):
    sample = float(text)
    phase = rotate(sample)
    print(calibrate(sample), align(int(sample)), repr(phase.real), repr(phase.imag))
for index in range(int(input())):
    zero = signed_zero(index)
    # Scalar repr round-trips finite binary64 values, including each zero sign.
    # Complex repr spelling is a separate contract from component preservation.
    print(repr(zero.real), repr(zero.imag))
""",
    )
    # Inspect all retained module definitions, including native entries and
    # compiler-generated helpers. A moved unary call must not satisfy the test.
    module = case.function_ir(r".*")
    assert not re.search(
        r"\b(?:call|invoke)\b[^\n]*@\"?(?:mule[.](?:fast|fixnum)[.]unary|mule_unary|"
        r"mule_sequences_(?:positive|negative|invert))",
        module,
    )
    calibration = _native_path_definitions(case, "calibrate")
    assert all(not re.search(r"\bfneg\b", body) for body in calibration)
    assert any(
        (
            (scaled := _returned_arithmetic(body, "fmul", -2.5)) is not None
            and _returned_arithmetic(body, "fadd", 0.25, scaled) is not None
        )
        or (
            # LLVM can canonicalize x * -2.5 + 0.25 to 0.25 - x * 2.5.
            (scaled := _returned_arithmetic(body, "fmul", 2.5)) is not None
            and _returned_arithmetic(body, "fsub", 0.25, scaled, 0) is not None
        )
        for body in calibration
    ), "Calibration must return a runtime sample scaled by -2.5 and offset by 0.25"
    # The runtime-dependent mask must survive even though ~15 is already -16.
    alignment = _native_path_definitions(case, "align")
    assert any(_returned_arithmetic(body, "and", -16) for body in alignment), (
        "Alignment must return its runtime argument masked with -16"
    )
    for samples in ("0,1,16,31", "-0.0,-1,-16,-31", "0.125,4096.5,-2048.25"):
        case.compare(samples + "\n6\n")


@pytest.mark.integration
def test_rejected_domains_and_overloaded_unary_effects_match_oracle(tmp_path):
    case = build_case(
        tmp_path,
        """
def bad_float():
    return ~1.5
def bad_complex():
    return ~0j
def bad_text():
    return -'12'
for operation in (bad_float, bad_complex, bad_text):
    try:
        operation()
    except TypeError:
        print('type error')

def oversized(sample):
    return sample + (+340282366920938463463374607431768211456), sample + (~340282366920938463463374607431768211455)
print(oversized(int(input())))

events = []
result = [input()]
class Sensor:
    def __pos__(self):
        events.append('positive')
        return result
    def __neg__(self):
        events.append('negative')
        result.append('changed')
        return result
    def __invert__(self):
        events.append('invert')
        raise ValueError('invalid sensor bits')

def consume(value):
    first = +value
    second = -value
    print(first is result, second is result, first)
    try:
        return ~value
    finally:
        print('cleanup', result)

sensor = Sensor()
try:
    consume(sensor)
except ValueError as error:
    print(str(error))
print(events, result)
""",
    )
    for sample in ("0", "-17", "99999999999999999999999999999999999999999"):
        case.compare(sample + "\nsample\n")

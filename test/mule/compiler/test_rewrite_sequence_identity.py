"""Sequence identities must forward exact immutable records, not copy them."""

import re
import subprocess
from textwrap import dedent

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case
from test.python_host import python_host

_OPERATIONS = (
    ("add_right", "record + {empty}"),
    ("add_left", "{empty} + record"),
    ("repeat_right", "record * 1"),
    ("repeat_left", "1 * record"),
)


def _record_source():
    # Long records prevent singleton-text propagation from replacing the value.
    # The input selects the payload at runtime; none of the identities receives
    # a compile-time-known value.
    source = "def process(select):\n"
    for kind, empty in (("text", "''"), ("bytes", "b''")):
        for operation, expression in _OPERATIONS:
            source += (
                f" def identity_{kind}_{operation}(record):\n"
                f"  return {expression.format(empty=empty)}\n"
            )
    for kind, prefix in (("text", ""), ("bytes", "b")):
        first = "customer:alpha;" * 8
        second = "customer:beta;" * 9
        source += f" {kind} = {prefix}{first!r} if select else {prefix}{second!r}\n"
    source += " retained = []\n"
    for kind in ("text", "bytes"):
        for operation, _ in _OPERATIONS:
            source += (
                f" result = identity_{kind}_{operation}({kind})\n"
                " retained.append(result)\n"
                f" print(result is {kind}, len(result), result)\n"
            )
    # Retained results outlive replacements of both source bindings and entries.
    source += (
        " original = retained[0]\n"
        " text = 'replacement'\n"
        " bytes = b'replacement'\n"
        " retained[0] = None\n"
        " print(original is retained[1], original == retained[2])\n"
        " print(retained[4] is retained[7])\n"
        "process(bool(int(input())))\n"
    )
    return source


def test_sequence_identities_eliminate_ir1_operations_on_runtime_selected_records():
    compilation = compile_source(_record_source())
    for kind in ("text", "bytes"):
        for operation, _ in _OPERATIONS:
            name = f"identity_{kind}_{operation}"
            function = next(
                function
                for function in compilation.lambdas
                if function.source_name == name
            )
            assert not list(known_calls(function, "%add")), name
            assert not list(known_calls(function, "%multiply")), name
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
def test_sequence_identities_preserve_receiver_and_retained_aliases(tmp_path):
    case = build_case(tmp_path, _record_source())
    case.compare("0\n")
    case.compare("1\n")


@pytest.mark.integration
def test_sequence_identities_forward_records_without_optimized_copy_work(tmp_path):
    case = build_case(tmp_path, _record_source())
    for kind in ("text", "bytes"):
        for operation, _ in _OPERATIONS:
            name = f"identity_{kind}_{operation}"
            body = case.function_ir(rf"^mule_fn_process\.<locals>\.{name}\.body$")
            assert "ret ptr" in body, body
            # Follow every call in the actual forwarding body. No generic call,
            # allocation, text/bytes helper, or hidden operation is acceptable.
            # Lifetime bookkeeping, traceback TLS lookup, safepoints, and
            # exception-state checks do not implement concat/repeat.
            calls = re.findall(r'\bcall\b[^\n]*?@(?:"([^"]+)"|([^\s(]+))\(', body)
            unexpected = []
            for quoted, unquoted in calls:
                callee = quoted or unquoted
                if callee not in {
                    "mule_retain",
                    "mule_release",
                    "mule_destroy",
                    "mule_error_pending",
                    "mule_frame_head_slot",
                    "mule_safepoint",
                    "mule_traceback_here",
                } and not callee.startswith("llvm."):
                    unexpected.append(callee)
            assert not unexpected, f"{name}: {unexpected}\n{body}"
            assert not re.search(r"\b(?:memcpy|memmove)\b", body), body
            assert not re.search(r"\b(?:call|invoke)\b[^\n]*?%[^\n]*\(", body), body
    case.compare("0\n")
    case.compare("1\n")


_BOUNDARIES = dedent(
    """
    def apply(record, operation):
        if operation == 0:
            return record + ''
        if operation == 1:
            return '' + record
        if operation == 2:
            return record * 1
        return 1 * record

    def apply_bytes(record, operation):
        if operation == 0:
            return record + b''
        if operation == 1:
            return b'' + record
        if operation == 2:
            return record * 1
        return 1 * record

    class Text(str):
        def __add__(self, other):
            print('Text.add', other)
            return 'added'
        def __radd__(self, other):
            print('Text.radd', other)
            return 'reverse-added'
        def __mul__(self, other):
            print('Text.mul', other)
            return 'repeated'
        def __rmul__(self, other):
            print('Text.rmul', other)
            return 'reverse-repeated'

    class Binary(bytes):
        def __add__(self, other):
            print('Binary.add', other)
            return b'added'
        def __radd__(self, other):
            print('Binary.radd', other)
            return b'reverse-added'
        def __mul__(self, other):
            print('Binary.mul', other)
            return b'repeated'
        def __rmul__(self, other):
            print('Binary.rmul', other)
            return b'reverse-repeated'

    class InheritedText(str):
        pass

    class InheritedBytes(bytes):
        pass

    def observe(value, binary, operation):
        try:
            if binary:
                result = apply_bytes(value, operation)
            else:
                result = apply(value, operation)
            print(result, result is value, type(result) is type(value))
        except TypeError:
            print('TypeError')

    operation = int(input())
    raw = input()
    observe(Text(raw), False, operation)
    observe(Binary(b'payload'), True, operation)
    observe(InheritedText(raw), False, operation)
    observe(InheritedBytes(b'payload'), True, operation)
    observe(3.5, False, operation)
    observe([raw], False, operation)
    observe(raw, True, operation)
    observe(b'payload', False, operation)

    # Mutable sequences must allocate for repeat-by-one. Changing the result
    # must not change the input, even though immutable inputs can be forwarded.
    items = [raw]
    duplicate = apply(items, 2)
    duplicate.append('new')
    print(items, duplicate, duplicate is items)

    class Count:
        def __index__(self):
            print('index')
            return 1

    print(raw * Count())
    print(Count() * raw)
    for count in [False, True, 0, -1, 2]:
        print(raw * count, count * raw)
    try:
        print(raw * 1.0)
    except TypeError:
        print('TypeError')

    def record():
        print('record')
        return raw

    def one():
        print('one')
        return 1

    def empty():
        print('empty')
        return ''

    print(record() + empty())
    print(empty() + record())
    print(record() * one())
    print(one() * record())

    def fail():
        print('fail')
        raise ValueError('record failed')

    try:
        print(fail() * one())
    except ValueError:
        print('caught')
    try:
        print(one() * fail())
    except ValueError:
        print('caught')
    """
)


@pytest.mark.integration
@pytest.mark.parametrize("operation", range(4))
def test_sequence_identities_preserve_subclass_dispatch_type_errors_and_effects(
    tmp_path, operation
):
    case = build_case(tmp_path, _BOUNDARIES)
    case.compare(f"{operation}\ncustomer:runtime-record\n")


@pytest.mark.integration
def test_bytes_repetition_rejects_overflow_before_materialization(tmp_path):
    case = build_case(
        tmp_path,
        """
for reverse in (False, True):
    for data, count in (
        (b'abcd', 1 << 62),
        (b'x', 1 << 63),
        (b'', 1 << 100),
        (b'abcd', -(1 << 63) - 1),
    ):
        try:
            result = count * data if reverse else data * count
            print('unexpected result', len(result))
        except (OverflowError, MemoryError) as error:
            print(type(error).__name__)
""",
    )
    # Bound a failed regression instead of exhausting memory on an impossible size.
    for command in (
        [python_host(), str(case.source_path)],
        [str(case.executable)],
    ):
        result = subprocess.run(
            ["/bin/sh", "-c", 'ulimit -v 524288; exec "$@"', "limited", *command],
            capture_output=True,
            text=True,
            timeout=30,
            check=False,
        )
        assert result.returncode == 0, result.stderr
        assert result.stdout == "OverflowError\n" * 8

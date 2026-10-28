"""Record sizing must not construct text or bytes merely to measure them."""

import re

import pytest

from mule.compiler.ctype import BYTES_TYPE, INTEGER, TEXT_TYPE
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import lvar_type
from mule.compiler.ir1util import known_calls
from mule.compiler.locall import specialized_ir1_context
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

_FUSED_FORMS = {
    "direct": "return len(name + payload)",
    "nested": "return len((name + DELIMITER) + payload + TERMINATOR)",
    "local": "record = name + payload\n    return len(record)",
    "annotated": "record: KIND = name + payload\n    return len(record)",
    "reassigned": (
        "record = name + payload\n"
        "    name = payload\n"
        "    payload = name\n"
        "    return len(record)"
    ),
}


def _literal(kind, text):
    return repr(text if kind == "str" else text.encode())


def _expression(kind, form):
    return (
        _FUSED_FORMS[form]
        .replace("KIND", kind)
        .replace("DELIMITER", _literal(kind, ": "))
        .replace("TERMINATOR", _literal(kind, "\n"))
    )


def _flow_source(kind, body, *, safety=1):
    # Branches establish exact builtin types without supplying compiler facts.
    # Different values and lengths prevent literal-only folding of the result.
    return (
        f"__safety__ = {safety}\n"
        "def record_size(flag):\n"
        f"    name = {_literal(kind, 'sensor')} if flag else {_literal(kind, 'tag')}\n"
        f"    payload = {_literal(kind, 'ready')} if flag else {_literal(kind, 'pending')}\n"
        f"    {body}\n"
    )


@pytest.mark.parametrize("kind", ["str", "bytes"])
@pytest.mark.parametrize("form", _FUSED_FORMS)
def test_record_length_fusion_uses_normal_flow_facts(kind, form):
    compilation = compile_source(_flow_source(kind, _expression(kind, form)))
    check_ir1_consistency(compilation.components)
    record_size = next(
        fn for fn in compilation.lambdas if fn.source_name == "record_size"
    )
    additions = list(known_calls(record_size, "%add"))
    assert additions, "runtime-dependent record lengths must still be added"
    assert all(lvar_type(call.lvar).fits(INTEGER) for call in additions)
    primitive = "_text_length" if kind == "str" else "_storage_length"
    assert len(list(known_calls(record_size, primitive))) >= 2


@pytest.mark.parametrize("kind", ["str", "bytes"])
@pytest.mark.parametrize("exclusion", ["multiuse", "safety"])
def test_record_length_fusion_retains_ineligible_concatenation(kind, exclusion):
    if exclusion == "multiuse":
        body = "record = name + payload\n    return len(record), record"
    else:
        body = "return len(name + payload)"
    compilation = compile_source(
        _flow_source(kind, body, safety=3 if exclusion == "safety" else 1)
    )
    check_ir1_consistency(compilation.components)
    record_size = next(
        fn for fn in compilation.lambdas if fn.source_name == "record_size"
    )
    sequence_type = TEXT_TYPE if kind == "str" else BYTES_TYPE
    assert any(
        lvar_type(call.lvar).fits(sequence_type)
        for call in known_calls(record_size, "%add")
    ), "the original concatenation remains necessary or explicitly ineligible"
    if exclusion == "multiuse":
        primitive = "_text_length" if kind == "str" else "_storage_length"
        assert len(list(known_calls(record_size, primitive))) == 2
        assert any(
            lvar_type(call.lvar).fits(INTEGER)
            for call in known_calls(record_size, "%add")
        )


def test_sequence_length_transforms_and_flushing_cover_containers_slices_and_repeats():
    source = """
def list_concat_len(a: list, b: list) -> int:
    combined: list = a + b
    return len(combined)

def tuple_concat_len(flag: bool) -> int:
    left = (1, 2) if flag else (1, 2, 3)
    right = (4, 5, 6) if flag else (4,)
    combined: tuple = left + right
    return len(combined)

def slice_len_bounded(s: str, i: int) -> int:
    if 0 <= i and i <= len(s):
        sub: str = s[i:]
        return len(sub)
    return 0

def slice_len_general(s: str, a: int, b: int) -> int:
    if a >= 0 and b >= 0:
        sub: str = s[a:b]
        return len(sub)
    return 0

def list_fill_len(n: int) -> int:
    if 0 <= n and n <= 1024:
        buf: list = [0] * n
        return len(buf)
    return 0

def text_repeat_len(s: str, n: int) -> int:
    if 0 <= n and n <= 64:
        rep: str = s * n
        return len(rep)
    return 0

def symbolic_constraint_fold(a: str) -> bool:
    t = a + "xyz"
    if len(a) >= 4:
        return len(t) >= 7
    return True
"""
    compilation = compile_source(source)
    check_ir1_consistency(compilation.components)
    by_name = {fn.source_name: fn for fn in compilation.lambdas}

    list_fn = by_name["list_concat_len"]
    with specialized_ir1_context(list_fn, compilation.facts[list_fn].variants[0]):
        list_adds = list(known_calls(list_fn, "%add"))
        assert list_adds and all(lvar_type(c.lvar).fits(INTEGER) for c in list_adds)
        assert len(list(known_calls(list_fn, "_storage_length"))) == 2

    tuple_fn = by_name["tuple_concat_len"]
    tuple_adds = list(known_calls(tuple_fn, "%add"))
    assert tuple_adds and all(lvar_type(c.lvar).fits(INTEGER) for c in tuple_adds)
    assert len(list(known_calls(tuple_fn, "_storage_length"))) == 2

    slice_bounded_fn = by_name["slice_len_bounded"]
    with specialized_ir1_context(
        slice_bounded_fn, compilation.facts[slice_bounded_fn].variants[0]
    ):
        assert not list(known_calls(slice_bounded_fn, "%getslice"))
        assert not list(known_calls(slice_bounded_fn, "_text_slice"))
        assert list(known_calls(slice_bounded_fn, "%subtract"))

    slice_general_fn = by_name["slice_len_general"]
    with specialized_ir1_context(
        slice_general_fn, compilation.facts[slice_general_fn].variants[0]
    ):
        assert not list(known_calls(slice_general_fn, "%getslice"))
        assert not list(known_calls(slice_general_fn, "_text_slice"))
        assert list(known_calls(slice_general_fn, "%subtract"))

    fill_fn = by_name["list_fill_len"]
    with specialized_ir1_context(fill_fn, compilation.facts[fill_fn].variants[0]):
        assert not list(known_calls(fill_fn, "%_list_fill"))
        assert not list(known_calls(fill_fn, "_storage_length"))

    repeat_fn = by_name["text_repeat_len"]
    with specialized_ir1_context(repeat_fn, compilation.facts[repeat_fn].variants[0]):
        muls = list(known_calls(repeat_fn, "%multiply"))
        assert muls and all(lvar_type(c.lvar).fits(INTEGER) for c in muls)

    sym_fn = by_name["symbolic_constraint_fold"]
    with specialized_ir1_context(sym_fn, compilation.facts[sym_fn].variants[0]):
        assert not list(known_calls(sym_fn, "%compare_greater_equal")) or all(
            c.args[1].uses[0].leaf.value.value == 4
            for c in known_calls(sym_fn, "%compare_greater_equal")
        )


def _runtime_source(kind, *, safety=1):
    definitions = "\n\n".join(
        f"def {form}_record_size(name: {kind}, payload: {kind}):\n"
        f"    {_expression(kind, form)}"
        for form in _FUSED_FORMS
    )
    preparation = "name = input()\npayload = input()\n"
    if kind == "bytes":
        preparation += (
            "name = name.encode('utf-8')\npayload = payload.encode('utf-8')\n"
        )
    return (
        f"__safety__ = {safety}\n"
        + definitions
        + f"""

def retained_record(name, payload):
    record = name + payload
    return len(record), record

def captured_record(name, payload):
    record = name + payload
    def inspect():
        return record
    return len(record), inspect()

def annotated_record(name, payload):
    record: {kind} = name + payload
    return len(record)

def side_effecting_record(name, payload):
    events = []
    def part(value, label):
        events.append(label)
        if value == {_literal(kind, "!raise")}:
            raise ValueError('invalid record field')
        return value
    try:
        size = len(part(name, 'name') + part(payload, 'payload'))
        return size, events
    except ValueError:
        return 'error', events

def incompatible_record(name):
    events = []
    def payload():
        events.append('payload')
        return 42
    try:
        return len(name + payload()), events
    except TypeError:
        return 'type-error', events

"""
        + preparation
        + """
print(direct_record_size(name, payload))
print(nested_record_size(name, payload))
print(local_record_size(name, payload))
print(annotated_record_size(name, payload))
print(reassigned_record_size(name, payload))
print(retained_record(name, payload))
print(captured_record(name, payload))
print(annotated_record(name, payload))
print(side_effecting_record(name, payload))
print(incompatible_record(name))
"""
    )


def _shadowed_length_source(kind, safety):
    preparation = "name = input()\npayload = input()\n"
    if kind == "bytes":
        preparation += (
            "name = name.encode('utf-8')\npayload = payload.encode('utf-8')\n"
        )
    return (
        f"__safety__ = {safety}\n"
        "def shadowed_length(name, payload):\n"
        "    def len(value):\n"
        "        return value\n"
        "    return len(name + payload)\n"
        + preparation
        + "print(shadowed_length(name, payload))\n"
    )


@pytest.fixture(scope="module", params=["str", "bytes"])
def optimized_records(request, tmp_path_factory):
    kind = request.param
    case = build_case(
        tmp_path_factory.mktemp(f"concat-length-{kind}"), _runtime_source(kind)
    )
    return kind, case


@pytest.mark.integration
@pytest.mark.parametrize(
    "stdin",
    [
        "sensor\nready\n",
        "\n\n",
        "café🚀\n状态=已完成\n",
        "name\n" + "large payload αβ🚀" * 256 + "\n",
        "!raise\nnever-read-as-a-field\n",
        "sensor\n!raise\n",
    ],
    ids=["ascii", "empty", "unicode", "large", "left-error", "right-error"],
)
def test_optimized_record_sizing_preserves_results_order_and_errors(
    optimized_records, stdin
):
    _kind, case = optimized_records
    case.compare(stdin)


def _outgoing_symbols(assembly):
    return re.findall(r"\b(?:call\w*|jmp\w*)\s+[^\n]*<([^>]+)>", assembly)


def _assert_text_length_reads_and_tags_storage(case):
    helper = case.function_assembly(r"^mule_text_length$")
    # Rust's physical text variants select stored character lengths, not UTF-8
    # byte counts. The external error edge is allowed, but no data-building
    # helper may be hidden behind the selected length primitive.
    assert re.search(r"\bmov\w*\s+[^\n]*\(%rdi\),%[a-z0-9]+", helper), helper
    assert re.search(r"\blea\s+0x1\(,%[a-z0-9]+,2\),%[a-z0-9]+", helper), helper
    assert not re.search(r"\bcall\w*\s+\*", helper), helper
    assert all(
        target.startswith("mule_text_length+") or "text_storage_error" in target
        for target in _outgoing_symbols(helper)
    ), helper


def _assert_bytes_length_reads_and_tags_storage(case):
    helper = case.function_assembly(r"^mule_storage_length$")
    # The bytes storage arm forwards the loaded length, not the bytes object,
    # into the integer constructor. Its small-size arm must return a tagged
    # integer before the legitimate bigint overflow arm performs allocations.
    assert re.search(r"\bmov\w*\s+[^\n]*\(%rdi\),%rdi", helper), helper
    assert re.search(r"\bjmp\s+[^\n]*<[^>]*integer_from_usize[^>]*>", helper), helper
    integer = case.function_assembly(r"numeric.*integer_from_usize")
    first_return = re.search(r"\bret\w*\b", integer)
    assert first_return is not None, integer
    small_length = integer[: first_return.end()]
    assert re.search(r"\b(?:shr|cmp|test)\w*\s", small_length), small_length
    assert re.search(r"\bj(?:ne|a|ae|b|be)\s", small_length), small_length
    assert re.search(r"\blea\s+0x1\(,%[a-z0-9]+,2\),%rax", small_length), small_length
    assert not re.search(r"\bcall\w*\s", small_length), small_length


def _called_record_body(case, form):
    # Follow the guarded direct call's real target. A proven general entry can
    # use .body, while other contexts use .spec or .numeric entries. Requiring
    # a particular suffix would test compiler bookkeeping, not optimization.
    function = f"mule_fn_{form}_record_size"
    wrapper = case.inlined_function_ir(rf"^mule\.direct\.{re.escape(function)}\.")
    targets = {
        quoted or unquoted
        for quoted, unquoted in re.findall(
            r'\b(?:call|invoke)\b[^\n]*?@(?:"([^"\n]+)"|([^\s(]+))\(', wrapper
        )
        if (quoted or unquoted).startswith(function + ".")
    }
    specialized = {
        target for target in targets if ".spec." in target or ".numeric." in target
    }
    selected = specialized or targets
    if not selected:
        # LLVM can inline the complete source operation into its direct-call
        # wrapper. The same efficiency assertions then apply to that code.
        return wrapper
    return "\n\n".join(
        case.function_ir(rf"^{re.escape(target)}$") for target in sorted(selected)
    )


@pytest.mark.integration
@pytest.mark.parametrize("form", _FUSED_FORMS)
def test_optimized_record_sizing_eliminates_concatenation_not_just_dispatch(
    optimized_records, form
):
    kind, case = optimized_records
    body = _called_record_body(case, form)
    primitive = "mule_text_length" if kind == "str" else "mule_storage_length"
    assert f"@{primitive}(" in body, (
        f"{form} {kind} record sizing did not replace concatenation with operand lengths\n"
        f"Selected calls: {re.findall(r'@(?:\"[^\"]+\"|[^\s(]+)\(', body)!r}"
    )
    assert re.search(r"\badd(?:\s+nuw|\s+nsw)*\s+i64\b", body), body
    for forbidden in (
        "@mule_fast_len(",
        "@mule_binary(",
        "@mule_binary_reuse(",
        "@mule_call(",
        "TextStorage.__add__",
        "BytesStorage.__add__",
        "@mule_text_join(",
        "@mule_text_from_codepoints(",
    ):
        assert forbidden not in body, body


@pytest.mark.integration
def test_record_length_primitives_read_existing_storage_without_concatenation(
    optimized_records,
):
    kind, case = optimized_records
    if kind == "str":
        _assert_text_length_reads_and_tags_storage(case)
    else:
        _assert_bytes_length_reads_and_tags_storage(case)


@pytest.mark.integration
@pytest.mark.parametrize("kind", ["str", "bytes"])
def test_safety_three_record_sizing_preserves_real_concatenation_semantics(
    tmp_path, kind
):
    case = build_case(tmp_path, _runtime_source(kind, safety=3))
    case.compare("café🚀\n状态=已完成\n")
    case.compare("sensor\n!raise\n")


@pytest.mark.integration
@pytest.mark.parametrize("kind", ["str", "bytes"])
@pytest.mark.parametrize("safety", [1, 3])
def test_shadowed_len_retains_its_actual_noninteger_return(tmp_path, kind, safety):
    # Keep this lookup/inference boundary independent from the positive sizing
    # program so a return-type defect cannot hide all its other observations.
    case = build_case(tmp_path, _shadowed_length_source(kind, safety))
    case.compare("sensor\nready\n")

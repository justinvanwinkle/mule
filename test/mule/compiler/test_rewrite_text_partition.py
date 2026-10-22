"""Runtime record parsing exercises exact-text partition rewrites and fallbacks."""

import pytest

from mule.compiler.vops import ARGS_TAG, DATA_OFFSET
from test.mule.compiler.rewrite_native_trace import trace_native
from test.mule.compiler.rewrite_support import build_case

pytestmark = pytest.mark.integration


RECORD_PARSER = """
def header_fields(record: str, separator: str):
    name, marker, payload = record.partition(separator)
    return name, marker, payload

def trailer_fields(record: str, separator: str):
    prefix, marker, suffix = record.rpartition(separator)
    return prefix, marker, suffix

def retained_header(record: str, separator: str):
    fields = record.partition(separator)
    return fields

def retained_trailer(record: str, separator: str):
    fields = record.rpartition(separator)
    return fields

record = input()
separator = input()
for operation in (
    header_fields,
    trailer_fields,
    retained_header,
    retained_trailer,
):
    try:
        print(operation(record, separator))
    except (TypeError, ValueError) as error:
        print(type(error).__name__)
"""


HEADER_CHECKSUMS = """
def header_checksum(record):
    text = f"{record}"
    key, marker, value = text.partition("=")
    return len(key) * 10000 + len(marker) * 100 + len(value)

def trailer_checksum(record):
    text = f"{record}"
    prefix, marker, suffix = text.rpartition("=")
    return len(prefix) * 10000 + len(marker) * 100 + len(suffix)

def escaped_header(record):
    text = f"{record}"
    return text.partition("=")

def escaped_trailer(record):
    text = f"{record}"
    return text.rpartition("=")

record = input()
handlers = [header_checksum, trailer_checksum, escaped_header, escaped_trailer]
for handler in handlers:
    print(handler(record))
"""


BOUNDARY_CASES = """
class RoutedRecord(str):
    def partition(self, separator):
        print('partition override')
        return 'custom prefix', separator, self

    def rpartition(self, separator):
        print('rpartition override')
        return self, separator, 'custom suffix'

class Separator(str):
    pass

def subclass_forward(record, separator):
    return record.partition(separator)

def subclass_reverse(record, separator):
    return record.rpartition(separator)

def separator_subclass_forward(record: str, separator):
    return record.partition(separator)

def separator_subclass_reverse(record: str, separator):
    return record.rpartition(separator)

def keyword_forward(record: str, separator: str):
    return record.partition(sep=separator)

def keyword_reverse(record: str, separator: str):
    return record.rpartition(sep=separator)

def missing_forward(record: str):
    return record.partition()

def missing_reverse(record: str):
    return record.rpartition()

def extra_forward(record: str, separator: str):
    return record.partition(separator, separator)

def extra_reverse(record: str, separator: str):
    return record.rpartition(separator, separator)

def nontext_forward(record: str, separator: int):
    return record.partition(separator)

def nontext_reverse(record: str, separator: int):
    return record.rpartition(separator)

record = input()
separator = input()
print(subclass_forward(RoutedRecord(record), separator))
print(subclass_reverse(RoutedRecord(record), separator))
print(separator_subclass_forward(record, Separator(separator)))
print(separator_subclass_reverse(record, Separator(separator)))
for operation in (
    lambda: keyword_forward(record, separator),
    lambda: keyword_reverse(record, separator),
    lambda: missing_forward(record),
    lambda: missing_reverse(record),
    lambda: extra_forward(record, separator),
    lambda: extra_reverse(record, separator),
    lambda: nontext_forward(record, 17),
    lambda: nontext_reverse(record, 17),
):
    try:
        operation()
    except TypeError as error:
        print(type(error).__name__)
"""


EVALUATION_ORDER = """
record = input()
separator = input()

def receiver():
    print('receiver')
    return record

def argument(label, value):
    print(label)
    return value

def forward_effects():
    head, middle, tail = receiver().partition(argument('separator', separator))
    print(head, middle, tail)

def reverse_effects():
    head, middle, tail = receiver().rpartition(argument('separator', separator))
    print(head, middle, tail)

forward_effects()
reverse_effects()
for operation in (
    lambda: receiver().partition(argument('first', separator), argument('second', separator)),
    lambda: receiver().rpartition(argument('first', separator), argument('second', separator)),
    lambda: receiver().partition(argument('invalid', 7)),
    lambda: receiver().rpartition(argument('invalid', 7)),
):
    try:
        operation()
    except TypeError as error:
        print(type(error).__name__)
"""


@pytest.fixture(scope="module")
def record_parser(tmp_path_factory):
    return build_case(tmp_path_factory.mktemp("partition-records"), RECORD_PARSER)


@pytest.fixture(scope="module")
def partition_boundaries(tmp_path_factory):
    return build_case(tmp_path_factory.mktemp("partition-boundaries"), BOUNDARY_CASES)


@pytest.fixture(scope="module")
def checksum_parser(tmp_path_factory):
    return build_case(tmp_path_factory.mktemp("partition-checksums"), HEADER_CHECKSUMS)


@pytest.mark.parametrize(
    ("record", "separator"),
    [
        ("content-type:text/plain:charset=utf8", ":"),
        ("archive::owner::payload", "::"),
        ("record without delimiter", "::"),
        ("", ":"),
        ("short", "longer delimiter"),
        ("whole", "whole"),
        (":trailing:", ":"),
        ("café§élève§fin", "§"),
        ("帳簿🐍項目🐍終端", "🐍"),
        ("α🚀β🐍γ🚀δ", "🚀"),
        ("before🐍🚀after🐍🚀end", "🐍🚀"),
        ("invalid delimiter", ""),
    ],
)
def test_partition_and_rpartition_runtime_records_match_python(
    record_parser, record, separator
):
    record_parser.compare(f"{record}\n{separator}\n")


@pytest.mark.parametrize(
    ("name", "control"),
    (("header_fields", "retained_header"), ("trailer_fields", "retained_trailer")),
)
def test_partition_record_unpack_omits_intermediate_tuple(
    original_trace, name, control
):
    retained = original_trace[control]["watched"]["tuple"]
    assert retained == 1, original_trace[control]
    assert original_trace[control]["value_tags"]["object"] == retained, original_trace[
        control
    ]
    observed = original_trace[name]["watched"]["tuple"]
    assert original_trace[name]["value_tags"]["object"] == observed, original_trace[
        name
    ]
    assert observed == retained, (
        f"{name} constructs extra tuples while unpacking and rebuilding the result: "
        f"root={original_trace[name]!r}, retained control={original_trace[control]!r}"
    )


@pytest.fixture(
    scope="module",
    params=(
        ("header:value:trailer", ":"),
        ("record without delimiter", ":"),
        ("café§élève§fin", "§"),
        ("帳簿🐍項目🐍終端", "🐍"),
    ),
    ids=("found", "missing", "latin1", "wide-unicode"),
)
def original_trace(record_parser, request):
    record, separator = request.param
    stdin = f"{record}\n{separator}\n"
    record_parser.compare(stdin)
    roots = ("header_fields", "trailer_fields", "retained_header", "retained_trailer")
    trace = trace_native(
        record_parser,
        roots={name: rf"^mule_fn_{name}\.fn$" for name in roots},
        watched={
            "tuple": r"^mule_sequence_tuple(?:_owned)?$",
            "object": r"object9new_value",
        },
        value_tags={"object": {"offset": DATA_OFFSET, "tag": ARGS_TAG}},
        stdin=stdin,
    )
    for name in roots:
        assert trace[name]["entries"] == trace[name]["exits"] == 1, trace
    return trace


@pytest.fixture(
    scope="module",
    params=(
        "content-type=text/plain=charset=utf8",
        "record without delimiter",
        "=value",
        "name=",
        "",
        "café=élève=fin",
        "帳簿🐍=項目🐍=終端",
    ),
    ids=("found", "missing", "empty-key", "empty-value", "empty", "latin1", "unicode"),
)
def checksum_trace(checksum_parser, request):
    stdin = request.param + "\n"
    checksum_parser.compare(stdin)
    roots = ("header_checksum", "trailer_checksum", "escaped_header", "escaped_trailer")
    trace = trace_native(
        checksum_parser,
        roots={name: rf"^mule_fn_{name}\.fn$" for name in roots},
        watched={
            "tuple": r"^mule_sequence_tuple(?:_owned)?$",
            "partition_slots": r"^mule_text_partition_into$",
            "partition_object": r"^mule_text_partition$",
            "object": r"object9new_value",
        },
        value_tags={"object": {"offset": DATA_OFFSET, "tag": ARGS_TAG}},
        stdin=stdin,
    )
    for name in roots:
        assert trace[name]["entries"] == trace[name]["exits"] == 1, trace
    return trace


@pytest.mark.parametrize("name", ("escaped_header", "escaped_trailer"))
def test_partition_escaping_result_exercises_tuple_constructor(checksum_trace, name):
    observed = checksum_trace[name]["watched"]
    assert observed["tuple"] == 1, checksum_trace[name]
    assert observed["partition_object"] == 1, checksum_trace[name]
    assert observed["partition_slots"] == 1, checksum_trace[name]
    assert checksum_trace[name]["value_tags"]["object"] == 1, checksum_trace[name]


@pytest.mark.parametrize(
    ("name", "control"),
    (("header_checksum", "escaped_header"), ("trailer_checksum", "escaped_trailer")),
)
def test_partition_scalar_header_parser_never_constructs_tuple(
    checksum_trace, name, control
):
    # The sibling root must prove that this executable and tracer observe real
    # tuple construction. All helper calls remain inside the measured root.
    assert checksum_trace[control]["watched"]["tuple"] == 1, checksum_trace[control]
    assert checksum_trace[control]["watched"]["partition_object"] == 1, checksum_trace[
        control
    ]
    assert checksum_trace[control]["value_tags"]["object"] == 1, checksum_trace[control]
    assert checksum_trace[name]["watched"]["partition_slots"] == 1, checksum_trace[name]
    assert checksum_trace[name]["watched"]["partition_object"] == 0, checksum_trace[
        name
    ]
    assert checksum_trace[name]["watched"]["tuple"] == 0, (
        f"{name} constructs a tuple despite returning only a scalar checksum: "
        f"{checksum_trace[name]!r}"
    )
    assert checksum_trace[name]["value_tags"]["object"] == 0, checksum_trace[name]


def test_partition_near_misses_preserve_overrides_and_argument_errors(
    partition_boundaries,
):
    partition_boundaries.compare("prefix::value::suffix\n::\n")
    partition_boundaries.compare("préfixe🐍valeur🐍fin\n🐍\n")


def test_partition_receiver_and_arguments_keep_evaluation_order(tmp_path):
    case = build_case(tmp_path, EVALUATION_ORDER)
    case.compare("header:value:trailer\n:\n")

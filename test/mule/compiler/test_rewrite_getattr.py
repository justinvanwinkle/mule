"""Constant-name getattr must expose cached lookup, not builtin dispatch."""

import re

import pytest

from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case


def _function(source, name):
    compilation = compile_source(source)
    return next(fn for fn in compilation.lambdas if fn.source_name == name)


@pytest.mark.parametrize("attribute", ["score", "__hidden", "λ"])
def test_constant_identifier_getattr_reaches_attribute_ir(attribute):
    function = _function(
        f"def read(record):\n return getattr(record, {attribute!r})\n", "read"
    )
    lookups = list(known_calls(function, "%getattr"))
    assert lookups, "a constant identifier must select attribute lookup"
    assert all(lookup.info.name == attribute for lookup in lookups)
    assert all(len(lookup.args) == 1 for lookup in lookups)


@pytest.mark.parametrize(
    "source",
    [
        "def read(record, attribute):\n return getattr(record, attribute)\n",
        "def read(record):\n return getattr(record, 'not-an-identifier')\n",
        "def read(record):\n return getattr(record, '')\n",
        (
            "def getattr(record, attribute):\n return 'shadowed ' + attribute\n"
            "def read(record):\n return getattr(record, 'score')\n"
        ),
    ],
    ids=["dynamic-name", "nonidentifier", "empty-name", "shadowed-builtin"],
)
def test_getattr_near_misses_preserve_full_call(source):
    assert not list(known_calls(_function(source, "read"), "%getattr"))


FIELD_SOURCE = """
class Record:
    def __init__(self, score):
        self.score = score

def field_total(record, count):
    total = 0
    while count > 0:
        total += getattr(record, "score")
        count -= 1
    return total

score = int(input())
count = int(input())
record = Record(score)
print(field_total(record, count))
record.score = score + 7
print(field_total(record, count))
del record.score
try:
    field_total(record, 1)
except AttributeError:
    print("missing field")
"""


DESCRIPTOR_SOURCE = """
class Record:
    def __init__(self, score):
        self.score = score

    @property
    def measured(self):
        print("descriptor", self.score)
        if self.score < 0:
            raise ValueError("invalid measurement")
        return self.score * 2

    def __getattr__(self, attribute):
        if attribute == "fallback":
            print("fallback")
            return self.score + 3
        raise AttributeError(attribute)

def descriptor_read(record):
    return getattr(record, "measured")

def fallback_read(record):
    return getattr(record, "fallback")

record = Record(int(input()))
try:
    print(descriptor_read(record))
except ValueError as error:
    print(type(error).__name__, str(error))
print(fallback_read(record))
record.__dict__["measured"] = 999
try:
    print(descriptor_read(record))
except ValueError as error:
    print(type(error).__name__, str(error))
"""


NAME_SOURCE = """
class Record:
    def __init__(self, value):
        self.__hidden = value

    def raw_private(self):
        return getattr(self, "__hidden")

    def mangled_private(self):
        return getattr(self, "_Record__hidden")

record = Record(input())
setattr(record, "not-an-identifier", "punctuation")
setattr(record, "", "empty")
setattr(record, "λ", "unicode")
print(getattr(record, input()))
print(getattr(record, "not-an-identifier"))
print(getattr(record, ""))
print(getattr(record, "λ"))
print(record.mangled_private())
try:
    record.raw_private()
except AttributeError:
    print("private spelling preserved")
"""


SHADOWED_SOURCE = """
def getattr(record, attribute):
    print("shadowed", attribute)
    return record + ":" + attribute

def read(record):
    return getattr(record, "score")

print(read(input()))
"""


@pytest.mark.integration
def test_getattr_field_processing_uses_inlined_cache_lookup(tmp_path):
    case = build_case(tmp_path, FIELD_SOURCE, optimize=3, lto=False)
    for stdin in ("6\n4\n", "-3\n2\n", "0\n0\n"):
        case.compare(stdin)

    body = case.function_ir(r"mule_fn_field_total\.")
    assert "@mule_attribute_epoch" in body
    assert "load atomic i64" in body
    assert "@mule_get_attr_cached(" in body
    assert "@mule_call(" not in body
    assert "@mule.call.2(" not in body
    assert "@mule.attr.hit(" not in body
    assert "@mule.attr.field(" not in body

    # Follow the cached encoding through its decoded index, field-slot address,
    # and value load. LLVM may canonicalize pointer-sized GEPs to [8 x i8].
    encodings = re.findall(r"(%[-\w.]+) = load i64, ptr [^\n]*@mule\.cache\.", body)
    assert encodings, "the warmed lookup must read its cached field encoding"
    indices = [
        index
        for encoding in encodings
        for index in re.findall(
            rf"(%[-\w.]+) = lshr i64 {re.escape(encoding)}, \d+\b", body
        )
    ]
    assert indices, "the warmed lookup must decode the cached field index"
    slots = [
        slot
        for index in indices
        for slot in re.findall(
            rf"(%[-\w.]+) = getelementptr[^\n]+, ptr %[-\w.]+, "
            rf"i64 {re.escape(index)}(?:,|\s|$)",
            body,
        )
    ]
    assert slots, "the warmed lookup must address the cached instance field"
    assert any(
        re.search(rf"\bload ptr, ptr {re.escape(slot)}(?:,|\s|$)", body)
        for slot in slots
    ), "the warmed lookup must load the value from its cached field slot"


@pytest.mark.integration
def test_getattr_descriptor_precedence_fallback_and_errors_match_python(tmp_path):
    case = build_case(tmp_path, DESCRIPTOR_SOURCE, optimize=3, lto=False)
    for stdin in ("5\n", "-2\n"):
        case.compare(stdin)
    body = case.function_ir(r"mule_fn_(?:descriptor_read|fallback_read)\.")
    assert "@mule_get_attr_cached(" in body
    assert "@mule_call(" not in body
    assert "@mule.call.2(" not in body


@pytest.mark.integration
def test_getattr_preserves_dynamic_nonidentifier_and_private_spelling(tmp_path):
    case = build_case(tmp_path, NAME_SOURCE, optimize=3, lto=False)
    for stdin in ("payload\nnot-an-identifier\n", "other\n_Record__hidden\n"):
        case.compare(stdin)
    body = case.function_ir(r"mule_fn_Record\.(?:raw_private|mangled_private)\.")
    assert "@mule_get_attr_cached(" in body
    assert "@mule_call(" not in body


@pytest.mark.integration
def test_getattr_shadowed_builtin_keeps_user_call(tmp_path):
    case = build_case(tmp_path, SHADOWED_SOURCE, optimize=3, lto=False)
    for stdin in ("value\n", "\n"):
        case.compare(stdin)

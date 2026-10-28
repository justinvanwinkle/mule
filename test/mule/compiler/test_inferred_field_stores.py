"""Receiver uncertainty must not discard precise field-store values."""

import pytest

from mule.compiler.ctype import (
    FIXNUM_MAX,
    FIXNUM_MIN,
    NONE_TYPE,
    TEXT_TYPE,
    UNKNOWN,
    IntegerType,
)
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source

FIXNUM = IntegerType(FIXNUM_MIN, FIXNUM_MAX)


@pytest.mark.parametrize("variadic_constructor", [False, True])
@pytest.mark.parametrize(
    ("stored_value", "first_type", "second_type"),
    [
        ("0", FIXNUM, TEXT_TYPE.join(FIXNUM)),
        ("'ready'", FIXNUM.join(TEXT_TYPE), TEXT_TYPE),
        ("None", FIXNUM.join(NONE_TYPE), TEXT_TYPE.join(NONE_TYPE)),
        ("args[0]", None, None),
    ],
)
def test_unattributed_field_store_widens_only_existing_candidates(
    stored_value, first_type, second_type, variadic_constructor
):
    if variadic_constructor:
        writer_source = f"""class Writer(First):
    def __init__(self, *args, **options):
        self.value = {stored_value}
"""
        writer_name = "__init__"
    else:
        writer_source = f"""def write(target, *args):
    target.value = {stored_value}
"""
        writer_name = "write"
    compilation = compile_source(f"""
class First:
    def __init__(self):
        self.value = 0
        self.untouched = 1

class Second:
    def __init__(self):
        self.value = "initial"
        self.untouched = 1

class Third:
    def __init__(self):
        self.untouched = 1

{writer_source}

first = First()
second = Second()
third = Third()
""")
    class_ids = {}
    for call in known_calls(compilation.entry, "%make_class"):
        assert call.info is not None
        class_ids[call.info.public_name] = call.info.identity
    state = compilation.locall_state
    assert state is not None
    field_types = state["class_fields"]
    fields = {name: field_types[identity] for name, identity in class_ids.items()}
    writer_owner = class_ids["Writer"] if variadic_constructor else None
    writer = next(
        fn
        for fn in compilation.lambdas
        if fn.source_name == writer_name and fn.method_class == writer_owner
    )
    facts = compilation.facts
    assert facts is not None
    assert facts[writer].parameters[0] is UNKNOWN
    for name in ("First", "Second", "Third"):
        assert fields[name]["untouched"] == FIXNUM
    assert "value" not in fields["Third"]
    assert fields["First"].get("value") == first_type
    assert fields["Second"].get("value") == second_type

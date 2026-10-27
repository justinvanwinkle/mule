"""Method exports condition on receiver facts, not ordinary argument guesses."""

from mule.compiler.ctype import (
    FIXNUM_MAX,
    FIXNUM_MIN,
    UNKNOWN,
    InstanceType,
    IntegerType,
)
from mule.compiler.main import compile_source


def test_method_summary_uses_receiver_fact_without_narrowing_other_parameters():
    compilation = compile_source(
        "class Record:\n"
        " def __init__(self): self.value = 3\n"
        " def read(self): return self.value\n"
        " def add(self, amount): return self.value + amount\n"
        "record = Record()\n"
        "print(record.read(), record.add(2))\n"
    )
    assert compilation.facts is not None
    methods = {
        fn.source_name: (fn, facts)
        for fn, facts in compilation.facts.items()
        if fn.method_class is not None
    }
    read, read_facts = methods["read"]
    assert read.method_class is not None
    assert read_facts.return_type is UNKNOWN
    assert read_facts.method_return_type(
        InstanceType(read.method_class)
    ) == IntegerType(FIXNUM_MIN, FIXNUM_MAX)
    add, add_facts = methods["add"]
    assert add.method_class is not None
    assert add_facts.method_return_type(InstanceType(add.method_class)) is UNKNOWN
    assert read_facts.method_return_type(InstanceType("unrelated")) is UNKNOWN

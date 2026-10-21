from dataclasses import dataclass

import pytest

from mule import ast
from mule.compiler.ctype import (
    BOOLEAN_TRUE,
    BOTTOM,
    BYTES_TYPE,
    FLOAT,
    IMMORTAL,
    NONE_TYPE,
    REAL,
    TEXT_TYPE,
    UNBOUND,
    UNKNOWN,
    UNKNOWN_UNBOUND,
    ClassType,
    InstanceType,
    IntegerType,
    ListType,
    TupleType,
    UnionType,
    ValueType,
    union_type,
    value_type_from_dict,
    widen_fact,
)
from mule.compiler.driver import emit_llvm
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.test_exception_groups import compare


@dataclass(frozen=True)
class ExtensionType(ValueType):
    identity: str
    may_be_unbound: bool = False


@pytest.mark.parametrize(
    "fact",
    [FLOAT, TEXT_TYPE, BYTES_TYPE, ListType(FLOAT), TupleType(2), InstanceType("C")],
)
def test_optional_join_recovers_each_type_after_none_check(fact):
    joined = fact.join(NONE_TYPE)
    assert joined == NONE_TYPE.join(fact)
    assert joined.narrow_none(False) == fact
    assert joined.narrow_none(True) is NONE_TYPE
    assert joined.join(fact) == joined
    assert value_type_from_dict(joined.to_dict()) == joined


def test_new_type_inherits_alternative_join_without_type_pair_methods():
    extension = ExtensionType("extension")
    joined = extension.join(FLOAT)
    assert isinstance(joined, UnionType)
    assert joined == FLOAT.join(extension)
    assert joined.members == frozenset((extension, FLOAT))
    assert extension.join(extension) == extension


def test_union_join_flattens_and_combines_existing_domains():
    left = IntegerType(1, 2).join(TEXT_TYPE)
    right = IntegerType(3, 4).join(FLOAT)
    joined = left.join(right)
    assert joined == union_type(IntegerType(1, 4), TEXT_TYPE, FLOAT)
    assert joined == right.join(left)
    assert joined.join(joined) == joined
    assert IntegerType(2, 3).fits(joined)
    assert not joined.fits(IntegerType(1, 4))


def test_union_top_bottom_and_unbound_remain_distinct():
    joined = FLOAT.join(TEXT_TYPE)
    assert joined.join(BOTTOM) == joined
    assert BOTTOM.join(joined) == joined
    assert joined.join(UNKNOWN) is UNKNOWN
    assert UNKNOWN.join(joined) is UNKNOWN
    unbound = joined.join(UNBOUND)
    assert unbound.may_be_unbound
    assert unbound.as_bound() == joined
    assert unbound.join(UNKNOWN) is UNKNOWN_UNBOUND
    assert unbound.narrow_none(False) == unbound


def test_union_transfers_distribute_and_widen_member_intervals():
    joined = IntegerType(0, 0).join(FLOAT)
    result = joined.binary(ast.BinaryOperator.ADD, IntegerType(1, 1))
    assert result == IntegerType(1, 1).join(FLOAT)
    widened = widen_fact(joined, IntegerType(0, 1).join(FLOAT))
    assert widened == IntegerType(0, None).join(FLOAT)


@pytest.mark.parametrize(
    "left,middle,right",
    [
        (IntegerType(0, 0), FLOAT, REAL),
        (IntegerType(0, 0), BOOLEAN_TRUE, TEXT_TYPE),
        (IntegerType(0, 0), BOOLEAN_TRUE, IMMORTAL),
        (IMMORTAL, NONE_TYPE, ClassType((InstanceType("C"),))),
    ],
)
def test_union_join_grouping_preserves_summary_domains(left, middle, right):
    assert left.join(middle).join(right) == left.join(middle.join(right))


def test_unbound_union_map_keeps_unbound_when_no_bound_alternative_remains():
    joined = FLOAT.join(TEXT_TYPE).join(UNBOUND)
    assert joined.map(lambda member: BOTTOM) is UNBOUND
    assert BOTTOM.with_unbound() is UNBOUND


def test_real_summary_retains_boolean_when_joined_with_other_types():
    joined = REAL.join(TEXT_TYPE)
    assert BOOLEAN_TRUE.fits(joined)


def test_optional_class_uses_general_union_and_narrowing():
    cls = ClassType((InstanceType("C"),))
    joined = cls.join(NONE_TYPE)
    assert isinstance(joined, UnionType)
    assert joined.narrow_none(False) == cls
    assert joined.narrow_none(True) is NONE_TYPE
    assert value_type_from_dict(joined.to_dict()) == joined


@pytest.mark.parametrize(
    "fact,proof,possible",
    [
        (
            TEXT_TYPE,
            lambda fact: fact.fits_text(fact),
            lambda fact: fact.fits_possible_text(fact),
        ),
        (
            TupleType(2),
            lambda fact: fact.fits_tuple(fact),
            lambda fact: fact.fits_possible_tuple(fact),
        ),
        (
            ListType(FLOAT),
            lambda fact: fact.fits_list(fact),
            lambda fact: fact.fits_possible_list(fact),
        ),
        (
            InstanceType("C"),
            lambda fact: fact.fits_instance(fact),
            lambda fact: fact.fits_possible_instance(fact),
        ),
    ],
)
def test_family_queries_accept_union_contract_without_assuming_member_fields(
    fact, proof, possible
):
    joined = fact.join(NONE_TYPE)
    assert not proof(joined)
    assert possible(joined)


def test_mixed_text_comparison_lowers_without_literal_contract_on_union():
    llvm = emit_llvm(
        "def check(flag, other):\n value = 'text' if flag else None\n return value == other\n"
    )
    assert "mule_fn_check.body" in llvm


def test_branch_join_retains_float_for_arithmetic_after_none_check():
    compilation = compile_source(
        "def use(flag):\n"
        " if flag:\n"
        "  value = 1.25\n"
        " else:\n"
        "  value = None\n"
        " if value is None:\n"
        "  return 0.0\n"
        " return value + 2.0\n"
    )
    function = compilation.lambdas[1]
    addition = next(known_calls(function, "%add"))
    assert addition.args[0].derived_type is FLOAT
    assert addition.derived_type is FLOAT


def test_union_parameters_and_none_branches_execute_with_object_identity(tmp_path):
    compare(
        tmp_path,
        """
def choose(flag):
    if flag:
        value = 1.25
    else:
        value = None
    if value is None:
        return 0.0
    return value + 2.0
def sequence(flag):
    if flag:
        value = [2]
    else:
        value = None
    if value is None:
        return 0
    return value[0] + 3
def identity(value):
    return value
for flag in (True, False):
    print(choose(flag))
    print(sequence(flag))
for value in (1.25, None, [2], "text"):
    print(identity(value) is value)
""",
    )

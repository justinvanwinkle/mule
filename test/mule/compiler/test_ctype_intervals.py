from mule import ast
from mule.compiler.ctype import (
    INTEGER,
    PYTHON_INTEGER,
    TEXT_TYPE,
    UNKNOWN,
    IntegerType,
    TextType,
    TupleType,
)


def test_modulo_interval_bounds():
    op = ast.BinaryOperator.MODULO

    # Bounded positive dividend and positive constant divisor
    assert IntegerType(0, 8388607).binary(op, IntegerType(500, 500)) == IntegerType(
        0, 499
    )

    # Bounded negative/mixed dividend and positive divisor
    assert IntegerType(-100, -1).binary(op, IntegerType(10, 20)) == IntegerType(0, 19)
    assert IntegerType(-50, 50).binary(op, IntegerType(7, 7)) == IntegerType(0, 6)

    # Bounded dividend with negative divisor
    assert IntegerType(0, 100).binary(op, IntegerType(-10, -5)) == IntegerType(-9, 0)
    assert IntegerType(-3, -1).binary(op, IntegerType(-10, -10)) == IntegerType(-3, -1)

    # Unbounded dividend with positive/negative bounded divisor
    unbounded_mod = INTEGER.binary(op, IntegerType(500, 500))
    assert unbounded_mod == IntegerType(0, 499)
    assert unbounded_mod.is_fixnum is True
    assert INTEGER.binary(op, IntegerType(-20, -10)) == IntegerType(-19, 0)

    # Dividend already strictly inside [0, rlo)
    assert IntegerType(10, 25).binary(op, IntegerType(100, 100)) == IntegerType(10, 25)

    # Divisor spanning 0 returns unbounded INTEGER without ZeroDivisionError
    assert IntegerType(10, 20).binary(op, IntegerType(-5, 5)) == INTEGER
    assert IntegerType(5, 5).binary(op, IntegerType(0, 0)) == INTEGER


def test_floor_divide_interval_bounds():
    op = ast.BinaryOperator.FLOOR_DIVIDE

    # Bounded positive dividend and positive divisor
    assert IntegerType(0, 10000).binary(op, IntegerType(1000, 1000)) == IntegerType(
        0, 10
    )
    assert IntegerType(10, 50).binary(op, IntegerType(2, 5)) == IntegerType(2, 25)

    # Bounded negative/mixed dividend and negative divisor
    assert IntegerType(-20, -10).binary(op, IntegerType(-5, -2)) == IntegerType(2, 10)
    assert IntegerType(-10, 10).binary(op, IntegerType(2, 5)) == IntegerType(-5, 5)

    # Partially bounded non-negative dividend and positive divisor
    assert IntegerType(100, None).binary(op, IntegerType(10, 20)) == IntegerType(
        5, None
    )
    assert IntegerType(100, 200).binary(op, IntegerType(10, None)) == IntegerType(0, 20)

    # Divisor spanning 0 returns unbounded INTEGER without ZeroDivisionError
    assert IntegerType(10, 20).binary(op, IntegerType(-5, 5)) == INTEGER
    assert IntegerType(5, 5).binary(op, IntegerType(0, 0)) == INTEGER


def test_tuple_type_elements():
    t1 = TupleType(2, (IntegerType(1, 1), TextType("a")))
    t2 = TupleType(2, (IntegerType(2, 2), TextType("b")))

    # Join of two TupleTypes with matching arity and elements joins elementwise
    joined = t1.join(t2)
    assert joined == TupleType(2, (IntegerType(1, 2), TEXT_TYPE))

    # Identity fast path on join
    assert t1.join(t1) is t1
    assert joined.join(t1) is joined
    assert t1.join(joined) is joined

    # Binary ADD concatenates elements when both are present
    added = t1.binary(ast.BinaryOperator.ADD, t2)
    assert added == TupleType(
        4, (IntegerType(1, 1), TextType("a"), IntegerType(2, 2), TextType("b"))
    )

    # getitem_type with exact constant index (0, -1) returns exact element ValueType
    assert t1.getitem_type(IntegerType(0, 0)) == IntegerType(1, 1)
    assert t1.getitem_type(IntegerType(-1, -1)) == TextType("a")

    # getitem_type on a homogeneous TupleType with a non-constant integer index
    tags = TupleType(3, (TextType("a"), TextType("b"), TextType("c")))
    assert tags.getitem_type(INTEGER) == TEXT_TYPE
    assert tags.getitem_type(PYTHON_INTEGER) == TEXT_TYPE
    assert tags.getitem_type(UNKNOWN) == UNKNOWN

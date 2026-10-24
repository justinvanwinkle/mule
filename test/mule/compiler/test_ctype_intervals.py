from mule import ast
from mule.compiler import emit_llvm
from mule.compiler.ctype import (
    BOOLEAN,
    BOOLEAN_FALSE,
    BOOLEAN_TRUE,
    BOTTOM,
    INTEGER,
    INTERNED_TEXT_TYPE,
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


def test_text_type_interning_and_identity_comparisons():
    op_is = ast.ComparisonOperator.IS
    op_is_not = ast.ComparisonOperator.IS_NOT

    # Singletons (empty and 1-char Latin-1) are both immortal and interned
    empty = TextType("")
    ascii_char = TextType("a")
    latin1_char = TextType("\xff")
    for singleton in (empty, ascii_char, latin1_char):
        assert singleton.is_immortal is True
        assert singleton.singleton_text() == singleton.literal
        assert singleton.is_interned_text is True
        assert singleton.interned_text() == singleton.literal

    # Multi-character literals are interned heap objects, not immediate singletons
    multi = TextType("compile_time_literal")
    other = TextType("other_literal")
    assert multi.is_immortal is False
    assert multi.singleton_text() is None
    assert multi.is_interned_text is True
    assert multi.interned_text() == "compile_time_literal"
    assert INTERNED_TEXT_TYPE.is_immortal is False
    assert INTERNED_TEXT_TYPE.singleton_text() is None
    assert INTERNED_TEXT_TYPE.is_interned_text is True
    assert INTERNED_TEXT_TYPE.interned_text() is None
    assert TEXT_TYPE.is_interned_text is False

    # Identity comparisons fold on known literals
    assert multi.compare_identity_with(multi, op_is) is BOOLEAN_TRUE
    assert multi.compare_identity_with(multi, op_is_not) is BOOLEAN_FALSE
    assert multi.compare_identity_with(other, op_is) is BOOLEAN_FALSE
    assert multi.compare_identity_with(other, op_is_not) is BOOLEAN_TRUE
    assert multi.compare_identity_with(TEXT_TYPE, op_is) is BOOLEAN

    # Join preserves interned proof when both branches are explicitly interned
    assert INTERNED_TEXT_TYPE.join(multi) == INTERNED_TEXT_TYPE
    assert INTERNED_TEXT_TYPE.join(TEXT_TYPE) == TEXT_TYPE

    # Equality narrowing refines exact text value without assuming uninterned strings are interned
    narrowed_uninterned = TEXT_TYPE.narrow_equality(multi, True)
    assert narrowed_uninterned.exact_text() == "compile_time_literal"
    assert narrowed_uninterned.is_interned_text is False
    narrowed_interned = INTERNED_TEXT_TYPE.narrow_equality(multi, True)
    assert narrowed_interned.exact_text() == "compile_time_literal"
    assert narrowed_interned.is_interned_text is True
    assert multi.narrow_equality(other, True) is BOTTOM
    assert multi.narrow_equality(multi, False) is BOTTOM


def test_interned_text_equality_and_identity_llvm_lowering():
    ir = emit_llvm(
        "import _mule_native\n"
        "def same_literal():\n"
        " return 'alpha_beta' is 'alpha_beta'\n"
        "def diff_literal():\n"
        " return 'alpha_beta' is 'alpha_gamma'\n"
        "def eq_interned(x: str, y: str):\n"
        " a = _mule_native.intern_text(x)\n"
        " b = _mule_native.intern_text(y)\n"
        " return a == b\n"
        "def eq_inline_latin1_literal(x):\n"
        " s = str(x)\n"
        " return s == 'alpha_beta'\n"
        "def eq_heap_latin1_literal(x):\n"
        " s = str(x)\n"
        " return s == 'alpha_beta_gamma_delta_epsilon'\n"
    )
    same_body = ir.split('define internal ptr @"mule_fn_same_literal.body"')[1].split(
        "\n}"
    )[0]
    diff_body = ir.split('define internal ptr @"mule_fn_diff_literal.body"')[1].split(
        "\n}"
    )[0]
    assert "@mule.true" in same_body
    assert "@mule.false" in diff_body
    inline_body = ir.split(
        'define internal ptr @"mule_fn_eq_inline_latin1_literal.body"'
    )[1].split("\n}")[0]
    assert "icmp eq i16" in inline_body
    assert "icmp eq i64" in inline_body
    assert "@mule_text_eq" not in inline_body
    assert "@mule.borrowed.text" not in inline_body
    heap_body = ir.split('define internal ptr @"mule_fn_eq_heap_latin1_literal.body"')[
        1
    ].split("\n}")[0]
    assert "call i32 @mule_text_eq" in heap_body
    eq_interned_body = ir.split('define internal ptr @"mule_fn_eq_interned.body"')[
        1
    ].split("\n}")[0]
    assert "icmp eq ptr" in eq_interned_body
    assert "@mule_text_eq" not in eq_interned_body

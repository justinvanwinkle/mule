from mule import ast
from mule.compiler.ctype import (
    BOOLEAN,
    BOOLEAN_FALSE,
    BOOLEAN_TRUE,
    BOTTOM,
    BYTES_TYPE,
    DICT_TYPE,
    EMPTY_DICT_TYPE,
    EMPTY_LIST_TYPE,
    FIXNUM_MAX,
    FIXNUM_MIN,
    INTEGER,
    ITERATOR_TYPE,
    LIST_TYPE,
    NONE_TYPE,
    PYTHON_INTEGER,
    TEXT_TYPE,
    UNBOUND,
    UNKNOWN,
    DictType,
    IntegerType,
    IteratorType,
    ListType,
    TextType,
    TupleType,
    ValueTypeVisitor,
    widen_fact,
)


def test_parameterized_list_type():
    assert ListType() == LIST_TYPE
    assert LIST_TYPE.element is UNKNOWN
    assert EMPTY_LIST_TYPE == ListType(BOTTOM)

    int_list = EMPTY_LIST_TYPE.with_element(IntegerType(1, 10))
    assert int_list == ListType(IntegerType(1, 10))
    assert int_list.getitem_type(IntegerType(0, 0)) == IntegerType(1, 10)
    assert int_list.getitem_type(INTEGER) == IntegerType(1, 10)
    assert int_list.getitem_type(PYTHON_INTEGER) == IntegerType(1, 10)
    assert int_list.getitem_type(TEXT_TYPE) is UNKNOWN
    assert EMPTY_LIST_TYPE.getitem_type(IntegerType(0, 0)) is UNKNOWN

    it = int_list.iterate_type()
    assert it == IteratorType(IntegerType(1, 10))
    assert it.next_item_type() == IntegerType(1, 10)
    assert EMPTY_LIST_TYPE.iterate_type() == ITERATOR_TYPE

    wider_list = int_list.with_element(IntegerType(20, 30))
    assert wider_list == ListType(IntegerType(1, 30))

    # Bound / unbound preservation
    unbound_list = int_list.as_maybe_unbound()
    assert unbound_list == ListType(IntegerType(1, 10), True)
    assert unbound_list.as_bound() == int_list
    assert unbound_list.getitem_type(IntegerType(0, 0)) is UNKNOWN
    assert unbound_list.iterate_type() is UNKNOWN

    # Join and fits
    assert EMPTY_LIST_TYPE.join(int_list) == int_list
    assert int_list.join(EMPTY_LIST_TYPE) == int_list
    assert int_list.join(ListType(TEXT_TYPE)) is LIST_TYPE
    assert EMPTY_LIST_TYPE.fits(int_list)
    assert int_list.fits(LIST_TYPE)
    assert int_list.fits(ListType(INTEGER))
    assert not ListType(INTEGER).fits(int_list)
    assert not unbound_list.fits(int_list)
    assert unbound_list.fits(unbound_list)

    # Binary operations (+ and *)
    added = int_list.binary(ast.BinaryOperator.ADD, ListType(IntegerType(15, 20)))
    assert added == ListType(IntegerType(1, 20))
    assert EMPTY_LIST_TYPE.binary(ast.BinaryOperator.ADD, int_list) == int_list
    assert int_list.binary(ast.BinaryOperator.MULTIPLY, IntegerType(3, 3)) == int_list
    assert IntegerType(3, 3).binary(ast.BinaryOperator.MULTIPLY, int_list) == int_list


def test_parameterized_dict_type():
    assert DictType() == DICT_TYPE
    assert DICT_TYPE.key is UNKNOWN and DICT_TYPE.value is UNKNOWN
    assert EMPTY_DICT_TYPE == DictType(BOTTOM, BOTTOM)

    d = EMPTY_DICT_TYPE.with_entry(TEXT_TYPE, IntegerType(1, 1))
    assert d == DictType(TEXT_TYPE, IntegerType(1, 1))
    assert d.getitem_type(TEXT_TYPE) == IntegerType(1, 1)
    assert EMPTY_DICT_TYPE.getitem_type(TEXT_TYPE) is UNKNOWN

    assert d.dict_get_type(IntegerType(0, 0)) == IntegerType(0, 1)
    assert d.dict_get_type(NONE_TYPE) is UNKNOWN
    assert EMPTY_DICT_TYPE.dict_get_type(IntegerType(0, 0)) == IntegerType(0, 0)

    assert d.iterate_type() == IteratorType(TEXT_TYPE)
    assert d.iterate_type().next_item_type() is TEXT_TYPE
    assert d.dict_keys_iterator_type() == IteratorType(TEXT_TYPE)
    assert d.dict_values_iterator_type() == IteratorType(IntegerType(1, 1))
    assert d.dict_values_iterator_type().next_item_type() == IntegerType(1, 1)
    items_it = d.dict_items_iterator_type()
    assert items_it == IteratorType(TupleType(2, (TEXT_TYPE, IntegerType(1, 1))))
    assert items_it.next_item_type() == TupleType(2, (TEXT_TYPE, IntegerType(1, 1)))

    # Join and fits
    d2 = DictType(TextType("k"), IntegerType(5, 10))
    assert EMPTY_DICT_TYPE.join(d) == d
    assert d.join(EMPTY_DICT_TYPE) == d
    assert d.join(d2) == DictType(TEXT_TYPE, IntegerType(1, 10))
    assert EMPTY_DICT_TYPE.fits(d)
    assert d2.fits(DictType(TEXT_TYPE, INTEGER))
    assert d.fits(DICT_TYPE)
    assert not DictType(TEXT_TYPE, INTEGER).fits(d)
    assert d.truth_value() is None


def test_iterator_type_and_sequence_iteration():
    assert IteratorType() == ITERATOR_TYPE
    it = IteratorType(IntegerType(0, 5))
    assert it.iterate_type() == it
    assert it.next_item_type() == IntegerType(0, 5)
    assert it.truth_value() is None

    it2 = IteratorType(IntegerType(3, 10))
    assert it.join(it2) == IteratorType(IntegerType(0, 10))
    assert it.fits(ITERATOR_TYPE)
    assert it.fits(IteratorType(INTEGER))
    assert not IteratorType(INTEGER).fits(it)

    # TupleType, TextType, BytesType iterate_type
    t = TupleType(2, (IntegerType(1, 2), IntegerType(5, 6)))
    assert t.iterate_type() == IteratorType(IntegerType(1, 6))
    assert TupleType(0).iterate_type() == IteratorType(BOTTOM)
    assert TupleType(0).iterate_type().next_item_type() is UNKNOWN
    assert TupleType(3).iterate_type() is ITERATOR_TYPE
    assert TEXT_TYPE.iterate_type() == IteratorType(TEXT_TYPE)
    assert BYTES_TYPE.iterate_type() == IteratorType(IntegerType(0, 255))


def test_widen_fact_containers():
    prev_list = ListType(IntegerType(0, 0))
    curr_list = ListType(IntegerType(0, 1))
    assert widen_fact(prev_list, curr_list) == ListType(IntegerType(0, None))

    prev_dict = DictType(TEXT_TYPE, IntegerType(0, 0))
    curr_dict = DictType(TEXT_TYPE, IntegerType(0, 1))
    assert widen_fact(prev_dict, curr_dict) == DictType(TEXT_TYPE, IntegerType(0, None))

    prev_it = IteratorType(IntegerType(1, 5))
    curr_it = IteratorType(IntegerType(1, 6))
    assert widen_fact(prev_it, curr_it) == IteratorType(IntegerType(1, None))


def test_widen_constant():
    assert IntegerType(42, 42).widen_constant() == IntegerType(FIXNUM_MIN, FIXNUM_MAX)
    assert IntegerType(2**80, 2**80).widen_constant() is INTEGER
    assert BOOLEAN_TRUE.widen_constant() is BOOLEAN
    assert BOOLEAN_FALSE.widen_constant() is BOOLEAN
    assert TextType("hello").widen_constant() is TEXT_TYPE

    tup = TupleType(2, (IntegerType(42, 42), TextType("hello")))
    assert tup.widen_constant() == TupleType(
        2, (IntegerType(FIXNUM_MIN, FIXNUM_MAX), TEXT_TYPE)
    )
    assert UNBOUND.as_bound() is BOTTOM


def test_visitor_hooks_for_dict_and_iterator():
    class _KindVisitor(ValueTypeVisitor):
        def visit_ValueType(self, fact):
            return "other"

        def visit_DictType(self, fact):
            return "dict"

        def visit_IteratorType(self, fact):
            return "iterator"

    visitor = _KindVisitor()
    assert DICT_TYPE.accept(visitor) == "dict"
    assert ITERATOR_TYPE.accept(visitor) == "iterator"
    assert LIST_TYPE.accept(visitor) == "other"

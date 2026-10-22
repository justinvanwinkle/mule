"""Small sequence containment IR1 transforms, following ``src/compiler/seqtran.lisp``.

Transforms registered via ``@deftransform`` simplify membership tests against
empty and single-element tuple and list displays (`x in ()`, `x in []`,
`x in (a,)`, `x in [a]`, and their `not in` counterparts) and fold bounded
constant tuple/list displays when compile-time literal, interval, or type
information decides the membership test while preserving evaluation order,
exception behavior, and identity-before-equality semantics.
"""

from __future__ import annotations

from mule import ast

from . import fndb
from .constraint import constant_type
from .ctype import (
    INTEGER,
    UNKNOWN,
    BooleanType,
    BytesType,
    IntegerType,
    NoneType,
    PythonIntegerType,
    TextType,
    ValueType,
    ValueTypeVisitor,
)
from .ir1opt import (
    _flush_operand,
    bound_constant_literal,
    constant_literal,
    lvar_type,
    replace_combination_with_constant,
    retarget_known_combination,
)
from .ir1util import delete_ref, flush_combination, reoptimize_lvar, unlink_node
from .knownfun import FunInfo, deftransform
from .node import (
    CallKind,
    Combination,
    Constant,
    KnownFunction,
    Lambda,
    LambdaVar,
    LeafVisitor,
    Lvar,
    Node,
    NodeVisitor,
    Ref,
    Set,
    linked,
)

_MAX_CONSTANT_CONTAINER_ELEMENTS = 32


class _ConstantTupleElements(ast.Visitor):
    """Extract the element literals of a constant ``ast.Tuple``, or None."""

    def visit_Node(self, node):
        return None

    def visit_Tuple(self, node: ast.Tuple):
        return node.elements


_CONSTANT_TUPLE_ELEMENTS = _ConstantTupleElements()


class _ContainerProducer(NodeVisitor):
    """Return ``node`` when it is a bounded ``%build_tuple`` or ``%build_list``."""

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info in (fndb.BUILD_TUPLE, fndb.BUILD_LIST)
            and len(node.args) <= _MAX_CONSTANT_CONTAINER_ELEMENTS
        ):
            return node
        return None


_CONTAINER_PRODUCER = _ContainerProducer()


class _ConstantTupleRef(NodeVisitor, LeafVisitor):
    """Return ``(ref, elements)`` when ``node`` is a ``Ref`` to a bounded ``Constant(ast.Tuple)``."""

    def __init__(self):
        self._ref: Ref | None = None

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        self._ref = node
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_Constant(self, leaf: Constant):
        elements = leaf.value.accept(_CONSTANT_TUPLE_ELEMENTS)
        if elements is not None and len(elements) <= _MAX_CONSTANT_CONTAINER_ELEMENTS:
            return self._ref, tuple(elements)
        return None


def _extract_build_constant_elements(
    build_node: Combination,
) -> tuple[ast.Node, ...] | None:
    """Return the constant AST literals of ``build_node``'s arguments when
    ``1 <= len(build_node.args) <= 32`` and every argument is a single-use
    ``Ref(Constant)`` preceding ``build_node`` in the same block."""
    if not (
        1 <= len(build_node.args) <= _MAX_CONSTANT_CONTAINER_ELEMENTS
        and len(build_node.fun.uses) == 1
    ):
        return None
    block = build_node.block
    fun_use = build_node.fun.uses[0]
    if fun_use.prev is None or fun_use.block is not block:
        return None
    elements: list[ast.Node] = []
    arg_uses: set[Node] = set()
    for arg in build_node.args:
        if len(arg.uses) != 1:
            return None
        use = arg.uses[0]
        if use.prev is None or use.block is not block:
            return None
        literal = constant_literal(arg)
        if literal is None:
            return None
        elements.append(literal)
        arg_uses.add(use)
    seen = 0
    for current in block.nodes():
        if current is build_node:
            break
        if current in arg_uses:
            seen += 1
    if seen != len(arg_uses):
        return None
    return tuple(elements)


class _SetToVar(NodeVisitor):
    """Whether a node is a binding store to ``var``."""

    def __init__(self, var: LambdaVar):
        self.var = var

    def visit_Node(self, node) -> bool:
        return False

    def visit_Set(self, node: Set) -> bool:
        return node.var is self.var and node.value is not None


def _lambda_var_bound_at(leaf: LambdaVar, ref: Ref) -> bool:
    if any(store.value is None for store in leaf.sets):
        return False
    if leaf.home is not None and leaf in leaf.home.vars:
        return True
    checker = _SetToVar(leaf)
    for node in ref.block.nodes():
        if node is ref:
            return False
        if node.accept(checker):
            return True
    return False


class _SafeDroppedOperand(NodeVisitor, LeafVisitor):
    """Whether an operand is a single-use, non-raising ``Ref``."""

    def __init__(self, lvar: Lvar):
        self.lvar = lvar
        self._ref: Ref | None = None

    def visit_Node(self, node) -> bool:
        return False

    def visit_Ref(self, node: Ref) -> bool:
        if node.final or lvar_type(self.lvar).may_be_unbound:
            return False
        self._ref = node
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf) -> bool:
        return False

    def visit_Constant(self, leaf: Constant) -> bool:
        return True

    def visit_KnownFunction(self, leaf: KnownFunction) -> bool:
        return True

    def visit_Lambda(self, leaf: Lambda) -> bool:
        return True

    def visit_LambdaVar(self, leaf: LambdaVar) -> bool:
        if leaf.type is not None:
            return not leaf.type.may_be_unbound
        ref = linked(self._ref, leaf, "reference")
        if ref.derived_type is not None and ref.derived_type is not UNKNOWN:
            return not ref.derived_type.may_be_unbound
        return _lambda_var_bound_at(leaf, ref)


def _is_safe_dropped_operand(lvar: Lvar) -> bool:
    if len(lvar.uses) != 1:
        return False
    return lvar.uses[0].accept(_SafeDroppedOperand(lvar))


class _DeletePureRef(NodeVisitor):
    def visit_Ref(self, node: Ref) -> None:
        delete_ref(node)
        unlink_node(node)


_DELETE_PURE_REF = _DeletePureRef()


def _delete_pure_ref(lvar: Lvar) -> None:
    uses = list(lvar.uses)
    lvar.dest = None
    for use in uses:
        use.lvar = None
    lvar.uses = []
    for use in uses:
        if use.prev is not None:
            use.accept(_DELETE_PURE_REF)


class _ReflexiveEqualityType(ValueTypeVisitor):
    """Whether a value type is a bound primitive with reflexive equality."""

    def visit_ValueType(self, fact: ValueType) -> bool:
        return False

    def visit_IntegerType(self, fact: IntegerType) -> bool:
        return not fact.may_be_unbound

    def visit_BooleanType(self, fact: BooleanType) -> bool:
        return not fact.may_be_unbound

    def visit_PythonIntegerType(self, fact: PythonIntegerType) -> bool:
        return not fact.may_be_unbound

    def visit_TextType(self, fact: TextType) -> bool:
        return not fact.may_be_unbound

    def visit_BytesType(self, fact: BytesType) -> bool:
        return not fact.may_be_unbound

    def visit_NoneType(self, fact: NoneType) -> bool:
        return not fact.may_be_unbound


_REFLEXIVE_EQUALITY_TYPE = _ReflexiveEqualityType()


def _has_reflexive_equality(fact: ValueType) -> bool:
    return bool(fact.accept(_REFLEXIVE_EQUALITY_TYPE))


class _LeftTypeLiteral(ValueTypeVisitor):
    """Extract a known primitive AST literal from ``left_lvar``'s value type."""

    def visit_ValueType(self, fact: ValueType) -> ast.Node | None:
        return None

    def visit_TextType(self, fact: TextType) -> ast.Node | None:
        if not fact.may_be_unbound and fact.literal is not None:
            return ast.TextLiteral(fact.literal)
        return None

    def visit_IntegerType(self, fact: IntegerType) -> ast.Node | None:
        exact = fact.exact_integer()
        if exact is not None and fact.fits(INTEGER):
            return ast.IntegerLiteral(exact)
        return None

    def visit_BooleanType(self, fact: BooleanType) -> ast.Node | None:
        if (
            not fact.may_be_unbound
            and fact.minimum is not None
            and fact.minimum == fact.maximum
        ):
            if fact.minimum == 1:
                return ast.BooleanLiteral(True)
            if fact.minimum == 0:
                return ast.BooleanLiteral(False)
        return None

    def visit_NoneType(self, fact: NoneType) -> ast.Node | None:
        if not fact.may_be_unbound:
            return ast.NoneLiteral(None)
        return None


_LEFT_TYPE_LITERAL = _LeftTypeLiteral()


def _extract_left_constant_literal(left_lvar: Lvar) -> ast.Node | None:
    literal = bound_constant_literal(left_lvar)
    if literal is not None:
        return literal
    return lvar_type(left_lvar).accept(_LEFT_TYPE_LITERAL)


class _PrimitiveElementComparator(ast.Visitor):
    """Compare a known left primitive literal against candidate element literals.

    Returns ``True`` or ``False`` for foldable primitive literals, or ``None``
    when the candidate element is not a foldable primitive literal (e.g. float).
    """

    def visit_Node(self, node: ast.Node) -> bool | None:
        return None

    def visit_IntegerLiteral(self, node: ast.IntegerLiteral) -> bool | None:
        return False

    def visit_BooleanLiteral(self, node: ast.BooleanLiteral) -> bool | None:
        return False

    def visit_TextLiteral(self, node: ast.TextLiteral) -> bool | None:
        return False

    def visit_BytesLiteral(self, node: ast.BytesLiteral) -> bool | None:
        return False

    def visit_NoneLiteral(self, node: ast.NoneLiteral) -> bool | None:
        return False


class _IntElementComparator(_PrimitiveElementComparator):
    def __init__(self, value: int):
        self.value = value

    def visit_IntegerLiteral(self, node: ast.IntegerLiteral) -> bool:
        return self.value == node.value

    def visit_BooleanLiteral(self, node: ast.BooleanLiteral) -> bool:
        return self.value == int(node.value)


class _TextElementComparator(_PrimitiveElementComparator):
    def __init__(self, value: str):
        self.value = value

    def visit_TextLiteral(self, node: ast.TextLiteral) -> bool:
        return self.value == node.value


class _BytesElementComparator(_PrimitiveElementComparator):
    def __init__(self, value: bytes):
        self.value = value

    def visit_BytesLiteral(self, node: ast.BytesLiteral) -> bool:
        return self.value == node.value


class _NoneElementComparator(_PrimitiveElementComparator):
    def visit_NoneLiteral(self, node: ast.NoneLiteral) -> bool:
        return True


class _MakePrimitiveComparator(ast.Visitor):
    def visit_Node(self, node: ast.Node) -> _PrimitiveElementComparator | None:
        return None

    def visit_IntegerLiteral(
        self, node: ast.IntegerLiteral
    ) -> _PrimitiveElementComparator:
        return _IntElementComparator(node.value)

    def visit_BooleanLiteral(
        self, node: ast.BooleanLiteral
    ) -> _PrimitiveElementComparator:
        return _IntElementComparator(int(node.value))

    def visit_TextLiteral(self, node: ast.TextLiteral) -> _PrimitiveElementComparator:
        return _TextElementComparator(node.value)

    def visit_BytesLiteral(self, node: ast.BytesLiteral) -> _PrimitiveElementComparator:
        return _BytesElementComparator(node.value)

    def visit_NoneLiteral(self, node: ast.NoneLiteral) -> _PrimitiveElementComparator:
        return _NoneElementComparator()


_MAKE_PRIMITIVE_COMPARATOR = _MakePrimitiveComparator()


class _DisjointElementChecker(ast.Visitor):
    """Whether a container element literal can never equal any value of ``left_type``."""

    def visit_Node(self, node: ast.Node) -> bool:
        return False


class _IntegerDisjointChecker(_DisjointElementChecker):
    def __init__(self, minimum: int | None, maximum: int | None):
        self.minimum = minimum
        self.maximum = maximum

    def _outside_bounds(self, value: int) -> bool:
        return (self.minimum is not None and value < self.minimum) or (
            self.maximum is not None and value > self.maximum
        )

    def visit_IntegerLiteral(self, node: ast.IntegerLiteral) -> bool:
        return self._outside_bounds(node.value)

    def visit_BooleanLiteral(self, node: ast.BooleanLiteral) -> bool:
        return self._outside_bounds(int(node.value))

    def visit_TextLiteral(self, node: ast.TextLiteral) -> bool:
        return True

    def visit_BytesLiteral(self, node: ast.BytesLiteral) -> bool:
        return True

    def visit_NoneLiteral(self, node: ast.NoneLiteral) -> bool:
        return True


class _TextDisjointChecker(_DisjointElementChecker):
    def visit_IntegerLiteral(self, node: ast.IntegerLiteral) -> bool:
        return True

    def visit_BooleanLiteral(self, node: ast.BooleanLiteral) -> bool:
        return True

    def visit_BytesLiteral(self, node: ast.BytesLiteral) -> bool:
        return True

    def visit_NoneLiteral(self, node: ast.NoneLiteral) -> bool:
        return True


class _BytesDisjointChecker(_DisjointElementChecker):
    def visit_IntegerLiteral(self, node: ast.IntegerLiteral) -> bool:
        return True

    def visit_BooleanLiteral(self, node: ast.BooleanLiteral) -> bool:
        return True

    def visit_TextLiteral(self, node: ast.TextLiteral) -> bool:
        return True

    def visit_NoneLiteral(self, node: ast.NoneLiteral) -> bool:
        return True


class _MakeDisjointChecker(ValueTypeVisitor):
    def visit_ValueType(self, fact: ValueType) -> _DisjointElementChecker | None:
        return None

    def visit_IntegerType(self, fact: IntegerType) -> _DisjointElementChecker | None:
        if fact.may_be_unbound:
            return None
        return _IntegerDisjointChecker(fact.minimum, fact.maximum)

    def visit_PythonIntegerType(
        self, fact: PythonIntegerType
    ) -> _DisjointElementChecker | None:
        if fact.may_be_unbound:
            return None
        return _IntegerDisjointChecker(fact.minimum, fact.maximum)

    def visit_TextType(self, fact: TextType) -> _DisjointElementChecker | None:
        if fact.may_be_unbound:
            return None
        return _TextDisjointChecker()

    def visit_BytesType(self, fact: BytesType) -> _DisjointElementChecker | None:
        if fact.may_be_unbound:
            return None
        return _BytesDisjointChecker()


_MAKE_DISJOINT_CHECKER = _MakeDisjointChecker()


def _decide_constant_containment(
    left_lvar: Lvar, elements: tuple[ast.Node, ...]
) -> bool | None:
    """Return ``True`` or ``False`` when ``left_lvar in elements`` is decided at
    compile time and ``left_lvar`` can be safely dropped, or ``None`` otherwise."""
    if not (
        1 <= len(elements) <= _MAX_CONSTANT_CONTAINER_ELEMENTS
        and _is_safe_dropped_operand(left_lvar)
    ):
        return None
    left_const = _extract_left_constant_literal(left_lvar)
    if left_const is not None:
        comparator = left_const.accept(_MAKE_PRIMITIVE_COMPARATOR)
        if comparator is not None:
            matches = [el.accept(comparator) for el in elements]
            if all(match is not None for match in matches):
                return any(matches)
    disjoint_checker = lvar_type(left_lvar).accept(_MAKE_DISJOINT_CHECKER)
    if disjoint_checker is not None and all(
        el.accept(disjoint_checker) for el in elements
    ):
        return False
    return None


def _fold_containment_to_bool(node: Combination, left_lvar: Lvar, result: bool) -> None:
    _delete_pure_ref(left_lvar)
    folded = ast.BooleanLiteral(result)
    folded.span = node.source.span if node.source is not None else None
    replace_combination_with_constant(node, folded)


def _transform_small_containment(
    node: Combination, *, empty_result: bool, single_info: FunInfo
) -> bool:
    if node.lvar is None or len(node.args) != 2 or len(node.fun.uses) != 1:
        return False
    left_lvar, right_lvar = node.args
    if len(right_lvar.uses) != 1:
        return False
    fun_ref = node.fun.uses[0]
    if fun_ref.prev is None or fun_ref.next is not node.prev:
        return False

    right_use = right_lvar.uses[0]

    # Case B: right_lvar is produced by a Ref to a bounded Constant(ast.Tuple).
    const_match = right_use.accept(_ConstantTupleRef())
    if const_match is not None:
        const_ref, elements = const_match
        if (
            const_ref is None
            or const_ref.prev is None
            or const_ref.block is not node.block
            or const_ref.next is not fun_ref.prev
        ):
            return False
        if len(elements) == 0:
            if not _is_safe_dropped_operand(left_lvar):
                return False
            _fold_containment_to_bool(node, left_lvar, empty_result)
            return True
        found = _decide_constant_containment(left_lvar, elements)
        if found is not None:
            _fold_containment_to_bool(
                node, left_lvar, not found if empty_result else found
            )
            return True
        if len(elements) != 1:
            return False
        elem_literal = elements[0]
        elem_type = constant_type(elem_literal)
        if not (
            _has_reflexive_equality(elem_type)
            or _has_reflexive_equality(lvar_type(left_lvar))
        ):
            return False
        if const_ref in const_ref.leaf.refs:
            const_ref.leaf.refs.remove(const_ref)
        new_leaf = Constant(elem_literal)
        const_ref.leaf = new_leaf
        new_leaf.refs.append(const_ref)
        const_ref.derived_type = None
        right_lvar.derived_type = None
        retarget_known_combination(node, single_info, [left_lvar, right_lvar])
        reoptimize_lvar(right_lvar)
        return True

    # Case A: right_lvar is produced by a bounded %build_tuple or %build_list.
    build_node = right_use.accept(_CONTAINER_PRODUCER)
    if (
        build_node is None
        or build_node.prev is None
        or build_node.block is not node.block
        or build_node.next is not fun_ref.prev
    ):
        return False
    if len(build_node.args) == 0:
        if not _is_safe_dropped_operand(left_lvar):
            return False
        _fold_containment_to_bool(node, left_lvar, empty_result)
        flush_combination(build_node)
        return True
    elements = _extract_build_constant_elements(build_node)
    if elements is not None:
        found = _decide_constant_containment(left_lvar, elements)
        if found is not None:
            _fold_containment_to_bool(
                node, left_lvar, not found if empty_result else found
            )
            flush_combination(build_node)
            return True
    if len(build_node.args) != 1:
        return False
    elem_lvar = build_node.args[0]
    if not (
        _has_reflexive_equality(lvar_type(elem_lvar))
        or _has_reflexive_equality(lvar_type(left_lvar))
    ):
        return False
    build_node.args = []
    retarget_known_combination(node, single_info, [left_lvar, elem_lvar])
    _flush_operand(build_node.fun)
    unlink_node(build_node)
    return True


@deftransform("%compare_in")
def _transform_compare_in(node: Combination) -> bool:
    return _transform_small_containment(
        node,
        empty_result=False,
        single_info=fndb.COMPARE[ast.ComparisonOperator.EQUAL],
    )


@deftransform("%compare_not_in")
def _transform_compare_not_in(node: Combination) -> bool:
    return _transform_small_containment(
        node,
        empty_result=True,
        single_info=fndb.COMPARE[ast.ComparisonOperator.NOT_EQUAL],
    )


class _IsContainmentConsumer(NodeVisitor):
    def visit_Node(self, node) -> bool:
        return False

    def visit_Combination(self, node: Combination) -> bool:
        return node.kind is CallKind.KNOWN and node.fun_info in (
            fndb.COMPARE[ast.ComparisonOperator.IN],
            fndb.COMPARE[ast.ComparisonOperator.NOT_IN],
        )


_IS_CONTAINMENT_CONSUMER = _IsContainmentConsumer()


@deftransform("%build_tuple")
@deftransform("%build_list")
def _reoptimize_small_containment_consumer(node: Combination) -> bool:
    if (
        node.lvar is not None
        and len(node.args) == 1
        and _has_reflexive_equality(lvar_type(node.args[0]))
    ):
        dest = node.lvar.dest
        if (
            dest is not None
            and not dest.reoptimize
            and dest.accept(_IS_CONTAINMENT_CONSUMER)
        ):
            reoptimize_lvar(node.lvar)
    return False


__all__ = [
    "_transform_compare_in",
    "_transform_compare_not_in",
]

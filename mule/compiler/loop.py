"""Dominators, loop nesting, and loop-invariant code motion, following
SBCL's ``src/compiler/loop.lisp``."""

from __future__ import annotations

from mule import ast

from . import fndb
from .ctype import (
    BYTES_TYPE,
    LIST_TYPE,
    PYTHON_INTEGER,
    REAL,
    TEXT_TYPE,
    UNBOUND,
    TupleType,
    ValueType,
    ValueTypeVisitor,
)
from .dfo import find_dfo, walk_successors
from .ir1util import (
    add_lvar_use,
    block_exception_target,
    block_home_lambda,
    delete_lvar_use,
    delete_ref,
    flush_dest,
    insert_node_after,
    insert_node_before,
    unlink_node,
)
from .knownfun import FunInfo
from .node import (
    Bind,
    Block,
    CallKind,
    Cast,
    Combination,
    Component,
    Constant,
    Exit,
    GlobalVar,
    KnownFunction,
    LambdaVar,
    LeafVisitor,
    Loop,
    Lvar,
    NaturalLoop,
    Node,
    NodeVisitor,
    OuterLoop,
    Ref,
    Set,
    StrangeLoop,
    linked,
)


class _Graph:
    """The component's normal and exceptional control-flow edges."""

    def __init__(self, component):
        self.blocks = [component.head, *component.blocks(), component.tail]
        owned = set(self.blocks)
        self.successors = {
            component.head: tuple(component.head.succ),
            component.tail: tuple(component.tail.succ),
        }
        self.successors.update(
            {
                block: tuple(
                    dict.fromkeys(
                        successor
                        for successor in walk_successors(block)
                        if successor in owned
                    )
                )
                for block in self.blocks[1:-1]
            }
        )
        for block in self.blocks[1:-1]:
            if not self.successors[block]:
                self.successors[block] = (component.tail,)
        self.predecessors = {block: [] for block in self.blocks}
        for block, successors in self.successors.items():
            for successor in successors:
                self.predecessors[successor].append(block)


def lowest_common_dominator(left, right):
    while left is not right:
        if left.number < right.number:
            left = left.dominator
        else:
            right = right.dominator
        if left is None or right is None:
            raise ValueError("blocks do not share a dominator tree")
    return left


def find_dominators(component):
    if component.reanalyze:
        find_dfo(component)
    graph = _Graph(component)
    head = component.head
    for block in graph.blocks:
        block.dominator = None
    head.dominator = head
    while True:
        changed = False
        for block in graph.blocks[1:]:
            dominator = None
            for predecessor in graph.predecessors[block]:
                if predecessor.dominator is None:
                    continue
                dominator = (
                    predecessor
                    if dominator is None
                    else lowest_common_dominator(predecessor, dominator)
                )
            if block.dominator is not dominator:
                block.dominator = dominator
                changed = True
        if not changed:
            break
    head.dominator = None


def _note_loop_head(head, graph):
    tails = [
        predecessor
        for predecessor in graph.predecessors[head]
        if predecessor.number <= head.number
    ]
    if all(tail.dominated_by(head) for tail in tails):
        return NaturalLoop(head, tails)
    return StrangeLoop(head, tails, graph.successors)


def _find_blocks_from_here(start, loop, graph):
    pending = [start]
    while pending:
        block = pending.pop()
        if block.loop is loop or not loop.accepts(block):
            continue
        inner = block.loop
        if inner is None:
            block.loop = loop
            loop.blocks.append(block)
            pending.extend(graph.predecessors[block])
        elif inner.superior is None:
            loop.attach(inner)
            pending.extend(inner.predecessor_seeds(graph.predecessors))
        elif inner.superior is not loop:
            pending.append(inner.superior.head)


def _find_loop_blocks(loop, graph):
    for tail in loop.tails:
        _find_blocks_from_here(tail, loop, graph)
    for seed in loop.completion_seeds(graph.predecessors):
        _find_blocks_from_here(seed, loop, graph)
    exits = []
    candidates = [*loop.blocks]
    for inner in loop.inferiors:
        candidates.extend(inner.exits)
    for block in candidates:
        if any(successor.loop is None for successor in graph.successors[block]):
            exits.append(block)
    loop.exits = list(dict.fromkeys(exits))


def loop_analyze(component):
    graph = _Graph(component)
    outer = OuterLoop(component.head, (component.tail,))
    component.outer_loop = outer
    for block in graph.blocks:
        block.loop = None
    for block in reversed(graph.blocks[1:-1]):
        if not any(
            predecessor.number <= block.number
            for predecessor in graph.predecessors[block]
        ):
            continue
        loop = _note_loop_head(block, graph)
        _find_loop_blocks(loop, graph)
        if not loop.exits:
            outer.attach(loop)
    _find_loop_blocks(outer, graph)
    outer.assign_depths()


def analyze(component):
    find_dominators(component)
    loop_analyze(component)


_ARITH_BINARY_OPS = (
    fndb.BINARY[ast.BinaryOperator.ADD],
    fndb.BINARY[ast.BinaryOperator.SUBTRACT],
    fndb.BINARY[ast.BinaryOperator.MULTIPLY],
)
_BITWISE_BINARY_OPS = (
    fndb.BINARY[ast.BinaryOperator.BIT_AND],
    fndb.BINARY[ast.BinaryOperator.BIT_OR],
    fndb.BINARY[ast.BinaryOperator.BIT_XOR],
)
_SHIFT_BINARY_OPS = (
    fndb.BINARY[ast.BinaryOperator.LEFT_SHIFT],
    fndb.BINARY[ast.BinaryOperator.RIGHT_SHIFT],
)
_DIVMOD_BINARY_OPS = (
    fndb.BINARY[ast.BinaryOperator.FLOOR_DIVIDE],
    fndb.BINARY[ast.BinaryOperator.MODULO],
)
_ARITH_UNARY_OPS = (
    fndb.UNARY[ast.UnaryOperator.POSITIVE],
    fndb.UNARY[ast.UnaryOperator.NEGATIVE],
)
_BITWISE_UNARY_OPS = (fndb.UNARY[ast.UnaryOperator.INVERT],)

HOISTABLE_OPS: frozenset[FunInfo] = frozenset(
    (
        *_ARITH_BINARY_OPS,
        *_BITWISE_BINARY_OPS,
        *_SHIFT_BINARY_OPS,
        *_DIVMOD_BINARY_OPS,
        *_ARITH_UNARY_OPS,
        *_BITWISE_UNARY_OPS,
    )
)


def is_hoistable_op(info: FunInfo) -> bool:
    return info in HOISTABLE_OPS or info.name in ("_text_length", "_storage_length")


_PURE_PROTOCOL_OPS: frozenset[FunInfo] = frozenset(
    (
        *fndb.BINARY.values(),
        *fndb.INPLACE_BINARY.values(),
        *fndb.UNARY.values(),
        *fndb.COMPARE.values(),
        fndb.PROTOCOLS["getitem"],
        fndb.LEN,
    )
)

_NON_HOISTABLE_GLOBAL_NAMES: frozenset[str] = frozenset(
    {
        "bool",
        "bytearray",
        "bytes",
        "chr",
        "delattr",
        "dict",
        "float",
        "getattr",
        "hasattr",
        "int",
        "isinstance",
        "issubclass",
        "len",
        "list",
        "object",
        "ord",
        "range",
        "setattr",
        "str",
        "tuple",
        "type",
    }
)


class _HasSuspension(NodeVisitor):
    def visit_Node(self, node) -> bool:
        return False

    def visit_Combination(self, node: Combination) -> bool:
        if node.fun_info is None:
            return False
        return node.fun_info.name in (
            "%yield",
            "%yield_from",
            "%await_value",
            "%async_generator_yield",
        )


_HAS_SUSPENSION = _HasSuspension()


class _PreheaderInsertionSite(NodeVisitor):
    def visit_Node(self, node) -> str | None:
        return None

    def visit_Bind(self, node: Bind) -> str:
        return "after"

    def visit_Ref(self, node: Ref) -> str:
        return "after"

    def visit_Set(self, node: Set) -> str:
        return "after"

    def visit_Cast(self, node: Cast) -> str:
        return "after"

    def visit_Combination(self, node: Combination) -> str | None:
        if node.fun_info is not None and node.fun_info.never_returns:
            return None
        return "after"

    def visit_Exit(self, node: Exit) -> str | None:
        if node.entry is None and node.value is None:
            return "before"
        return None


_PREHEADER_INSERTION_SITE = _PreheaderInsertionSite()


class _ConstantKey(ast.Visitor):
    def visit_Node(self, node):
        return ("node", id(node))

    def visit_IntegerLiteral(self, node: ast.IntegerLiteral):
        return ("int", node.value)

    def visit_BooleanLiteral(self, node: ast.BooleanLiteral):
        return ("bool", node.value)

    def visit_FloatLiteral(self, node: ast.FloatLiteral):
        return ("float", node.value)

    def visit_NoneLiteral(self, node: ast.NoneLiteral):
        return ("none", None)

    def visit_TextLiteral(self, node: ast.TextLiteral):
        return ("str", node.value)


_CONSTANT_KEY = _ConstantKey()


class _IsHoistableGlobalType(ValueTypeVisitor):
    def visit_ValueType(self, fact) -> bool:
        return False

    def visit_IntegerType(self, fact) -> bool:
        return not fact.may_be_unbound

    def visit_PythonIntegerType(self, fact) -> bool:
        return not fact.may_be_unbound

    def visit_FloatType(self, fact) -> bool:
        return not fact.may_be_unbound

    def visit_RealType(self, fact) -> bool:
        return not fact.may_be_unbound

    def visit_NoneType(self, fact) -> bool:
        return not fact.may_be_unbound

    def visit_TextType(self, fact) -> bool:
        return not fact.may_be_unbound

    def visit_BytesType(self, fact) -> bool:
        return not fact.may_be_unbound

    def visit_TupleType(self, fact: TupleType) -> bool:
        if fact.may_be_unbound:
            return False
        if fact.elements is None:
            return True
        return all(el.accept(self) for el in fact.elements)


_IS_HOISTABLE_GLOBAL_TYPE = _IsHoistableGlobalType()


class _IsSetDest(NodeVisitor):
    def visit_Node(self, node) -> bool:
        return False

    def visit_Set(self, node: Set) -> bool:
        return True


_IS_SET_DEST = _IsSetDest()


class _IsCalleeOrSpecialDest(NodeVisitor):
    def __init__(self, lvar: Lvar):
        self.lvar = lvar

    def visit_Node(self, node) -> bool:
        return False

    def visit_Combination(self, node: Combination) -> bool:
        if node.fun is self.lvar:
            return True
        return node.fun_info in (
            fndb.RANGE_PREPARE,
            fndb.MAKE_FUNCTION,
            fndb.MAKE_CLASS,
        )


class _LoopEffects(NodeVisitor, LeafVisitor):
    def __init__(self):
        self.written_vars: set[LambdaVar] = set()
        self.written_globals: set[str] = set()
        self.has_impure_call = False
        self.has_list_mutation = False
        self.ref_types_by_var: dict[LambdaVar, list[ValueType | None]] = {}
        self.ref_types_by_global: dict[str, list[ValueType | None]] = {}
        self._in_set = False
        self._current_ref: Ref | None = None

    def visit_Set(self, node: Set) -> None:
        self._in_set = True
        node.var.accept(self)
        self._in_set = False

    def visit_Ref(self, node: Ref) -> None:
        self._current_ref = node
        node.leaf.accept(self)
        self._current_ref = None

    def visit_LambdaVar(self, leaf: LambdaVar) -> None:
        if self._in_set:
            self.written_vars.add(leaf)
        elif self._current_ref is not None:
            self.ref_types_by_var.setdefault(leaf, []).append(
                self._current_ref.derived_type
            )

    def visit_GlobalVar(self, leaf: GlobalVar) -> None:
        if self._in_set:
            self.written_globals.add(leaf.name)
        elif self._current_ref is not None:
            self.ref_types_by_global.setdefault(leaf.name, []).append(
                self._current_ref.derived_type
            )

    def visit_Combination(self, node: Combination) -> None:
        if node.kind is not CallKind.KNOWN or node.fun_info is None:
            self.has_impure_call = True
            self.has_list_mutation = True
            return
        info = node.fun_info
        if info.name in ("_list_append", "_storage_grow", "_storage_set_length"):
            self.has_list_mutation = True
        if info in _PURE_PROTOCOL_OPS:
            if any(
                arg.derived_type is None or arg.derived_type.may_run_user_code
                for arg in node.args
            ):
                self.has_impure_call = True
                self.has_list_mutation = True
            return
        if info.may_call or info.unsafe or info.protocol is not None:
            self.has_impure_call = True
            self.has_list_mutation = True


class _KnownFunRefCheck(NodeVisitor, LeafVisitor):
    def __init__(self, expected_block: Block):
        self.expected_block = expected_block
        self._ref: Ref | None = None

    def visit_Node(self, node) -> Ref | None:
        return None

    def visit_Ref(self, node: Ref) -> Ref | None:
        if node.prev is None or node.block is not self.expected_block:
            return None
        self._ref = node
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf) -> Ref | None:
        return None

    def visit_KnownFunction(self, leaf: KnownFunction) -> Ref | None:
        return self._ref


class _OperandRefCheck(NodeVisitor, LeafVisitor):
    def __init__(
        self,
        expected_block: Block,
        preheader: Block,
        effects: _LoopEffects,
        required_type: ValueType,
    ):
        self.expected_block = expected_block
        self.preheader = preheader
        self.effects = effects
        self.required_type = required_type
        self._ref: Ref | None = None

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if (
            node.prev is None
            or node.block is not self.expected_block
            or node.final
            or node.allow_unbound
            or node.derived_type is None
            or node.derived_type.may_be_unbound
            or not node.derived_type.fits(self.required_type)
        ):
            return None
        self._ref = node
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_Constant(self, leaf: Constant):
        ref = linked(self._ref, leaf, "ref")
        return (("const", leaf.value.accept(_CONSTANT_KEY)), ref, ref.derived_type)

    def visit_LambdaVar(self, leaf: LambdaVar):
        ref = linked(self._ref, leaf, "ref")
        if (
            leaf.indirect
            or leaf.expression_temporary
            or leaf in self.effects.written_vars
        ):
            return None
        if any(s.value is None for s in leaf.sets):
            if not (
                all(s.value is not None for s in leaf.sets[1:])
                and any(
                    s.value is not None and self.preheader.dominated_by(s.block)
                    for s in leaf.sets
                )
            ):
                return None
        elif leaf.sets and not any(
            s.value is not None and self.preheader.dominated_by(s.block)
            for s in leaf.sets
        ):
            return None
        loop_ref_types = self.effects.ref_types_by_var.get(leaf, ())
        if not loop_ref_types or not all(
            t is not None and not t.may_be_unbound and t.fits(self.required_type)
            for t in loop_ref_types
        ):
            return None
        if leaf.sets:
            if not all(
                s.value.derived_type is not None
                and not s.value.derived_type.may_be_unbound
                and s.value.derived_type.fits(self.required_type)
                for s in leaf.sets
                if s.value is not None
            ):
                return None
        else:
            for other_ref in leaf.refs:
                if (
                    other_ref.lvar is not None
                    and other_ref.lvar.dest is not None
                    and other_ref.lvar.dest.accept(_IS_SET_DEST)
                    and (
                        other_ref.derived_type is None
                        or other_ref.derived_type.may_be_unbound
                        or not other_ref.derived_type.fits(self.required_type)
                    )
                ):
                    return None
        return (("var", leaf), ref, ref.derived_type)


class _CandidateHoist(NodeVisitor, LeafVisitor):
    def __init__(self, preheader: Block, effects: _LoopEffects):
        self.preheader = preheader
        self.effects = effects
        self._ref: Ref | None = None

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if (
            self.effects.has_impure_call
            or node.lvar is None
            or len(node.lvar.uses) != 1
            or node.lvar.dest is None
            or node.derived_type is None
            or not node.derived_type.accept(_IS_HOISTABLE_GLOBAL_TYPE)
            or node.lvar.dest.accept(_IsCalleeOrSpecialDest(node.lvar))
        ):
            return None
        self._ref = node
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_GlobalVar(self, leaf: GlobalVar):
        if (
            leaf.name in self.effects.written_globals
            or leaf.name in _NON_HOISTABLE_GLOBAL_NAMES
            or leaf.name.startswith("$")
        ):
            return None
        loop_types = self.effects.ref_types_by_global.get(leaf.name, ())
        if not loop_types or not all(
            t is not None and t.accept(_IS_HOISTABLE_GLOBAL_TYPE) for t in loop_types
        ):
            return None
        ref = linked(self._ref, leaf, "ref")
        return (("global", leaf), ref.derived_type, ())

    def visit_Combination(self, node: Combination):
        if (
            node.kind is not CallKind.KNOWN
            or node.fun_info is None
            or not is_hoistable_op(node.fun_info)
            or node.lvar is None
            or len(node.lvar.uses) != 1
            or node.lvar.dest is None
            or node.derived_type is None
            or node.derived_type.may_be_unbound
        ):
            return None
        if len(node.fun.uses) != 1 or any(len(arg.uses) != 1 for arg in node.args):
            return None
        if len({node.fun, *node.args}) != 1 + len(node.args):
            return None
        fun_ref = node.fun.uses[0].accept(_KnownFunRefCheck(node.block))
        if fun_ref is None:
            return None
        info = node.fun_info
        res_type = node.derived_type
        if info in _ARITH_BINARY_OPS:
            if len(node.args) != 2:
                return None
            req_type = (
                PYTHON_INTEGER
                if res_type.fits(PYTHON_INTEGER)
                else (REAL if res_type.fits(REAL) else None)
            )
            if req_type is None:
                return None
            checker = _OperandRefCheck(
                node.block, self.preheader, self.effects, req_type
            )
            left = node.args[0].uses[0].accept(checker)
            right = node.args[1].uses[0].accept(checker)
            if left is None or right is None:
                return None
            return (
                ("comb", info, left[0], right[0]),
                res_type,
                (fun_ref, left[1], right[1]),
            )
        if info in _BITWISE_BINARY_OPS:
            if len(node.args) != 2 or not res_type.fits(PYTHON_INTEGER):
                return None
            checker = _OperandRefCheck(
                node.block, self.preheader, self.effects, PYTHON_INTEGER
            )
            left = node.args[0].uses[0].accept(checker)
            right = node.args[1].uses[0].accept(checker)
            if left is None or right is None:
                return None
            return (
                ("comb", info, left[0], right[0]),
                res_type,
                (fun_ref, left[1], right[1]),
            )
        if info in _SHIFT_BINARY_OPS:
            if len(node.args) != 2 or not res_type.fits(PYTHON_INTEGER):
                return None
            checker = _OperandRefCheck(
                node.block, self.preheader, self.effects, PYTHON_INTEGER
            )
            left = node.args[0].uses[0].accept(checker)
            right = node.args[1].uses[0].accept(checker)
            if left is None or right is None:
                return None
            rhs_type = right[2]
            if (
                rhs_type.minimum is None
                or rhs_type.minimum < 0
                or rhs_type.maximum is None
                or rhs_type.maximum >= 62
            ):
                return None
            return (
                ("comb", info, left[0], right[0]),
                res_type,
                (fun_ref, left[1], right[1]),
            )
        if info in _DIVMOD_BINARY_OPS:
            if len(node.args) != 2 or not res_type.fits(PYTHON_INTEGER):
                return None
            checker = _OperandRefCheck(
                node.block, self.preheader, self.effects, PYTHON_INTEGER
            )
            left = node.args[0].uses[0].accept(checker)
            right = node.args[1].uses[0].accept(checker)
            if left is None or right is None:
                return None
            rhs_type = right[2]
            if not (
                (rhs_type.minimum is not None and rhs_type.minimum >= 1)
                or (rhs_type.maximum is not None and rhs_type.maximum <= -1)
            ):
                return None
            return (
                ("comb", info, left[0], right[0]),
                res_type,
                (fun_ref, left[1], right[1]),
            )
        if info in _ARITH_UNARY_OPS:
            if len(node.args) != 1:
                return None
            req_type = (
                PYTHON_INTEGER
                if res_type.fits(PYTHON_INTEGER)
                else (REAL if res_type.fits(REAL) else None)
            )
            if req_type is None:
                return None
            checker = _OperandRefCheck(
                node.block, self.preheader, self.effects, req_type
            )
            operand = node.args[0].uses[0].accept(checker)
            if operand is None:
                return None
            return (("comb", info, operand[0]), res_type, (fun_ref, operand[1]))
        if info in _BITWISE_UNARY_OPS:
            if len(node.args) != 1 or not res_type.fits(PYTHON_INTEGER):
                return None
            checker = _OperandRefCheck(
                node.block, self.preheader, self.effects, PYTHON_INTEGER
            )
            operand = node.args[0].uses[0].accept(checker)
            if operand is None:
                return None
            return (("comb", info, operand[0]), res_type, (fun_ref, operand[1]))
        if info.name == "_text_length":
            if len(node.args) != 1 or not res_type.fits(PYTHON_INTEGER):
                return None
            checker = _OperandRefCheck(
                node.block, self.preheader, self.effects, TEXT_TYPE
            )
            operand = node.args[0].uses[0].accept(checker)
            if operand is None:
                return None
            return (("comb", info, operand[0]), res_type, (fun_ref, operand[1]))
        if info.name == "_storage_length":
            if len(node.args) != 1 or not res_type.fits(PYTHON_INTEGER):
                return None
            arg_node = node.args[0].uses[0]
            arg_type = arg_node.derived_type
            if arg_type is None or arg_type.may_be_unbound:
                return None
            if arg_type.fits(BYTES_TYPE):
                req_type = BYTES_TYPE
            elif arg_type.fits_tuple(TupleType()):
                req_type = TupleType()
            elif not self.effects.has_list_mutation and arg_type.fits_list(LIST_TYPE):
                req_type = LIST_TYPE
            else:
                return None
            checker = _OperandRefCheck(
                node.block, self.preheader, self.effects, req_type
            )
            operand = arg_node.accept(checker)
            if operand is None:
                return None
            return (("comb", info, operand[0]), res_type, (fun_ref, operand[1]))
        return None


def _insert_into_preheader(preheader: Block, mode: str, node: Node) -> None:
    anchor = linked(preheader.last, preheader, "last")
    if mode == "before":
        insert_node_before(anchor, node)
    else:
        insert_node_after(anchor, node)


def _hoist_natural_loop(component: Component, loop: Loop, counter: list[int]) -> bool:
    if not loop.is_natural:
        return False
    all_loop_blocks = loop.all_blocks()
    entry_preds = [pred for pred in loop.head.pred if pred not in all_loop_blocks]
    if len(entry_preds) != 1:
        return False
    preheader = entry_preds[0]
    if preheader.succ != [loop.head] or preheader.last is None:
        return False
    insert_mode = preheader.last.accept(_PREHEADER_INSERTION_SITE)
    if insert_mode is None:
        return False
    preheader_exc = block_exception_target(preheader)
    preheader_home = block_home_lambda(preheader)

    effects = _LoopEffects()
    loop_blocks_dfo = [
        block for block in component.blocks() if block in all_loop_blocks
    ]
    for block in loop_blocks_dfo:
        for node in block.nodes():
            node.accept(effects)

    candidate_checker = _CandidateHoist(preheader, effects)
    hoisted_cse: dict[tuple, tuple[LambdaVar, ValueType]] = {}
    changed = False

    for block in loop_blocks_dfo:
        if (
            block_exception_target(block) is not preheader_exc
            or block_home_lambda(block) is not preheader_home
        ):
            continue
        for node in list(block.nodes()):
            if node.prev is None:
                continue
            match = node.accept(candidate_checker)
            if match is None:
                continue
            cse_key, result_type, input_refs = match
            orig_lvar = linked(node.lvar, node, "lvar")
            if cse_key in hoisted_cse:
                licm_var, licm_type = hoisted_cse[cse_key]
                replacement_ref = Ref(node.lexenv, node.source, licm_var)
                licm_var.refs.append(replacement_ref)
                insert_node_before(node, replacement_ref)
                delete_lvar_use(node)
                add_lvar_use(replacement_ref, orig_lvar)
                replacement_ref.derived_type = licm_type
                orig_lvar.derived_type = licm_type
                effects.ref_types_by_var.setdefault(licm_var, []).append(licm_type)
                if input_refs:
                    comb: Combination = node  # type: ignore[assignment]
                    flush_dest(comb.fun)
                    for arg in comb.args:
                        flush_dest(arg)
                    unlink_node(comb)
                    for in_ref in input_refs:
                        delete_ref(in_ref)
                        unlink_node(in_ref)
                else:
                    ref_node: Ref = node  # type: ignore[assignment]
                    delete_ref(ref_node)
                    unlink_node(ref_node)
                changed = True
                continue

            licm_var = LambdaVar(f"$licm{counter[0]}", hidden=True)
            counter[0] += 1
            licm_var.home = preheader_home
            preheader_home.locals.append(licm_var)
            hoisted_cse[cse_key] = (licm_var, result_type)

            replacement_ref = Ref(node.lexenv, node.source, licm_var)
            licm_var.refs.append(replacement_ref)
            insert_node_before(node, replacement_ref)
            delete_lvar_use(node)
            add_lvar_use(replacement_ref, orig_lvar)
            replacement_ref.derived_type = result_type
            orig_lvar.derived_type = result_type
            effects.ref_types_by_var[licm_var] = [result_type]

            anchor_lexenv = linked(preheader.last, preheader, "last").lexenv
            preheader_lexenv = anchor_lexenv.derive(policy=node.lexenv.policy)

            for in_ref in input_refs:
                in_lvar = linked(in_ref.lvar, in_ref, "lvar")
                in_type = in_ref.derived_type
                unlink_node(in_ref)
                add_lvar_use(in_ref, in_lvar)
                in_ref.derived_type = in_type
                in_lvar.derived_type = in_type
                in_ref.lexenv = preheader_lexenv
                _insert_into_preheader(preheader, insert_mode, in_ref)

            unlink_node(node)
            node.lexenv = preheader_lexenv
            hoisted_lvar = Lvar()
            store_node = Set(preheader_lexenv, node.source, licm_var, hoisted_lvar)
            licm_var.sets.append(store_node)
            hoisted_lvar.dest = store_node
            add_lvar_use(node, hoisted_lvar)
            node.derived_type = result_type
            hoisted_lvar.derived_type = result_type
            store_node.derived_type = UNBOUND

            _insert_into_preheader(preheader, insert_mode, node)
            _insert_into_preheader(preheader, insert_mode, store_node)
            changed = True

    return changed


def hoist_loop_invariants(component: Component) -> bool:
    """Hoist loop-invariant pure expressions and proven-bound constant global
    reads out of natural loops into their preheaders."""
    if any(lam.generator_body or lam.coroutine_body for lam in component.lambdas):
        return False
    for block in component.blocks():
        for node in block.nodes():
            if node.accept(_HAS_SUSPENSION):
                return False
    analyze(component)
    outer = component.outer_loop
    if outer is None:
        return False

    counter = [
        sum(
            1
            for lam in component.lambdas
            for var in lam.locals
            if var.name.startswith("$licm")
        )
    ]
    changed = False

    def walk_loop(loop: Loop) -> None:
        nonlocal changed
        for child in loop.inferiors:
            walk_loop(child)
        if _hoist_natural_loop(component, loop, counter):
            changed = True

    for child in outer.inferiors:
        walk_loop(child)

    if changed:
        component.reoptimize = True
    return changed

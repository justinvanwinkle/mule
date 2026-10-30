"""Pure double-dispatch AST/IR visitors and polymorphic type-lattice workloads.

Models compiler IR transformation and type-system operations (like ctype.py and
ir1tran.py) without isinstance checks, relying on polymorphic method dispatch,
visitor accept/visit double dispatch, and tuple-keyed memoization caches.
"""

import sys
import time

DEFAULT_SIZE = 2000
DEFAULT_LOOPS = 1


class IRNode:
    def accept(self, visitor: "NodeVisitor") -> object:
        raise NotImplementedError

    def node_weight(self) -> int:
        return 1

    def is_constant(self) -> bool:
        return False

    def const_int_value(self) -> int:
        return 0


class LiteralInt(IRNode):
    def __init__(self, value: int) -> None:
        self.value = value

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_literal_int(self)

    def is_constant(self) -> bool:
        return True

    def const_int_value(self) -> int:
        return self.value


class LiteralFloat(IRNode):
    def __init__(self, value: float) -> None:
        self.value = value

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_literal_float(self)

    def is_constant(self) -> bool:
        return True

    def const_int_value(self) -> int:
        return int(self.value)


class LiteralStr(IRNode):
    def __init__(self, value: str) -> None:
        self.value = value

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_literal_str(self)

    def is_constant(self) -> bool:
        return True

    def const_int_value(self) -> int:
        return len(self.value)


class LiteralBool(IRNode):
    def __init__(self, value: bool) -> None:
        self.value = value

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_literal_bool(self)

    def is_constant(self) -> bool:
        return True

    def const_int_value(self) -> int:
        return 1 if self.value else 0


class LiteralNone(IRNode):
    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_literal_none(self)

    def is_constant(self) -> bool:
        return True


class VarRef(IRNode):
    def __init__(self, name: str, slot: int) -> None:
        self.name = name
        self.slot = slot

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_var_ref(self)

    def node_weight(self) -> int:
        return 2


class AttrRead(IRNode):
    def __init__(self, obj: IRNode, attr: str) -> None:
        self.obj = obj
        self.attr = attr

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_attr_read(self)

    def node_weight(self) -> int:
        return 3 + self.obj.node_weight()


class AttrWrite(IRNode):
    def __init__(self, obj: IRNode, attr: str, value: IRNode) -> None:
        self.obj = obj
        self.attr = attr
        self.value = value

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_attr_write(self)

    def node_weight(self) -> int:
        return 4 + self.obj.node_weight() + self.value.node_weight()


class Subscript(IRNode):
    def __init__(self, target: IRNode, index: IRNode) -> None:
        self.target = target
        self.index = index

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_subscript(self)

    def node_weight(self) -> int:
        return 3 + self.target.node_weight() + self.index.node_weight()


class SliceNode(IRNode):
    def __init__(self, target: IRNode, start: IRNode, stop: IRNode) -> None:
        self.target = target
        self.start = start
        self.stop = stop

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_slice_node(self)

    def node_weight(self) -> int:
        return (
            4
            + self.target.node_weight()
            + self.start.node_weight()
            + self.stop.node_weight()
        )


class BinaryOp(IRNode):
    def __init__(self, op: str, left: IRNode, right: IRNode) -> None:
        self.op = op
        self.left = left
        self.right = right

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_binary_op(self)

    def node_weight(self) -> int:
        return 2 + self.left.node_weight() + self.right.node_weight()


class UnaryOp(IRNode):
    def __init__(self, op: str, operand: IRNode) -> None:
        self.op = op
        self.operand = operand

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_unary_op(self)

    def node_weight(self) -> int:
        return 2 + self.operand.node_weight()


class CompareOp(IRNode):
    def __init__(self, op: str, left: IRNode, right: IRNode) -> None:
        self.op = op
        self.left = left
        self.right = right

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_compare_op(self)

    def node_weight(self) -> int:
        return 2 + self.left.node_weight() + self.right.node_weight()


class CallNode(IRNode):
    def __init__(self, func: IRNode, args: list[IRNode]) -> None:
        self.func = func
        self.args = args

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_call_node(self)

    def node_weight(self) -> int:
        total = 5 + self.func.node_weight()
        for a in self.args:
            total += a.node_weight()
        return total


class IfExpr(IRNode):
    def __init__(self, cond: IRNode, then_br: IRNode, else_br: IRNode) -> None:
        self.cond = cond
        self.then_br = then_br
        self.else_br = else_br

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_if_expr(self)

    def node_weight(self) -> int:
        return (
            3
            + self.cond.node_weight()
            + self.then_br.node_weight()
            + self.else_br.node_weight()
        )


class LetBind(IRNode):
    def __init__(self, name: str, slot: int, val: IRNode, body: IRNode) -> None:
        self.name = name
        self.slot = slot
        self.val = val
        self.body = body

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_let_bind(self)

    def node_weight(self) -> int:
        return 3 + self.val.node_weight() + self.body.node_weight()


class SeqBlock(IRNode):
    def __init__(self, stmts: list[IRNode]) -> None:
        self.stmts = stmts

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_seq_block(self)

    def node_weight(self) -> int:
        total = 1
        for s in self.stmts:
            total += s.node_weight()
        return total


class WhileLoop(IRNode):
    def __init__(self, cond: IRNode, body: IRNode) -> None:
        self.cond = cond
        self.body = body

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_while_loop(self)

    def node_weight(self) -> int:
        return 6 + self.cond.node_weight() + self.body.node_weight()


class TryCatch(IRNode):
    def __init__(self, body: IRNode, err_var: str, handler: IRNode) -> None:
        self.body = body
        self.err_var = err_var
        self.handler = handler

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_try_catch(self)

    def node_weight(self) -> int:
        return 5 + self.body.node_weight() + self.handler.node_weight()


class TupleConstruct(IRNode):
    def __init__(self, elements: list[IRNode]) -> None:
        self.elements = elements

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_tuple_construct(self)

    def node_weight(self) -> int:
        total = 2
        for e in self.elements:
            total += e.node_weight()
        return total


class DictConstruct(IRNode):
    def __init__(self, keys: list[IRNode], values: list[IRNode]) -> None:
        self.keys = keys
        self.values = values

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_dict_construct(self)

    def node_weight(self) -> int:
        total = 3
        for k in self.keys:
            total += k.node_weight()
        for v in self.values:
            total += v.node_weight()
        return total


class ReturnNode(IRNode):
    def __init__(self, value: IRNode) -> None:
        self.value = value

    def accept(self, visitor: "NodeVisitor") -> object:
        return visitor.visit_return_node(self)

    def node_weight(self) -> int:
        return 2 + self.value.node_weight()


class NodeVisitor:
    def visit_literal_int(self, node: LiteralInt) -> object:
        raise NotImplementedError

    def visit_literal_float(self, node: LiteralFloat) -> object:
        raise NotImplementedError

    def visit_literal_str(self, node: LiteralStr) -> object:
        raise NotImplementedError

    def visit_literal_bool(self, node: LiteralBool) -> object:
        raise NotImplementedError

    def visit_literal_none(self, node: LiteralNone) -> object:
        raise NotImplementedError

    def visit_var_ref(self, node: VarRef) -> object:
        raise NotImplementedError

    def visit_attr_read(self, node: AttrRead) -> object:
        raise NotImplementedError

    def visit_attr_write(self, node: AttrWrite) -> object:
        raise NotImplementedError

    def visit_subscript(self, node: Subscript) -> object:
        raise NotImplementedError

    def visit_slice_node(self, node: SliceNode) -> object:
        raise NotImplementedError

    def visit_binary_op(self, node: BinaryOp) -> object:
        raise NotImplementedError

    def visit_unary_op(self, node: UnaryOp) -> object:
        raise NotImplementedError

    def visit_compare_op(self, node: CompareOp) -> object:
        raise NotImplementedError

    def visit_call_node(self, node: CallNode) -> object:
        raise NotImplementedError

    def visit_if_expr(self, node: IfExpr) -> object:
        raise NotImplementedError

    def visit_let_bind(self, node: LetBind) -> object:
        raise NotImplementedError

    def visit_seq_block(self, node: SeqBlock) -> object:
        raise NotImplementedError

    def visit_while_loop(self, node: WhileLoop) -> object:
        raise NotImplementedError

    def visit_try_catch(self, node: TryCatch) -> object:
        raise NotImplementedError

    def visit_tuple_construct(self, node: TupleConstruct) -> object:
        raise NotImplementedError

    def visit_dict_construct(self, node: DictConstruct) -> object:
        raise NotImplementedError

    def visit_return_node(self, node: ReturnNode) -> object:
        raise NotImplementedError


class TreeCloneVisitor(NodeVisitor):
    """Deep-clones an IR tree while shifting variable slots by an offset."""

    def __init__(self, slot_offset: int) -> None:
        self.slot_offset = slot_offset
        self.cloned_nodes = 0

    def clone(self, node: IRNode) -> IRNode:
        self.cloned_nodes += 1
        res: IRNode = node.accept(self)  # type: ignore[assignment]
        return res

    def visit_literal_int(self, node: LiteralInt) -> IRNode:
        return LiteralInt(node.value)

    def visit_literal_float(self, node: LiteralFloat) -> IRNode:
        return LiteralFloat(node.value)

    def visit_literal_str(self, node: LiteralStr) -> IRNode:
        return LiteralStr(node.value)

    def visit_literal_bool(self, node: LiteralBool) -> IRNode:
        return LiteralBool(node.value)

    def visit_literal_none(self, node: LiteralNone) -> IRNode:
        return LiteralNone()

    def visit_var_ref(self, node: VarRef) -> IRNode:
        return VarRef(node.name, node.slot + self.slot_offset)

    def visit_attr_read(self, node: AttrRead) -> IRNode:
        return AttrRead(self.clone(node.obj), node.attr)

    def visit_attr_write(self, node: AttrWrite) -> IRNode:
        return AttrWrite(self.clone(node.obj), node.attr, self.clone(node.value))

    def visit_subscript(self, node: Subscript) -> IRNode:
        return Subscript(self.clone(node.target), self.clone(node.index))

    def visit_slice_node(self, node: SliceNode) -> IRNode:
        return SliceNode(
            self.clone(node.target), self.clone(node.start), self.clone(node.stop)
        )

    def visit_binary_op(self, node: BinaryOp) -> IRNode:
        return BinaryOp(node.op, self.clone(node.left), self.clone(node.right))

    def visit_unary_op(self, node: UnaryOp) -> IRNode:
        return UnaryOp(node.op, self.clone(node.operand))

    def visit_compare_op(self, node: CompareOp) -> IRNode:
        return CompareOp(node.op, self.clone(node.left), self.clone(node.right))

    def visit_call_node(self, node: CallNode) -> IRNode:
        return CallNode(self.clone(node.func), [self.clone(a) for a in node.args])

    def visit_if_expr(self, node: IfExpr) -> IRNode:
        return IfExpr(
            self.clone(node.cond),
            self.clone(node.then_br),
            self.clone(node.else_br),
        )

    def visit_let_bind(self, node: LetBind) -> IRNode:
        return LetBind(
            node.name,
            node.slot + self.slot_offset,
            self.clone(node.val),
            self.clone(node.body),
        )

    def visit_seq_block(self, node: SeqBlock) -> IRNode:
        return SeqBlock([self.clone(s) for s in node.stmts])

    def visit_while_loop(self, node: WhileLoop) -> IRNode:
        return WhileLoop(self.clone(node.cond), self.clone(node.body))

    def visit_try_catch(self, node: TryCatch) -> IRNode:
        return TryCatch(self.clone(node.body), node.err_var, self.clone(node.handler))

    def visit_tuple_construct(self, node: TupleConstruct) -> IRNode:
        return TupleConstruct([self.clone(e) for e in node.elements])

    def visit_dict_construct(self, node: DictConstruct) -> IRNode:
        return DictConstruct(
            [self.clone(k) for k in node.keys],
            [self.clone(v) for v in node.values],
        )

    def visit_return_node(self, node: ReturnNode) -> IRNode:
        return ReturnNode(self.clone(node.value))


class ConstantFoldAndPruneVisitor(NodeVisitor):
    """Folds constant expressions, propagates LetBind constants, and prunes branches."""

    def __init__(self) -> None:
        self.env: dict[str, IRNode] = {}
        self.folded_count = 0
        self.pruned_branches = 0

    def rewrite(self, node: IRNode) -> IRNode:
        res: IRNode = node.accept(self)  # type: ignore[assignment]
        return res

    def visit_literal_int(self, node: LiteralInt) -> IRNode:
        return node

    def visit_literal_float(self, node: LiteralFloat) -> IRNode:
        return node

    def visit_literal_str(self, node: LiteralStr) -> IRNode:
        return node

    def visit_literal_bool(self, node: LiteralBool) -> IRNode:
        return node

    def visit_literal_none(self, node: LiteralNone) -> IRNode:
        return node

    def visit_var_ref(self, node: VarRef) -> IRNode:
        bound = self.env.get(node.name)
        if bound is not None:
            self.folded_count += 1
            return bound
        return node

    def visit_attr_read(self, node: AttrRead) -> IRNode:
        return AttrRead(self.rewrite(node.obj), node.attr)

    def visit_attr_write(self, node: AttrWrite) -> IRNode:
        return AttrWrite(self.rewrite(node.obj), node.attr, self.rewrite(node.value))

    def visit_subscript(self, node: Subscript) -> IRNode:
        return Subscript(self.rewrite(node.target), self.rewrite(node.index))

    def visit_slice_node(self, node: SliceNode) -> IRNode:
        return SliceNode(
            self.rewrite(node.target),
            self.rewrite(node.start),
            self.rewrite(node.stop),
        )

    def visit_binary_op(self, node: BinaryOp) -> IRNode:
        left = self.rewrite(node.left)
        right = self.rewrite(node.right)
        if left.is_constant() and right.is_constant():
            lv = left.const_int_value()
            rv = right.const_int_value()
            self.folded_count += 1
            if node.op == "+":
                return LiteralInt((lv + rv) & 0xFFFF)
            if node.op == "-":
                return LiteralInt((lv - rv) & 0xFFFF)
            if node.op == "*":
                return LiteralInt((lv * rv) & 0xFFFF)
            if node.op == "&":
                return LiteralInt(lv & rv)
            if node.op == "|":
                return LiteralInt(lv | rv)
            if node.op == "^":
                return LiteralInt(lv ^ rv)
        return BinaryOp(node.op, left, right)

    def visit_unary_op(self, node: UnaryOp) -> IRNode:
        opnd = self.rewrite(node.operand)
        if opnd.is_constant():
            v = opnd.const_int_value()
            self.folded_count += 1
            if node.op == "-":
                return LiteralInt(-v)
            if node.op == "~":
                return LiteralInt(~v & 0xFFFF)
            if node.op == "not":
                return LiteralBool(v == 0)
        return UnaryOp(node.op, opnd)

    def visit_compare_op(self, node: CompareOp) -> IRNode:
        left = self.rewrite(node.left)
        right = self.rewrite(node.right)
        if left.is_constant() and right.is_constant():
            lv = left.const_int_value()
            rv = right.const_int_value()
            self.folded_count += 1
            if node.op == "<":
                return LiteralBool(lv < rv)
            if node.op == "==":
                return LiteralBool(lv == rv)
            if node.op == ">":
                return LiteralBool(lv > rv)
            if node.op == "!=":
                return LiteralBool(lv != rv)
        return CompareOp(node.op, left, right)

    def visit_call_node(self, node: CallNode) -> IRNode:
        return CallNode(self.rewrite(node.func), [self.rewrite(a) for a in node.args])

    def visit_if_expr(self, node: IfExpr) -> IRNode:
        cond = self.rewrite(node.cond)
        if cond.is_constant():
            self.pruned_branches += 1
            if cond.const_int_value() != 0:
                return self.rewrite(node.then_br)
            return self.rewrite(node.else_br)
        return IfExpr(cond, self.rewrite(node.then_br), self.rewrite(node.else_br))

    def visit_let_bind(self, node: LetBind) -> IRNode:
        val = self.rewrite(node.val)
        prev = self.env.get(node.name)
        if val.is_constant():
            self.env[node.name] = val
        elif node.name in self.env:
            del self.env[node.name]
        body = self.rewrite(node.body)
        if prev is not None:
            self.env[node.name] = prev
        elif node.name in self.env:
            del self.env[node.name]
        return LetBind(node.name, node.slot, val, body)

    def visit_seq_block(self, node: SeqBlock) -> IRNode:
        new_stmts: list[IRNode] = []
        for s in node.stmts:
            rw = self.rewrite(s)
            new_stmts.append(rw)
        return SeqBlock(new_stmts)

    def visit_while_loop(self, node: WhileLoop) -> IRNode:
        cond = self.rewrite(node.cond)
        if cond.is_constant() and cond.const_int_value() == 0:
            self.pruned_branches += 1
            return LiteralNone()
        return WhileLoop(cond, self.rewrite(node.body))

    def visit_try_catch(self, node: TryCatch) -> IRNode:
        return TryCatch(
            self.rewrite(node.body), node.err_var, self.rewrite(node.handler)
        )

    def visit_tuple_construct(self, node: TupleConstruct) -> IRNode:
        return TupleConstruct([self.rewrite(e) for e in node.elements])

    def visit_dict_construct(self, node: DictConstruct) -> IRNode:
        return DictConstruct(
            [self.rewrite(k) for k in node.keys],
            [self.rewrite(v) for v in node.values],
        )

    def visit_return_node(self, node: ReturnNode) -> IRNode:
        return ReturnNode(self.rewrite(node.value))


class ScopeAndCostVisitor(NodeVisitor):
    """Computes variable usage counts, lexical depth, and abstract evaluation hash."""

    def __init__(self) -> None:
        self.depth = 0
        self.var_reads = 0
        self.bound_vars: dict[str, int] = {}

    def eval_node(self, node: IRNode) -> int:
        self.depth += 1
        res: int = node.accept(self)  # type: ignore[assignment]
        self.depth -= 1
        return res

    def visit_literal_int(self, node: LiteralInt) -> int:
        return (node.value * 31 + self.depth) & 0x7FFFFFFF

    def visit_literal_float(self, node: LiteralFloat) -> int:
        return (int(node.value * 100.0) + self.depth) & 0x7FFFFFFF

    def visit_literal_str(self, node: LiteralStr) -> int:
        return (len(node.value) * 97 + self.depth) & 0x7FFFFFFF

    def visit_literal_bool(self, node: LiteralBool) -> int:
        return (13 if node.value else 7) + self.depth

    def visit_literal_none(self, node: LiteralNone) -> int:
        return 3 + self.depth

    def visit_var_ref(self, node: VarRef) -> int:
        self.var_reads += 1
        lex_depth = self.bound_vars.get(node.name, 0)
        return (node.slot * 17 + lex_depth * 5 + self.depth) & 0x7FFFFFFF

    def visit_attr_read(self, node: AttrRead) -> int:
        return (self.eval_node(node.obj) * 3 + len(node.attr) * 11) & 0x7FFFFFFF

    def visit_attr_write(self, node: AttrWrite) -> int:
        return (
            self.eval_node(node.obj) * 5
            + self.eval_node(node.value) * 7
            + len(node.attr)
        ) & 0x7FFFFFFF

    def visit_subscript(self, node: Subscript) -> int:
        return (
            self.eval_node(node.target) * 13 + self.eval_node(node.index)
        ) & 0x7FFFFFFF

    def visit_slice_node(self, node: SliceNode) -> int:
        return (
            self.eval_node(node.target)
            + self.eval_node(node.start) * 3
            + self.eval_node(node.stop) * 7
        ) & 0x7FFFFFFF

    def visit_binary_op(self, node: BinaryOp) -> int:
        lv = self.eval_node(node.left)
        rv = self.eval_node(node.right)
        return (lv * 19 + rv * 23 + ord(node.op[0])) & 0x7FFFFFFF

    def visit_unary_op(self, node: UnaryOp) -> int:
        return (self.eval_node(node.operand) * 29 + len(node.op)) & 0x7FFFFFFF

    def visit_compare_op(self, node: CompareOp) -> int:
        return (
            self.eval_node(node.left) * 37
            + self.eval_node(node.right) * 41
            + len(node.op)
        ) & 0x7FFFFFFF

    def visit_call_node(self, node: CallNode) -> int:
        acc = self.eval_node(node.func) * 43
        for a in node.args:
            acc = (acc + self.eval_node(a) * 17) & 0x7FFFFFFF
        return acc

    def visit_if_expr(self, node: IfExpr) -> int:
        c = self.eval_node(node.cond)
        t = self.eval_node(node.then_br)
        e = self.eval_node(node.else_br)
        return (c * 11 + t * 13 + e * 17) & 0x7FFFFFFF

    def visit_let_bind(self, node: LetBind) -> int:
        v = self.eval_node(node.val)
        prev = self.bound_vars.get(node.name)
        self.bound_vars[node.name] = self.depth
        b = self.eval_node(node.body)
        if prev is not None:
            self.bound_vars[node.name] = prev
        else:
            del self.bound_vars[node.name]
        return (v * 23 + b * 29 + node.slot) & 0x7FFFFFFF

    def visit_seq_block(self, node: SeqBlock) -> int:
        acc = 19
        for s in node.stmts:
            acc = (acc * 31 + self.eval_node(s)) & 0x7FFFFFFF
        return acc

    def visit_while_loop(self, node: WhileLoop) -> int:
        return (
            self.eval_node(node.cond) * 47 + self.eval_node(node.body) * 53
        ) & 0x7FFFFFFF

    def visit_try_catch(self, node: TryCatch) -> int:
        b = self.eval_node(node.body)
        prev = self.bound_vars.get(node.err_var)
        self.bound_vars[node.err_var] = self.depth
        h = self.eval_node(node.handler)
        if prev is not None:
            self.bound_vars[node.err_var] = prev
        else:
            del self.bound_vars[node.err_var]
        return (b * 59 + h * 61) & 0x7FFFFFFF

    def visit_tuple_construct(self, node: TupleConstruct) -> int:
        acc = 67
        for e in node.elements:
            acc = (acc * 13 + self.eval_node(e)) & 0x7FFFFFFF
        return acc

    def visit_dict_construct(self, node: DictConstruct) -> int:
        acc = 71
        for k, v in zip(node.keys, node.values):
            acc = (acc + self.eval_node(k) * 7 + self.eval_node(v) * 11) & 0x7FFFFFFF
        return acc

    def visit_return_node(self, node: ReturnNode) -> int:
        return (self.eval_node(node.value) * 73 + 9) & 0x7FFFFFFF


# ==============================================================================
# Polymorphic Type Lattice Hierarchy (15 classes, zero isinstance)
# ==============================================================================


class CType:
    """Base compiler type supporting double-dispatch lattice operations."""

    def type_code(self) -> int:
        raise NotImplementedError

    def type_rank(self) -> int:
        return 1

    def accept(self, visitor: "TypeVisitor") -> object:
        raise NotImplementedError

    def fits(self, other: "CType") -> bool:
        return other.accept_fits_from(self)

    def accept_fits_from(self, source: "CType") -> bool:
        return False

    def fits_range_int(self, lo: int, hi: int) -> bool:
        return False

    def fits_float(self) -> bool:
        return False

    def fits_text(self, max_len: int) -> bool:
        return False

    def fits_tuple(self, items: tuple["CType", ...]) -> bool:
        return False

    def fits_list(self, elem: "CType") -> bool:
        return False

    def fits_dict(self, key_t: "CType", val_t: "CType") -> bool:
        return False

    def fits_set(self, elem: "CType") -> bool:
        return False

    def fits_instance(self, class_id: int, super_ids: frozenset[int]) -> bool:
        return False

    def fits_callable(self, params: tuple["CType", ...], ret: "CType") -> bool:
        return False

    def fits_frozen_map(self, key_t: "CType", val_t: "CType") -> bool:
        return False

    def union_members(self) -> tuple["CType", ...]:
        return (self,)

    def merge_compatible(self, other: "CType") -> "CType | None":
        return None

    def merge_range_int(self, lo: int, hi: int) -> "CType | None":
        return None

    def merge_float(self) -> "CType | None":
        return None

    def merge_text(self, max_len: int) -> "CType | None":
        return None

    def merge_list(self, elem: "CType") -> "CType | None":
        return None

    def merge_set(self, elem: "CType") -> "CType | None":
        return None

    def merge_instance(
        self, class_id: int, super_ids: frozenset[int]
    ) -> "CType | None":
        return None

    def union(self, other: "CType") -> "CType":
        if self.fits(other):
            return other
        if other.fits(self):
            return self
        merged = self.merge_compatible(other)
        if merged is not None:
            return merged
        combined: list[CType] = list(self.union_members())
        for m in other.union_members():
            already = False
            for idx, existing in enumerate(combined):
                if m.fits(existing):
                    already = True
                    break
                if existing.fits(m):
                    combined[idx] = m
                    already = True
                    break
                m_compat = existing.merge_compatible(m)
                if m_compat is not None:
                    combined[idx] = m_compat
                    already = True
                    break
            if not already:
                combined.append(m)
        if len(combined) == 1:
            return combined[0]
        if len(combined) > 5:
            return UNKNOWN_TYPE
        combined.sort(key=lambda t: t.type_code())
        return UnionType(tuple(combined))

    def intersection(self, other: "CType") -> "CType":
        if self.fits(other):
            return self
        if other.fits(self):
            return other
        return self.intersect_with(other)

    def intersect_with(self, other: "CType") -> "CType":
        return NEVER_TYPE

    def intersect_range_int(self, lo: int, hi: int) -> "CType":
        return NEVER_TYPE


class UnknownType(CType):
    def type_code(self) -> int:
        return 1000

    def accept(self, visitor: "TypeVisitor") -> object:
        return visitor.visit_unknown(self)

    def accept_fits_from(self, source: CType) -> bool:
        return True

    def union(self, other: CType) -> CType:
        return self

    def intersection(self, other: CType) -> CType:
        return other


class NeverType(CType):
    def type_code(self) -> int:
        return 0

    def accept(self, visitor: "TypeVisitor") -> object:
        return visitor.visit_never(self)

    def fits(self, other: CType) -> bool:
        return True

    def accept_fits_from(self, source: CType) -> bool:
        return source.type_code() == 0

    def union(self, other: CType) -> CType:
        return other

    def intersection(self, other: CType) -> CType:
        return self


class NoneType_(CType):
    def type_code(self) -> int:
        return 10

    def accept(self, visitor: "TypeVisitor") -> object:
        return visitor.visit_none(self)

    def accept_fits_from(self, source: CType) -> bool:
        return source.type_code() == 10


class BoolType(CType):
    def type_code(self) -> int:
        return 20

    def accept(self, visitor: "TypeVisitor") -> object:
        return visitor.visit_bool(self)

    def accept_fits_from(self, source: CType) -> bool:
        return source.type_code() == 20

    def fits_range_int(self, lo: int, hi: int) -> bool:
        return lo <= 0 and hi >= 1

    def fits_float(self) -> bool:
        return True

    def merge_compatible(self, other: CType) -> CType | None:
        return other.merge_range_int(0, 1)


class RangeIntType(CType):
    def __init__(self, lo: int, hi: int) -> None:
        self.lo = lo
        self.hi = hi

    def type_code(self) -> int:
        return 30 + ((self.lo & 0xFF) << 4) + (self.hi & 0xFF)

    def accept(self, visitor: "TypeVisitor") -> object:
        return visitor.visit_range_int(self)

    def accept_fits_from(self, source: CType) -> bool:
        return source.fits_range_int(self.lo, self.hi)

    def fits_range_int(self, lo: int, hi: int) -> bool:
        return self.lo >= lo and self.hi <= hi

    def fits_float(self) -> bool:
        return True

    def merge_compatible(self, other: CType) -> CType | None:
        return other.merge_range_int(self.lo, self.hi)

    def merge_range_int(self, lo: int, hi: int) -> CType | None:
        new_lo = min(lo, self.lo)
        new_hi = max(hi, self.hi)
        return RangeIntType(new_lo, new_hi)

    def intersect_with(self, other: CType) -> CType:
        return other.intersect_range_int(self.lo, self.hi)

    def intersect_range_int(self, lo: int, hi: int) -> CType:
        max_lo = max(lo, self.lo)
        min_hi = min(hi, self.hi)
        if max_lo <= min_hi:
            if max_lo == 0 and min_hi == 1:
                return BOOL_TYPE
            return RangeIntType(max_lo, min_hi)
        return NEVER_TYPE


class FloatType_(CType):
    def type_code(self) -> int:
        return 40

    def accept(self, visitor: "TypeVisitor") -> object:
        return visitor.visit_float(self)

    def accept_fits_from(self, source: CType) -> bool:
        return source.fits_float()

    def fits_float(self) -> bool:
        return True

    def merge_range_int(self, lo: int, hi: int) -> CType | None:
        return self


class TextType(CType):
    def __init__(self, max_len: int) -> None:
        self.max_len = max_len

    def type_code(self) -> int:
        return 50 + (self.max_len & 0xFFF)

    def accept(self, visitor: "TypeVisitor") -> object:
        return visitor.visit_text(self)

    def accept_fits_from(self, source: CType) -> bool:
        return source.fits_text(self.max_len)

    def fits_text(self, max_len: int) -> bool:
        return self.max_len <= max_len

    def merge_compatible(self, other: CType) -> CType | None:
        return other.merge_text(self.max_len)

    def merge_text(self, max_len: int) -> CType | None:
        return TextType(max(max_len, self.max_len))


class BytesType(CType):
    def __init__(self, fixed_len: int) -> None:
        self.fixed_len = fixed_len

    def type_code(self) -> int:
        return 60 + (self.fixed_len & 0xFF)

    def accept(self, visitor: "TypeVisitor") -> object:
        return visitor.visit_bytes(self)

    def accept_fits_from(self, source: CType) -> bool:
        return source.type_code() == self.type_code()


class TupleType(CType):
    def __init__(self, items: tuple[CType, ...]) -> None:
        self.items = items

    def type_code(self) -> int:
        acc = 70 + len(self.items) * 13
        for it in self.items:
            acc = (acc * 31 + it.type_code()) & 0x7FFFF
        return 2000 + acc

    def type_rank(self) -> int:
        total = 2
        for it in self.items:
            total += it.type_rank()
        return total

    def accept(self, visitor: "TypeVisitor") -> object:
        return visitor.visit_tuple(self)

    def accept_fits_from(self, source: CType) -> bool:
        return source.fits_tuple(self.items)

    def fits_tuple(self, items: tuple[CType, ...]) -> bool:
        if len(self.items) != len(items):
            return False
        for a, b in zip(self.items, items):
            if not a.fits(b):
                return False
        return True


class ListType(CType):
    def __init__(self, elem: CType) -> None:
        self.elem = elem

    def type_code(self) -> int:
        return 3000 + (self.elem.type_code() & 0xFFFF)

    def type_rank(self) -> int:
        return 2 + self.elem.type_rank()

    def accept(self, visitor: "TypeVisitor") -> object:
        return visitor.visit_list(self)

    def accept_fits_from(self, source: CType) -> bool:
        return source.fits_list(self.elem)

    def fits_list(self, elem: CType) -> bool:
        return self.elem.fits(elem) and elem.fits(self.elem)

    def merge_compatible(self, other: CType) -> CType | None:
        return other.merge_list(self.elem)

    def merge_list(self, elem: CType) -> CType | None:
        return ListType(self.elem.union(elem))


class SetType(CType):
    def __init__(self, elem: CType) -> None:
        self.elem = elem

    def type_code(self) -> int:
        return 3500 + (self.elem.type_code() & 0xFFFF)

    def type_rank(self) -> int:
        return 2 + self.elem.type_rank()

    def accept(self, visitor: "TypeVisitor") -> object:
        return visitor.visit_set(self)

    def accept_fits_from(self, source: CType) -> bool:
        return source.fits_set(self.elem)

    def fits_set(self, elem: CType) -> bool:
        return self.elem.fits(elem) and elem.fits(self.elem)

    def merge_compatible(self, other: CType) -> CType | None:
        return other.merge_set(self.elem)

    def merge_set(self, elem: CType) -> CType | None:
        return SetType(self.elem.union(elem))


class DictType(CType):
    def __init__(self, key_t: CType, val_t: CType) -> None:
        self.key_t = key_t
        self.val_t = val_t

    def type_code(self) -> int:
        return 4000 + ((self.key_t.type_code() * 17 + self.val_t.type_code()) & 0xFFFF)

    def type_rank(self) -> int:
        return 3 + self.key_t.type_rank() + self.val_t.type_rank()

    def accept(self, visitor: "TypeVisitor") -> object:
        return visitor.visit_dict(self)

    def accept_fits_from(self, source: CType) -> bool:
        return source.fits_dict(self.key_t, self.val_t)

    def fits_dict(self, key_t: CType, val_t: CType) -> bool:
        return (
            self.key_t.fits(key_t)
            and key_t.fits(self.key_t)
            and self.val_t.fits(val_t)
            and val_t.fits(self.val_t)
        )

    def fits_frozen_map(self, key_t: CType, val_t: CType) -> bool:
        return self.key_t.fits(key_t) and self.val_t.fits(val_t)


class FrozenMapType(CType):
    def __init__(self, key_t: CType, val_t: CType) -> None:
        self.key_t = key_t
        self.val_t = val_t

    def type_code(self) -> int:
        return 4500 + ((self.key_t.type_code() * 19 + self.val_t.type_code()) & 0xFFFF)

    def type_rank(self) -> int:
        return 3 + self.key_t.type_rank() + self.val_t.type_rank()

    def accept(self, visitor: "TypeVisitor") -> object:
        return visitor.visit_frozen_map(self)

    def accept_fits_from(self, source: CType) -> bool:
        return source.fits_frozen_map(self.key_t, self.val_t)

    def fits_frozen_map(self, key_t: CType, val_t: CType) -> bool:
        return self.key_t.fits(key_t) and self.val_t.fits(val_t)


class InstanceType(CType):
    def __init__(self, class_id: int, super_ids: frozenset[int]) -> None:
        self.class_id = class_id
        self.super_ids = super_ids

    def type_code(self) -> int:
        return 5000 + self.class_id

    def accept(self, visitor: "TypeVisitor") -> object:
        return visitor.visit_instance(self)

    def accept_fits_from(self, source: CType) -> bool:
        return source.fits_instance(self.class_id, self.super_ids)

    def fits_instance(self, class_id: int, super_ids: frozenset[int]) -> bool:
        return self.class_id == class_id or class_id in self.super_ids

    def merge_compatible(self, other: CType) -> CType | None:
        return other.merge_instance(self.class_id, self.super_ids)

    def merge_instance(self, class_id: int, super_ids: frozenset[int]) -> CType | None:
        common = self.super_ids & super_ids
        if not common:
            return None
        top_id = max(common)
        ancestors = frozenset(cid for cid in common if cid <= top_id)
        return InstanceType(top_id, ancestors)


class CallableType(CType):
    def __init__(self, params: tuple[CType, ...], ret: CType) -> None:
        self.params = params
        self.ret = ret

    def type_code(self) -> int:
        acc = 6000 + self.ret.type_code()
        for p in self.params:
            acc = (acc * 17 + p.type_code()) & 0x7FFFF
        return acc

    def type_rank(self) -> int:
        total = 2 + self.ret.type_rank()
        for p in self.params:
            total += p.type_rank()
        return total

    def accept(self, visitor: "TypeVisitor") -> object:
        return visitor.visit_callable(self)

    def accept_fits_from(self, source: CType) -> bool:
        return source.fits_callable(self.params, self.ret)

    def fits_callable(self, params: tuple[CType, ...], ret: CType) -> bool:
        if len(self.params) != len(params):
            return False
        if not self.ret.fits(ret):
            return False
        for my_p, target_p in zip(self.params, params):
            if not target_p.fits(my_p):
                return False
        return True


class UnionType(CType):
    def __init__(self, members: tuple[CType, ...]) -> None:
        self.members = members

    def type_code(self) -> int:
        acc = 7000
        for m in self.members:
            acc = (acc * 23 + m.type_code()) & 0x7FFFF
        return acc

    def type_rank(self) -> int:
        total = 1
        for m in self.members:
            total += m.type_rank()
        return total

    def union_members(self) -> tuple[CType, ...]:
        return self.members

    def accept(self, visitor: "TypeVisitor") -> object:
        return visitor.visit_union(self)

    def fits(self, other: CType) -> bool:
        for m in self.members:
            if not m.fits(other):
                return False
        return True

    def accept_fits_from(self, source: CType) -> bool:
        for m in self.members:
            if source.fits(m):
                return True
        return False

    def intersect_with(self, other: CType) -> CType:
        acc: CType = NEVER_TYPE
        for m in self.members:
            part = m.intersection(other)
            acc = acc.union(part)
        return acc


UNKNOWN_TYPE = UnknownType()
NEVER_TYPE = NeverType()
NONE_TYPE = NoneType_()
BOOL_TYPE = BoolType()
FLOAT_TYPE = FloatType_()


class TypeVisitor:
    def visit_unknown(self, t: UnknownType) -> object:
        raise NotImplementedError

    def visit_never(self, t: NeverType) -> object:
        raise NotImplementedError

    def visit_none(self, t: NoneType_) -> object:
        raise NotImplementedError

    def visit_bool(self, t: BoolType) -> object:
        raise NotImplementedError

    def visit_range_int(self, t: RangeIntType) -> object:
        raise NotImplementedError

    def visit_float(self, t: FloatType_) -> object:
        raise NotImplementedError

    def visit_text(self, t: TextType) -> object:
        raise NotImplementedError

    def visit_bytes(self, t: BytesType) -> object:
        raise NotImplementedError

    def visit_tuple(self, t: TupleType) -> object:
        raise NotImplementedError

    def visit_list(self, t: ListType) -> object:
        raise NotImplementedError

    def visit_set(self, t: SetType) -> object:
        raise NotImplementedError

    def visit_dict(self, t: DictType) -> object:
        raise NotImplementedError

    def visit_frozen_map(self, t: FrozenMapType) -> object:
        raise NotImplementedError

    def visit_instance(self, t: InstanceType) -> object:
        raise NotImplementedError

    def visit_callable(self, t: CallableType) -> object:
        raise NotImplementedError

    def visit_union(self, t: UnionType) -> object:
        raise NotImplementedError


class TypeLayoutVisitor(TypeVisitor):
    """Computes representation descriptor (size_bytes, align, pointer_mask) via double dispatch."""

    def __init__(self) -> None:
        self.memo: dict[int, tuple[int, int, int]] = {}

    def layout_of(self, t: CType) -> tuple[int, int, int]:
        code = t.type_code()
        cached = self.memo.get(code)
        if cached is not None:
            return cached
        res: tuple[int, int, int] = t.accept(self)  # type: ignore[assignment]
        self.memo[code] = res
        return res

    def visit_unknown(self, t: UnknownType) -> tuple[int, int, int]:
        return (16, 8, 3)

    def visit_never(self, t: NeverType) -> tuple[int, int, int]:
        return (0, 1, 0)

    def visit_none(self, t: NoneType_) -> tuple[int, int, int]:
        return (0, 1, 0)

    def visit_bool(self, t: BoolType) -> tuple[int, int, int]:
        return (1, 1, 0)

    def visit_range_int(self, t: RangeIntType) -> tuple[int, int, int]:
        span = t.hi - t.lo
        if span <= 255:
            return (1, 1, 0)
        if span <= 65535:
            return (2, 2, 0)
        return (8, 8, 0)

    def visit_float(self, t: FloatType_) -> tuple[int, int, int]:
        return (8, 8, 0)

    def visit_text(self, t: TextType) -> tuple[int, int, int]:
        return (16, 8, 1)

    def visit_bytes(self, t: BytesType) -> tuple[int, int, int]:
        return (8 + t.fixed_len, 8, 0)

    def visit_tuple(self, t: TupleType) -> tuple[int, int, int]:
        size = 0
        max_align = 1
        mask = 0
        for idx, item in enumerate(t.items):
            isz, ialign, imask = self.layout_of(item)
            max_align = max(max_align, ialign)
            rem = size % ialign
            if rem != 0:
                size += ialign - rem
            size += isz
            mask |= (imask << (idx & 7)) & 0xFFFF
        return (size, max_align, mask)

    def visit_list(self, t: ListType) -> tuple[int, int, int]:
        esz, _ealign, emask = self.layout_of(t.elem)
        return (24 + (esz & 7), 8, 1 | ((emask << 1) & 0xFF))

    def visit_set(self, t: SetType) -> tuple[int, int, int]:
        esz, _ealign, emask = self.layout_of(t.elem)
        return (32 + (esz & 7), 8, 3 | ((emask << 2) & 0xFF))

    def visit_dict(self, t: DictType) -> tuple[int, int, int]:
        ksz, _ka, km = self.layout_of(t.key_t)
        vsz, _va, vm = self.layout_of(t.val_t)
        return (40 + ((ksz + vsz) & 15), 8, 7 | ((km ^ vm) & 0xFF))

    def visit_frozen_map(self, t: FrozenMapType) -> tuple[int, int, int]:
        ksz, _ka, km = self.layout_of(t.key_t)
        vsz, _va, vm = self.layout_of(t.val_t)
        return (24 + ((ksz + vsz) & 7), 8, 3 | ((km | vm) & 0xFF))

    def visit_instance(self, t: InstanceType) -> tuple[int, int, int]:
        return (8 + len(t.super_ids) * 8, 8, (1 << len(t.super_ids)) - 1)

    def visit_callable(self, t: CallableType) -> tuple[int, int, int]:
        rsz, _ra, rm = self.layout_of(t.ret)
        acc = rsz + rm
        for p in t.params:
            psz, _pa, pm = self.layout_of(p)
            acc += psz + pm
        return (16, 8, acc & 0xFF)

    def visit_union(self, t: UnionType) -> tuple[int, int, int]:
        max_sz = 0
        max_align = 1
        mask = 1
        for m in t.members:
            msz, malign, mmask = self.layout_of(m)
            max_sz = max(max_sz, msz)
            max_align = max(max_align, malign)
            mask |= mmask
        return (max_sz + 8, max_align, mask)


def _build_ir_tree(seed: int) -> IRNode:
    s = seed
    c1 = LiteralInt((s & 0x3F) + 2)
    c2 = LiteralInt(((s >> 3) & 0x1F) + 1)
    c3 = LiteralFloat(float((s & 0xF) + 1.5))
    s_lit = LiteralStr("field_" + str(s & 7))
    b_lit = LiteralBool((s & 1) == 0)

    folded_arith = BinaryOp("+", BinaryOp("*", c1, c2), UnaryOp("-", LiteralInt(5)))
    v_x = VarRef("x", 1)
    v_y = VarRef("y", 2)

    dead_cond = CompareOp("<", LiteralInt(10), LiteralInt(20))
    live_branch = BinaryOp("+", v_x, folded_arith)
    dead_branch = CallNode(
        VarRef("slow_fn", 9), [LiteralStr("unused"), c3, LiteralNone()]
    )
    prunable_if = IfExpr(dead_cond, live_branch, dead_branch)

    attr_r = AttrRead(v_y, "payload")
    sub_node = Subscript(attr_r, BinaryOp("&", v_x, LiteralInt(7)))
    slc_node = SliceNode(sub_node, LiteralInt(0), c2)

    tup_node = TupleConstruct([prunable_if, slc_node, b_lit, s_lit])
    dict_node = DictConstruct([LiteralStr("k0"), LiteralStr("k1")], [tup_node, c3])

    let_inner = LetBind(
        "k_const",
        3,
        BinaryOp("|", LiteralInt(12), LiteralInt(3)),
        BinaryOp("*", VarRef("k_const", 3), v_x),
    )

    loop_node = WhileLoop(
        CompareOp("==", LiteralInt(1), LiteralInt(2)),
        AttrWrite(v_y, "mutated", let_inner),
    )

    try_node = TryCatch(
        SeqBlock([let_inner, loop_node, dict_node]),
        "err",
        ReturnNode(UnaryOp("~", VarRef("err", 4))),
    )
    return LetBind("x", 1, LiteralInt((s & 15) + 3), try_node)


def _build_type_catalog(size: int) -> list[CType]:
    base_supers_a = frozenset([1, 2])
    base_supers_b = frozenset([1, 3])
    base_supers_c = frozenset([1, 2, 4])

    small_int = RangeIntType(0, 100)
    signed_byte = RangeIntType(-128, 127)
    wide_int = RangeIntType(-10000, 50000)
    short_str = TextType(32)
    long_str = TextType(256)

    pool: list[CType] = [
        UNKNOWN_TYPE,
        NEVER_TYPE,
        NONE_TYPE,
        BOOL_TYPE,
        FLOAT_TYPE,
        small_int,
        signed_byte,
        wide_int,
        short_str,
        long_str,
        BytesType(16),
        BytesType(64),
        InstanceType(2, base_supers_a),
        InstanceType(3, base_supers_b),
        InstanceType(4, base_supers_c),
        ListType(small_int),
        ListType(BOOL_TYPE),
        SetType(short_str),
        DictType(short_str, small_int),
        FrozenMapType(long_str, wide_int),
        TupleType((BOOL_TYPE, small_int, short_str)),
        TupleType((RangeIntType(0, 1), signed_byte, long_str)),
        CallableType((wide_int, short_str), BOOL_TYPE),
        CallableType((small_int, long_str), small_int),
        UnionType((NONE_TYPE, small_int)),
        UnionType((BOOL_TYPE, short_str, InstanceType(2, base_supers_a))),
    ]

    catalog: list[CType] = []
    state = 2901845
    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        t1 = pool[state % len(pool)]
        t2 = pool[(state >> 5) % len(pool)]
        if (i & 3) == 0:
            catalog.append(t1.union(t2))
        elif (i & 3) == 1:
            catalog.append(TupleType((t1, t2)))
        elif (i & 3) == 2:
            catalog.append(DictType(t1, t2))
        else:
            catalog.append(t1)
    return catalog


def prepare(size: int) -> tuple[list[IRNode], list[CType]]:
    trees: list[IRNode] = []
    state = 918273645
    for i in range(size):
        state = (state * 1664525 + 1013904223) & 0x7FFFFFFF
        trees.append(_build_ir_tree(state ^ i))
    types = _build_type_catalog(size)
    return trees, types


def scenario_ast_visitor_rewriter(trees: list[IRNode]) -> tuple[int, int, int, int]:
    """Run multi-pass double-dispatch IR visitors (clone, constant fold/prune, scope/cost)."""
    total_folded = 0
    total_pruned = 0
    total_weight_delta = 0
    eval_checksum = 0

    cloner = TreeCloneVisitor(slot_offset=4)
    folder = ConstantFoldAndPruneVisitor()
    analyzer = ScopeAndCostVisitor()

    for _ in range(205):
        for tree in trees:
            before_weight = tree.node_weight()
            cloned = cloner.clone(tree)
            folder.env.clear()
            rewritten = folder.rewrite(cloned)
            after_weight = rewritten.node_weight()
            total_weight_delta += before_weight - after_weight
            eval_checksum = (eval_checksum + analyzer.eval_node(rewritten)) & 0x7FFFFFFF

    total_folded = folder.folded_count
    total_pruned = folder.pruned_branches
    return total_folded, total_pruned, total_weight_delta, eval_checksum


def scenario_type_lattice_operations(types: list[CType]) -> tuple[int, int, int, int]:
    """Run polymorphic subtype checks, lattice union/intersection, and memoized layout visitors."""
    fits_hits = 0
    union_rank_sum = 0
    intersect_non_never = 0
    layout_hash = 0

    fits_cache: dict[tuple[int, int], bool] = {}
    union_cache: dict[tuple[int, int], CType] = {}
    layout_visitor = TypeLayoutVisitor()
    n = len(types)

    for round_idx in range(360):
        fits_cache.clear()
        union_cache.clear()
        layout_visitor.memo.clear()
        stride = (round_idx % 5) + 1
        for i in range(n):
            a = types[i]
            b = types[(i + stride) % n]
            c = types[(i + stride * 2) % n]

            ca = a.type_code()
            cb = b.type_code()
            pair_key = (ca, cb)

            is_sub = fits_cache.get(pair_key)
            if is_sub is None:
                is_sub = a.fits(b)
                fits_cache[pair_key] = is_sub
            if is_sub:
                fits_hits += 1

            u_ab = union_cache.get(pair_key)
            if u_ab is None:
                u_ab = a.union(b)
                union_cache[pair_key] = u_ab

            u_abc = u_ab.union(c)
            union_rank_sum += u_abc.type_rank()

            inter = u_ab.intersection(b.union(c))
            if inter.type_code() != 0:
                intersect_non_never += 1

            sz, align, mask = layout_visitor.layout_of(u_abc)
            layout_hash = (layout_hash + sz * 13 + align * 7 + mask) & 0x7FFFFFFF

    return fits_hits, union_rank_sum, intersect_non_never, layout_hash


def main() -> None:
    size = DEFAULT_SIZE
    loops = DEFAULT_LOOPS
    scenario = "all"
    args = sys.argv[1:]
    i = 0
    while i < len(args):
        if args[i] == "--size" and i + 1 < len(args):
            size = int(args[i + 1])
            i += 2
        elif args[i] == "--loops" and i + 1 < len(args):
            loops = int(args[i + 1])
            i += 2
        elif args[i] == "--scenario" and i + 1 < len(args):
            scenario = args[i + 1]
            i += 2
        else:
            raise ValueError(f"Unknown argument: {args[i]}")

    t0 = time.perf_counter()
    trees, types = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    ast_s = 0.0
    lat_s = 0.0
    for _ in range(loops):
        r1: tuple[int, ...] = ()
        r2: tuple[int, ...] = ()
        if scenario in ("all", "ast_visitor_rewriter"):
            s0 = time.perf_counter()
            r1 = scenario_ast_visitor_rewriter(trees)
            ast_s += time.perf_counter() - s0
        if scenario in ("all", "type_lattice_operations"):
            s1 = time.perf_counter()
            r2 = scenario_type_lattice_operations(types)
            lat_s += time.perf_counter() - s1
        combined = r1 + r2
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (ast_s + lat_s) / loops
    print("topic\tdispatch/visitors_and_lattice")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "ast_visitor_rewriter"):
        print(f"ast_visitor_rewriter\t{ast_s / loops:.6f}")
    if scenario in ("all", "type_lattice_operations"):
        print(f"type_lattice_operations\t{lat_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()

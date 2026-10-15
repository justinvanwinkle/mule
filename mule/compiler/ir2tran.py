"""IR1 to IR2 conversion, following ``src/compiler/ir2tran.lisp``.

Each IR1 block becomes an ``IR2Block`` of VOPs. Every lvar delivers its value
through the TN that LTN's annotation designates; every local variable has a
TN of its own; captured variables live in hidden TNs holding their cells or
copied values. Known calls become instances of the template LTN selected;
full calls, references, assignments, branches, casts, and returns become the
fixed templates the backend defines for them.

Conversion runs once per lambda per entry context. A context carries the
facts a body is generated under; its TNs record the derived value type so
code generation can refine representations and skip releases of immortal
values.

Python adaptations (``spec/python-changes.md``):

- A ``Bind`` converts to the entry prologue: parameter moves, closure loads,
  and cell allocation for the lambda's own captured-and-assigned variables.
  SBCL's XEP conventions are code generation's wrappers instead.
- A never-returning known call (``%raise``, ``%reraise``, ``%unwind``) ends
  its block the way SBCL's ``%unwind`` templates do; the error edge to the
  handler block is implicit in the block's cleanup.
- A resolved method delivers two locations: the method and the receiver.
"""

from __future__ import annotations

from dataclasses import dataclass, replace

from mule import ast

from . import fndb, vops
from .ctype import UNKNOWN, NoneType, TupleType
from .ir1util import block_exception_target, made_lambda
from .ltn import EntryContext, primitive_type_of
from .node import (
    Bind,
    CallKind,
    Cast,
    Combination,
    Constant,
    GlobalVar,
    If,
    KnownFunction,
    Lambda,
    LambdaVar,
    LeafVisitor,
    NodeVisitor,
    Ref,
    Return,
    Set,
    linked,
)
from .vmdef import templates_for
from .vop import (
    RANGE_T,
    VOP,
    IR2Block,
    IR2Component,
    IR2Environment,
    IR2LvarKind,
    T,
    TNRef,
)


@dataclass(frozen=True, slots=True)
class LocalAccess:
    """Code generation data of a local read: the name for the unbound-local
    error, whether the read is the last use (ownership moves), whether
    the variable is proven bound, and whether the consumer borrows the slot."""

    name: str
    move: bool
    bound: bool = False
    borrow: bool = False


_BORROWING_VOPS = frozenset(
    {
        "getattr",
        "setattr",
        "resolve-method",
        "branch",
        "store-global",
        "build-list",
        "unpack-ref",
        "has-binding",
        "class-store",
        "class-delete",
        "enter-handler",
        "assertion-failure",
        "unary-not",
        "compare-equal",
        "compare-not_equal",
        "compare-less",
        "compare-less_equal",
        "compare-greater",
        "compare-greater_equal",
        "compare-is",
        "compare-is_not",
        "binary-divide",
        "binary-floor_divide",
        "binary-power",
        "binary-left_shift",
        "binary-right_shift",
        "binary-bit_and",
        "binary-bit_xor",
        "binary-bit_or",
        "inplace-divide",
        "inplace-floor_divide",
        "inplace-power",
        "inplace-left_shift",
        "inplace-right_shift",
        "inplace-bit_and",
        "inplace-bit_xor",
        "inplace-bit_or",
        "sequence-available",
    }
)
_RIGHT_BORROWING_VOPS = frozenset(
    {
        "binary-add",
        "binary-subtract",
        "binary-multiply",
        "inplace-add",
        "inplace-subtract",
        "inplace-multiply",
    }
)
_PURE_FIXNUM_VOPS = frozenset(
    {
        "binary-add",
        "binary-subtract",
        "binary-multiply",
        "binary-bit_and",
        "binary-bit_or",
        "binary-bit_xor",
        "binary-left_shift",
        "binary-right_shift",
    }
)


_NON_BORROWING_PRIMITIVES = frozenset(
    {
        "primitive-_string_protocol",
        "primitive-_decimal_protocol",
        "primitive-_percent_decimal_protocol",
        "primitive-_format_value_protocol",
        "primitive-_representation_protocol",
        "primitive-_hex_protocol",
        "primitive-_initialize",
        "primitive-_read_int64",
        "primitive-_write_int64",
    }
)


def _borrows_operand(vop: VOP, ref: TNRef) -> bool:
    name = vop.info.name
    if name in _BORROWING_VOPS:
        return True
    if name in _RIGHT_BORROWING_VOPS and len(vop.args) == 2 and ref is vop.args[1]:
        return True
    if name == "full-call" and vop.args and ref is vop.args[0]:
        return True
    return name.startswith("primitive-") and name not in _NON_BORROWING_PRIMITIVES


def _safe_borrow_step(step: VOP, source) -> bool:
    name = step.info.name
    if name == "constant":
        return True
    if name == "load-local":
        return not (step.args[0].tn is source and step.codegen_info.move)
    if name in _PURE_FIXNUM_VOPS and step.results:
        return (step.results[0].tn.type or UNKNOWN).is_fixnum and all(
            (arg.tn.type or UNKNOWN).is_fixnum for arg in step.args
        )
    return False


@dataclass(frozen=True, slots=True)
class UnpackCheck:
    arity: int
    proved: bool


@dataclass(frozen=True, slots=True)
class UnpackSlot:
    index: int
    proved: bool
    borrow_parent: bool


@dataclass(frozen=True, slots=True)
class CallSite:
    """Code generation data of a full call: the keyword names per argument,
    the source location, and the predicted target when flow analysis found
    one (``GUARDED``), with whether the identity was proven."""

    keywords: tuple
    location: tuple | None
    target: Lambda | None
    proven: bool


@dataclass(frozen=True, slots=True)
class MethodResolution:
    name: str
    exact_list_append: bool = False


@dataclass(frozen=True, slots=True)
class MethodCall:
    keywords: tuple
    name: str
    exact_list_append: bool = False


@dataclass(frozen=True, slots=True)
class FunctionObject:
    lambda_: Lambda
    info: object


@dataclass(frozen=True, slots=True)
class TypeCheck:
    contract: object
    location: tuple | None
    safety: int


@dataclass(frozen=True, slots=True)
class BranchTargets:
    consequent: str
    alternative: str


def ir2_convert(lambda_: Lambda, context: EntryContext) -> IR2Component:
    """Convert ``lambda_`` under ``context`` into VOPs."""
    environment = IR2Environment(lambda_, context)
    component = IR2Component(lambda_, context, environment)
    converter = _Converter(component)
    converter.prepare_environment()
    blocks = list(linked(lambda_.component, lambda_, "component").blocks())
    for index, block in enumerate(blocks):
        ir2_block = IR2Block(block, index)
        component.ir2_blocks[block] = ir2_block
        component.blocks.append(ir2_block)
    for block in blocks:
        converter.convert_block(block)
    _mark_last_local_reads(component)
    _mark_borrowed_local_reads(component)
    _clear_lvar_info(lambda_)
    return component


def _mark_borrowed_local_reads(component: IR2Component) -> None:
    """Mark bound ``LOAD_LOCAL`` VOPs whose single block-local consumer only
    borrows the value while the local slot still retains it."""
    reads = component.reads
    writes = component.writes
    for block in component.blocks:
        positions = {vop: index for index, vop in enumerate(block.vops)}
        for index, vop in enumerate(block.vops):
            if vop.info is not vops.LOAD_LOCAL:
                continue
            access = vop.codegen_info
            if not access.bound or access.move:
                continue
            source = vop.args[0].tn
            result = vop.results[0].tn
            result_reads = reads.get(result)
            result_writes = writes.get(result)
            if (
                result_writes is None
                or len(result_writes) != 1
                or result_reads is None
                or len(result_reads) != 1
            ):
                continue
            reader, reader_ref = result_reads[0]
            reader_index = positions.get(reader)
            if reader_index is None or reader_index <= index:
                continue
            if not _borrows_operand(reader, reader_ref):
                continue
            end_index = reader_index
            if (
                reader.info.name == "resolve-method"
                and reader.codegen_info.exact_list_append
            ):
                bound = reader.results[1].tn
                bound_reads = reads.get(bound)
                bound_writes = writes.get(bound)
                if (
                    bound_writes is None
                    or len(bound_writes) != 1
                    or bound_reads is None
                    or len(bound_reads) != 1
                ):
                    continue
                call_index = positions.get(bound_reads[0][0])
                if call_index is None or call_index <= reader_index:
                    continue
                end_index = call_index
            if any(
                step is not reader and not _safe_borrow_step(step, source)
                for step in block.vops[index + 1 : end_index]
            ):
                continue
            vop.codegen_info = replace(access, borrow=True)


def _mark_last_local_reads(component: IR2Component) -> None:
    """Mark ``LOAD_LOCAL`` VOPs that are the final read of their local on all
    normal and exception paths before the local is overwritten or the body exits."""
    environment = component.environment
    candidates = {
        tn
        for var, tn in environment.vars.items()
        if not var.indirect and not var.expression_temporary
    }
    if not candidates:
        return
    normal_succs = {
        block: [
            component.ir2_blocks[succ]
            for succ in block.block.succ
            if succ in component.ir2_blocks
        ]
        for block in component.blocks
    }
    handlers = {
        block: component.ir2_blocks.get(block_exception_target(block.block))
        for block in component.blocks
    }
    live_in = {block: set() for block in component.blocks}
    changed = True
    while changed:
        changed = False
        for block in reversed(component.blocks):
            live = set()
            for succ in normal_succs[block]:
                live |= live_in[succ]
            handler = handlers[block]
            handler_live = live_in[handler] if handler is not None else ()
            for vop in reversed(block.vops):
                if handler_live:
                    live.update(handler_live)
                for ref in vop.results:
                    live.discard(ref.tn)
                for ref in vop.args:
                    if ref.tn in candidates:
                        live.add(ref.tn)
            if handler_live:
                live.update(handler_live)
            if live != live_in[block]:
                live_in[block] = live
                changed = True
    for block in component.blocks:
        live = set()
        for succ in normal_succs[block]:
            live |= live_in[succ]
        handler = handlers[block]
        handler_live = live_in[handler] if handler is not None else ()
        for vop in reversed(block.vops):
            if handler_live:
                live.update(handler_live)
            if vop.info is vops.LOAD_LOCAL:
                tn = vop.args[0].tn
                if tn in candidates and tn not in live and not vop.codegen_info.move:
                    vop.codegen_info = replace(vop.codegen_info, move=True)
            for ref in vop.results:
                live.discard(ref.tn)
            for ref in vop.args:
                if ref.tn in candidates:
                    live.add(ref.tn)


def _clear_lvar_info(lambda_: Lambda) -> None:
    """LTN annotations belong to one conversion; the next context annotates
    afresh with its own primitive types."""
    clear = _ClearLvarInfo()
    for block in linked(lambda_.component, lambda_, "component").blocks():
        for node in block.nodes():
            node.accept(clear)


class _ClearLvarInfo(NodeVisitor):
    def visit_ValuedNode(self, node):
        if node.lvar is not None:
            node.lvar.info = None

    def visit_Return(self, node: Return):
        node.result.info = None


class _ConstantIndex(NodeVisitor, LeafVisitor):
    """The fixed nonnegative index an operand delivers: a reference to an
    integer literal constant."""

    def visit_Node(self, node):
        raise ValueError("structural unpack requires a fixed nonnegative index")

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return self.visit_Node(leaf)

    def visit_Constant(self, leaf: Constant):
        return leaf.value.accept(_IntegerIndex())


class _IntegerIndex(ast.Visitor):
    def visit_Node(self, node):
        raise ValueError("structural unpack requires a fixed nonnegative index")

    def visit_IntegerLiteral(self, node: ast.IntegerLiteral):
        if node.value < 0:
            return self.visit_Node(node)
        return node.value


class _ProvedSlotRead(NodeVisitor):
    """Whether the consumer of a tuple of type ``shape`` reads a slot the
    shape proves present: a known ``%unpack_ref`` call."""

    def __init__(self, shape):
        self.shape = shape

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN or node.fun_info is not fndb.UNPACK_REF:
            return False
        index = _Converter.constant_index(node.args[1])
        return self.shape.proves_tuple_index(index)


class _LoadLeaf(LeafVisitor):
    """Convert a ``Ref`` by the kind of leaf it references."""

    def __init__(self, converter: _Converter, node: Ref):
        self.converter = converter
        self.node = node

    def visit_Leaf(self, leaf):
        raise TypeError(f"no IR2 conversion for a reference to {leaf!r}")

    def visit_KnownFunction(self, leaf: KnownFunction):
        return None

    def visit_Lambda(self, leaf: Lambda):
        return None

    def visit_Constant(self, leaf: Constant):
        self.load(vops.CONSTANT, info=leaf.value)

    def visit_GlobalVar(self, leaf: GlobalVar):
        self.load(vops.LOAD_GLOBAL, info=leaf)

    def visit_LambdaVar(self, leaf: LambdaVar):
        converter, node = self.converter, self.node
        if converter.borrows_unpack_capture(node, leaf):
            # The capture dominates this infallible slot read and remains owned
            # until the assignment's explicit discard or exception transfer.
            assert node.lvar is not None and node.lvar.info is not None
            node.lvar.info.locs.append(converter.var_tn(leaf))
            return
        result, discard = converter.result_tn(node)
        environment = converter.environment
        if leaf in environment.cells:
            cell = converter.cell(node, leaf)
            converter.emit(
                templates_for(fndb.PROTOCOLS["cell_get"])[0],
                node,
                args=(cell,),
                results=(result,),
            )
        else:
            source = environment.copies.get(leaf) or converter.var_tn(leaf)
            types = converter.component.context.types
            bound = leaf in environment.copies or (
                node in types and not types[node].may_be_unbound
            )
            converter.emit(
                vops.LOAD_LOCAL,
                node,
                args=(source,),
                results=(result,),
                info=LocalAccess(leaf.name, node.final, bound),
            )
        if discard:
            converter.emit_discard(node, result)

    def load(self, template, info):
        converter, node = self.converter, self.node
        result, discard = converter.result_tn(node)
        converter.emit(template, node, results=(result,), info=info)
        if discard:
            converter.emit_discard(node, result)


class _StoreVar(LeafVisitor):
    """Convert a ``Set`` by the kind of variable it assigns."""

    def __init__(self, converter: _Converter, node: Set):
        self.converter = converter
        self.node = node

    def visit_Leaf(self, leaf):
        raise TypeError(f"no IR2 conversion for an assignment to {leaf!r}")

    def visit_LambdaVar(self, leaf: LambdaVar):
        converter, node = self.converter, self.node
        if leaf in converter.environment.cells:
            cell = converter.cell(node, leaf)
            result = converter.temporary()
            if node.value is None:
                converter.emit(
                    templates_for(fndb.PROTOCOLS["cell_clear"])[0],
                    node,
                    args=(cell,),
                    results=(result,),
                )
            else:
                converter.emit(
                    templates_for(fndb.PROTOCOLS["cell_set"])[0],
                    node,
                    args=(cell, converter.lvar_tn(node.value)),
                    results=(result,),
                )
            converter.emit_discard(node, result)
            return
        target = converter.var_tn(leaf)
        release = converter.context.type_of(node).needs_release
        if node.value is None:
            converter.emit(vops.UNBIND_LOCAL, node, results=(target,), info=release)
        else:
            converter.emit(
                vops.MOVE,
                node,
                args=(converter.lvar_tn(node.value),),
                results=(target,),
                info=release,
            )

    def visit_GlobalVar(self, leaf: GlobalVar):
        converter, node = self.converter, self.node
        if node.value is None:
            converter.emit(vops.UNBIND_GLOBAL, node, info=leaf)
        else:
            converter.emit(
                vops.STORE_GLOBAL,
                node,
                args=(converter.lvar_tn(node.value),),
                info=leaf,
            )


class _MethodResolution(NodeVisitor):
    """The ``%resolve_method`` combination delivering a resolved method."""

    def __init__(self, resolved):
        self.resolved = resolved

    def visit_Node(self, node):
        raise TypeError(f"{self.resolved!r} is not delivered by a method resolution")

    def visit_Combination(self, node: Combination) -> Combination:
        return node


class _ExactListAppendCall(NodeVisitor):
    """Whether the consumer of a resolved method is a 1-argument positional call."""

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        return (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.CALL_RESOLVED
            and len(node.args) == 2
            and not linked(node.info, node, "call info").keywords
        )


class _Converter(NodeVisitor):
    def __init__(self, component: IR2Component):
        self.component = component
        self.environment = component.environment
        self.context = component.context
        self.lambda_ = component.lambda_
        self.current: IR2Block | None = None

    # TNs

    def var_tn(self, var: LambdaVar):
        tn = self.environment.vars.get(var)
        if tn is None:
            plan = self.context.plan
            ptype = plan.var_type(var) if plan is not None else None
            tn = self.component.tn(ptype or T, leaf=var, name=var.name)
            self.environment.vars[var] = tn
        return tn

    def prepare_environment(self):
        """Create the TNs of locals, captured cells and copies, and range
        states before any block refers to them."""
        lambda_ = self.lambda_
        environment = self.environment
        for var in lambda_.locals:
            self.var_tn(var)
        closure = lambda_.environment.closure if lambda_.environment else ()
        environment.captured = list(closure)
        for var in closure:
            if var.indirect:
                environment.cells[var] = self.component.tn(T, name=f"$cell:{var.name}")
            else:
                environment.copies[var] = self.component.tn(T, name=f"$copy:{var.name}")
        for var in lambda_.locals:
            if var.indirect:
                environment.cells[var] = self.component.tn(T, name=f"$cell:{var.name}")
                environment.home_cells.append(var)

    def lvar_tn(self, lvar):
        """The TN delivering ``lvar``'s value (``lvar-tn``)."""
        info = lvar.info
        if info is None:
            raise ValueError(f"{lvar!r} was not annotated by LTN")
        if info.kind is IR2LvarKind.DELAYED:
            raise ValueError(f"{lvar!r} is a compile-time operand")
        if not info.locs:
            tn = self.component.tn(info.primitive_type)
            uses = lvar.uses
            tn.type = self.context.type_of(uses[0]) if len(uses) == 1 else UNKNOWN
            info.locs.append(tn)
        return info.locs[0]

    def temporary(self, primitive_type=T, value_type=UNKNOWN):
        tn = self.component.tn(primitive_type)
        tn.type = value_type
        return tn

    def result_tn(self, node):
        """The TN a valued node writes: the lvar's, or a fresh temporary whose
        value is discarded."""
        if node.lvar is not None:
            return self.lvar_tn(node.lvar), False
        value_type = self.context.type_of(node)
        plan = self.context.plan
        ptype = plan.node_type(node) if plan is not None else None
        return self.temporary(ptype or primitive_type_of(value_type), value_type), True

    def range_tn(self, state):
        tn = self.environment.ranges.get(id(state))
        if tn is None:
            tn = self.environment.ranges[id(state)] = self.component.tn(
                RANGE_T, name=f"$range{len(self.environment.ranges)}"
            )
        return tn

    # VOP emission

    def emit(self, template, node, args=(), results=(), info=None) -> VOP:
        arg_refs = [TNRef(tn, False) for tn in args]
        result_refs = [TNRef(tn, True) for tn in results]
        vop = VOP(template, node, arg_refs, result_refs)
        vop.codegen_info = info
        reads = self.component.reads
        for ref in arg_refs:
            reads[ref.tn].append((vop, ref))
        writes = self.component.writes
        for ref in result_refs:
            writes[ref.tn].append((vop, ref))
        return linked(self.current, self, "current block").append(vop)

    def emit_discard(self, node, tn) -> None:
        self.emit(vops.DISCARD, node, args=(tn,))

    def convert_block(self, block) -> None:
        self.current = self.component.ir2_blocks[block]
        # SBCL ir2-convert: Bind emits its own entry poll, while the loop
        # analysis identifies natural and strange loop heads here.
        if block.loop is not None and block.loop.is_header(block):
            self.emit(vops.INSERT_SAFEPOINT, block.start.next)
        for node in block.nodes():
            node.accept(self)
        self.convert_transfer(block)

    def convert_transfer(self, block) -> None:
        """A block that does not end in a branch, return, or raise jumps to
        its single successor."""
        current = linked(self.current, self, "current block")
        last = current.vops[-1] if current.vops else None
        if last is not None and last.info.name in vops.TERMINATORS:
            return
        successors = [succ for succ in block.succ if succ.start is not None]
        if len(successors) != 1:
            raise ValueError(f"{block!r} has no IR2 control transfer")
        self.emit(
            vops.JUMP, block.last, info=self.component.ir2_blocks[successors[0]].label
        )

    # Nodes

    def visit_Bind(self, node: Bind):
        environment = self.environment
        for index, var in enumerate(self.lambda_.vars):
            self.emit(vops.MOVE_ARGUMENT, node, results=(self.var_tn(var),), info=index)
        for index, var in enumerate(environment.captured):
            target = environment.cells.get(var) or environment.copies[var]
            self.emit(vops.LOAD_CLOSURE, node, results=(target,), info=index)
        for var in environment.home_cells:
            cell = self.temporary()
            self.emit(
                templates_for(fndb.PROTOCOLS["cell_new"])[0], node, results=(cell,)
            )
            self.emit(
                vops.MOVE,
                node,
                args=(cell,),
                results=(environment.cells[var],),
                info=False,
            )
            if var in self.lambda_.vars:
                # A captured parameter's cell starts out holding its argument.
                value = self.temporary()
                self.emit(
                    vops.LOAD_LOCAL,
                    node,
                    args=(self.var_tn(var),),
                    results=(value,),
                    info=LocalAccess(var.name, True, True),
                )
                result = self.temporary()
                self.emit(
                    templates_for(fndb.PROTOCOLS["cell_set"])[0],
                    node,
                    args=(self.cell(node, var), value),
                    results=(result,),
                )
                self.emit_discard(node, result)

        self.emit(vops.INSERT_SAFEPOINT, node)

    def cell(self, node, var):
        """Load the cell object of a captured-and-assigned variable."""
        loaded = self.temporary()
        self.emit(
            vops.LOAD_LOCAL,
            node,
            args=(self.environment.cells[var],),
            results=(loaded,),
            info=LocalAccess(var.name, False, True),
        )
        return loaded

    def captured_value(self, node, var):
        """The value a nested function's closure holds for ``var``."""
        environment = self.environment
        if var in environment.cells:
            return self.cell(node, var)
        source = environment.copies.get(var) or self.var_tn(var)
        loaded = self.temporary()
        self.emit(
            vops.LOAD_LOCAL,
            node,
            args=(source,),
            results=(loaded,),
            info=LocalAccess(var.name, False, var in environment.copies),
        )
        return loaded

    def visit_Ref(self, node: Ref):
        node.leaf.accept(_LoadLeaf(self, node))

    def borrows_unpack_capture(self, node: Ref, leaf: LambdaVar):
        if not (
            leaf.expression_temporary
            and node.lvar is not None
            and len(node.lvar.uses) == 1
        ):
            return False
        consumer = node.lvar.dest
        if consumer is None:
            return False
        return consumer.accept(_ProvedSlotRead(self.context.type_of(node)))

    @staticmethod
    def constant_index(lvar):
        return lvar.use().accept(_ConstantIndex())

    def visit_Set(self, node: Set):
        node.var.accept(_StoreVar(self, node))

    def visit_Cast(self, node: Cast):
        result, discard = self.result_tn(node)
        self.emit(
            vops.TYPE_CHECK,
            node,
            args=(self.lvar_tn(node.value),),
            results=(result,),
            info=TypeCheck(
                node.contract, node.location, node.safety if node.type_check else 0
            ),
        )
        if discard:
            self.emit_discard(node, result)

    def visit_If(self, node: If):
        blocks = self.component.ir2_blocks
        self.emit(
            vops.BRANCH,
            node,
            args=(self.lvar_tn(node.test),),
            info=BranchTargets(
                blocks[node.consequent].label, blocks[node.alternative].label
            ),
        )

    def visit_Return(self, node: Return):
        self.emit(vops.RETURN, node, args=(self.lvar_tn(node.result),))

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN:
            self.convert_full_call(node)
            return
        name = linked(node.fun_info, node, "known function").name
        if name == "%make_function":
            self.convert_make_function(node)
        elif name == "%resolve_method":
            self.convert_resolve_method(node)
        elif name == "%call_resolved":
            self.convert_call_resolved(node)
        elif name in ("%range_prepare", "%range_available", "%range_advance"):
            self.convert_range(node)
        elif name == "%unpack":
            arity = self.constant_index(node.args[1])
            shape = self.context.type_of(node.args[0].use())
            self.convert_template(
                node,
                [self.lvar_tn(arg) for arg in node.args],
                UnpackCheck(arity, shape.fits(TupleType(arity))),
            )
        elif name == "%unpack_ref":
            index = self.constant_index(node.args[1])
            shape = self.context.type_of(node.args[0].use())
            args = [self.lvar_tn(arg) for arg in node.args]
            self.convert_template(
                node,
                args,
                UnpackSlot(
                    index,
                    shape.proves_tuple_index(index),
                    args[0].leaf is not None and args[0].leaf.expression_temporary,
                ),
            )
        else:
            self.convert_template(node, [self.lvar_tn(arg) for arg in node.args])

    def convert_template(self, node, args, info=None):
        """Emit the template LTN selected (``ir2-convert-template``)."""
        template = node.template
        if template is None:
            raise ValueError(f"{node!r} has no template")
        if linked(node.fun_info, node, "known function").result:
            result, discard = self.result_tn(node)
            self.emit(
                template,
                node,
                args=args,
                results=(result,),
                info=node.info if info is None else info,
            )
            if discard:
                self.emit_discard(node, result)
        else:
            self.emit(
                template, node, args=args, info=node.info if info is None else info
            )

    def convert_full_call(self, node: Combination):
        fun = node.fun
        target = self.context.targets.get(fun.use()) if len(fun.uses) == 1 else None
        info = node.info
        site = CallSite(
            info.keywords if info is not None else (),
            info.location if info is not None else None,
            target if node.kind is CallKind.GUARDED else None,
            node.proven if node.kind is CallKind.GUARDED else False,
        )
        result, discard = self.result_tn(node)
        plan = self.context.plan
        template = vops.FULL_CALL
        if plan is not None and plan.allocation(node) is not None:
            template = vops.RAW_CONSTRUCT
        self.emit(
            template,
            node,
            args=(self.lvar_tn(fun), *(self.lvar_tn(arg) for arg in node.args)),
            results=(result,),
            info=site,
        )
        if discard:
            self.emit_discard(node, result)

    def convert_make_function(self, node: Combination):
        lambda_ = made_lambda(node)
        closure = lambda_.environment.closure if lambda_.environment else ()
        captured = [self.captured_value(node, var) for var in closure]
        defaults = [self.lvar_tn(arg) for arg in node.args[1:]]
        result, discard = self.result_tn(node)
        self.emit(
            node.template,
            node,
            args=[*defaults, *captured],
            results=(result, self.temporary(), self.temporary()),
            info=FunctionObject(lambda_, node.info),
        )
        if discard:
            self.emit_discard(node, result)

    def is_exact_list_append(self, resolve: Combination, call) -> bool:
        receiver = self.context.type_of(resolve.args[0].use())
        info = linked(resolve.info, resolve, "method info")
        return (
            receiver.is_exact_list
            and info.name == "append"
            and call is not None
            and call.accept(_ExactListAppendCall())
        )

    def convert_resolve_method(self, node: Combination):
        receiver = self.lvar_tn(node.args[0])
        lvar = node.lvar
        if lvar is None:
            raise ValueError("a resolved method is always consumed by its call")
        exact_list_append = self.is_exact_list_append(node, lvar.dest)
        method = self.lvar_tn(lvar)
        bound = self.component.tn(T, name="receiver")
        if exact_list_append:
            method.type = NoneType()
            bound.type = receiver.type
        else:
            bound.type = UNKNOWN
        linked(lvar.info, lvar, "LTN annotation").locs.append(bound)
        info = linked(node.info, node, "method info")
        self.emit(
            node.template,
            node,
            args=(receiver,),
            results=(method, bound),
            info=MethodResolution(info.name, exact_list_append),
        )

    def convert_call_resolved(self, node: Combination):
        resolved = node.args[0]
        method, bound = linked(resolved.info, resolved, "LTN annotation").locs
        resolve = resolved.use().accept(_MethodResolution(resolved))
        info = linked(node.info, node, "call info")
        exact_list_append = self.is_exact_list_append(resolve, node)
        result, discard = self.result_tn(node)
        self.emit(
            node.template,
            node,
            args=(method, bound, *(self.lvar_tn(arg) for arg in node.args[1:])),
            results=(result,),
            info=MethodCall(
                info.keywords,
                linked(resolve.info, resolve, "method info").name,
                exact_list_append,
            ),
        )
        if discard:
            self.emit_discard(node, result)

    def convert_range(self, node: Combination):
        state = self.range_tn(node.info)
        args = [self.lvar_tn(arg) for arg in node.args]
        self.convert_template(node, [*args, state])

    def visit_Node(self, node):
        return None


__all__ = [
    "BranchTargets",
    "CallSite",
    "FunctionObject",
    "LocalAccess",
    "MethodCall",
    "TypeCheck",
    "ir2_convert",
]

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

from dataclasses import dataclass

from mule import ast

from . import fndb, vops
from .ctype import UNKNOWN, TupleType
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
    NodeVisitor,
    Ref,
    Return,
    Set,
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
    error and whether the read is the last use (ownership moves)."""

    name: str
    move: bool


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
class MethodCall:
    keywords: tuple
    name: str


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
    consequent: IR2Block
    alternative: IR2Block


def ir2_convert(lambda_: Lambda, context: EntryContext) -> IR2Component:
    """Convert ``lambda_`` under ``context`` into VOPs."""
    environment = IR2Environment(lambda_, context)
    component = IR2Component(lambda_, context, environment)
    converter = _Converter(component)
    converter.prepare_environment()
    blocks = list(lambda_.component.blocks())
    for index, block in enumerate(blocks):
        ir2_block = IR2Block(block, index)
        component.ir2_blocks[block] = ir2_block
        component.blocks.append(ir2_block)
    for block in blocks:
        converter.convert_block(block)
    _clear_lvar_info(lambda_)
    return component


def _clear_lvar_info(lambda_: Lambda) -> None:
    """LTN annotations belong to one conversion; the next context annotates
    afresh with its own primitive types."""
    for block in lambda_.component.blocks():
        for node in block.nodes():
            lvar = getattr(node, "lvar", None)
            if lvar is not None:
                lvar.info = None
            if isinstance(node, Return):
                node.result.info = None


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

    def read(self, tn) -> TNRef:
        ref = TNRef(tn, False)
        tn.reads.append(ref)
        return ref

    def write(self, tn) -> TNRef:
        ref = TNRef(tn, True)
        tn.writes.append(ref)
        return ref

    def range_tn(self, state):
        tn = self.environment.ranges.get(id(state))
        if tn is None:
            tn = self.environment.ranges[id(state)] = self.component.tn(
                RANGE_T, name=f"$range{len(self.environment.ranges)}"
            )
        return tn

    # VOP emission

    def emit(self, template, node, args=(), results=(), info=None) -> VOP:
        vop = VOP(
            template,
            node,
            [self.read(tn) for tn in args],
            [self.write(tn) for tn in results],
        )
        vop.codegen_info = info
        return self.current.append(vop)

    def emit_discard(self, node, tn) -> None:
        self.emit(vops.DISCARD, node, args=(tn,))

    def convert_block(self, block) -> None:
        self.current = self.component.ir2_blocks[block]
        for node in block.nodes():
            node.accept(self)
        self.convert_transfer(block)

    def convert_transfer(self, block) -> None:
        """A block that does not end in a branch, return, or raise jumps to
        its single successor."""
        last = self.current.vops[-1] if self.current.vops else None
        if last is not None and last.info.name in vops.TERMINATORS:
            return
        successors = [succ for succ in block.succ if succ.start is not None]
        if len(successors) != 1:
            raise ValueError(f"{block!r} has no IR2 control transfer")
        self.emit(vops.JUMP, block.last, info=self.component.ir2_blocks[successors[0]])

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
            self.emit(vops.MOVE, node, args=(cell,), results=(environment.cells[var],))

    def cell(self, node, var):
        """Load the cell object of a captured-and-assigned variable."""
        loaded = self.temporary()
        self.emit(
            vops.LOAD_LOCAL,
            node,
            args=(self.environment.cells[var],),
            results=(loaded,),
            info=LocalAccess(var.name, False),
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
            info=LocalAccess(var.name, False),
        )
        return loaded

    def visit_Ref(self, node: Ref):
        leaf = node.leaf
        if isinstance(leaf, (KnownFunction, Lambda)):
            return
        if self.borrows_unpack_capture(node):
            # The capture dominates this infallible slot read and remains owned
            # until the assignment's explicit discard or exception transfer.
            assert isinstance(leaf, LambdaVar)
            assert node.lvar is not None and node.lvar.info is not None
            node.lvar.info.locs.append(self.var_tn(leaf))
            return
        result, discard = self.result_tn(node)
        if isinstance(leaf, Constant):
            self.emit(vops.CONSTANT, node, results=(result,), info=leaf.value)
        elif isinstance(leaf, LambdaVar):
            environment = self.environment
            if leaf in environment.cells:
                cell = self.cell(node, leaf)
                self.emit(
                    templates_for(fndb.PROTOCOLS["cell_get"])[0],
                    node,
                    args=(cell,),
                    results=(result,),
                )
            else:
                source = environment.copies.get(leaf) or self.var_tn(leaf)
                self.emit(
                    vops.LOAD_LOCAL,
                    node,
                    args=(source,),
                    results=(result,),
                    info=LocalAccess(leaf.name, node.final),
                )
        elif isinstance(leaf, GlobalVar):
            self.emit(vops.LOAD_GLOBAL, node, results=(result,), info=leaf.name)
        else:
            raise TypeError(f"no IR2 conversion for a reference to {leaf!r}")
        if discard:
            self.emit_discard(node, result)

    def borrows_unpack_capture(self, node):
        leaf = node.leaf
        if not (
            isinstance(leaf, LambdaVar)
            and leaf.expression_temporary
            and node.lvar is not None
            and len(node.lvar.uses) == 1
        ):
            return False
        consumer = node.lvar.dest
        if not (
            isinstance(consumer, Combination)
            and consumer.kind is CallKind.KNOWN
            and consumer.fun_info is fndb.UNPACK_REF
        ):
            return False
        shape = self.context.type_of(node)
        index = self.constant_index(consumer.args[1])
        return (
            isinstance(shape, TupleType)
            and not shape.may_be_unbound
            and shape.arity is not None
            and index < shape.arity
        )

    @staticmethod
    def constant_index(lvar):
        producer = lvar.use()
        if not (
            isinstance(producer, Ref)
            and isinstance(producer.leaf, Constant)
            and isinstance(producer.leaf.value, ast.IntegerLiteral)
            and producer.leaf.value.value >= 0
        ):
            raise ValueError("structural unpack requires a fixed nonnegative index")
        return producer.leaf.value.value

    def visit_Set(self, node: Set):
        var = node.var
        if isinstance(var, LambdaVar) and var in self.environment.cells:
            cell = self.cell(node, var)
            result = self.temporary()
            if node.value is None:
                self.emit(
                    templates_for(fndb.PROTOCOLS["cell_clear"])[0],
                    node,
                    args=(cell,),
                    results=(result,),
                )
            else:
                self.emit(
                    templates_for(fndb.PROTOCOLS["cell_set"])[0],
                    node,
                    args=(cell, self.lvar_tn(node.value)),
                    results=(result,),
                )
            self.emit_discard(node, result)
            return
        if isinstance(var, LambdaVar):
            target = self.var_tn(var)
            if node.value is None:
                self.emit(vops.UNBIND_LOCAL, node, results=(target,))
            else:
                self.emit(
                    vops.MOVE, node, args=(self.lvar_tn(node.value),), results=(target,)
                )
            return
        if node.value is None:
            self.emit(vops.UNBIND_GLOBAL, node, info=var.name)
        else:
            self.emit(
                vops.STORE_GLOBAL, node, args=(self.lvar_tn(node.value),), info=var.name
            )

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
            info=BranchTargets(blocks[node.consequent], blocks[node.alternative]),
        )

    def visit_Return(self, node: Return):
        self.emit(vops.RETURN, node, args=(self.lvar_tn(node.result),))

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN:
            self.convert_full_call(node)
            return
        name = node.fun_info.name
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
                UnpackCheck(
                    arity,
                    isinstance(shape, TupleType)
                    and not shape.may_be_unbound
                    and shape.arity == arity,
                ),
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
                    isinstance(shape, TupleType)
                    and not shape.may_be_unbound
                    and shape.arity is not None
                    and index < shape.arity,
                    isinstance(args[0].leaf, LambdaVar)
                    and args[0].leaf.expression_temporary,
                ),
            )
        else:
            self.convert_template(node, [self.lvar_tn(arg) for arg in node.args])

    def convert_template(self, node, args, info=None):
        """Emit the template LTN selected (``ir2-convert-template``)."""
        template = node.template
        if template is None:
            raise ValueError(f"{node!r} has no template")
        if node.fun_info.result:
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
        lambda_ = node.args[0].use().leaf
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

    def convert_resolve_method(self, node: Combination):
        receiver = self.lvar_tn(node.args[0])
        lvar = node.lvar
        if lvar is None:
            raise ValueError("a resolved method is always consumed by its call")
        method = self.lvar_tn(lvar)
        bound = self.component.tn(T, name="receiver")
        bound.type = UNKNOWN
        lvar.info.locs.append(bound)
        self.emit(
            node.template,
            node,
            args=(receiver,),
            results=(method, bound),
            info=node.info,
        )

    def convert_call_resolved(self, node: Combination):
        resolved = node.args[0]
        method_info = resolved.info
        method, bound = method_info.locs
        resolve = resolved.use()
        result, discard = self.result_tn(node)
        self.emit(
            node.template,
            node,
            args=(method, bound, *(self.lvar_tn(arg) for arg in node.args[1:])),
            results=(result,),
            info=MethodCall(node.info.keywords, resolve.info.name),
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

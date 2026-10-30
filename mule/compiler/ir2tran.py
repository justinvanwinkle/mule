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

from dataclasses import replace

from mule import ast

from . import fndb, vops
from .annotation_types import InferredReturnContract
from .callconv import physical_entry, tuple_return_convention
from .ctype import (
    BOTTOM,
    NONE_TYPE,
    PYTHON_INTEGER,
    TEXT_TYPE,
    UNKNOWN,
    TupleType,
)
from .ir1util import block_exception_target, made_lambda
from .ir2_borrow import (
    _BORROWING_VOPS,
    _INERT_LOCAL_VALUE,
    DEAD_LOCAL_STORE,
    BranchTargets,
    CallSite,
    FunctionObject,
    LocalAccess,
    MethodCall,
    MethodResolution,
    TypeCheck,
    UnpackCheck,
    UnpackSlot,
    _borrow_fixed_slot_reads,
    _BorrowedLifetime,
    _final_call_needs_object,
    _FinalBindingRef,
    _local_may_run_user_code,
    _mark_borrowed_local_reads,
    _mark_last_local_reads,
    _ObjectFreeFinalUse,
    _propagate_local_aliases,
    _safe_borrow_step,
    _specialize_list_getitems,
)
from .ir2_specialize import (
    DirectSequenceIteration,
    _mark_boolean_comparisons,
    _specialize_sequence_iterations,
    select_tuple_delivery,
)
from .ltn import (
    EntryContext,
    primitive_type_of,
    template_args_ok,
    template_results_ok,
)
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
from .seqtran import (
    _PREDICTABLE_RANGE_CALLEE_REF,
    _RangeIteratorRef,
    is_predictable_len_call,
    known_length_primitive,
)
from .vmdef import templates_for
from .vop import (
    RANGE_T,
    REFERENCE_T,
    VOP,
    IR2Block,
    IR2Component,
    IR2Environment,
    IR2LvarKind,
    T,
    TNRef,
)


def ir2_convert(
    lambda_: Lambda, context: EntryContext, *, ltn_token: object | None = None
) -> IR2Component:
    """Convert ``lambda_`` under ``context`` into VOPs.

    A token from LTN permits reuse of its derived types only while the graph
    and context facts remain unchanged since that analysis.
    """
    environment = IR2Environment(lambda_, context)
    component = IR2Component(lambda_, context, environment)
    component.tuple_return = tuple_return_convention(lambda_, context)
    converter = _Converter(component, ltn_token=ltn_token)
    converter.prepare_environment()
    blocks = list(linked(lambda_.component, lambda_, "component").blocks())
    for index, block in enumerate(blocks):
        ir2_block = IR2Block(block, index, block_exception_target(block))
        component.ir2_blocks[block] = ir2_block
        component.blocks.append(ir2_block)
    for block in blocks:
        converter.convert_block(block)
    converter.close()
    borrowed_names = {lambda_.vars[index].name for index in context.borrowed_parameters}
    for block in component.blocks:
        for vop in block.vops:
            if (
                vop.info.reuses_local
                and vop.codegen_info is not None
                and vop.codegen_info.reuse_local in borrowed_names
            ):
                vop.codegen_info = replace(vop.codegen_info, reuse_local=None)
    _specialize_sequence_iterations(component)
    _mark_last_local_reads(component)
    _propagate_local_aliases(component)
    _mark_boolean_comparisons(component)
    _mark_borrowed_local_reads(component)
    _specialize_list_getitems(component)
    _borrow_fixed_slot_reads(component)
    _specialize_list_getitems(component)
    _clear_lvar_info(lambda_)
    return component


def _clear_lvar_info(lambda_: Lambda) -> None:
    """LTN annotations belong to one conversion; the next context annotates
    afresh with its own primitive types."""
    for block in linked(lambda_.component, lambda_, "component").blocks():
        for node in block.nodes():
            node.accept(_CLEAR_LVAR_INFO)


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
        return leaf.value.accept(_INTEGER_INDEX)


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

    def __init__(self, converter: _Converter):
        self.converter = converter
        self.node: Ref | None = None

    def visit_Leaf(self, leaf):
        raise TypeError(f"no IR2 conversion for a reference to {leaf!r}")

    def visit_KnownFunction(self, leaf: KnownFunction):
        return None

    def visit_Lambda(self, leaf: Lambda):
        return None

    def visit_Constant(self, leaf: Constant):
        self.load(vops.CONSTANT, info=leaf.value)

    def visit_GlobalVar(self, leaf: GlobalVar):
        node = linked(self.node, self, "reference node")
        final_target = node.accept(_FinalBindingRef(self.converter.context))
        if final_target is not None and node.lvar is not None:
            consumer = node.lvar.dest
            if consumer is not None and consumer.accept(
                _ObjectFreeFinalUse(node.lvar, final_target, self.converter.lambda_)
            ):
                return
        self.load(vops.LOAD_GLOBAL, info=leaf)

    def visit_LambdaVar(self, leaf: LambdaVar):
        converter = self.converter
        node = linked(self.node, self, "reference node")
        if converter.borrows_unpack_capture(node, leaf):
            # The capture dominates this infallible slot read and remains owned
            # until the assignment's explicit discard or exception transfer.
            assert node.lvar is not None and node.lvar.info is not None
            node.lvar.info.locs.append(converter.var_tn(leaf))
            return
        result, discard = converter.result_tn(node)
        environment = converter.environment
        local = converter.context.local_environments
        if local is not None and local.function_for_binding(leaf) is not None:
            result.primitive_type = REFERENCE_T
        if leaf in converter.stack_cells:
            converter.emit(
                vops.LOCAL_CELL_REF,
                node,
                args=(converter.stack_cells[leaf],),
                results=(result,),
                info=node.allow_unbound,
            )
        elif leaf in environment.cells:
            converter.emit(
                vops.VALUE_CELL_REF,
                node,
                args=(environment.cells[leaf],),
                results=(result,),
                info=node.allow_unbound,
            )
        else:
            source = environment.copies.get(leaf) or converter.var_tn(leaf)
            types = converter.component.context.types
            bound = (
                leaf in environment.copies
                or (leaf.hidden and node.lvar is None and node.final)
                or (
                    not node.allow_unbound
                    and node in types
                    and not types[node].may_be_unbound
                )
            )
            converter.emit(
                vops.LOAD_LOCAL,
                node,
                args=(source,),
                results=(result,),
                info=LocalAccess(
                    leaf.name,
                    node.final and source.borrowed_from is not source,
                    bound,
                    allow_unbound=node.allow_unbound,
                ),
            )
        if discard:
            converter.emit_discard(node, result)

    def load(self, template, info):
        converter = self.converter
        node = linked(self.node, self, "reference node")
        result, discard = converter.result_tn(node)
        converter.emit(template, node, results=(result,), info=info)
        if discard:
            converter.emit_discard(node, result)


class _StoreVar(LeafVisitor):
    """Convert a ``Set`` by the kind of variable it assigns."""

    def __init__(self, converter: _Converter):
        self.converter = converter
        self.node: Set | None = None

    def visit_Leaf(self, leaf):
        raise TypeError(f"no IR2 conversion for an assignment to {leaf!r}")

    def visit_LambdaVar(self, leaf: LambdaVar):
        converter = self.converter
        node = linked(self.node, self, "assignment node")
        if leaf in converter.stack_cells:
            local = converter.context.local_environments
            owned = local.function_for_binding(leaf) is None
            if node.value is None:
                converter.emit(
                    vops.LOCAL_CELL_CLEAR,
                    node,
                    args=(converter.stack_cells[leaf],),
                    info=owned,
                )
            else:
                converter.emit(
                    vops.LOCAL_CELL_SET,
                    node,
                    args=(converter.stack_cells[leaf], converter.lvar_tn(node.value)),
                    info=owned,
                )
            return
        if leaf in converter.environment.cells:
            cell = converter.environment.cells[leaf]
            if node.value is None:
                converter.emit(
                    vops.VALUE_CELL_CLEAR,
                    node,
                    args=(cell,),
                )
            else:
                converter.emit(
                    vops.VALUE_CELL_SET,
                    node,
                    args=(cell, converter.lvar_tn(node.value)),
                )
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
        converter = self.converter
        node = linked(self.node, self, "assignment node")
        if node.value is None:
            converter.emit(vops.UNBIND_GLOBAL, node, info=(leaf, node.missing_ok))
        else:
            converter.emit(
                vops.STORE_GLOBAL,
                node,
                args=(converter.lvar_tn(node.value),),
                info=(leaf, node.final_binding),
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


class _ExactListPopCall(NodeVisitor):
    """Whether the consumer of a resolved method is a 0-argument positional call."""

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        return (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.CALL_RESOLVED
            and len(node.args) == 1
            and not linked(node.info, node, "call info").keywords
        )


class _ExactTextFindCall(NodeVisitor):
    """Whether the consumer of a resolved ``str.find``/``str.rfind`` method is
    a 1-, 2-, or 3-argument positional call with exact ``str`` substring and
    fixnum or ``None`` bounds."""

    def __init__(self, context: EntryContext):
        self.context = context

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        if (
            node.kind is not CallKind.KNOWN
            or node.fun_info is not fndb.CALL_RESOLVED
            or not (2 <= len(node.args) <= 4)
            or linked(node.info, node, "call info").keywords
        ):
            return False
        sub = (
            self.context.type_of(node.args[1].uses[0])
            if len(node.args[1].uses) == 1
            else UNKNOWN
        )
        if not sub.fits_text(TEXT_TYPE):
            return False
        for bound_arg in node.args[2:]:
            bound = (
                self.context.type_of(bound_arg.uses[0])
                if len(bound_arg.uses) == 1
                else UNKNOWN
            )
            if not (bound.is_fixnum or bound.fits_none(NONE_TYPE)):
                return False
        return True


class _ExactTextSplitCall(NodeVisitor):
    """Whether the consumer of a resolved ``str.split`` method is a 1- or
    2-argument positional call with exact ``str`` separator and optional integer
    ``maxsplit``."""

    def __init__(self, context: EntryContext):
        self.context = context

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        if (
            node.kind is not CallKind.KNOWN
            or node.fun_info is not fndb.CALL_RESOLVED
            or not (2 <= len(node.args) <= 3)
            or linked(node.info, node, "call info").keywords
        ):
            return False
        sep = (
            self.context.type_of(node.args[1].uses[0])
            if len(node.args[1].uses) == 1
            else UNKNOWN
        )
        if not sep.fits_text(TEXT_TYPE):
            return False
        if len(node.args) == 3:
            maxsplit = (
                self.context.type_of(node.args[2].uses[0])
                if len(node.args[2].uses) == 1
                else UNKNOWN
            )
            if not maxsplit.fits_python_integer(PYTHON_INTEGER):
                return False
        return True


class _ExactTextPartitionCall(NodeVisitor):
    """Whether the consumer of a resolved ``str.partition``/``str.rpartition``
    method is a 1-argument positional call with exact ``str`` separator."""

    def __init__(self, context: EntryContext):
        self.context = context

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        if (
            node.kind is not CallKind.KNOWN
            or node.fun_info is not fndb.CALL_RESOLVED
            or len(node.args) != 2
            or linked(node.info, node, "call info").keywords
        ):
            return False
        sep = (
            self.context.type_of(node.args[1].uses[0])
            if len(node.args[1].uses) == 1
            else UNKNOWN
        )
        return sep.fits_text(TEXT_TYPE)


_EXACT_TEXT_METHODS: dict[str, tuple[int, int]] = {
    "isalpha": (0, 0),
    "isalnum": (0, 0),
    "isdigit": (0, 0),
    "isdecimal": (0, 0),
    "isnumeric": (0, 0),
    "isspace": (0, 0),
    "isupper": (0, 0),
    "islower": (0, 0),
    "istitle": (0, 0),
    "isascii": (0, 0),
    "isprintable": (0, 0),
    "isidentifier": (0, 0),
    "lower": (0, 0),
    "upper": (0, 0),
    "capitalize": (0, 0),
    "title": (0, 0),
    "swapcase": (0, 0),
    "casefold": (0, 0),
    "strip": (0, 1),
    "lstrip": (0, 1),
    "rstrip": (0, 1),
    "splitlines": (0, 1),
    "expandtabs": (0, 1),
    "removeprefix": (1, 1),
    "removesuffix": (1, 1),
    "join": (1, 1),
    "zfill": (1, 1),
    "split": (0, 2),
    "rsplit": (0, 2),
    "count": (1, 3),
    "index": (1, 3),
    "rindex": (1, 3),
    "replace": (2, 3),
}


class _ExactTextMethodCall(NodeVisitor):
    """Whether the consumer of a resolved ``TextStorage`` method is a
    positional call within the method's supported arity range."""

    def __init__(self, min_args: int, max_args: int):
        self.min_args = min_args
        self.max_args = max_args

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        return (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.CALL_RESOLVED
            and not linked(node.info, node, "call info").keywords
            and 1 + self.min_args <= len(node.args) <= 1 + self.max_args
        )


class _ExactTextAffixCall(NodeVisitor):
    """Whether the consumer of a resolved ``str.startswith``/``str.endswith``
    method is a 1-, 2-, or 3-argument positional call with exact ``str`` or
    tuple affix and fixnum or ``None`` bounds."""

    def __init__(self, context: EntryContext):
        self.context = context

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        if (
            node.kind is not CallKind.KNOWN
            or node.fun_info is not fndb.CALL_RESOLVED
            or not (2 <= len(node.args) <= 4)
            or linked(node.info, node, "call info").keywords
        ):
            return False
        affix = (
            self.context.type_of(node.args[1].uses[0])
            if len(node.args[1].uses) == 1
            else UNKNOWN
        )
        if not (affix.fits_text(TEXT_TYPE) or affix.fits_tuple(TupleType())):
            return False
        for bound_arg in node.args[2:]:
            bound = (
                self.context.type_of(bound_arg.uses[0])
                if len(bound_arg.uses) == 1
                else UNKNOWN
            )
            if not (bound.is_fixnum or bound.fits_none(NONE_TYPE)):
                return False
        return True


class _ScanIntParse(NodeVisitor):
    def __init__(self, converter: _Converter):
        self.converter = converter

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        self.converter.note_range_prepare(node)


class _ExactDictGetCall(NodeVisitor):
    """Whether the consumer of a resolved ``dict.get`` method is a 1- or
    2-argument positional call."""

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        return (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.CALL_RESOLVED
            and 2 <= len(node.args) <= 3
            and not linked(node.info, node, "call info").keywords
        )


_PROTOCOL_PREFIXES = (
    "mule_sequences_",
    "mule_format_",
    "mule_arguments_",
    "mule_native_class_",
    "mule_print_",
)
_CLEAR_LVAR_INFO = _ClearLvarInfo()
_INTEGER_INDEX = _IntegerIndex()
_CONSTANT_INDEX = _ConstantIndex()
_EXACT_LIST_APPEND_CALL = _ExactListAppendCall()
_EXACT_LIST_POP_CALL = _ExactListPopCall()
_EXACT_DICT_GET_CALL = _ExactDictGetCall()


class _Converter(NodeVisitor):
    def __init__(self, component: IR2Component, *, ltn_token: object | None = None):
        self.component = component
        self.ltn_token = ltn_token
        self.environment = component.environment
        self.context = component.context
        self.lambda_ = component.lambda_
        self.current: IR2Block | None = None
        self._load_leaf: _LoadLeaf | None = _LoadLeaf(self)
        self._store_var: _StoreVar | None = _StoreVar(self)
        self._scan_int_parse: _ScanIntParse | None = (
            _ScanIntParse(self)
            if self.context.plan is None
            and self.lambda_.policy.safety < 3
            and not self.lambda_.name.startswith(_PROTOCOL_PREFIXES)
            else None
        )
        self._skipped_nodes: set = set()
        self._fused_range_prepares: dict[Combination, tuple] = {}
        self.stack_cells = {}

    def close(self) -> None:
        self._load_leaf = None
        self._store_var = None
        self._scan_int_parse = None
        self._skipped_nodes.clear()
        self._fused_range_prepares.clear()

    # TNs

    def var_tn(self, var: LambdaVar):
        tn = self.environment.vars.get(var)
        if tn is None:
            plan = self.context.plan
            ptype = plan.var_type(var) if plan is not None else None
            local = self.context.local_environments
            reference = (
                local is not None and local.function_for_binding(var) is not None
            )
            tn = self.component.tn(
                REFERENCE_T if reference else (ptype or T),
                leaf=None if reference else var,
                name=f"$function:{var.name}" if reference else var.name,
            )
            self.environment.vars[var] = tn
        return tn

    def prepare_environment(self):
        """Create the TNs of locals, captured cells and copies, and range
        states before any block refers to them."""
        lambda_ = self.lambda_
        environment = self.environment
        for var in lambda_.locals:
            self.var_tn(var)
        for index in self.context.borrowed_parameters:
            root = self.var_tn(lambda_.vars[index])
            root.borrowed_from = root
        closure = lambda_.environment.closure if lambda_.environment else ()
        environment.captured = list(closure)
        for var in closure:
            local = self.context.local_environments
            if local is not None and var in local.stack_cells:
                self.stack_cells[var] = self.component.tn(
                    REFERENCE_T, name=f"$slot:{var.name}"
                )
            elif var.indirect:
                environment.cells[var] = self.component.tn(T, name=f"$cell:{var.name}")
            else:
                vtype = var.optimistic_type or UNKNOWN
                reference = (
                    local is not None and local.function_for_binding(var) is not None
                )
                tn = self.component.tn(
                    REFERENCE_T if reference else primitive_type_of(vtype),
                    leaf=None if reference else var,
                    name=f"$copy:{var.name}",
                )
                tn.type = vtype
                environment.copies[var] = tn
        for var in lambda_.locals:
            local = self.context.local_environments
            if var.indirect and not (local is not None and var in local.stack_cells):
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
            if (
                self.ltn_token is not None
                and info.ltn_token is self.ltn_token
                and info.value_type is not None
            ):
                value_type = info.value_type
            else:
                value_type = BOTTOM
                for producer in lvar.uses:
                    value_type = value_type.join(self.context.type_of(producer))
            tn.type = value_type
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
        arg_refs = [TNRef(tn, False) for tn in args] if args else []
        result_refs = [TNRef(tn, True) for tn in results] if results else []
        vop = VOP(template, node, arg_refs, result_refs)
        vop.codegen_info = info
        if arg_refs:
            reads = self.component.reads
            for ref in arg_refs:
                reads[ref.tn].append((vop, ref))
        if result_refs:
            writes = self.component.writes
            for ref in result_refs:
                writes[ref.tn].append((vop, ref))
        self.current.vops.append(vop)
        return vop

    def emit_discard(self, node, tn) -> None:
        self.emit(vops.DISCARD, node, args=(tn,))

    def convert_block(self, block) -> None:
        self.current = self.component.ir2_blocks[block]
        # SBCL ir2-convert: Bind emits its own entry poll, while the loop
        # analysis identifies natural and strange loop heads here.
        if (
            self.lambda_.policy.inserts_safepoints
            and block.loop is not None
            and block.loop.is_header(block)
        ):
            self.emit(vops.INSERT_SAFEPOINT, block.start.next)
        self._skipped_nodes.clear()
        self._fused_range_prepares.clear()
        nodes = tuple(block.nodes())
        if self._scan_int_parse is not None:
            for node in nodes:
                node.accept(self._scan_int_parse)
        for node in nodes:
            if node not in self._skipped_nodes:
                node.accept(self)
        self.convert_transfer(block)

    def note_range_prepare(self, node: Combination) -> None:
        if (
            node.kind is not CallKind.KNOWN
            or node.fun_info is not fndb.RANGE_PREPARE
            or node.lexenv.policy.safety >= 3
        ):
            return
        state = node.info
        if (
            state is None
            or state.direct
            or len(node.args) != 2
            or not (1 <= len(state.arguments) <= 3)
        ):
            return
        callee_lvar = node.args[0]
        if len(callee_lvar.uses) != 1 or not callee_lvar.uses[0].accept(
            _PREDICTABLE_RANGE_CALLEE_REF
        ):
            return
        if len(node.args[1].uses) != 1:
            return
        setup = (
            node.args[1].uses[0].accept(_RangeIteratorRef(node.block, state.arguments))
        )
        if setup is None:
            return
        iterator_ref, iterator_set, iterate_call, range_call = setup
        iterator_var = iterator_ref.leaf
        if (
            iterator_var in self.stack_cells
            or iterator_var in self.environment.cells
            or iterator_var in self.environment.copies
        ):
            return
        if any(
            len(arg.uses) != 1 or arg.dest is not range_call for arg in state.arguments
        ):
            return
        if len(iterate_call.fun.uses) != 1 or len(range_call.fun.uses) != 1:
            return
        callee_ref_2 = callee_lvar.uses[0]
        callee_var = callee_ref_2.leaf
        callee_ref_1 = range_call.fun.uses[0]
        iterate_ref = iterate_call.fun.uses[0]
        if (
            callee_ref_1.block is not node.block
            or iterate_ref.block is not node.block
            or callee_ref_2.block is not node.block
            or iterator_ref.block is not node.block
        ):
            return
        if len(callee_var.refs) != 2 or len(callee_var.sets) != 1:
            return
        callee_set = callee_var.sets[0]
        if (
            callee_set.block is not node.block
            or callee_set.value is None
            or len(callee_set.value.uses) != 1
        ):
            return
        range_global_ref = callee_set.value.uses[0]
        if range_global_ref.block is not node.block:
            return
        self._skipped_nodes.update(
            (
                range_global_ref,
                callee_set,
                callee_ref_1,
                range_call,
                iterate_ref,
                iterate_call,
                iterator_set,
                callee_ref_2,
                iterator_ref,
            )
        )
        self._fused_range_prepares[node] = (iterator_var, range_global_ref.leaf)

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
            target = (
                self.stack_cells.get(var)
                or environment.cells.get(var)
                or environment.copies[var]
            )
            local = self.context.local_environments
            template = (
                vops.LOCAL_LOAD_CLOSURE
                if local is not None and local.is_local(self.lambda_)
                else vops.LOAD_CLOSURE
            )
            self.emit(
                template,
                node,
                results=(target,),
                info=(index, var in self.stack_cells)
                if template is vops.LOCAL_LOAD_CLOSURE
                else index,
            )
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
                self.emit(
                    vops.VALUE_CELL_SET,
                    node,
                    args=(environment.cells[var], value),
                )

        if self.lambda_.policy.inserts_safepoints:
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
        local = self.context.local_environments
        reference = local is not None and local.function_for_binding(var) is not None
        loaded = self.temporary(REFERENCE_T if reference else T)
        self.emit(
            vops.LOAD_LOCAL,
            node,
            args=(source,),
            results=(loaded,),
            info=LocalAccess(
                var.name, False, var in environment.copies or var in self.lambda_.vars
            ),
        )
        return loaded

    def visit_Ref(self, node: Ref):
        if (
            node.lvar is not None
            and node.lvar.info is not None
            and node.lvar.info.kind is IR2LvarKind.DELAYED
        ):
            return
        load_leaf = linked(self._load_leaf, self, "leaf loader")
        load_leaf.node = node
        node.leaf.accept(load_leaf)
        load_leaf.node = None

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
        return lvar.use().accept(_CONSTANT_INDEX)

    def visit_Set(self, node: Set):
        store_var = linked(self._store_var, self, "variable storer")
        store_var.node = node
        node.var.accept(store_var)
        store_var.node = None

    def visit_Cast(self, node: Cast):
        operand = self.lvar_tn(node.value)
        actual = operand.type or UNKNOWN
        target_type = self.context.type_of(node)
        checked = (
            node.type_check
            and bool(node.safety)
            and not actual.fits(node.contract.checked_type)
        )
        if (
            not checked
            and self.context.plan is None
            and node not in self.context.element_contracts
        ):
            if (
                node.lvar is not None
                and len(node.lvar.uses) == 1
                and not node.lvar.info.locs
                and operand.leaf is None
            ):
                target_ptype = node.lvar.info.primitive_type
                if actual.fits(target_type) and operand.primitive_type is target_ptype:
                    node.lvar.info.locs.append(operand)
                    return
                writes = self.component.writes.get(operand, ())
                if (
                    (not target_type.is_fixnum or actual.is_fixnum)
                    and len(writes) == 1
                    and not self.component.reads.get(operand)
                    and writes[0][0].info.translate is not None
                    and template_results_ok(writes[0][0].info, target_ptype)
                ):
                    operand.type = target_type
                    operand.primitive_type = target_ptype
                    node.lvar.info.locs.append(operand)
                    return
            if actual.fits(target_type):
                result, discard = self.result_tn(node)
                if operand is not result:
                    self.emit(vops.MOVE, node, args=(operand,), results=(result,))
                if discard:
                    self.emit_discard(node, result)
                return
        contract = node.contract
        if (
            checked
            and node.declaration is not None
            and node.declaration[0] == "inferred-return"
            and contract.checked_type is PYTHON_INTEGER
            and target_type.is_fixnum
        ):
            contract = InferredReturnContract(
                contract.name, target_type.widen_constant()
            )
        result, discard = self.result_tn(node)
        self.emit(
            vops.TYPE_CHECK,
            node,
            args=(operand,),
            results=(result,),
            info=TypeCheck(
                contract, node.location, node.safety if node.type_check else 0
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
        if name == "%setattr" and node.info.name == "__annotate__":
            local = self.context.local_environments
            receiver = node.args[0].use()
            target = local.function_for_value(receiver) if local is not None else None
            if target is not None:
                for arg in node.args:
                    self.emit_discard(node, self.lvar_tn(arg))
                return
        if name == "%make_function":
            self.convert_make_function(node)
        elif name in ("%resolve_method", "%resolve_super_method"):
            self.convert_resolve_method(node)
        elif name == "%call_resolved":
            self.convert_call_resolved(node)
        elif name in ("%range_prepare", "%range_available", "%range_advance"):
            self.convert_range(node)
        elif name in ("%byte_iterate", "%byte_available", "%byte_next"):
            self.emit_template(
                node,
                [*(self.lvar_tn(arg) for arg in node.args), self.range_tn(node.info)],
                None,
            )
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
        elif name == "%next_item":
            self.emit_template(node, [self.lvar_tn(arg) for arg in node.args], None)
        else:
            self.convert_template(node, [self.lvar_tn(arg) for arg in node.args])

    def convert_template(self, node, args, info=None):
        """Emit the template LTN selected (``ir2-convert-template``)."""
        if info is None:
            info = self.context.codegen_info.get(node, node.info)
        self.emit_template(node, args, node.info if info is None else info)

    def emit_template(self, node, args, info):
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
                info=info,
            )
            if discard:
                self.emit_discard(node, result)
        else:
            self.emit(template, node, args=args, info=info)

    def convert_full_call(self, node: Combination):
        local = self.context.local_environments
        target = local.call_target(node) if local is not None else None
        if target is not None:
            result, discard = self.result_tn(node)
            self.emit(
                vops.LOCAL_EXTENT_CALL,
                node,
                args=(
                    self.lvar_tn(node.fun),
                    *(self.lvar_tn(arg) for arg in node.args),
                ),
                results=(result,),
                info=CallSite(
                    (),
                    node.info.location if node.info is not None else None,
                    target,
                    True,
                    self.context.borrowed_callees.get(target, frozenset()),
                    local_environment=True,
                ),
            )
            if discard:
                self.emit_discard(node, result)
            return
        if self.context.plan is None and is_predictable_len_call(node):
            arg = node.args[0]
            arg_type = (
                self.context.type_of(arg.use()) if len(arg.uses) == 1 else UNKNOWN
            )
            primitive = known_length_primitive(arg_type)
            if primitive is not None:
                arg_ptype = (
                    arg.info.primitive_type
                    if arg.info is not None
                    else primitive_type_of(arg_type)
                )
                template = next(
                    candidate
                    for candidate in templates_for(primitive)
                    if candidate.ltn_policy != "raw"
                    and template_args_ok(candidate, (arg_ptype,))
                )
            else:
                template = vops.FAST_LEN
            result, discard = self.result_tn(node)
            self.emit(
                template,
                node,
                args=(self.lvar_tn(arg),),
                results=(result,),
            )
            if discard:
                self.emit_discard(node, result)
            return
        fun = node.fun
        target = self.context.targets.get(fun.use()) if len(fun.uses) == 1 else None
        code_target = (
            self.context.type_of(fun.use()).callable_target()
            if len(fun.uses) == 1
            else None
        )
        final_binding = (
            fun.use().accept(_FinalBindingRef(self.context)) is target
            and target is not None
        )
        proven = target is not None and (
            code_target is target
            or final_binding
            or (node.kind is CallKind.GUARDED and node.proven)
        )
        info = node.info
        keywords = info.keywords if info is not None else ()
        order = (
            target.call_order(keywords, len(node.args))
            if target is not None
            and (
                target in self.context.borrowed_callees
                or (target, "variants") in self.context.borrowed_callees
            )
            else None
        )
        borrowed_params = (
            set(self.context.borrowed_callees.get(target, ()))
            if order is not None
            else set()
        )
        if (
            order is not None
            and (target, "variants") in self.context.borrowed_callees
            and all(arg_idx is not None and arg_idx >= 0 for arg_idx in order)
            and not physical_entry(target)
        ):
            arg_types = tuple(
                self.context.type_of(node.args[arg_idx].use())
                if len(node.args[arg_idx].uses) == 1
                else UNKNOWN
                for arg_idx in order
            )
            for v_params, v_borrowed in self.context.borrowed_callees[
                target, "variants"
            ]:
                if all(
                    expected is UNKNOWN or actual.dispatch_fits(expected)
                    for actual, expected in zip(arg_types, v_params, strict=True)
                ):
                    borrowed_params |= v_borrowed
                    break
        borrowable = (
            frozenset(
                arg_idx
                for param_idx, arg_idx in enumerate(order)
                if arg_idx is not None and arg_idx >= 0 and param_idx in borrowed_params
            )
            if order is not None
            else frozenset()
        )
        site = CallSite(
            keywords,
            info.location if info is not None else None,
            target,
            proven,
            borrowable,
            final_binding=final_binding,
            callee_required=not (
                final_binding
                and not _final_call_needs_object(target, node, self.lambda_)
            ),
        )
        result, discard = self.result_tn(node)
        if discard and not proven:
            result.type = UNKNOWN
            result.primitive_type = T
        plan = self.context.plan
        template = vops.FULL_CALL
        if plan is not None and plan.allocation(node) is not None:
            template = vops.RAW_CONSTRUCT
        self.emit(
            template,
            node,
            args=(
                *((self.lvar_tn(fun),) if site.callee_required else ()),
                *(self.lvar_tn(arg) for arg in node.args),
            ),
            results=(result,),
            info=site,
        )
        if discard:
            self.emit_discard(node, result)

    def convert_make_function(self, node: Combination):
        lambda_ = made_lambda(node)
        local = self.context.local_environments
        if local is not None and local.discards_function(lambda_):
            # Defaults were evaluated before construction; discarding metadata
            # must not discard their effects or execute lazy annotations.
            for argument in node.args[1:]:
                self.emit_discard(node, self.lvar_tn(argument))
            result, discard = self.result_tn(node)
            self.emit(
                vops.CONSTANT, node, results=(result,), info=ast.NoneLiteral(None)
            )
            if discard:
                self.emit_discard(node, result)
            return
        if local is not None and local.is_local(lambda_):
            captured = []
            closure = lambda_.environment.closure if lambda_.environment else ()
            for var in closure:
                if var in self.stack_cells:
                    captured.append(self.stack_cells[var])
                    continue
                if var in local.stack_cells:
                    source = self.var_tn(var)
                else:
                    value = self.captured_value(node, var)
                    reference = local.function_for_binding(var) is not None
                    source = self.component.tn(
                        REFERENCE_T if reference else T,
                        name=f"$capture:{lambda_.name}:{var.name}",
                    )
                    self.emit(vops.MOVE, node, args=(value,), results=(source,))
                address = self.temporary(REFERENCE_T)
                self.emit(vops.LOCAL_ADDRESS, node, args=(source,), results=(address,))
                captured.append(address)
            result, discard = self.result_tn(node)
            result.primitive_type = REFERENCE_T
            self.emit(
                vops.LOCAL_FUNCTION,
                node,
                args=captured,
                results=(result,),
                info=lambda_,
            )
            if discard:
                self.emit_discard(node, result)
            return
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
        args = tuple(self.lvar_tn(arg) for arg in node.args)
        lvar = node.lvar
        if lvar is None:
            raise ValueError("a resolved method is always consumed by its call")
        method = self.lvar_tn(lvar)
        bound = self.component.tn(T, name="receiver")
        info = linked(node.info, node, "method info")
        resolution = MethodResolution(info.name)
        bound.type = UNKNOWN
        linked(lvar.info, lvar, "LTN annotation").locs.append(bound)
        self.emit(
            node.template,
            node,
            args=args,
            results=(method, bound),
            info=resolution,
        )

    def convert_call_resolved(self, node: Combination):
        resolved = node.args[0]
        method, bound = linked(resolved.info, resolved, "LTN annotation").locs
        resolve = resolved.use().accept(_MethodResolution(resolved))
        info = linked(node.info, node, "call info")
        result, discard = self.result_tn(node)
        self.emit(
            node.template,
            node,
            args=(method, bound, *(self.lvar_tn(arg) for arg in node.args[1:])),
            results=(result,),
            info=MethodCall(
                info.keywords,
                linked(resolve.info, resolve, "method info").name,
                self.context.targets.get(node),
                self.context.type_of(resolve.args[0].use())
                if len(resolve.args[0].uses) == 1
                else UNKNOWN,
            ),
        )
        if discard:
            self.emit_discard(node, result)

    def convert_range(self, node: Combination):
        info = node.info
        state = self.range_tn(info)
        step_sign = info.step_sign if info is not None else 0
        if (
            info is not None
            and step_sign == 0
            and len(info.arguments) == 3
            and len(info.arguments[2].uses) == 1
        ):
            step_type = self.context.type_of(info.arguments[2].use())
            if step_type.minimum is not None and step_type.minimum > 0:
                step_sign = 1
            elif step_type.maximum is not None and step_type.maximum < 0:
                step_sign = -1
        if node in self._fused_range_prepares:
            iterator_var, callee_leaf = self._fused_range_prepares[node]
            arg_tns = [self.lvar_tn(arg) for arg in info.arguments]
            result, discard = self.result_tn(node)
            iterator_tn = self.var_tn(iterator_var)
            fused_info = replace(
                info,
                step_sign=step_sign,
                fused=True,
                callee_leaf=callee_leaf,
            )
            self.emit(
                node.template,
                node,
                args=(*arg_tns, state),
                results=(result, iterator_tn),
                info=fused_info,
            )
            if discard:
                self.emit_discard(node, result)
            return
        args = [self.lvar_tn(arg) for arg in node.args]
        codegen_info = (
            replace(info, step_sign=step_sign)
            if info is not None and step_sign != info.step_sign
            else info
        )
        self.convert_template(node, [*args, state], codegen_info)

    def visit_Node(self, node):
        return None


__all__ = [
    "DEAD_LOCAL_STORE",
    "_BORROWING_VOPS",
    "_INERT_LOCAL_VALUE",
    "BranchTargets",
    "CallSite",
    "DirectSequenceIteration",
    "FunctionObject",
    "LocalAccess",
    "MethodCall",
    "MethodResolution",
    "TypeCheck",
    "UnpackCheck",
    "UnpackSlot",
    "_BorrowedLifetime",
    "_local_may_run_user_code",
    "_safe_borrow_step",
    "ir2_convert",
    "select_tuple_delivery",
]

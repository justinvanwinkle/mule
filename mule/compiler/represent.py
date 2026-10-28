"""Representation selection, following ``src/compiler/represent.lisp``.

Two decisions live here:

- ``select_representations`` assigns every TN of an IR2 component a storage
  class from its primitive type's candidates by the cost of the VOP operands
  that read and write it (``select-tn-representation``). Proven fixnum locals
  admit signed words; other immortal locals use tagged immediate storage.
  Identity demand propagates backward through float copies. Numeric-only
  copies select storage independently; explicit coercion VOPs connect them.

- ``select_native_plans`` decides which lambdas get a native entry: a body
  whose parameters, locals, and intermediate values all have proven machine
  representations under ``int64``/``bool``/native-class contracts. The plan
  is the representation assignment that entry is generated under, with the
  class layouts it depends on (guarded once at entry) and the allocations it
  owns. SBCL has no such all-or-nothing decision because its types are
  static; Mule guards class identity at runtime, so a native body needs a
  general body to fall back to.
"""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass, field
from typing import TYPE_CHECKING

from mule import ast

from . import vops
from .annotation_types import EXPORTS, ContractVisitor
from .ctype import BOTTOM, PYTHON_INTEGER, UNKNOWN
from .ir1util import block_exception_target
from .ltn import primitive_type_of
from .node import (
    CallKind,
    Cast,
    Combination,
    Constant,
    FunctionalKind,
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
from .vop import (
    BOOLEAN_T,
    BOXED_FLOAT,
    CLASS_T,
    DESCRIPTOR,
    DOUBLE,
    FIXNUM,
    FLOAT64,
    FLOAT_BOX_LAYOUT,
    IMMEDIATE,
    IMMORTAL_T,
    NONE_T,
    RANGE,
    RANGE_T,
    REFERENCE_T,
    SIGNED,
    SIGNED64,
    VOP,
    BoxLayout,
    IR2Component,
    PrimitiveType,
    T,
    TNRef,
    instance_type,
)

if TYPE_CHECKING:
    from .native_protocol import Layout

IMMORTAL_TYPES = (FIXNUM, BOOLEAN_T, NONE_T, IMMORTAL_T)


# Storage-class selection for one IR2 component.


def assign_variable_primitive_types(component: IR2Component) -> None:
    if component.context.plan is not None:
        return
    writes = component.writes
    for tn in component.tns:
        if tn.leaf is not None or (
            tn.name is not None and tn.name.startswith("$scalar.")
        ):
            tn.primitive_type = _variable_type(tn, component, writes)


def select_representations(component: IR2Component) -> None:
    raw = component.context.plan is not None
    reads = component.reads
    writes = component.writes
    if not raw:
        assign_variable_primitive_types(component)
    selected = {}
    if not raw:
        selected.update(_select_float_representations(component))
        selected.update(_select_fixnum_representations(component))
    for tn in component.tns:
        tn.sc = selected.get(tn) or select_tn_representation(
            tn,
            reads=reads.get(tn, ()),
            writes=writes.get(tn, ()),
            raw=raw,
            convention=component.context.convention,
        )


def _variable_type(tn, component, writes) -> PrimitiveType:
    """Derive a local variable TN's primitive type from its writers."""
    context = component.context
    value_type = BOTTOM
    written_types = set()
    for vop, _ref in writes.get(tn, ()):
        name = vop.info.name
        if name in ("move", "scalar-element-store"):
            source = vop.args[0].tn
            written = source.primitive_type
            written_value = source.type or UNKNOWN
        elif name == "move-argument":
            written_value = context.parameters[vop.codegen_info]
            written = primitive_type_of(written_value)
        elif name == "load-closure":
            written = tn.primitive_type
            written_value = tn.type or UNKNOWN
        elif name == "unbind-local":
            continue
        else:
            written = T
            written_value = UNKNOWN
        written_types.add(written)
        value_type = value_type.join(written_value)
    joined_ptype = primitive_type_of(value_type)
    if joined_ptype is FIXNUM or written_types == {FIXNUM}:
        # A raw slot cannot encode the unbound marker, and stack closures
        # access address-taken slots through the object-pointer convention.
        # Coroutine frame inspection also reads locals before assignment.
        if tn.leaf is not None and component.lambda_.coroutine_body:
            return IMMORTAL_T
        for reader, _ref in component.reads.get(tn, ()):
            if reader.info is vops.LOCAL_ADDRESS or (
                reader.info is vops.LOAD_LOCAL
                and (not reader.codegen_info.bound or reader.codegen_info.allow_unbound)
            ):
                return IMMORTAL_T
        if any(writer.info is vops.UNBIND_LOCAL for writer, _ in writes.get(tn, ())):
            return IMMORTAL_T
        return FIXNUM
    if joined_ptype.boxed_layout is not None:
        return joined_ptype
    if len(written_types) == 1:
        written = next(iter(written_types))
        if written.boxed_layout is not None:
            return written
    if joined_ptype in IMMORTAL_TYPES or written_types.issubset(IMMORTAL_TYPES):
        return IMMORTAL_T
    return T


def _representation_move(vop) -> bool:
    """Whether its inputs/results may alias and require matching storage."""
    if vop.info.name in ("move", "boxed-float-positive"):
        return True
    if vop.info.name == "load-local":
        access = vop.codegen_info
        return access.bound and not access.allow_unbound
    return False


def _is_non_identity_list_setitem(
    component, reader, ref, has_identity_compare: bool
) -> bool:
    if (
        has_identity_compare
        or reader.info.name != "protocol-setitem"
        or len(reader.args) != 3
        or ref is not reader.args[2]
    ):
        return False
    seq_tn = reader.args[0].tn
    if not (seq_tn.type or UNKNOWN).is_exact_list:
        return False
    seq_writes = component.writes.get(seq_tn, ())
    if len(seq_writes) != 1 or seq_writes[0][0].info is not vops.LOAD_LOCAL:
        return False
    seq_local = seq_writes[0][0].args[0].tn
    if (
        seq_local.leaf is None
        or seq_local.leaf.indirect
        or seq_local.leaf in component.lambda_.vars
    ):
        return False
    for load_vop, _ in component.reads.get(seq_local, ()):
        if load_vop.info is not vops.LOAD_LOCAL:
            return False
        loaded_tn = load_vop.results[0].tn
        for use_vop, use_ref in component.reads.get(loaded_tn, ()):
            u_name = use_vop.info.name
            if (
                u_name
                in (
                    "protocol-getitem",
                    "fast-len",
                    "primitive-_storage_length",
                    "protocol-iterate",
                )
                and use_ref is use_vop.args[0]
            ):
                continue
            if u_name == "protocol-setitem" and use_ref is use_vop.args[0]:
                continue
            if (
                u_name == "resolve-method"
                and use_vop.codegen_info is not None
                and use_vop.codegen_info.exact_list_append
            ):
                continue
            return False
    return True


def _select_float_representations(component):
    """Keep original objects on identity paths, not on numeric-only branches.

    Demand flows backward through every copy writer, including loop-carried
    assignments. It does not flow forward to an independent numeric read.
    Moves impose no intrinsic descriptor cost, as in SBCL's initial costing
    before their other operands have selected representations.
    """
    convention = component.context.convention
    has_identity_compare = any(
        vop.info.name in ("compare-is", "compare-is_not")
        for block in component.blocks
        for vop in block.vops
    )
    unboxed_locals = {
        tn
        for tn in component.tns
        if tn.leaf is not None
        and (contract := component.lambda_.local_annotations.get(tn.leaf.name))
        is not None
        and primitive_type_of(contract.fact) is BOXED_FLOAT
    }
    demanded = set()
    for tn in component.tns:
        if tn.primitive_type is not BOXED_FLOAT:
            if not tn.primitive_type.admits(DOUBLE):
                demanded.add(tn)
            continue
        if tn in unboxed_locals:
            continue
        for reader, ref in component.reads.get(tn, ()):
            if _representation_move(reader):
                continue
            if (
                reader.info.name == "protocol-setitem"
                and len(reader.args) == 3
                and ref is reader.args[2]
            ):
                if not _is_non_identity_list_setitem(
                    component, reader, ref, has_identity_compare
                ):
                    demanded.add(tn)
                continue
            restriction = _operand_restriction(reader, ref, convention=convention)
            if restriction is not None and DOUBLE not in restriction:
                demanded.add(tn)
        if tn.leaf is not None and any(
            writer.info is vops.UNBIND_LOCAL
            for writer, _ref in component.writes.get(tn, ())
        ):
            demanded.add(tn)
    pending = list(demanded)
    while pending:
        tn = pending.pop()
        for writer, _ref in component.writes.get(tn, ()):
            if not _representation_move(writer):
                continue
            for operand in writer.args:
                source = operand.tn
                if source not in demanded and source not in unboxed_locals:
                    demanded.add(source)
                    pending.append(source)
    selected = {
        tn: DESCRIPTOR
        if tn in demanded
        else select_tn_representation(
            tn,
            reads=[
                (vop, ref)
                for vop, ref in component.reads.get(tn, ())
                if not _representation_move(vop)
            ],
            writes=[
                (vop, ref)
                for vop, ref in component.writes.get(tn, ())
                if not _representation_move(vop)
            ],
            convention=convention,
        )
        for tn in component.tns
        if tn.primitive_type is BOXED_FLOAT
    }
    return _refine_copy_representations(component, selected, demanded)


def _select_fixnum_representations(component):
    """Select each proven integer's storage independently of object boundaries."""
    selected = {
        tn: select_tn_representation(
            tn,
            reads=[
                (vop, ref)
                for vop, ref in component.reads.get(tn, ())
                if not _representation_move(vop)
            ],
            writes=[
                (vop, ref)
                for vop, ref in component.writes.get(tn, ())
                if not _representation_move(vop)
            ],
            convention=component.context.convention,
        )
        for tn in component.tns
        if tn.primitive_type is FIXNUM
    }
    return _refine_copy_representations(component, selected)


def _refine_copy_representations(component, selected, demanded=()):
    # Once both ends are selected, charge moves for actual representation
    # changes. Keep ties stable: each update must reduce the graph's total
    # operand/conversion cost, so cyclic copy graphs cannot oscillate.
    changed = True
    while changed:
        changed = False
        for tn, current in selected.items():
            if tn in demanded:
                continue
            best = current
            best_cost = _copy_representation_cost(component, tn, current, selected)
            if best_cost == 0:
                continue
            for sc in tn.primitive_type.scs:
                if sc is current:
                    continue
                cost = _copy_representation_cost(component, tn, sc, selected)
                if cost < best_cost:
                    best, best_cost = sc, cost
                    if best_cost == 0:
                        break
            if best is not current:
                selected[tn] = best
                changed = True
    return selected


def _copy_representation_cost(component, tn, sc, selected):
    cost = 0
    for vop, ref in (*component.reads.get(tn, ()), *component.writes.get(tn, ())):
        if _representation_move(vop):
            other = vop.args[0].tn if ref.write_p else vop.results[0].tn
            other_sc = selected.get(other, other.primitive_type.scs[0])
            cost += sc.llvm_type != other_sc.llvm_type
        else:
            restriction = _operand_restriction(
                vop, ref, convention=component.context.convention
            )
            cost += restriction is not None and sc not in restriction
    return cost


def emit_moves_and_coercions(component: IR2Component) -> None:
    """Make scalar representation changes explicit after final call selection.

    Keep local reads and assignment moves: their metadata governs borrowing,
    last reads, and releasing overwritten locals. Only the intervening scalar
    conversion consumes its new temporary. Native plans already define their
    complete representation and use their existing native generators.
    """
    if component.context.plan is not None:
        return

    def temporary(tn, sc):
        result = component.tn(tn.primitive_type)
        result.type = tn.type
        result.sc = sc
        return result

    def coerce(node, source, target):
        float_change = {source.sc, target.sc} == {DESCRIPTOR, DOUBLE}
        fixnum_change = (
            source.primitive_type is FIXNUM or target.primitive_type is FIXNUM
        ) and (
            (source.sc is SIGNED and target.sc in vops.OBJECT)
            or (target.sc is SIGNED and source.sc in vops.OBJECT)
        )
        if not (float_change or fixnum_change):
            raise ValueError(
                f"unsupported representation conversion: {source} -> {target}"
            )
        return VOP(vops.COERCE, node, [TNRef(source, False)], [TNRef(target, True)])

    for block in component.blocks:
        converted = []
        for vop in block.vops:
            if _representation_move(vop):
                source, target = vop.args[0].tn, vop.results[0].tn
                if source.sc.llvm_type != target.sc.llvm_type and (
                    source.primitive_type is BOXED_FLOAT
                    or target.primitive_type is BOXED_FLOAT
                    or source.primitive_type is FIXNUM
                    or target.primitive_type is FIXNUM
                ):
                    proof = (
                        source
                        if source.primitive_type in (BOXED_FLOAT, FIXNUM)
                        else target
                    )
                    if vop.info is vops.MOVE:
                        intermediate = temporary(proof, target.sc)
                        converted.append(coerce(vop.node, source, intermediate))
                        vop.args[0] = TNRef(intermediate, False)
                        converted.append(vop)
                    else:
                        intermediate = temporary(proof, source.sc)
                        vop.results[0] = TNRef(intermediate, True)
                        converted.extend((vop, coerce(vop.node, intermediate, target)))
                    continue
                converted.append(vop)
                continue
            before, after = [], []
            for operands in (vop.args, vop.results):
                for index, ref in enumerate(operands):
                    tn = ref.tn
                    if tn.primitive_type not in (BOXED_FLOAT, FIXNUM):
                        continue
                    restriction = _operand_restriction(
                        vop, ref, convention=component.context.convention
                    )
                    if restriction is None or tn.sc in restriction:
                        continue
                    candidates = [
                        sc for sc in tn.primitive_type.scs if sc in restriction
                    ]
                    if not candidates:
                        raise ValueError(f"no legal representation for {ref} in {vop}")
                    intermediate = temporary(tn, candidates[0])
                    operands[index] = TNRef(intermediate, ref.write_p)
                    if ref.write_p:
                        after.append(coerce(vop.node, intermediate, tn))
                    else:
                        before.append(coerce(vop.node, tn, intermediate))
            converted.extend((*before, vop, *after))
        block.vops = converted
    component.reads.clear()
    component.writes.clear()
    for block in component.blocks:
        for vop in block.vops:
            for ref in vop.args:
                component.reads[ref.tn].append((vop, ref))
            for ref in vop.results:
                component.writes[ref.tn].append((vop, ref))


def can_use_double(component, tn, *, ignore=()) -> bool:
    """Whether every value and boundary in a copy group admits raw doubles.

    ``ignore`` omits boundary VOPs whose convention is currently being selected,
    such as the producer of a numeric call result or an argument initialization.
    The caller must replace each omitted boundary with a compatible convention.
    All other uses, including identity-sensitive uses, retain their restrictions.
    """
    _group, candidates = _move_group_representations(component, tn, ignore=ignore)
    return DOUBLE in candidates


def fresh_float_returns(component) -> bool:
    """Whether object returns can be reconstructed without losing aliases."""
    found = False
    for block in component.blocks:
        for vop in block.vops:
            if vop.info.name != "return":
                continue
            found = True
            group, candidates = _move_group_representations(
                component, vop.args[0].tn, ignore=(vop,)
            )
            if DOUBLE not in candidates:
                return False
            for tn in group:
                for writer, _ref in component.writes.get(tn, ()):
                    if _representation_move(writer):
                        continue
                    if not writer.info.produces_fresh(writer):
                        return False
    return found


def fresh_box_returns(component: IR2Component, layout: BoxLayout) -> bool:
    """Whether an object entry returns fresh leaf storage without another escape."""
    if component.context.plan is not None or component.context.convention is not None:
        return False
    found = False
    for block in component.blocks:
        for vop in block.vops:
            if vop.info.name != "return":
                continue
            found = True
            group, _candidates = _move_group_representations(component, vop.args[0].tn)
            if not _private_box_group(
                component, group, layout, None, allow_return=True
            ):
                return False
    return found


def mark_private_boxes(
    component: IR2Component,
    *,
    fresh_result: Callable[[VOP], BoxLayout | None] | None = None,
) -> None:
    """Prove private ownership for complete groups of leaf-box copies.

    Every origin must allocate fresh leaf storage, and every non-copy reader
    must explicitly promise not to capture the reference. A resolver can supply
    an interprocedural fresh-result proof for an otherwise opaque producer.
    Unknown origins and uses retain ordinary shared-object ownership.
    """
    for tn in component.tns:
        tn.private_box = None
    if component.context.plan is not None:
        return
    visited = set()
    for root in component.tns:
        layout = root.primitive_type.boxed_layout
        if root in visited or root.sc is not DESCRIPTOR or layout is None:
            continue
        group, _candidates = _move_group_representations(component, root)
        visited.update(group)
        if _private_box_group(component, group, layout, fresh_result):
            for tn in group:
                tn.private_box = layout


def _private_box_group(
    component, group, layout, fresh_result, *, allow_return=False
) -> bool:
    found_origin = False
    for tn in group:
        if tn.sc is not DESCRIPTOR or tn.primitive_type.boxed_layout is not layout:
            return False
        for reader, ref in component.reads.get(tn, ()):
            if _representation_move(reader):
                continue
            if allow_return and reader.info.name == "return":
                continue
            if reader.info.arg_escapes is None or len(reader.info.arg_escapes) != len(
                reader.args
            ):
                return False
            if reader.info.captures_argument(reader.args.index(ref), vop=reader):
                return False
        for writer, _ref in component.writes.get(tn, ()):
            if _representation_move(writer) or writer.info.name == "unbind-local":
                continue
            if writer.info.produces_fresh(writer):
                found_origin = True
                continue
            if fresh_result is not None and fresh_result(writer) is layout:
                found_origin = True
                continue
            return False
    return found_origin


def _move_group_representations(component, root, *, ignore=()):
    group = set()
    pending = [root]
    ignored = frozenset(ignore)
    candidates = [sc for sc in root.primitive_type.scs if sc.raw]
    convention = component.context.convention
    while pending:
        tn = pending.pop()
        if tn in group:
            continue
        group.add(tn)
        candidates = [sc for sc in candidates if tn.primitive_type.admits(sc)]
        for vop, ref in (
            *component.reads.get(tn, ()),
            *component.writes.get(tn, ()),
        ):
            if vop in ignored:
                continue
            if _representation_move(vop):
                pending.extend(operand.tn for operand in (*vop.args, *vop.results))
                continue
            restriction = _operand_restriction(vop, ref, convention=convention)
            if restriction is not None:
                candidates = [sc for sc in candidates if sc in restriction]
    return group, candidates


def select_tn_representation(tn, *, reads=(), writes=(), raw=False, convention=None):
    """The cheapest storage class among the primitive type's candidates,
    counting one coercion per operand whose restriction excludes it. A native
    body holds every value that has a machine representation in it."""
    if tn.primitive_type is RANGE_T:
        return RANGE
    candidates = tn.primitive_type.scs
    if raw and any(sc.raw for sc in candidates):
        candidates = [sc for sc in candidates if sc.raw]
    if len(candidates) == 1 or (not reads and not writes):
        return candidates[0]
    best, best_cost = None, None
    for sc in candidates:
        cost = 0
        for vop, ref in (*reads, *writes):
            restriction = _operand_restriction(vop, ref, raw=raw, convention=convention)
            if restriction is not None and sc not in restriction:
                cost += 1
        if cost == 0:
            return sc
        if best_cost is None or cost < best_cost:
            best, best_cost = sc, cost
    return best


def _operand_restriction(vop, ref, *, raw=False, convention=None):
    restriction = (
        convention.operand_restriction(vop, ref)
        if convention is not None and not raw
        else None
    )
    if restriction is None:
        restriction = _restriction(vop, ref)
    if (
        restriction is None
        and ref.tn.primitive_type is FIXNUM
        and vop.info.name in ("branch", "discard")
    ):
        return (IMMEDIATE, SIGNED)
    if not raw and ref.tn.primitive_type is FIXNUM:
        vop_name = vop.info.name
        if ref.write_p and vop_name == "range-advance":
            return (SIGNED,)
        if ref.write_p and vop_name == "primitive-_storage_length" and vop.args:
            seq_type = vop.args[0].tn.type or UNKNOWN
            if seq_type.is_exact_list or seq_type.is_exact_tuple:
                return (SIGNED,)
        if not ref.write_p and vop_name == "range-prepare" and ref is not vop.args[-1]:
            info = vop.codegen_info
            if info is not None and (
                info.direct
                or (
                    info.fused
                    and info.step_sign != 0
                    and all(
                        op.tn.type is not None
                        and op.tn.type.fits(PYTHON_INTEGER)
                        and op.tn.type.is_fixnum
                        and not op.tn.type.may_be_unbound
                        for op in vop.args[:-1]
                    )
                )
            ):
                return (SIGNED,)
        if (
            not ref.write_p
            and vop_name == "protocol-getitem"
            and len(vop.args) >= 2
            and ref is vop.args[1]
        ):
            seq_type = vop.args[0].tn.type or UNKNOWN
            if (seq_type.is_exact_list or seq_type.is_exact_tuple) and (
                vop.args[1].tn.type or UNKNOWN
            ).is_fixnum:
                return (SIGNED,)
        if (
            not ref.write_p
            and vop_name == "primitive-_storage_read"
            and len(vop.args) == 2
            and ref is vop.args[1]
        ):
            seq_type = vop.args[0].tn.type or UNKNOWN
            if (seq_type.is_exact_list or seq_type.is_exact_tuple) and (
                vop.args[1].tn.type or UNKNOWN
            ).is_fixnum:
                return (SIGNED,)
        if (
            not ref.write_p
            and vop_name in ("protocol-setitem", "primitive-_storage_write")
            and len(vop.args) >= 3
        ):
            seq_type = vop.args[0].tn.type or UNKNOWN
            idx_type = vop.args[1].tn.type or UNKNOWN
            if seq_type.is_exact_list and idx_type.is_fixnum:
                if ref is vop.args[1]:
                    return (SIGNED,)
                if ref is vop.args[2]:
                    return (*vops.OBJECT, SIGNED)
    return ref.tn.primitive_type.operand_restriction(restriction, raw=raw)


def _restriction(vop, ref):
    operands = vop.results if ref.write_p else vop.args
    restrictions = vop.info.results if ref.write_p else vop.info.args
    if not restrictions:
        return None
    index = operands.index(ref)
    if index >= len(restrictions):
        return None
    return restrictions[index]


# Native representation plans.


class NotNative(Exception):
    pass


class Deferred(Exception):
    pass


class Representation:
    llvm_type = "i64"
    zero = "0"
    primitive_type = SIGNED64
    owns_result = False

    def numeric(self):
        raise NotNative

    def python_integer(self):
        raise NotNative

    def python_float(self):
        raise NotNative

    def python_real(self):
        raise NotNative

    def field(self, name):
        raise NotNative

    def method(self, name):
        raise NotNative

    def constructor(self):
        raise NotNative

    def truth(self):
        raise NotNative

    def box(self, gen, value):
        """An owned object for a raw value; representations that cannot
        leave a native body raise ``NotNative`` during plan selection."""
        raise NotNative

    def parameter(self, gen, value):
        raise NotNative

    def validate_boxing(self) -> None:
        raise NotNative

    def prepare_return(self, gen, value):
        return value

    def can_adapt_return(self, component, plan=None) -> bool:
        return True


class BoxableRepresentation(Representation):
    """A representation with a defined conversion at an object boundary."""

    def validate_boxing(self) -> None:
        pass


class ObjectReference(BoxableRepresentation):
    """A borrowed object internally, transferred as an owned native result."""

    llvm_type = "ptr"
    zero = "null"
    owns_result = True

    def prepare_return(self, gen, value):
        return gen.operation(f"call ptr @mule.fast.retain(ptr {value})")

    def box(self, gen, value):
        # The raw return convention already transferred an owned reference.
        return gen.take_native_result(value)


class Int64(BoxableRepresentation):
    def python_real(self):
        return self

    def numeric(self):
        return self

    def python_integer(self):
        return self

    def truth(self):
        return True

    def box(self, gen, value):
        return gen.operation(f"call ptr @mule.box.i64(i64 {value})")

    def can_adapt_return(self, component, plan=None) -> bool:
        # Reconstructing a wide integer can replace an existing object's
        # identity. Tagged fixnum results do not have that ownership boundary.
        return plan is not None and not plan.result_may_alias

    def parameter(self, gen, value):
        slot = gen.operation("alloca i64")
        status = gen.operation(f"call i32 @mule_unbox_i64(ptr {value}, ptr {slot})")
        gen.check_status(status)
        return gen.operation(f"load i64, ptr {slot}")


class Float64(BoxableRepresentation):
    def python_real(self):
        return self

    llvm_type = "double"
    zero = "0.0"
    primitive_type = FLOAT64

    def numeric(self):
        return self

    def python_float(self):
        return self

    def truth(self):
        return True

    def box(self, gen, value):
        return gen.operation(f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {value})")

    def can_adapt_return(self, component, plan=None) -> bool:
        return fresh_float_returns(component)

    def parameter(self, gen, value):
        slot = gen.operation("alloca double")
        status = gen.operation(f"call i32 @mule_unbox_f64(ptr {value}, ptr {slot})")
        gen.check_status(status)
        return gen.operation(f"load double, ptr {slot}")


class Boolean(BoxableRepresentation):
    def python_real(self):
        return self

    llvm_type = "i1"
    zero = "false"
    primitive_type = BOOLEAN_T

    def python_integer(self):
        return self

    def truth(self):
        return True

    def box(self, gen, value):
        return gen.operation(f"select i1 {value}, ptr @mule.true, ptr @mule.false")


class Nothing(BoxableRepresentation):
    primitive_type = NONE_T

    def box(self, gen, value):
        return "@mule.none"


@dataclass(frozen=True)
class Instance(ObjectReference):
    layout_id: str
    layout: Layout = field(compare=False)
    llvm_type = "ptr"
    zero = "null"

    @property
    def primitive_type(self):
        return instance_type(self.layout)

    def field(self, name):
        if name in self.layout.info.fields:
            return INT64
        return self.method(name).bound()

    def method(self, name):
        target = self.layout.methods.get(name)
        if target is None:
            raise NotNative
        return Method(self, target, name)

    def parameter(self, gen, value):
        payload = gen.operation(
            f"call ptr @mule_native_instance_payload(ptr {value}, "
            f"ptr {self.layout.schema}, ptr %globals)"
        )
        gen.check_pointer(payload)
        return value


@dataclass(frozen=True)
class Class(ObjectReference):
    binding: str
    layout_id: str
    layout: Layout = field(compare=False)
    llvm_type = "ptr"
    zero = "null"
    primitive_type = CLASS_T

    def constructor(self):
        return Instance(self.layout_id, self.layout)


@dataclass(frozen=True)
class Method(Representation):
    receiver: Instance
    target: object
    name: str
    llvm_type = "ptr"
    zero = "null"

    @property
    def primitive_type(self):
        return self.receiver.primitive_type

    def bound(self):
        return BoundMethod(self.receiver, self.target, self.name)


@dataclass(frozen=True)
class BoundMethod(ObjectReference):
    receiver: Instance
    target: object
    name: str
    primitive_type = REFERENCE_T


INT64, FLOAT, BOOL, NONE = Int64(), Float64(), Boolean(), Nothing()


class _Contract(ContractVisitor):
    def visit_Contract(self, contract):
        raise NotNative

    def visit_IntegerContract(self, contract):
        if contract.bits != 64 or not contract.signed:
            raise NotNative
        return INT64

    def visit_FloatContract(self, contract):
        # The runtime stores every float as binary64; narrower widths would
        # need their own rounding at each operation.
        if contract.bits != 64:
            raise NotNative
        return FLOAT

    visit_PythonIntegerContract = visit_Contract
    visit_PythonFloatContract = visit_Contract


class _LocalContract(_Contract):
    def visit_PythonFloatContract(self, contract):
        return None

    def visit_PythonIntegerContract(self, contract):
        # Unbounded Python int is a semantic constraint, not a machine width.
        # Select storage from the local's definitions and arithmetic proofs.
        return None


class _AssertionContract(_Contract):
    def __init__(self, representation):
        self.representation = representation

    def visit_PythonIntegerContract(self, contract):
        return self.representation.python_integer()

    def visit_FloatContract(self, contract):
        if contract.bits != 64:
            raise NotNative
        return self.representation.python_float()

    visit_PythonFloatContract = visit_FloatContract


class _Literal(ast.Visitor):
    def visit_Node(self, node):
        raise NotNative

    def visit_IntegerLiteral(self, node):
        if not -(1 << 63) <= node.value < (1 << 63):
            raise NotNative
        return INT64

    def visit_FloatLiteral(self, node):
        return FLOAT

    def visit_BooleanLiteral(self, node):
        return BOOL

    def visit_NoneLiteral(self, node):
        return NONE


@dataclass(slots=True)
class NativePlan:
    """The representation assignment of one native entry."""

    function: Lambda
    index: int
    parameters: tuple[Representation, ...]
    locals: dict[LambdaVar, Representation]
    values: dict[object, Representation] = field(default_factory=dict)
    returned: Representation | None = None
    dependencies: dict[GlobalVar, Class] = field(default_factory=dict)
    calls: set[Lambda] = field(default_factory=set)
    allocations: dict[object, Class] = field(default_factory=dict)
    # Exactly one owning root for each constructor or owned native call result.
    owners: dict[object, Representation] = field(default_factory=dict)
    method_targets: dict[object, Lambda] = field(default_factory=dict)
    return_sources: list = field(default_factory=list)
    result_may_alias: bool = True

    @property
    def entry(self):
        return f"@mule.native.{self.index}"

    def lvar_type(self, lvar):
        """The primitive type every producer of ``lvar`` delivers, or None
        when a producer has no representation or they disagree."""
        types = {self.node_type(use) for use in lvar.uses}
        if len(types) != 1:
            return None
        return types.pop()

    def node_type(self, node):
        representation = self.values.get(node)
        return representation.primitive_type if representation is not None else None

    def var_type(self, var):
        representation = self.locals.get(var)
        return representation.primitive_type if representation is not None else None

    def allocation(self, node):
        return self.allocations.get(node)

    def method_target(self, lvar):
        return self.method_targets[lvar.use()]


_BINARY = {
    ast.BinaryOperator.ADD,
    ast.BinaryOperator.SUBTRACT,
    ast.BinaryOperator.MULTIPLY,
    ast.BinaryOperator.BIT_AND,
    ast.BinaryOperator.BIT_OR,
    ast.BinaryOperator.BIT_XOR,
}
_FLOAT_BINARY = {
    ast.BinaryOperator.ADD,
    ast.BinaryOperator.SUBTRACT,
    ast.BinaryOperator.MULTIPLY,
    ast.BinaryOperator.DIVIDE,
}
_COMPARE = {
    ast.ComparisonOperator.EQUAL,
    ast.ComparisonOperator.NOT_EQUAL,
    ast.ComparisonOperator.LESS,
    ast.ComparisonOperator.LESS_EQUAL,
    ast.ComparisonOperator.GREATER,
    ast.ComparisonOperator.GREATER_EQUAL,
}
_BINARY_NAMES = {
    **{f"%{operator.name.lower()}": operator for operator in ast.BinaryOperator},
    **{f"%i{operator.name.lower()}": operator for operator in ast.BinaryOperator},
}
_UNARY_NAMES = {
    f"%unary_{operator.name.lower()}": operator for operator in ast.UnaryOperator
}
_COMPARE_NAMES = {
    f"%compare_{operator.name.lower()}": operator for operator in ast.ComparisonOperator
}


class _Select(NodeVisitor, LeafVisitor):
    """Assign representations to one lambda's values (``_Select``). A
    reference's representation is its leaf's."""

    def __init__(self, plan, layouts, bindings, known_plans):
        self.plan, self.layouts, self.bindings = plan, layouts, bindings
        self.known_plans = known_plans
        self.block = None

    def value(self, lvar):
        if len(lvar.uses) != 1:
            raise NotNative
        value = self.plan.values.get(lvar.uses[0])
        if value is None:
            raise Deferred
        return value

    def visit_Node(self, node):
        raise NotNative

    def visit_Bind(self, node):
        return None

    def visit_Entry(self, node):
        return None

    def visit_Exit(self, node):
        if node.entry is not None:
            raise NotNative

    def visit_Enclose(self, node):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        raise NotNative

    def visit_Constant(self, leaf: Constant):
        return leaf.value.accept(_Literal())

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf.indirect or leaf.home is not self.plan.function:
            raise NotNative
        result = self.plan.locals.get(leaf)
        if result is None:
            raise Deferred
        return result

    def visit_GlobalVar(self, leaf: GlobalVar):
        identity = self.bindings.get(leaf.name)
        if identity is None:
            raise NotNative
        result = Class(leaf.name, identity, self.layouts[identity])
        self.plan.dependencies[leaf] = result
        return result

    def visit_KnownFunction(self, leaf: KnownFunction):
        return None

    def visit_Lambda(self, leaf: Lambda):
        return None

    def visit_Set(self, node: Set):
        node.var.accept(_AssignLocal(self, node))

    def visit_Cast(self, node: Cast):
        value = self.value(node.value)
        expected = node.contract.accept(_AssertionContract(value))
        if value != expected:
            raise NotNative
        return expected

    def visit_If(self, node: If):
        self.value(node.test).truth()

    def visit_Return(self, node: Return):
        if not node.result.uses:
            raise NotNative
        if node.result not in self.plan.return_sources:
            self.plan.return_sources.append(node.result)
        for use in node.result.uses:
            value = self.plan.values.get(use)
            if value is None:
                raise Deferred
            value.validate_boxing()
            if self.plan.returned is not None and self.plan.returned != value:
                raise NotNative
            self.plan.returned = value

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN:
            return self.construct(node)
        name = linked(node.fun_info, node, "known function").name
        args = node.args
        if name in _BINARY_NAMES:
            return self.binary(_BINARY_NAMES[name], args)
        if name in _UNARY_NAMES:
            operator = _UNARY_NAMES[name]
            value = self.value(args[0])
            if operator == ast.UnaryOperator.NOT:
                value.truth()
                return BOOL
            value.numeric()
            if operator == ast.UnaryOperator.INVERT and value is FLOAT:
                raise NotNative
            return value if value is FLOAT else INT64
        if name in _COMPARE_NAMES:
            if _COMPARE_NAMES[name] not in _COMPARE:
                raise NotNative
            self.numeric_operands(args)
            return BOOL
        if name == "%getattr":
            result = self.value(args[0]).field(
                linked(node.info, node, "attribute").name
            )
            if result.owns_result:
                self._own_result(node, result)
            return result
        if name == "%setattr":
            attribute = linked(node.info, node, "attribute").name
            if self.value(args[0]).field(attribute) != self.value(args[1]):
                raise NotNative
            return None
        if name == "%resolve_method":
            method = self.value(args[0]).method(linked(node.info, node, "method").name)
            self.plan.method_targets[node] = method.target
            return method
        if name == "%call_resolved":
            return self.call_method(node)
        if name in ("%min2", "%max2"):
            left, right = self.numeric_operands(args)
            if left != right:
                raise NotNative
            return left
        if name == "%math_sqrt":
            self.value(args[0]).numeric()
            return FLOAT
        raise NotNative

    def numeric_operands(self, args):
        return self.value(args[0]).numeric(), self.value(args[1]).numeric()

    def binary(self, operator, args):
        left, right = self.numeric_operands(args)
        if FLOAT in (left, right):
            # Python rounds the integer operand to a float before a mixed
            # operation, as ``sitofp`` does.
            if operator not in _FLOAT_BINARY:
                raise NotNative
            return FLOAT
        if operator not in _BINARY:
            raise NotNative
        return INT64

    def call_method(self, node):
        if node.info.keywords:
            raise NotNative
        method = self.value(node.args[0])
        target = method.target
        arguments = node.args[1:]
        if not target.signature.positional_body:
            raise NotNative
        if len(target.vars) != len(arguments) + 1:
            raise NotNative
        for argument, contract in zip(
            arguments, target.signature.annotations[1:], strict=True
        ):
            if contract is None or self.value(argument) != contract.accept(_Contract()):
                raise NotNative
        self.plan.calls.add(target)
        annotation = target.signature.implementation_return_annotation
        if annotation is not None:
            result = annotation.accept(_Contract())
        else:
            known = self.known_plans.get(target)
            if known is None or known.returned is None:
                raise Deferred
            result = known.returned
        if result.owns_result:
            self._own_result(node, result)
        return result

    def _own_result(self, node, representation):
        """Record a root only when its site cannot overwrite a live loop owner."""
        pending, visited = list(linked(self.block, self, "current block").succ), set()
        while pending:
            block = pending.pop()
            if block is self.block:
                raise NotNative
            if block not in visited:
                visited.add(block)
                pending.extend(block.succ)
        self.plan.owners[node] = representation

    def construct(self, node):
        if node.info is not None and node.info.keywords:
            raise NotNative
        callee = self.value(node.fun)
        instance = callee.constructor()
        initializer = instance.layout.methods["__init__"]
        if not initializer.signature.positional_body:
            raise NotNative
        if len(node.args) != len(initializer.vars) - 1:
            raise NotNative
        for argument, contract in zip(
            node.args, initializer.signature.annotations[1:], strict=True
        ):
            if contract is None or self.value(argument) != contract.accept(_Contract()):
                raise NotNative
        self._own_result(node, instance)
        self.plan.calls.add(initializer)
        self.plan.allocations[node] = callee
        return instance


class _AssignLocal(LeafVisitor):
    """A native body assigns only its own direct locals."""

    def __init__(self, selector: _Select, node: Set):
        self.selector = selector
        self.node = node

    def visit_Leaf(self, leaf):
        raise NotNative

    def visit_LambdaVar(self, leaf: LambdaVar):
        if self.node.value is None or leaf.indirect:
            raise NotNative
        plan = self.selector.plan
        value = self.selector.value(self.node.value)
        old = plan.locals.get(leaf)
        if old is not None and old != value:
            raise NotNative
        plan.locals[leaf] = value


def class_bindings(lambdas, layouts) -> dict[str, str]:
    """Global names bound to a native class definition anywhere in the
    program, keyed to the class identity."""
    if not layouts:
        return {}
    collector = _ClassBindings(layouts)
    for lambda_ in lambdas:
        for block in lambda_.component.blocks():
            for node in block.nodes():
                node.accept(collector)
    return collector.bindings


class _ClassBindings(NodeVisitor, LeafVisitor):
    """Record global assignments whose single producer makes a native class."""

    def __init__(self, layouts):
        self.layouts = layouts
        self.bindings: dict[str, str] = {}
        self.node: Set | None = None

    def visit_Set(self, node: Set):
        self.node = node
        node.var.accept(self)
        self.node = None

    def visit_GlobalVar(self, leaf: GlobalVar):
        node = linked(self.node, self, "assignment")
        value = node.value
        if value is None or len(value.uses) != 1:
            return
        class_id = value.uses[0].accept(self)
        if class_id is not None:
            self.bindings.setdefault(leaf.name, class_id)

    def visit_Combination(self, node: Combination):
        if (
            self.node is None
            or node.kind is not CallKind.KNOWN
            or linked(node.fun_info, node, "known function").name != "%make_class"
        ):
            return None
        class_id = linked(node.info, node, "class definition").class_id
        return class_id if class_id in self.layouts else None


def select_native_plans(lambdas, facts, layouts) -> dict[Lambda, NativePlan]:
    """Choose native entries; remove any caller whose local edges lack a body."""
    bindings = class_bindings(lambdas, layouts)
    plans: dict[Lambda, NativePlan] = {}
    previous_count = -1
    while len(plans) != previous_count:
        previous_count = len(plans)
        for index, lambda_ in enumerate(lambdas):
            if (
                lambda_.kind is FunctionalKind.TOPLEVEL
                or lambda_.policy.speed < 3
                or lambda_ in plans
            ):
                continue
            try:
                plans[lambda_] = _select_function(
                    lambda_, index, facts[lambda_], layouts, bindings, plans
                )
            except NotNative:
                continue
    # Native recursion requires SBCL's stack/entry checks before local edges can
    # bypass the existing boxed frame guard. Keep recursive components boxed.
    cyclic = set()
    for lambda_, plan in plans.items():
        pending, seen = list(plan.calls), set()
        while pending:
            target = pending.pop()
            if target is lambda_:
                cyclic.add(lambda_)
                break
            if target not in seen and target in plans:
                seen.add(target)
                pending.extend(plans[target].calls)
    for lambda_ in cyclic:
        del plans[lambda_]
    while True:
        invalid = {
            lambda_
            for lambda_, plan in plans.items()
            if any(target not in plans for target in plan.calls)
        }
        if not invalid:
            changed = True
            while changed:
                changed = False
                for lambda_, plan in plans.items():
                    for target in plan.calls:
                        for variable, required in plans[target].dependencies.items():
                            previous = plan.dependencies.get(variable)
                            if previous is not None and previous != required:
                                invalid.add(lambda_)
                            elif previous is None:
                                plan.dependencies[variable] = required
                                changed = True
        if not invalid:
            _propagate_native_results(plans)
            return plans
        for lambda_ in invalid:
            del plans[lambda_]


class _LiteralAliases(ast.Visitor):
    def visit_Node(self, node):
        return True

    def visit_IntegerLiteral(self, node):
        return not -(1 << 62) <= node.value < 1 << 62


_LITERAL_ALIASES = _LiteralAliases()


class _ResultAliases(NodeVisitor, LeafVisitor):
    """Whether a boxed result can forward a pre-existing object identity."""

    def __init__(self, plan, plans, nodes, variables):
        self.plan, self.plans = plan, plans
        self.nodes, self.variables = nodes, variables

    def lvar(self, value):
        return not value.uses or any(self.nodes.get(node, True) for node in value.uses)

    def visit_Node(self, node):
        return True

    def visit_Leaf(self, leaf):
        return True

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_Constant(self, leaf):
        return leaf.value.accept(_LITERAL_ALIASES)

    def visit_LambdaVar(self, leaf):
        if leaf in self.plan.function.vars:
            return True
        return self.variables.get(leaf, True)

    def visit_Cast(self, node):
        return self.lvar(node.value)

    def visit_Combination(self, node):
        if node.kind is not CallKind.KNOWN:
            return True
        name = linked(node.fun_info, node, "known operation").name
        if name == "%getattr":
            # Native fields store machine words, not boxed object identities.
            return False
        if name in _BINARY_NAMES:
            if name.startswith("%i"):
                return any(self.lvar(argument) for argument in node.args)
            return False
        if name in _UNARY_NAMES:
            return name == "%unary_positive" and any(
                self.lvar(argument) for argument in node.args
            )
        if name in ("%min2", "%max2", "%math_sqrt"):
            return False
        if name == "%call_resolved":
            target = self.plan.method_target(node.args[0])
            callee = self.plans.get(target)
            return callee is None or callee.result_may_alias
        return True


def _propagate_native_results(plans):
    nodes = {
        function: dict.fromkeys(plan.values, False) for function, plan in plans.items()
    }
    variables = {
        function: dict.fromkeys(plan.locals, False) for function, plan in plans.items()
    }
    for plan in plans.values():
        plan.result_may_alias = False
    changed = True
    while changed:
        changed = False
        for function, plan in plans.items():
            visitor = _ResultAliases(plan, plans, nodes[function], variables[function])
            for variable in plan.locals:
                sources = [
                    site.value for site in variable.sets if site.value is not None
                ]
                aliases = (
                    variable in function.vars
                    or not sources
                    or any(visitor.lvar(source) for source in sources)
                )
                if aliases and not variables[function][variable]:
                    variables[function][variable] = True
                    changed = True
            for node in plan.values:
                if not nodes[function][node] and node.accept(visitor):
                    nodes[function][node] = True
                    changed = True
            aliases = not plan.return_sources or any(
                visitor.lvar(source) for source in plan.return_sources
            )
            if aliases and not plan.result_may_alias:
                plan.result_may_alias = True
                changed = True


def _select_function(lambda_, index, facts, layouts, bindings, plans) -> NativePlan:
    if lambda_.environment is not None and lambda_.environment.closure:
        raise NotNative
    if any(var.indirect for var in lambda_.locals):
        raise NotNative
    signature = lambda_.signature
    if signature.variadic:
        raise NotNative
    parameters = []
    for ordinal, contract in enumerate(signature.annotations):
        if ordinal == 0 and lambda_.owner_class is not None:
            parameters.append(
                Instance(lambda_.owner_class, layouts[lambda_.owner_class])
            )
        elif contract is not None:
            parameters.append(contract.accept(_Contract()))
        else:
            raise NotNative
    if len(parameters) != len(lambda_.vars):
        raise NotNative
    plan = NativePlan(
        lambda_,
        index,
        tuple(parameters),
        dict(zip(lambda_.vars, parameters, strict=True)),
    )
    by_name = {var.name: var for var in lambda_.locals}
    for name, contract in lambda_.local_annotations.items():
        representation = contract.accept(_LocalContract())
        if representation is not None and name in by_name:
            plan.locals[by_name[name]] = representation
    blocks = list(lambda_.component.blocks())
    for block in blocks:
        if block_exception_target(block) is not None:
            raise NotNative
    selector = _Select(plan, layouts, bindings, plans)
    previous = None
    while True:
        deferred = False
        for block in blocks:
            selector.block = block
            for node in block.nodes():
                try:
                    result = node.accept(selector)
                    if result is not None:
                        plan.values[node] = result
                except Deferred:
                    deferred = True
        if not deferred:
            break
        state = (dict(plan.locals), dict(plan.values))
        if state == previous:
            break
        previous = state
    if deferred or plan.returned is None:
        raise NotNative
    proof = _MachineProof(plan, facts.types, _result_contracts(lambda_, blocks))
    for block in blocks:
        for node in block.nodes():
            node.accept(proof)
    return plan


class _MachineProof(NodeVisitor, LeafVisitor):
    """Reject a plan whose locals may be read unbound or whose integer
    arithmetic is not proven to stay within ``int64``."""

    def __init__(self, plan, types, constrained):
        self.plan, self.types, self.constrained = plan, types, constrained
        self.int64 = EXPORTS["int64"].fact

    def visit_Ref(self, node: Ref):
        if self.types.get(node, UNKNOWN).may_be_unbound:
            node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        raise NotNative

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN:
            return
        name = linked(node.fun_info, node, "known function").name
        arithmetic = name in _BINARY_NAMES and _BINARY_NAMES[name] not in {
            ast.BinaryOperator.BIT_AND,
            ast.BinaryOperator.BIT_OR,
            ast.BinaryOperator.BIT_XOR,
        }
        negation = name in _UNARY_NAMES and _UNARY_NAMES[name] != ast.UnaryOperator.NOT
        if (
            (arithmetic or negation)
            and self.plan.values.get(node) is not FLOAT
            and node not in self.constrained
            and not self.types.get(node, UNKNOWN).fits(self.int64)
        ):
            raise NotNative


class _Int64Operands(NodeVisitor):
    def __init__(self, constrained: set):
        self.constrained = constrained

    def visit_Cast(self, node: Cast):
        if node.contract is EXPORTS["int64"]:
            self.constrained.update(linked(node.value, node, "value").uses)


def _result_contracts(lambda_, blocks) -> set:
    """Producers whose value an ``int64`` contract constrains: cast operands
    and, under an implementation ``int64`` return constraint, returned values."""
    constrained = set()
    operands = _Int64Operands(constrained)
    for block in blocks:
        for node in block.nodes():
            node.accept(operands)
    if (
        lambda_.signature.implementation_return_annotation is EXPORTS["int64"]
        and lambda_.return_ is not None
    ):
        constrained.update(lambda_.return_.result.uses)
    return constrained


__all__ = [
    "BOOL",
    "FLOAT",
    "INT64",
    "NONE",
    "Boolean",
    "Class",
    "Float64",
    "Instance",
    "Int64",
    "Method",
    "NativePlan",
    "NotNative",
    "Nothing",
    "Representation",
    "assign_variable_primitive_types",
    "class_bindings",
    "select_native_plans",
    "select_representations",
    "select_tn_representation",
]

"""Scalar storage for fixed aggregates whose escape proof is already settled.

SBCL's envanal lifetime annotations select allocation consumers in IR2. The
Python adaptation replaces local list/tuple storage rather than inventing a
second runtime object layout. This pass only validates the consumer and owner
operations of the existing escape proof. It does not infer escaping values.
"""

from __future__ import annotations

from collections import defaultdict
from functools import cache

from mule import ast

from . import vops
from .ctype import (
    BOOLEAN,
    BOTTOM,
    NONE_TYPE,
    TEXT_TYPE,
    UNKNOWN,
    TupleType,
    ValueTypeVisitor,
)
from .ir2tran import LocalAccess, _BorrowedLifetime
from .ltn import primitive_type_of
from .vmdef import define_vop
from .vop import VOP, T, TNRef


@define_vop("scalar-element-store", args=(None,), results=(None,))
def _store_element(gen, vop):
    source = vop.args[0]
    value = gen.take(source)
    if source.tn.borrowed_from is not None:
        value = gen.retain(value)
    # Normal publication stores the new child before destroying the old one.
    gen.store(vop.results[0], value)


_STORE_ELEMENT = _store_element
_BUILDERS = frozenset({"build-list", "build-tuple", "protocol-getslice"})
_COPIES = frozenset({"move", "load-local"})
_READS = frozenset({"protocol-getitem", "unpack-ref"})


class _IntegerLiteral(ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_IntegerLiteral(self, node):
        return node.value


_INTEGER = _IntegerLiteral()


class _IndexBounds(ValueTypeVisitor):
    def visit_ValueType(self, value):
        return None

    def visit_IntegerType(self, value):
        if not value.is_fixnum:
            return None
        return value.minimum, value.maximum


_INDEX_BOUNDS = _IndexBounds()


@cache
def _projection_template(width, positions):
    @define_vop(
        "scalar-element-select",
        args=(vops.OBJECT,) * (len(positions) + 1),
        results=(vops.OBJECT,),
        # Any child may be forwarded through the projection's result. It is
        # not a private numeric box whose original identity can be discarded.
        arg_escapes=(False, *(True for _ in positions)),
    )
    def generate(gen, vop):
        index = gen.decode(gen.take(vop.args[0]))
        negative = gen.operation(f"icmp slt i64 {index}, 0")
        adjusted = gen.operation(f"add i64 {index}, {width}")
        index = gen.operation(f"select i1 {negative}, i64 {adjusted}, i64 {index}")
        labels = [gen.fresh("scalar.element")[1:] for _ in positions]
        joined = gen.fresh("scalar.selected")[1:]
        choices = " ".join(
            f"i64 {position}, label %{label}"
            for position, label in zip(positions[:-1], labels[:-1], strict=True)
        )
        # The range proof makes the remaining in-range element the default.
        # LLVM chooses the dispatch strategy instead of requiring a linear
        # comparison chain for every projection of a wide aggregate.
        gen.line(f"switch i64 {index}, label %{labels[-1]} [ {choices} ]")
        incoming = []
        for source, label in zip(vop.args[1:], labels, strict=True):
            gen.lines.append(label + ":")
            value = gen.load(source)
            incoming.append(f"[{value}, %{label}]")
            gen.line(f"br label %{joined}")
        gen.lines.append(joined + ":")
        value = gen.operation(f"phi ptr {', '.join(incoming)}")
        gen.record_facts(vop.results[0].tn, value)
        gen.publish(vop.results[0], gen.retain(value))

    return generate


@cache
def _contains_template(width, not_in: bool):
    @define_vop(
        "scalar-contains",
        args=(vops.OBJECT,) * (width + 1),
        results=(vops.OBJECT,),
        arg_escapes=(False,) * (width + 1),
    )
    def generate(gen, vop):
        needle_ref = vop.args[0]
        needle = gen.load(needle_ref)
        needle_type = needle_ref.tn.type or UNKNOWN
        elem_refs = vop.args[1:]
        elems = [gen.load(ref) for ref in elem_refs]
        combined = None
        for elem_ref, elem in zip(elem_refs, elems, strict=True):
            elem_type = elem_ref.tn.type or UNKNOWN
            if needle_type.is_fixnum and elem_type.is_fixnum:
                a, b = gen.decode(needle), gen.decode(elem)
                cond = gen.operation(f"icmp eq i64 {a}, {b}")
            elif (
                needle_type.fits_none(NONE_TYPE)
                or elem_type.fits_none(NONE_TYPE)
                or (
                    needle_type.fits_boolean(BOOLEAN)
                    and elem_type.fits_boolean(BOOLEAN)
                )
            ):
                cond = gen.operation(f"icmp eq ptr {needle}, {elem}")
            elif needle_type.fits_text(TEXT_TYPE) and elem_type.fits_text(TEXT_TYPE):
                if (
                    needle_type.singleton_text() is not None
                    or elem_type.singleton_text() is not None
                    or (needle_type.is_interned_text and elem_type.is_interned_text)
                ):
                    cond = gen.operation(f"icmp eq ptr {needle}, {elem}")
                else:
                    matched = gen.operation(
                        f"call i32 @mule_text_eq(ptr {needle}, ptr {elem})"
                    )
                    cond = gen.operation(f"icmp ne i32 {matched}, 0")
            else:
                cmp_res = gen.operation(
                    f"call ptr @mule.fast.compare.bool.0(ptr {needle}, ptr {elem})"
                )
                gen.check_pointer(cmp_res)
                cond = gen.operation(f"icmp eq ptr {cmp_res}, @mule.true")
            combined = (
                cond if combined is None else gen.operation(f"or i1 {combined}, {cond}")
            )
        if not_in:
            combined = gen.operation(f"xor i1 {combined}, true")
        boolean = gen.operation(
            f"select i1 {combined}, ptr @mule.true, ptr @mule.false"
        )
        gen.native_result(vop.results[0], boolean, (gen.take(needle_ref),))

    return generate


@cache
def _slice_compare_template(invert: bool):
    @define_vop(
        "scalar-slice-compare",
        args=(vops.OBJECT, vops.OBJECT, vops.OBJECT, vops.OBJECT),
        results=(vops.OBJECT,),
        arg_escapes=(False, False, False, False),
    )
    def generate(gen, vop):
        seq_ref, start_ref, stop_ref, target_ref = vop.args
        seq = gen.load(seq_ref)
        start = gen.load(start_ref)
        stop = gen.load(stop_ref)
        target = gen.take(target_ref)
        invert_flag = 1 if invert else 0
        res = gen.operation(
            f"call ptr @mule_text_slice_eq(ptr {seq}, ptr {start}, ptr {stop}, ptr {target}, i32 {invert_flag})"
        )
        gen.check_pointer(res)
        gen.native_result(vop.results[0], res, (target,))

    return generate


def _index(tn, writes):
    definitions = writes.get(tn, ())
    if len(definitions) != 1:
        return None
    writer, _ = definitions[0]
    if writer.info is not vops.CONSTANT:
        return None
    return writer.codegen_info.accept(_INTEGER)


def _operation(info, node, args=(), results=(), data=None):
    result = VOP(
        info,
        node,
        [TNRef(tn, False) for tn in args],
        [TNRef(tn, True) for tn in results],
    )
    result.codegen_info = data
    return result


def _constant(vop, literal):
    return _operation(
        vops.CONSTANT, vop.node, results=(vop.results[0].tn,), data=literal
    )


def _store_op(node, source_tn, slot_tn, *, release=True):
    if source_tn.borrowed_from is not None:
        return _operation(_STORE_ELEMENT, node, (source_tn,), (slot_tn,))
    return _operation(
        vops.MOVE,
        node,
        (source_tn,),
        (slot_tn,),
        data=None if release else False,
    )


def _named_owner(tn):
    return (
        tn.leaf is not None and not tn.leaf.expression_temporary and not tn.leaf.hidden
    )


def _infallible(vop):
    if vop.info.name in ("constant", "move", "discard"):
        return True
    return vop.info is vops.LOAD_LOCAL and vop.codegen_info.bound


class _Family:
    """Copy-connected aggregate locations; each block owns a closed lifetime.

    Different lowered range paths can reuse the same source locals. Each path
    must define its own aggregate before reading them and consume every owner
    before its outgoing edges. No object can survive a loop backedge.
    """

    def __init__(self, component, members):
        self.component = component
        self.members = members
        self.origins = {
            writer
            for tn in members
            for writer, _ in component.writes.get(tn, ())
            if writer.info.name in _BUILDERS
        }

    def eligible(self):
        facts = self.component.context.escape_facts
        if not self.origins or any(
            facts.allocation_escapes(vop.node) for vop in self.origins
        ):
            return False
        shape = {(vop.info.name, len(vop.args)) for vop in self.origins}
        if len(shape) != 1:
            return False
        origin_kind, origin_arity = next(iter(shape))
        self.is_text_slice = origin_kind == "protocol-getslice"
        self.borrow_slice_seq = False
        if self.is_text_slice:
            if origin_arity != 4:
                return False
            for origin in self.origins:
                seq_type = origin.args[0].tn.type or UNKNOWN
                start_type = origin.args[1].tn.type or UNKNOWN
                stop_type = origin.args[2].tn.type or UNKNOWN
                step_type = origin.args[3].tn.type or UNKNOWN
                if (
                    not seq_type.fits_text(TEXT_TYPE)
                    or not (start_type.is_fixnum or start_type.fits_none(NONE_TYPE))
                    or not (stop_type.is_fixnum or stop_type.fits_none(NONE_TYPE))
                    or not (
                        step_type.fits_none(NONE_TYPE)
                        or _index(origin.args[3].tn, self.component.writes) == 1
                    )
                ):
                    return False
            self.mutable = False
            self.width = 3
            compare_readers = []
            for tn in self.members:
                for writer, _ in self.component.writes.get(tn, ()):
                    if writer.info.name not in _BUILDERS | _COPIES | {"unbind-local"}:
                        return False
                for reader, ref in self.component.reads.get(tn, ()):
                    if reader.node in self.component.context.element_contracts:
                        return False
                    name = reader.info.name
                    if name in _COPIES | {"discard"}:
                        if reader.args[0] is not ref:
                            return False
                    elif name in ("compare-equal", "compare-not_equal"):
                        if len(reader.args) != 2:
                            return False
                        other = (
                            reader.args[1].tn
                            if reader.args[0] is ref
                            else reader.args[0].tn
                        )
                        if other in self.members or not (
                            other.type or UNKNOWN
                        ).fits_text(TEXT_TYPE):
                            return False
                        compare_readers.append(reader)
                    else:
                        return False
            if not compare_readers:
                return False
            if len(self.origins) == 1:
                origin = next(iter(self.origins))
                seq_tn = origin.args[0].tn
                seq_writes = self.component.writes.get(seq_tn, ())
                if seq_tn.borrowed_from is not None and len(seq_writes) == 1:
                    seq_writer = seq_writes[0][0]
                    local_source = (
                        None if seq_tn.borrowed_from is seq_tn else seq_tn.borrowed_from
                    )
                    if _BorrowedLifetime(self.component).protected(
                        seq_writer, compare_readers, local_source
                    ):
                        self.borrow_slice_seq = True
            return True
        self.mutable = origin_kind == "build-list"
        self.width = origin_arity
        for tn in self.members:
            for writer, _ in self.component.writes.get(tn, ()):
                if writer.info.name not in _BUILDERS | _COPIES | {
                    "structural-unpack",
                    "unbind-local",
                }:
                    return False
            for reader, ref in self.component.reads.get(tn, ()):
                if reader.node in self.component.context.element_contracts:
                    # Projection checks must retain their ordinary publication
                    # and failure-cleanup behavior.
                    return False
                name = reader.info.name
                if name == "structural-unpack":
                    if (
                        reader.args[0] is not ref
                        or _index(reader.args[1].tn, self.component.writes)
                        != self.width
                    ):
                        return False
                elif (
                    name
                    in _COPIES | _READS | {"discard", "fast-len", "unary-not", "branch"}
                    or name == "protocol-setitem"
                    and self.mutable
                ):
                    if reader.args[0] is not ref:
                        return False
                elif name in ("compare-is", "compare-is_not"):
                    if self._disjoint_identity_operand(reader, ref) is None:
                        return False
                elif name in ("compare-in", "compare-not_in"):
                    if (
                        len(reader.args) != 2
                        or reader.args[1] is not ref
                        or reader.args[0].tn in self.members
                        or not (1 <= self.width <= 8)
                        or not (
                            reader.args[0].tn.type or UNKNOWN
                        ).has_reflexive_equality()
                    ):
                        return False
                else:
                    return False
        return True

    def _disjoint_identity_operand(self, reader, ref):
        if len(reader.args) != 2:
            return None
        if reader.args[0] is ref and reader.args[1].tn not in self.members:
            other = reader.args[1].tn
        elif reader.args[1] is ref and reader.args[0].tn not in self.members:
            other = reader.args[0].tn
        else:
            return None
        other_type = other.type or UNKNOWN
        if self.mutable:
            if other_type.fits_possible_list():
                return None
        else:
            if other_type.fits_possible_tuple(TupleType(self.width)):
                return None
        return other

    def projection(self, vop, slots):
        index = vop.args[1].tn
        bounds = (index.type or UNKNOWN).accept(_INDEX_BOUNDS)
        if (
            vop.info.name != "protocol-getitem"
            or not self.width
            or bounds is None
            or not -self.width <= bounds[0] <= bounds[1] < self.width
        ):
            return None
        positions = tuple(
            sorted(
                {
                    position + self.width if position < 0 else position
                    for position in range(bounds[0], bounds[1] + 1)
                }
            )
        )
        return _operation(
            _projection_template(self.width, positions),
            vop.node,
            (index, *(slots[position] for position in positions)),
            (vop.results[0].tn,),
            data=positions,
        )

    def plan(self, block):
        relevant = [
            vop
            for vop in block.vops
            if any(ref.tn in self.members for ref in (*vop.args, *vop.results))
        ]
        if not relevant:
            return {}
        if not any(vop in self.origins for vop in relevant):
            return None
        # Scalar storage follows one dynamic allocation and never overlaps an
        # older allocation from the same source site.
        owners, valid, initialized = set(), set(), set()
        replacements = {}
        slots = []
        last_read: list[LocalAccess | None] = []
        children_inert = False
        order = {tn: index for index, tn in enumerate(self.component.tns)}
        for vop in block.vops:
            related = vop in relevant
            if not related:
                if (
                    owners
                    and not any(_named_owner(tn) for tn in owners)
                    and not _infallible(vop)
                    and (
                        not children_inert
                        or any(slot.cleanup_owner is not None for slot in slots)
                    )
                ):
                    return None
                if owners and not children_inert and not _infallible(vop):
                    # A handler can resume with the old aggregate still alive.
                    # This pass only closes lifetimes within the normal block.
                    if block.exception_target is not None:
                        return None
                    named_owners = [tn for tn in owners if _named_owner(tn)]
                    if not named_owners:
                        return None
                    anchor = max(named_owners, key=order.__getitem__)
                    if any(
                        slot.cleanup_owner is not None
                        and slot.cleanup_owner is not anchor
                        for slot in slots
                    ):
                        return None
                    for slot in slots:
                        slot.cleanup_owner = anchor
                continue
            name = vop.info.name
            rewritten = []
            trailing_terminator = None
            if vop in self.origins:
                if owners:
                    return None
                origin_args = vop.args[:3] if self.is_text_slice else vop.args
                slots = [
                    self.component.tn(
                        ref.tn.primitive_type,
                        name=f"$scalar.{vop.number}.{index}",
                    )
                    for index, ref in enumerate(origin_args)
                ]
                for ref, slot in zip(origin_args, slots, strict=True):
                    slot.type = ref.tn.type or UNKNOWN
                if self.is_text_slice and self.borrow_slice_seq:
                    slots[0].borrowed_from = origin_args[0].tn.borrowed_from
                last_read = [None] * self.width
                children_inert = all(
                    not (ref.tn.type or UNKNOWN).may_run_user_code
                    for ref in origin_args
                )
                rewritten = [
                    _operation(vops.MOVE, vop.node, (ref.tn,), (slot,), data=False)
                    if self.is_text_slice and self.borrow_slice_seq and slot_idx == 0
                    else _store_op(vop.node, ref.tn, slot, release=False)
                    for slot_idx, (ref, slot) in enumerate(
                        zip(origin_args, slots, strict=True)
                    )
                ]
                if self.is_text_slice and vop.args[3].tn.borrowed_from is None:
                    rewritten.append(
                        _operation(vops.DISCARD, vop.node, (vop.args[3].tn,))
                    )
                owners.add(vop.results[0].tn)
                valid.add(vop.results[0].tn)
                initialized = {vop.results[0].tn}
            elif name == "unbind-local":
                target = vop.results[0].tn
                # A last-read move can clear the binding before the explicit
                # deletion VOP. It must still have been initialized here.
                if target not in initialized:
                    return None
                owners.discard(target)
                valid.discard(target)
            elif name in ("compare-equal", "compare-not_equal") and self.is_text_slice:
                ref = vop.args[0] if vop.args[0].tn in self.members else vop.args[1]
                other = vop.args[1].tn if ref is vop.args[0] else vop.args[0].tn
                source = ref.tn
                if (
                    source not in valid
                    or not owners
                    or other in self.members
                    or not (other.type or UNKNOWN).fits_text(TEXT_TYPE)
                ):
                    return None
                last_read = [None] * self.width
                rewritten.append(
                    _operation(
                        _slice_compare_template(name == "compare-not_equal"),
                        vop.node,
                        (*slots, other),
                        (vop.results[0].tn,),
                    )
                )
                if source.borrowed_from is None:
                    owners.discard(source)
                    valid.discard(source)
            elif name in ("compare-is", "compare-is_not"):
                ref = vop.args[0] if vop.args[0].tn in self.members else vop.args[1]
                source = ref.tn
                other = self._disjoint_identity_operand(vop, ref)
                if other is None or source not in valid or not owners:
                    return None
                if other.borrowed_from is None:
                    rewritten.append(_operation(vops.DISCARD, vop.node, (other,)))
                rewritten.append(
                    _constant(vop, ast.BooleanLiteral(name == "compare-is_not"))
                )
                if source.borrowed_from is None:
                    owners.discard(source)
                    valid.discard(source)
            elif name in ("compare-in", "compare-not_in"):
                needle = vop.args[0].tn
                source = vop.args[1].tn
                if (
                    source not in valid
                    or not owners
                    or not (1 <= self.width <= 8)
                    or not (needle.type or UNKNOWN).has_reflexive_equality()
                    or not all(
                        (slot.type or UNKNOWN).has_reflexive_equality()
                        for slot in slots
                    )
                ):
                    return None
                last_read = [None] * self.width
                rewritten.append(
                    _operation(
                        _contains_template(self.width, name == "compare-not_in"),
                        vop.node,
                        (needle, *slots),
                        (vop.results[0].tn,),
                    )
                )
                if source.borrowed_from is None:
                    owners.discard(source)
                    valid.discard(source)
            else:
                source = vop.args[0].tn
                if source not in valid or not owners:
                    return None
                if name in _COPIES:
                    target = vop.results[0].tn
                    if name == "load-local":
                        access = vop.codegen_info
                        if not access.bound or access.allow_unbound:
                            return None
                        if access.move:
                            owners.discard(source)
                            valid.discard(source)
                        if not access.borrow and target.borrowed_from is None:
                            owners.add(target)
                    else:
                        owners.discard(target)
                        if source.borrowed_from is None:
                            owners.discard(source)
                            valid.discard(source)
                        owners.add(target)
                    valid.add(target)
                    initialized.add(target)
                elif name in _READS | {"protocol-setitem"}:
                    position = _index(vop.args[1].tn, self.component.writes)
                    if position is None:
                        projection = self.projection(vop, slots)
                        if projection is None:
                            return None
                        rewritten.append(projection)
                        # Any element may be read again, so earlier reads may
                        # no longer consume the scalar slot's last ownership.
                        for selected in projection.codegen_info:
                            last_read[selected] = None
                    elif name == "protocol-setitem":
                        position = position + self.width if position < 0 else position
                        if not 0 <= position < self.width:
                            return None
                        slot = slots[position]
                        value = vop.args[2].tn
                        children_inert = (
                            children_inert
                            and not (value.type or UNKNOWN).may_run_user_code
                        )
                        slot.type = (slot.type or UNKNOWN).join(value.type or UNKNOWN)
                        last_read[position] = None
                        rewritten.append(_store_op(vop.node, value, slot, release=True))
                        rewritten.append(_constant(vop, ast.NoneLiteral(None)))
                    else:
                        position = position + self.width if position < 0 else position
                        if not 0 <= position < self.width:
                            return None
                        slot = slots[position]
                        target = vop.results[0].tn
                        access = LocalAccess(slot.name, False, bound=True)
                        read_vop = _operation(
                            vops.LOAD_LOCAL,
                            vop.node,
                            (slot,),
                            (target,),
                            access,
                        )
                        last_read[position] = read_vop
                        rewritten.append(read_vop)
                    borrow_parent = (
                        name == "unpack-ref" and vop.codegen_info.borrow_parent
                    )
                    if not borrow_parent and source.borrowed_from is None:
                        owners.discard(source)
                        valid.discard(source)
                elif name == "fast-len":
                    rewritten.append(_constant(vop, ast.IntegerLiteral(self.width)))
                    if source.borrowed_from is None:
                        owners.discard(source)
                        valid.discard(source)
                elif name == "unary-not":
                    rewritten.append(
                        _constant(vop, ast.BooleanLiteral(not bool(self.width)))
                    )
                    if source.borrowed_from is None:
                        owners.discard(source)
                        valid.discard(source)
                elif name == "branch":
                    cond_tn = self.component.tn(T, name=f"$scalar.branch.{vop.number}")
                    cond_tn.type = BOOLEAN
                    rewritten.append(
                        _operation(
                            vops.CONSTANT,
                            vop.node,
                            results=(cond_tn,),
                            data=ast.BooleanLiteral(bool(self.width)),
                        )
                    )
                    trailing_terminator = _operation(
                        vops.BRANCH,
                        vop.node,
                        (cond_tn,),
                        (),
                        data=vop.codegen_info,
                    )
                    if source.borrowed_from is None:
                        owners.discard(source)
                        valid.discard(source)
                elif name == "discard":
                    if source.borrowed_from is None:
                        owners.discard(source)
                        valid.discard(source)
                else:
                    return None
            if not owners:
                # Match tuple/list child teardown on both last reads and
                # explicit deletion of the last alias.
                for index in reversed(range(self.width)):
                    slot = slots[index]
                    if self.is_text_slice and self.borrow_slice_seq and index == 0:
                        continue
                    if children_inert and last_read[index] is not None:
                        last_read[index].codegen_info = LocalAccess(
                            slot.name, True, bound=True
                        )
                    else:
                        rewritten.append(
                            _operation(vops.UNBIND_LOCAL, vop.node, results=(slot,))
                        )
                last_read = [None] * self.width
                valid.clear()
            if trailing_terminator is not None:
                rewritten.append(trailing_terminator)
            replacements[vop] = rewritten
        return replacements if not owners else None

    def plan_component(self):
        if not self.component.blocks:
            return None
        children_inert = not any(
            (ref.tn.type or UNKNOWN).may_run_user_code
            for origin in self.origins
            for ref in (origin.args[:3] if self.is_text_slice else origin.args)
        )
        anchor_id = min(tn.number for tn in self.members)
        slots = [
            self.component.tn(
                T,
                name=f"$scalar.shared.{anchor_id}.{index}",
            )
            for index in range(self.width)
        ]
        if self.is_text_slice and self.borrow_slice_seq:
            slots[0].borrowed_from = next(iter(self.origins)).args[0].tn.borrowed_from
        order = {tn: index for index, tn in enumerate(self.component.tns)}
        ir2_blocks = self.component.ir2_blocks
        succs = {
            block: [ir2_blocks[succ] for succ in block.block.succ if succ in ir2_blocks]
            for block in self.component.blocks
        }
        block_gen = {}
        block_kill = {}
        for block in self.component.blocks:
            gen = set()
            kill = set()
            for vop in reversed(block.vops):
                if vop.info.name == "unbind-local":
                    target = vop.results[0].tn
                    if target in self.members:
                        gen.add(target)
                        kill.add(target)
                    continue
                for ref in vop.results:
                    if ref.tn in self.members:
                        gen.discard(ref.tn)
                        kill.add(ref.tn)
                for ref in vop.args:
                    if ref.tn in self.members:
                        gen.add(ref.tn)
            block_gen[block] = gen
            block_kill[block] = kill
        member_live_in = {
            block: set(block_gen[block]) for block in self.component.blocks
        }
        changed = True
        while changed:
            changed = False
            for block in reversed(self.component.blocks):
                out = set()
                for succ in succs[block]:
                    out |= member_live_in[succ]
                incoming = block_gen[block] | (out - block_kill[block])
                if incoming != member_live_in[block]:
                    member_live_in[block] = incoming
                    changed = True

        entry = self.component.blocks[0]
        in_owners = {entry: frozenset()}
        in_init = {entry: frozenset()}
        block_replacements = {}
        block_slot_types = {}
        branch_tns = {}
        uses_contains = False
        worklist = [entry]

        while worklist:
            block = worklist.pop(0)
            owners = set(in_owners[block])
            valid = set(owners)
            initialized = set(in_init[block])
            local_slot_types = [BOTTOM] * self.width
            local_replacements = {}
            positions = {vop: index for index, vop in enumerate(block.vops)}
            last_uses = defaultdict(list)
            for tn in self.members:
                readers = {reader for reader, _ in self.component.reads.get(tn, ())}
                if readers and readers <= positions.keys():
                    last_uses[max(readers, key=positions.__getitem__)].append(tn)
            pending_origin_target = None
            for index, vop in enumerate(block.vops):
                related = any(
                    ref.tn in self.members for ref in (*vop.args, *vop.results)
                )
                if not related:
                    if pending_origin_target is not None:
                        return None
                    if (
                        owners
                        and not any(_named_owner(tn) for tn in owners)
                        and not _infallible(vop)
                        and (
                            not children_inert
                            or any(slot.cleanup_owner is not None for slot in slots)
                        )
                    ):
                        return None
                    if owners and not children_inert and not _infallible(vop):
                        if block.exception_target is not None:
                            return None
                        named_owners = [tn for tn in owners if _named_owner(tn)]
                        if not named_owners:
                            return None
                        anchor = max(named_owners, key=order.__getitem__)
                        if any(
                            slot.cleanup_owner is not None
                            and slot.cleanup_owner is not anchor
                            for slot in slots
                        ):
                            return None
                        for slot in slots:
                            slot.cleanup_owner = anchor
                    continue
                name = vop.info.name
                rewritten = []
                trailing_terminator = None
                was_owned = bool(owners)
                if vop in self.origins:
                    if pending_origin_target is not None or valid != owners:
                        return None
                    res_tn = vop.results[0].tn
                    if owners:
                        if not children_inert or len(owners) != 1:
                            return None
                        sole_owner = next(iter(owners))
                        if res_tn is not sole_owner:
                            if (
                                index + 1 >= len(block.vops)
                                or block.vops[index + 1].info.name != "move"
                                or block.vops[index + 1].args[0].tn is not res_tn
                                or block.vops[index + 1].results[0].tn is not sole_owner
                            ):
                                return None
                            pending_origin_target = sole_owner
                    origin_args = vop.args[:3] if self.is_text_slice else vop.args
                    for slot_idx, (ref, slot) in enumerate(
                        zip(origin_args, slots, strict=True)
                    ):
                        local_slot_types[slot_idx] = local_slot_types[slot_idx].join(
                            ref.tn.type or UNKNOWN
                        )
                        if (
                            self.is_text_slice
                            and self.borrow_slice_seq
                            and slot_idx == 0
                        ):
                            rewritten.append(
                                _operation(
                                    vops.MOVE, vop.node, (ref.tn,), (slot,), data=False
                                )
                            )
                        else:
                            rewritten.append(
                                _store_op(vop.node, ref.tn, slot, release=True)
                            )
                    if self.is_text_slice and vop.args[3].tn.borrowed_from is None:
                        rewritten.append(
                            _operation(vops.DISCARD, vop.node, (vop.args[3].tn,))
                        )
                    owners.add(res_tn)
                    valid.add(res_tn)
                    initialized.add(res_tn)
                elif pending_origin_target is not None:
                    if (
                        name != "move"
                        or vop.args[0].tn not in valid
                        or vop.results[0].tn is not pending_origin_target
                    ):
                        return None
                    source = vop.args[0].tn
                    if source is not pending_origin_target:
                        owners.discard(source)
                        valid.discard(source)
                    owners.add(pending_origin_target)
                    valid.add(pending_origin_target)
                    initialized.add(pending_origin_target)
                    pending_origin_target = None
                elif name == "unbind-local":
                    target = vop.results[0].tn
                    if target not in initialized:
                        return None
                    owners.discard(target)
                    valid.discard(target)
                    initialized.discard(target)
                elif (
                    name in ("compare-equal", "compare-not_equal")
                    and self.is_text_slice
                ):
                    ref = vop.args[0] if vop.args[0].tn in self.members else vop.args[1]
                    other = vop.args[1].tn if ref is vop.args[0] else vop.args[0].tn
                    source = ref.tn
                    if (
                        source not in valid
                        or not owners
                        or other in self.members
                        or not (other.type or UNKNOWN).fits_text(TEXT_TYPE)
                    ):
                        return None
                    rewritten.append(
                        _operation(
                            _slice_compare_template(name == "compare-not_equal"),
                            vop.node,
                            (*slots, other),
                            (vop.results[0].tn,),
                        )
                    )
                    if source.borrowed_from is None:
                        owners.discard(source)
                        valid.discard(source)
                elif name in ("compare-is", "compare-is_not"):
                    ref = vop.args[0] if vop.args[0].tn in self.members else vop.args[1]
                    source = ref.tn
                    other = self._disjoint_identity_operand(vop, ref)
                    if other is None or source not in valid or not owners:
                        return None
                    if other.borrowed_from is None:
                        rewritten.append(_operation(vops.DISCARD, vop.node, (other,)))
                    rewritten.append(
                        _constant(vop, ast.BooleanLiteral(name == "compare-is_not"))
                    )
                    if source.borrowed_from is None:
                        owners.discard(source)
                        valid.discard(source)
                elif name in ("compare-in", "compare-not_in"):
                    needle = vop.args[0].tn
                    source = vop.args[1].tn
                    if (
                        source not in valid
                        or not owners
                        or not (1 <= self.width <= 8)
                        or not (needle.type or UNKNOWN).has_reflexive_equality()
                    ):
                        return None
                    uses_contains = True
                    rewritten.append(
                        _operation(
                            _contains_template(self.width, name == "compare-not_in"),
                            vop.node,
                            (needle, *slots),
                            (vop.results[0].tn,),
                        )
                    )
                    if source.borrowed_from is None:
                        owners.discard(source)
                        valid.discard(source)
                else:
                    source = vop.args[0].tn
                    if source not in valid or not owners:
                        return None
                    if name == "load-local":
                        access = vop.codegen_info
                        target = vop.results[0].tn
                        if not access.bound or access.allow_unbound:
                            return None
                        if access.move:
                            owners.discard(source)
                            valid.discard(source)
                        if not access.borrow and target.borrowed_from is None:
                            owners.add(target)
                        if target in owners or self.component.reads.get(target):
                            valid.add(target)
                        initialized.add(target)
                    elif name in ("move", "structural-unpack"):
                        if name == "structural-unpack" and not children_inert:
                            return None
                        target = vop.results[0].tn
                        owners.discard(target)
                        if source.borrowed_from is None:
                            owners.discard(source)
                            valid.discard(source)
                        if target.borrowed_from is None:
                            owners.add(target)
                        if target in owners or self.component.reads.get(target):
                            valid.add(target)
                        initialized.add(target)
                    elif name in _READS | {"protocol-setitem"}:
                        position = _index(vop.args[1].tn, self.component.writes)
                        if position is None:
                            projection = self.projection(vop, slots)
                            if projection is None:
                                return None
                            rewritten.append(projection)
                        elif name == "protocol-setitem":
                            position = (
                                position + self.width if position < 0 else position
                            )
                            if not 0 <= position < self.width:
                                return None
                            slot = slots[position]
                            value = vop.args[2].tn
                            if (value.type or UNKNOWN).may_run_user_code:
                                children_inert = False
                                return None
                            local_slot_types[position] = local_slot_types[
                                position
                            ].join(value.type or UNKNOWN)
                            rewritten.append(
                                _store_op(vop.node, value, slot, release=True)
                            )
                            rewritten.append(_constant(vop, ast.NoneLiteral(None)))
                        else:
                            position = (
                                position + self.width if position < 0 else position
                            )
                            if not 0 <= position < self.width:
                                return None
                            slot = slots[position]
                            target = vop.results[0].tn
                            rewritten.append(
                                _operation(
                                    vops.LOAD_LOCAL,
                                    vop.node,
                                    (slot,),
                                    (target,),
                                    LocalAccess(slot.name, False, bound=True),
                                )
                            )
                        borrow_parent = (
                            name == "unpack-ref" and vop.codegen_info.borrow_parent
                        )
                        if not borrow_parent and source.borrowed_from is None:
                            owners.discard(source)
                            valid.discard(source)
                    elif name == "fast-len":
                        rewritten.append(_constant(vop, ast.IntegerLiteral(self.width)))
                        if source.borrowed_from is None:
                            owners.discard(source)
                            valid.discard(source)
                    elif name == "unary-not":
                        rewritten.append(
                            _constant(vop, ast.BooleanLiteral(not bool(self.width)))
                        )
                        if source.borrowed_from is None:
                            owners.discard(source)
                            valid.discard(source)
                    elif name == "branch":
                        cond_tn = branch_tns.get(vop)
                        if cond_tn is None:
                            cond_tn = self.component.tn(
                                T, name=f"$scalar.branch.{vop.number}"
                            )
                            cond_tn.type = BOOLEAN
                            branch_tns[vop] = cond_tn
                        rewritten.append(
                            _operation(
                                vops.CONSTANT,
                                vop.node,
                                results=(cond_tn,),
                                data=ast.BooleanLiteral(bool(self.width)),
                            )
                        )
                        trailing_terminator = _operation(
                            vops.BRANCH,
                            vop.node,
                            (cond_tn,),
                            (),
                            data=vop.codegen_info,
                        )
                        if source.borrowed_from is None:
                            owners.discard(source)
                            valid.discard(source)
                    elif name == "discard":
                        if source.borrowed_from is None:
                            owners.discard(source)
                            valid.discard(source)
                    else:
                        return None
                for tn in last_uses.get(vop, ()):
                    if tn not in owners:
                        valid.discard(tn)
                if (
                    was_owned
                    and not owners
                    and (not children_inert or name == "unbind-local")
                ):
                    for slot_idx, slot in reversed(list(enumerate(slots))):
                        if (
                            self.is_text_slice
                            and self.borrow_slice_seq
                            and slot_idx == 0
                        ):
                            continue
                        rewritten.append(
                            _operation(vops.UNBIND_LOCAL, vop.node, results=(slot,))
                        )
                if trailing_terminator is not None:
                    rewritten.append(trailing_terminator)
                local_replacements[vop] = rewritten
            if pending_origin_target is not None or valid != owners:
                return None
            if not succs[block] and not children_inert and owners:
                return None
            block_replacements[block] = local_replacements
            block_slot_types[block] = local_slot_types
            for succ in succs[block]:
                edge_owners = (
                    frozenset(owners & member_live_in[succ])
                    if children_inert
                    else frozenset(owners)
                )
                edge_init = frozenset(initialized)
                if succ not in in_owners:
                    in_owners[succ] = edge_owners
                    in_init[succ] = edge_init
                    worklist.append(succ)
                else:
                    if in_owners[succ] != edge_owners:
                        return None
                    merged_init = in_init[succ] & edge_init
                    if merged_init != in_init[succ]:
                        in_init[succ] = merged_init
                        if succ not in worklist:
                            worklist.append(succ)

        for block in self.component.blocks:
            if block not in block_replacements and any(
                ref.tn in self.members
                for vop in block.vops
                for ref in (*vop.args, *vop.results)
            ):
                return None
        slot_types = [BOTTOM] * self.width
        for local_types in block_slot_types.values():
            for idx, stype in enumerate(local_types):
                slot_types[idx] = slot_types[idx].join(stype)
        if uses_contains and not all(
            stype is not BOTTOM and stype.has_reflexive_equality()
            for stype in slot_types
        ):
            return None
        for slot, stype in zip(slots, slot_types, strict=True):
            slot.type = UNKNOWN if stype is BOTTOM else stype
            slot.primitive_type = primitive_type_of(slot.type)
        replacements = {}
        for local_repls in block_replacements.values():
            replacements.update(local_repls)
        return replacements


def _copy_groups(component):
    edges = defaultdict(set)
    for block in component.blocks:
        for vop in block.vops:
            if (
                vop.info.name in _COPIES and len(vop.args) == len(vop.results) == 1
            ) or (
                vop.info.name == "structural-unpack"
                and len(vop.args) == 2
                and len(vop.results) == 1
            ):
                source, target = vop.args[0].tn, vop.results[0].tn
                edges[source].add(target)
                edges[target].add(source)
    visited = set()
    for block in component.blocks:
        for vop in block.vops:
            if vop.info.name not in _BUILDERS:
                continue
            root = vop.results[0].tn
            if root in visited:
                continue
            members, pending = set(), [root]
            while pending:
                tn = pending.pop()
                if tn in members:
                    continue
                members.add(tn)
                pending.extend(edges[tn])
            visited.update(members)
            yield members


def replace_local_aggregates(component):
    """Consume allocation escape facts before representation/ownership analysis."""
    if component.context.plan is not None:
        return
    rewritten = {}
    for members in _copy_groups(component):
        family = _Family(component, members)
        if not family.eligible():
            continue
        planned = []
        original_tns = len(component.tns)
        for block in component.blocks:
            replacement = family.plan(block)
            if replacement is None:
                del component.tns[original_tns:]
                component_replacement = family.plan_component()
                if component_replacement is None:
                    del component.tns[original_tns:]
                else:
                    rewritten.update(component_replacement)
                break
            planned.append(replacement)
        else:
            for replacement in planned:
                rewritten.update(replacement)
    if not rewritten:
        return
    for block in component.blocks:
        block.vops = [item for vop in block.vops for item in rewritten.get(vop, (vop,))]
    # A projection is now an independent owned scalar read, not a borrow from
    # the removed container. Its original type facts remain authoritative.
    for replacement in rewritten.values():
        for vop in replacement:
            if vop.info.name in ("load-local", "scalar-element-select"):
                for ref in vop.results:
                    ref.tn.borrowed_from = None
    component.reads.clear()
    component.writes.clear()
    for block in component.blocks:
        for vop in block.vops:
            for ref in vop.args:
                component.reads[ref.tn].append((vop, ref))
            for ref in vop.results:
                component.writes[ref.tn].append((vop, ref))

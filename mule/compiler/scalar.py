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
from .ctype import BOTTOM, UNKNOWN, ValueTypeVisitor
from .ir1util import block_exception_target
from .ir2tran import LocalAccess
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
_BUILDERS = frozenset({"build-list", "build-tuple"})
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
        self.mutable = next(iter(shape))[0] == "build-list"
        self.width = next(iter(shape))[1]
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
                    name in _COPIES | _READS | {"discard", "fast-len"}
                    or name == "protocol-setitem"
                    and self.mutable
                ):
                    if reader.args[0] is not ref:
                        return False
                else:
                    return False
        return True

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
                    if block_exception_target(block.block) is not None:
                        return None
                    if not all(_named_owner(tn) for tn in owners):
                        return None
                    anchor = max(owners, key=order.__getitem__)
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
            if vop in self.origins:
                if owners:
                    return None
                slots = [
                    self.component.tn(
                        ref.tn.primitive_type,
                        name=f"$scalar.{vop.number}.{index}",
                    )
                    for index, ref in enumerate(vop.args)
                ]
                for ref, slot in zip(vop.args, slots, strict=True):
                    slot.type = ref.tn.type or UNKNOWN
                last_read = [None] * self.width
                children_inert = all(
                    not (ref.tn.type or UNKNOWN).may_run_user_code for ref in vop.args
                )
                rewritten = [
                    _store_op(vop.node, ref.tn, slot, release=False)
                    for ref, slot in zip(vop.args, slots, strict=True)
                ]
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
            replacements[vop] = rewritten
        return replacements if not owners else None

    def plan_component(self):
        named = [tn for tn in self.members if _named_owner(tn)]
        if len(named) != 1:
            return None
        owner_tn = named[0]
        if any(
            (ref.tn.type or UNKNOWN).may_run_user_code
            for origin in self.origins
            for ref in origin.args
        ):
            return None
        slots = [
            self.component.tn(
                T,
                name=f"$scalar.shared.{owner_tn.number}.{index}",
            )
            for index in range(self.width)
        ]
        slot_types = [BOTTOM] * self.width
        replacements = {}
        for block in self.component.blocks:
            positions = {vop: index for index, vop in enumerate(block.vops)}
            last_uses = defaultdict(list)
            for tn in self.members:
                if tn.borrowed_from is None:
                    continue
                readers = {reader for reader, _ in self.component.reads.get(tn, ())}
                if readers and readers <= positions.keys():
                    last_uses[max(readers, key=positions.__getitem__)].append(tn)
            pending_origin = None
            active_temps = set()
            for vop in block.vops:
                related = any(
                    ref.tn in self.members for ref in (*vop.args, *vop.results)
                )
                if not related:
                    if pending_origin is not None:
                        return None
                    continue
                name = vop.info.name
                rewritten = []
                if vop in self.origins:
                    if pending_origin is not None or active_temps:
                        return None
                    res_tn = vop.results[0].tn
                    if res_tn is not owner_tn:
                        pending_origin = res_tn
                    for index, (ref, slot) in enumerate(
                        zip(vop.args, slots, strict=True)
                    ):
                        slot_types[index] = slot_types[index].join(
                            ref.tn.type or UNKNOWN
                        )
                        rewritten.append(
                            _store_op(vop.node, ref.tn, slot, release=True)
                        )
                elif pending_origin is not None:
                    if (
                        name != "move"
                        or vop.args[0].tn is not pending_origin
                        or vop.results[0].tn is not owner_tn
                    ):
                        return None
                    pending_origin = None
                elif name == "unbind-local":
                    return None
                else:
                    source = vop.args[0].tn
                    if source is not owner_tn and source not in active_temps:
                        return None
                    if name == "load-local":
                        access = vop.codegen_info
                        target = vop.results[0].tn
                        if (
                            not access.bound
                            or access.allow_unbound
                            or target is owner_tn
                        ):
                            return None
                        if source in active_temps and access.move:
                            active_temps.discard(source)
                        active_temps.add(target)
                    elif name in ("move", "structural-unpack"):
                        target = vop.results[0].tn
                        if target is owner_tn:
                            return None
                        if source in active_temps and source.borrowed_from is None:
                            active_temps.discard(source)
                        active_temps.add(target)
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
                                return None
                            slot_types[position] = slot_types[position].join(
                                value.type or UNKNOWN
                            )
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
                        if (
                            source in active_temps
                            and not borrow_parent
                            and source.borrowed_from is None
                        ):
                            active_temps.discard(source)
                    elif name == "fast-len":
                        rewritten.append(_constant(vop, ast.IntegerLiteral(self.width)))
                        if source in active_temps and source.borrowed_from is None:
                            active_temps.discard(source)
                    elif name == "discard":
                        if source in active_temps and source.borrowed_from is None:
                            active_temps.discard(source)
                    else:
                        return None
                replacements[vop] = rewritten
                # A borrowed alias has no owning reference to consume, but its
                # final use still closes its extent. Cross-block aliases have
                # no entry here and remain unsupported.
                active_temps.difference_update(last_uses.get(vop, ()))
            if pending_origin is not None or active_temps:
                return None
        for slot, stype in zip(slots, slot_types, strict=True):
            slot.type = UNKNOWN if stype is BOTTOM else stype
            slot.primitive_type = primitive_type_of(slot.type)
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

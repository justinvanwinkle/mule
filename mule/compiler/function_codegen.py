"""Per-function LLVM IR emission and native C-ABI wrapper generation."""

from __future__ import annotations

from contextlib import contextmanager

from mule import ast

from . import vops
from .callconv import (
    entry_steps,
)
from .codegen_support import (
    _PROVENANCE_OPERATION,
    OBJECT,
    Function,
    _ArgumentGuard,
    _block_local_last_vop,
    _declared_protocol_context,
    _function_comment,
    _HelperBuilder,
    _lifetime_analyze_and_pack_slots,
    _llvm_comment,
    _needs_cleanup_release,
    _suffixed,
    _TypeAssumption,
    runtime_namespace,
)
from .ctype import (
    FIXNUM_MAX,
    FIXNUM_MIN,
    NONE_TYPE,
    UNKNOWN,
    ValueType,
)
from .ltn import FixnumGuard
from .node import (
    linked,
)
from .vop import (
    BOOLEAN,
    BOOLEAN_T,
    CLASS,
    DESCRIPTOR,
    DOUBLE,
    FLOAT_BOX_LAYOUT,
    INSTANCE,
    INT64_BOX_LAYOUT,
    NONE_T,
    NULL,
    RANGE,
    REFERENCE,
    SIGNED,
    TN,
    BoxLayout,
)


class FunctionGenerator:
    """Emit one LLVM function for one IR2 component (one body)."""

    def __init__(self, module, component, *, unpacked: bool = False):
        self.module = module
        self._literal_emitter = vops.LiteralEmitter(module)
        self._borrowed_literal_emitter = vops.BorrowedLiteralEmitter(module)
        self.component = component
        self.lambda_ = component.lambda_
        self.context = component.context
        self.plan = self.context.plan
        self.raw = self.plan is not None
        self.convention = self.context.convention
        self.unpacked = unpacked
        if unpacked:
            info = component.tuple_return
            self.return_uses = (
                info.direct
                if info is not None
                else module.return_tuple_uses.get(self.lambda_, frozenset())
            )
        else:
            self.return_uses = frozenset()
        self.environment = component.environment
        self.immortal_values: set[str] = set()
        self.nonnull_values: set[str] = set()
        self.borrowed_values: set[str] = set()
        self.heap_values: set[str] = set()
        self.private_values: dict[str, BoxLayout] = {}
        self.lines: list[str] = []
        self.serial = 0
        self.exception_target: str | None = None
        if self.raw:
            self.body = self.plan.entry
        elif self.convention is not None:
            self.body = module.numeric_entries[self.lambda_, self.context.id].symbol
        elif self.unpacked:
            self.body = module.unpacked[self.lambda_, self.context.id]
        elif self.context.rejected_fixnum_guard is not None:
            self.body = _suffixed(module.bodies[self.lambda_], ".fallback.fixnum")
        elif self.context.id is not None:
            self.body = module.specialized[self.lambda_, self.context.id]
        else:
            self.body = module.bodies[self.lambda_]
        if self.unpacked and self.return_uses == frozenset(
            self.lambda_.return_.result.uses
        ):
            for block in component.blocks:
                for vop in block.vops:
                    if (
                        vop.node is not None
                        and vop.node in self.return_uses
                        and vop.results
                    ):
                        vop.results[0].tn.type = NONE_TYPE
        delivery = component.tuple_delivery
        self.unpacked_calls = delivery.calls
        self.unpacked_call_assertions = delivery.call_assertions
        self.direct_call_assertions = delivery.direct_assertions
        self.unpacked_unpacks = delivery.unpacks
        self.unpacked_slot_unpacks = delivery.slot_unpacks
        self.unpacked_sequence_available = delivery.sequence_available
        self.unpacked_moves = delivery.moves
        self.unpacked_refs = delivery.refs
        self.unpacked_discard_loads = delivery.discard_loads
        self.unpacked_temp_tns = delivery.temp_tns
        self.max_unpack_out = delivery.max_arity
        self.local_slots: dict[str, str] = {}
        self.local_tns_by_name: dict[str, TN] = {}
        self.value_tns = []
        self.local_tns = []
        self.range_tns = []
        # Temporaries confined to one block are LLVM values rather than
        # slots: ``values`` holds each one's definition, ``live`` the owned
        # ones not yet consumed, which a failure edge must release, and
        # ``expiring`` the vop after which an unconsumed one is released.
        self.registers: set = set()
        self.values: dict = {}
        self.live: dict = {}
        self.expiring: dict = {}
        positions = {
            vop: (block, index)
            for block in component.blocks
            for index, vop in enumerate(block.vops)
        }
        reads = component.reads
        writes = component.writes
        for tn in component.tns:
            if tn.primitive_type is not None and tn.sc is RANGE:
                self.range_tns.append(tn)
                tn.slot = f"%range.{len(self.range_tns) - 1}"
            elif tn.leaf is not None and tn.leaf.expression_temporary:
                # A variable's slot is loaded and stored by name, so an
                # expression temporary stays in a slot, released when an
                # exception transfers out of the expression.
                if tn not in self.unpacked_temp_tns:
                    self.value_tns.append(tn)
            elif tn.leaf is None and not (
                tn.name is not None and tn.name.startswith("$")
            ):
                # Compiler temporaries. The leafless "$cell:"/"$copy:" TNs
                # holding captures are locals, below.
                last = _block_local_last_vop(tn, positions, reads, writes)
                if last is not None:
                    tn.slot = None
                    self.registers.add(tn)
                    self.expiring.setdefault(last, []).append(tn)
                else:
                    self.value_tns.append(tn)
            else:
                self.local_tns.append(tn)
        packable_locals = [
            tn
            for tn in self.local_tns
            if (
                tn.leaf is not None
                and tn.leaf.hidden
                and not tn.leaf.expression_temporary
                and tn.leaf not in self.environment.cells
                and tn.leaf not in self.environment.copies
            )
            or (
                tn.leaf is None
                and tn.cleanup_owner is None
                and tn.name is not None
                and tn.name.startswith("$scalar.")
            )
        ]
        packable_set = set(packable_locals)
        fixed_locals = [tn for tn in self.local_tns if tn not in packable_set]
        scalar_children = {}
        for tn in fixed_locals:
            if tn.cleanup_owner is not None:
                scalar_children.setdefault(tn.cleanup_owner, []).append(tn)
        fixed_locals = [
            member
            for tn in fixed_locals
            if tn.cleanup_owner is None
            for member in (tn, *reversed(scalar_children.get(tn, ())))
        ]
        (
            self.local_slot_groups,
            self.value_slot_groups,
            self.unwind_groups_by_target,
        ) = _lifetime_analyze_and_pack_slots(
            component, fixed_locals, packable_locals, self.value_tns
        )
        for tn in self.local_tns:
            if tn.leaf is not None:
                self.local_slots[tn.leaf.name] = tn.slot
                self.local_tns_by_name[tn.leaf.name] = tn
        if self.lambda_.coroutine_body and not self.raw:
            closed = self.module.closed_variables.get(self.lambda_, set())
            parameters = self.lambda_.parameters
            var_names = list(parameters) + [
                var.name
                for var in self.lambda_.locals
                if var.name not in parameters
                and not var.name.startswith("$")
                and var not in closed
            ]
            self.coroutine_local_tns = [
                self.local_tns_by_name.get(name) for name in var_names
            ]
        else:
            self.coroutine_local_tns = []
        self.landings: dict[tuple[str, tuple[str, ...], bool, bool], str] = {}
        # Each landing's predecessor blocks and the source line failing there.
        self.landing_sites: dict[str, dict[str, tuple[int, str]]] = {}
        self.block = None
        self.vop = None
        self.in_cold_block = False
        self.checking_element_contract = False
        self.unpacked_remaining_slots: tuple[int, ...] = ()
        self.entry_end = 0
        self.uses_out = False
        self.uses_reuse = False
        self.uses_float_scratch = False
        self.uses_numeric_error = False
        self.max_argv = 0
        self.owner_slots: dict[object, str] = {}
        if self.raw:
            self.owner_slots = {
                node: f"%owner.{index}" for index, node in enumerate(self.plan.owners)
            }
            self.dependencies = {
                var: f"%class.{index}"
                for index, var in enumerate(self.plan.dependencies)
            }

    def is_unpacked_return_vop(self, vop) -> bool:
        return self.unpacked and vop.node is not None and vop.node in self.return_uses

    def unpacked_return_result(self, ref, values) -> None:
        for index, value in enumerate(values):
            slot = self.operation(f"getelementptr ptr, ptr %tuple.out, i64 {index}")
            self.line(f"store ptr {value}, ptr {slot}")
        self.immortal_values.add("@mule.none")
        self.publish(ref, "@mule.none")

    def unpacked_call_arity(self, vop) -> int | None:
        return self.unpacked_calls.get(vop)

    def call_result_contract(self, vop):
        assertion = self.unpacked_call_assertions.get(
            vop
        ) or self.direct_call_assertions.get(vop)
        if assertion is None or not assertion.codegen_info.safety:
            return None
        return assertion.codegen_info.contract

    def is_unpacked_result_check(self, vop):
        return vop in self.unpacked_call_assertions.values()

    def is_direct_result_check(self, vop) -> bool:
        return vop in self.direct_call_assertions.values()

    def is_unpacked_unpack(self, vop) -> bool:
        return vop in self.unpacked_unpacks

    def unpacked_slot_arity(self, vop) -> int | None:
        return self.unpacked_slot_unpacks.get(vop)

    def unpacked_sequence_available_arity(self, vop) -> int | None:
        return self.unpacked_sequence_available.get(vop)

    def is_unpacked_move(self, vop) -> bool:
        return vop in self.unpacked_moves

    def unpacked_ref_slot(self, vop) -> int | None:
        info = self.unpacked_refs.get(vop)
        return None if info is None else info[0]

    def unpacked_ref_remaining(self, vop) -> tuple[int, ...]:
        info = self.unpacked_refs.get(vop)
        if info is None:
            return self.unpacked_remaining_slots
        slot_idx, arity = info
        return tuple(range(slot_idx + 1, arity))

    def is_unpacked_immortal_ref(self, vop) -> bool:
        return (vop.results[0].tn.type or UNKNOWN).is_immortal

    def is_unpacked_discard_load(self, vop) -> bool:
        return vop in self.unpacked_discard_loads

    def sync_coroutine_locals(self) -> None:
        if not self.coroutine_local_tns:
            return
        n_locals = len(self.coroutine_local_tns)
        for index, tn in enumerate(self.coroutine_local_tns):
            if tn is None or tn.slot is None or tn.sc not in OBJECT:
                continue
            val = self.operation(f"load ptr, ptr {tn.slot}")
            slot = self.operation(
                f"getelementptr [{n_locals} x ptr], ptr %coroutine.locals, i64 0, i64 {index}"
            )
            self.line(f"store ptr {val}, ptr {slot}")

    # Text helpers

    @contextmanager
    def cold_block(self):
        previous = self.in_cold_block
        self.in_cold_block = True
        try:
            yield
        finally:
            self.in_cold_block = previous

    def line(self, text: str) -> None:
        if (
            self.in_cold_block
            and text.startswith("call ")
            and "@llvm." not in text
            and not text.endswith(" cold noinline")
        ):
            text += " cold noinline"
        self.lines.append("  " + text)

    def fresh(self, prefix: str = "t") -> str:
        self.serial += 1
        return f"%{prefix}.{self.serial}"

    def operation(self, text: str) -> str:
        if (
            self.in_cold_block
            and text.startswith("call ")
            and "@llvm." not in text
            and not text.endswith(" cold noinline")
        ):
            text += " cold noinline"
        value = self.fresh()
        self.line(f"{value} = {text}")
        return value

    def failure_target(
        self,
        base: str | None = None,
        failed_value=None,
        *,
        record_traceback: bool = True,
        explicit_raise: bool = False,
    ) -> str:
        """The label a failure edge branches to: ``base`` (the handler's unwind
        block or ``error`` by default) after releasing the owned temporaries
        live here. ``failed_value`` is the NULL result being checked."""
        if base is None:
            base = (
                "error"
                if self.exception_target is None
                else (
                    "unwind." + self.exception_target
                    if self.exception_target in self.unwind_groups_by_target
                    else self.exception_target
                )
            )
        # Native entries leave value ownership to their boundary adapter and
        # report errors through their error flag.
        if self.raw:
            return base
        # Builtin algorithms have no Python source frame in the exception chain.
        # Their live activations remain available to native debug diagnostics.
        record_traceback = (
            record_traceback
            and not self.module.program.bootstrap
            and self.lambda_.policy.records_traceback
        )
        owned = (
            *(value for value in self.live.values() if value != failed_value),
            *(f"@unpack.slot.{i}" for i in self.unpacked_remaining_slots),
        )
        if not record_traceback and not owned:
            return base
        key = (base, owned, record_traceback, explicit_raise)
        label = self.landings.get(key)
        if label is None:
            label = self.landings[key] = self.fresh("release")[1:]
        if record_traceback:
            # Landings release owned temporaries; only new failures and explicit
            # raises add source entries. Resuming a caught transfer preserves them.
            sites = self.landing_sites.setdefault(label, {})
            block_label = self.current_block()
            if block_label not in sites:
                source = (
                    None
                    if self.vop is None or self.vop.node is None
                    else self.vop.node.source
                )
                marker = self.module.source_highlights.marker(
                    self.module.code(self.lambda_).filename, source
                )
                sites[block_label] = (
                    self.current_line(),
                    self.module.text(marker) if marker else "null",
                )
        return label

    def current_block(self) -> str:
        """The label of the block being emitted: each failure check starts a
        new block, so the label is a few lines back."""
        for line in reversed(self.lines):
            if not line.startswith(" ") and line.endswith(":"):
                return line[:-1]
        raise AssertionError("no block is being emitted")

    def current_line(self) -> int:
        """The source line of the operation being emitted."""
        node = None if self.vop is None else self.vop.node
        source = None if node is None else node.source
        if source is not None and source.span is not None:
            return source.span.start.line
        return self.module.code(self.lambda_).firstlineno

    def emit_landings(self) -> None:
        """Record source failures (unless resuming a caught transfer), then
        release owned temporaries. A separate address identifies this call
        without exposing the return-value slot to the runtime."""
        if self.landing_sites:
            # The runtime compares and stores this address, never its contents.
            # Keep result storage private so LLVM can promote it to SSA.
            self.lines.insert(
                self.lines.index("entry:") + 1, "  %activation = alloca i8"
            )
            code = self.module.code_descriptor(self.lambda_)
        else:
            code = ""
        # Group landings that share (base, record_traceback, explicit_raise,
        # len(owned), release_kinds) when all owned items are SSA values of the
        # same release category (1 or 2 values). Merging them with PHI nodes
        # collapses dozens of per-temporary release.<N> blocks into one block.
        merged_groups: dict[
            tuple[str, bool, bool, tuple[str, ...]],
            list[tuple[tuple[str, tuple[str, ...], bool, bool], str]],
        ] = {}
        unmerged_landings: list[
            tuple[tuple[str, tuple[str, ...], bool, bool], str]
        ] = []

        def _release_kind(val: str) -> str | None:
            if val.startswith("@unpack.slot.") or not self.owns_reference(val):
                return None
            if val in self.private_values:
                return None
            return "heap" if val in self.heap_values else "fast"

        for key, label in self.landings.items():
            base, owned, record_traceback, explicit_raise = key
            if record_traceback and 1 <= len(owned) <= 3:
                kinds = tuple(_release_kind(v) for v in owned)
                if all(k is not None for k in kinds):
                    merged_groups.setdefault(
                        (base, record_traceback, explicit_raise, kinds), []
                    ).append((key, label))
                    continue
            unmerged_landings.append((key, label))

        for group_entries in merged_groups.values():
            if len(group_entries) == 1:
                unmerged_landings.append(group_entries[0])

        owned_sets = list(
            dict.fromkeys(
                (base, owned)
                for (base, owned, _record, _explicit), _label in unmerged_landings
                if owned
            )
        )
        by_end: dict[tuple[str, int, str], list[tuple[str, ...]]] = {}
        for base, owned in owned_sets:
            by_end.setdefault((base, len(owned), owned[-1]), []).append(owned)
        parent_prefix: dict[tuple[str, tuple[str, ...]], tuple[str, ...]] = {}
        chain_labels: dict[tuple[str, tuple[str, ...]], str] = {}
        for base, owned in owned_sets:
            for index in range(len(owned) - 1, 0, -1):
                candidates = by_end.get((base, index, owned[index - 1]))
                if candidates is None:
                    continue
                prefix = owned[:index]
                if prefix in candidates:
                    parent_prefix[base, owned] = prefix
                    if (base, prefix) not in chain_labels:
                        chain_labels[base, prefix] = self.fresh("release")[1:]
                    break

        def emit_release_step(base: str, owned: tuple[str, ...]) -> None:
            prefix = parent_prefix.get((base, owned))
            if prefix is None:
                for value in owned:
                    self.release(value)
                self.line(f"br label %{base}")
            else:
                for value in owned[len(prefix) :]:
                    self.release(value)
                self.line(f"br label %{chain_labels[base, prefix]}")

        prev_cold = self.in_cold_block
        self.in_cold_block = True
        for (
            base,
            owned,
            record_traceback,
            explicit_raise,
        ), label in unmerged_landings:
            self.lines.append(label + ":")
            if record_traceback:
                sites = self.landing_sites[label]
                incoming = ", ".join(
                    f"[{site[0]}, %{block}]" for block, site in sites.items()
                )
                line = f"%failed.line.{label}"
                self.line(f"{line} = phi i32 {incoming}")
                incoming_markers = ", ".join(
                    f"[{site[1]}, %{block}]" for block, site in sites.items()
                )
                marker = f"%failed.marker.{label}"
                self.line(f"{marker} = phi ptr {incoming_markers}")
                self.line(
                    f"call void @mule_traceback_here(ptr {code}, i32 {line}, "
                    f"ptr %globals, ptr %activation, i32 {int(explicit_raise)}, ptr {marker})"
                )
            if (base, owned) in chain_labels:
                self.line(f"br label %{chain_labels[base, owned]}")
            else:
                emit_release_step(base, owned)

        for (
            base,
            record_traceback,
            explicit_raise,
            kinds,
        ), group_entries in merged_groups.items():
            if len(group_entries) < 2:
                continue
            canonical_label = group_entries[0][1]
            alias_map = {
                old_label: canonical_label for _key, old_label in group_entries[1:]
            }
            if alias_map:
                for idx, line_str in enumerate(self.lines):
                    if "label %release." in line_str:
                        for old_label, new_label in alias_map.items():
                            target_tok = f"label %{old_label}"
                            if target_tok in line_str:
                                line_str = line_str.replace(
                                    target_tok + ",", f"label %{new_label},"
                                )
                                if line_str.endswith(target_tok):
                                    line_str = (
                                        line_str[: -len(target_tok)]
                                        + f"label %{new_label}"
                                    )
                        self.lines[idx] = line_str
            self.lines.append(canonical_label + ":")
            if record_traceback:
                all_sites: list[tuple[str, tuple[int, str], tuple[str, ...]]] = []
                for (_b, owned, _rt, _er), old_label in group_entries:
                    for block, site in self.landing_sites[old_label].items():
                        all_sites.append((block, site, owned))
                incoming = ", ".join(
                    f"[{site[0]}, %{block}]" for block, site, _owned in all_sites
                )
                line = f"%failed.line.{canonical_label}"
                self.line(f"{line} = phi i32 {incoming}")
                incoming_markers = ", ".join(
                    f"[{site[1]}, %{block}]" for block, site, _owned in all_sites
                )
                marker = f"%failed.marker.{canonical_label}"
                self.line(f"{marker} = phi ptr {incoming_markers}")
                for pos, kind in enumerate(kinds):
                    phi_val = f"%failed.val.{pos}.{canonical_label}"
                    incoming_vals = ", ".join(
                        f"[{owned[pos]}, %{block}]" for block, _site, owned in all_sites
                    )
                    self.line(f"{phi_val} = phi ptr {incoming_vals}")
                self.line(
                    f"call void @mule_traceback_here(ptr {code}, i32 {line}, "
                    f"ptr %globals, ptr %activation, i32 {int(explicit_raise)}, ptr {marker})"
                )
                for pos, kind in enumerate(kinds):
                    phi_val = f"%failed.val.{pos}.{canonical_label}"
                    rel_fn = (
                        "@mule.heap.release" if kind == "heap" else "@mule.fast.release"
                    )
                    self.line(f"call void {rel_fn}(ptr {phi_val})")
                self.line(f"br label %{base}")
            else:
                for (_b, owned, _rt, _er), old_label in group_entries:
                    self.lines.append(
                        f"; merged landing {old_label}"
                        if old_label == canonical_label
                        else f"{old_label}:"
                    )
                    for value in owned:
                        self.release(value)
                    self.line(f"br label %{base}")

        for (base, prefix), label in chain_labels.items():
            self.lines.append(label + ":")
            emit_release_step(base, prefix)
        self.in_cold_block = prev_cold

    def guard(self, failed: str, failed_value=None) -> str:
        """Branch to the failure path when ``failed``; returns the label of
        the continuation block."""
        continuation = self.fresh("ok")[1:]
        target = self.failure_target(failed_value=failed_value)
        self.line(
            f"br i1 {failed}, label %{target}, label %{continuation}, {vops.COLD_BRANCH}"
        )
        self.lines.append(continuation + ":")
        return continuation

    def check_pointer(self, value: str) -> str:
        if value in self.nonnull_values:
            return self.current_block()
        return self.guard(self.operation(f"icmp eq ptr {value}, null"), value)

    def check_status(self, value: str) -> str:
        return self.guard(self.operation(f"icmp slt i32 {value}, 0"))

    def propagate(self, *, record_traceback=True, explicit_raise=False) -> None:
        target = self.failure_target(
            record_traceback=record_traceback, explicit_raise=explicit_raise
        )
        self.line(f"br label %{target}")

    # TN access

    def mark_immortal(self, tn, value: str) -> None:
        if tn.sc in OBJECT and not tn.sc.owned:
            self.immortal_values.add(value)

    def load(self, ref) -> str:
        tn = ref.tn
        if tn in self.registers:
            return self.values[tn]
        value = self.operation(f"load {tn.sc.llvm_type}, ptr {tn.slot}")
        self.record_facts(tn, value)
        return value

    def take(self, ref) -> str:
        """Consume a temporary: the failure paths no longer release it."""
        tn = ref.tn
        if tn.borrowed_from is not None:
            return self.load(ref)
        if tn in self.registers:
            self.live.pop(tn, None)
            return self.values[tn]
        value = self.load(ref)
        if tn.sc in OBJECT:
            self.line(f"store ptr null, ptr {tn.slot}")
        return value

    def store(self, ref, value: str, *, release: bool = True) -> None:
        tn = ref.tn
        if tn in self.registers:
            self.define(tn, value)
            return
        if tn.sc in OBJECT:
            self.publish(ref, value, release=release)
            return
        self.line(f"store {tn.sc.llvm_type} {value}, ptr {tn.slot}")

    def define(self, tn, value: str) -> None:
        # A vop may reuse its result as scratch once it consumed the earlier
        # value; an unconsumed one would need a phi, not a second definition.
        if tn in self.live:
            raise ValueError(f"{tn!r} redefined while it still owns a value")
        self.values[tn] = value
        self.record_facts(tn, value)
        if (
            tn.sc.owned
            and tn.borrowed_from is None
            and value not in self.immortal_values
        ):
            self.live[tn] = value

    def out_pointer(self) -> str:
        """An entry-block slot for the pointer a runtime call writes on every
        return; read it back right after the call."""
        self.uses_out = True
        return "%out"

    def reuse_slot(self) -> str:
        """An entry-block slot holding an inplace operation's local value for
        the slow-path reference-count check, without escaping the local slot."""
        self.uses_reuse = True
        return "%reuse.slot"

    def float_scratch(self) -> str:
        """One entry-block payload extraction slot, reused after each load."""
        self.uses_float_scratch = True
        return "%float.scratch"

    def _has_unchecked_element_contract(self, tn) -> bool:
        return (
            not self.raw
            and not self.checking_element_contract
            and self.vop is not None
            and bool(self.lambda_.policy.safety)
            and self.vop.node in self.context.element_contracts
            and tn.sc in OBJECT
        )

    def publish(self, ref, value: str, *, release: bool = True) -> None:
        tn = ref.tn
        if self._has_unchecked_element_contract(tn):
            if (
                tn not in self.registers
                and tn.sc.owned
                and tn.borrowed_from is None
                and release
            ):
                old = self.operation(f"load ptr, ptr {tn.slot}")
                self.line(f"store ptr null, ptr {tn.slot}")
                self.record_facts(tn, old)
                self.release(old)
            if tn in self.registers:
                if tn in self.live:
                    raise ValueError(f"{tn!r} redefined while it still owns a value")
                self.values[tn] = value
            if (
                tn.sc.owned
                and tn.borrowed_from is None
                and value not in self.immortal_values
            ):
                self.live[tn] = value
            return
        if tn in self.registers:
            self.define(tn, value)
            return
        if tn.sc.owned and tn.borrowed_from is None and release:
            old = self.operation(f"load ptr, ptr {tn.slot}")
            self.line(f"store ptr {value}, ptr {tn.slot}")
            self.record_facts(tn, old)
            self.record_facts(tn, value)
            self.release(old)
            return
        self.line(f"store ptr {value}, ptr {tn.slot}")
        self.record_facts(tn, value)

    def record_facts(self, tn, value: str) -> None:
        self.mark_immortal(tn, value)
        if tn.borrowed_from is not None:
            self.borrowed_values.add(value)
        if tn.private_box is not None:
            self.private_values[value] = tn.private_box
        fact = tn.type or UNKNOWN
        if fact.is_fixnum or fact.is_immortal:
            self.immortal_values.add(value)
            if self.live.get(tn) == value:
                self.live.pop(tn, None)
        elif fact.is_heap_object:
            self.heap_values.add(value)

    def retain(self, value: str) -> str:
        if value in self.immortal_values:
            return value
        layout = self.private_values.get(value)
        if layout is not None:
            operation = f"call ptr @{layout.symbol('private.retain')}(ptr {value})"
        elif value in self.heap_values:
            operation = f"call ptr @mule.heap.retain(ptr {value})"
        else:
            operation = f"call ptr @mule.fast.retain(ptr {value})"
        if value in self.borrowed_values:
            # The acquired owner and the existing borrow have the same pointer,
            # but only the newly acquired reference must be released.
            owned = self.operation(operation)
            if layout is not None:
                self.private_values[owned] = layout
            if value in self.heap_values:
                self.heap_values.add(owned)
            return owned
        self.line(operation)
        return value

    def release(self, value: str) -> None:
        if value.startswith("@unpack.slot."):
            rem_idx = int(value.rsplit(".", 1)[1])
            slot = self.operation(f"getelementptr ptr, ptr %unpack.out, i64 {rem_idx}")
            unpacked_val = self.operation(f"load ptr, ptr {slot}")
            self.line(f"call void @mule.fast.release(ptr {unpacked_val})")
            return
        if not self.owns_reference(value):
            return
        layout = self.private_values.get(value)
        if layout is not None:
            self.line(f"call void @{layout.symbol('private.release')}(ptr {value})")
            return
        if value in self.heap_values:
            self.line(f"call void @mule.heap.release(ptr {value})")
            return
        self.line(f"call void @mule.fast.release(ptr {value})")

    def owns_reference(self, value: str) -> bool:
        """Whether this acquisition may transfer or release a runtime owner."""
        return value not in self.immortal_values and value not in self.borrowed_values

    def refine(self, tn, value: str) -> None:
        """Expose proven representation/range facts after the error check."""
        if self._has_unchecked_element_contract(tn):
            current = linked(self.vop, self, "current VOP")
            contract = self.context.element_contracts[current.node]
            self.checking_element_contract = True
            self.unpacked_remaining_slots = self.unpacked_ref_remaining(self.vop)
            try:
                contract.accept(vops._ContractAssertion(self, value))
            finally:
                self.checking_element_contract = False
            self.live.pop(tn, None)
            value = self.operation(f"getelementptr i8, ptr {value}, i64 0")
            if tn in self.registers:
                self.define(tn, value)
            else:
                self.line(f"store ptr {value}, ptr {tn.slot}")
                self.record_facts(tn, value)
            if self.component.reads.get(tn):
                self.refine_type(tn.type or UNKNOWN, value)
            return
        self.record_facts(tn, value)
        if self.component.reads.get(tn):
            self.refine_type(tn.type or UNKNOWN, value)

    def refine_type(self, fact: ValueType, value: str, *, nullable=False) -> None:
        if not nullable:
            if fact.is_fixnum or fact.is_immortal:
                self.immortal_values.add(value)
            elif fact.is_heap_object:
                self.heap_values.add(value)
        if not fact.is_fixnum:
            return
        valid = fact.accept(_TypeAssumption(self, value))
        if valid is None:
            return
        if nullable:
            missing = self.operation(f"icmp eq ptr {value}, null")
            valid = self.operation(f"or i1 {missing}, {valid}")
        self.line(f"call void @llvm.assume(i1 {valid})")

    def pointer_result(self, ref, operation: str, consumed=()) -> None:
        value = self.operation(operation)
        self.publish(ref, value)
        for operand in consumed:
            self.release(operand)
        self.check_pointer(value)
        self.refine(ref.tn, value)

    def native_result(self, ref, value: str, consumed=()) -> None:
        self.record_facts(ref.tn, value)
        self.publish(ref, value)
        for operand in consumed:
            self.release(operand)

    def parameter(self, index: int) -> str:
        return f"%parameter.{index}"

    def check_native_receiver(self) -> None:
        """Keep native-class receiver rules independent of body specialization."""
        if self.raw or self.lambda_ in self.module.native_plans:
            return
        layout = self.module.layouts.get(self.lambda_.owner_class)
        if (
            layout is None
            or not self.lambda_.vars
            or self.lambda_ not in layout.methods.values()
        ):
            return
        matched = self.operation(
            f"call i32 @mule_native_instance_matches(ptr %parameter.0, "
            f"ptr {layout.schema}, ptr %globals)"
        )
        wrong = self.operation(f"icmp eq i32 {matched}, 0")
        failed, accepted = self.fresh("receiver.bad")[1:], self.fresh("receiver.ok")[1:]
        self.line(
            f"br i1 {wrong}, label %{failed}, label %{accepted}, {vops.COLD_BRANCH}"
        )
        self.lines.append(failed + ":")
        self.line("call i32 @mule_native_type_error() cold noinline")
        self.propagate()
        self.lines.append(accepted + ":")

    def parameter_value(self, index: int, ref) -> str:
        value = self.parameter(index)
        if self.convention is None:
            return value
        sc = self.convention.parameters[index]
        if sc is SIGNED and ref.tn.sc in OBJECT:
            return self.encode(value)
        if sc is DOUBLE and ref.tn.sc is not DOUBLE:
            raise ValueError(
                f"raw float parameter {index} requires double local storage"
            )
        return value

    def return_value(self, operand) -> None:
        if self.convention is None or self.convention.returned is DESCRIPTOR:
            value = self.take(operand)
            if self.unpacked and self.return_uses != frozenset(
                self.lambda_.return_.result.uses
            ):
                ready = self.fresh("tuple.return.ready")[1:]
                extract = self.fresh("tuple.return.extract")[1:]
                forwarded = self.operation(f"icmp eq ptr {value}, @mule.none")
                self.line(f"br i1 {forwarded}, label %{ready}, label %{extract}")
                self.lines.append(extract + ":")
                arity = self.module.return_tuple_arities[self.lambda_]
                for index in range(arity):
                    child = self.operation(
                        f"call ptr @mule.fast.tuple.slot(ptr {value}, i64 {index})"
                    )
                    slot = self.operation(
                        f"getelementptr ptr, ptr %tuple.out, i64 {index}"
                    )
                    self.line(f"store ptr {child}, ptr {slot}")
                self.release(value)
                self.line(f"br label %{ready}")
                self.lines.append(ready + ":")
                value = "@mule.none"
            if self.raw:
                returned = linked(
                    self.plan.returned, self.plan, "return representation"
                )
                value = returned.prepare_return(self, value)
            llvm_type = operand.tn.sc.llvm_type
        else:
            sc = self.convention.returned
            if sc is DOUBLE:
                value = vops.unbox_float_operand(self, operand)
                consumed = self.take(operand)
                if operand.tn.sc in OBJECT:
                    self.release(consumed)
            else:
                value = self.take(operand)
                if operand.tn.sc in OBJECT:
                    pointer = value
                    value = self.decode(pointer)
                    self.release(pointer)
            llvm_type = sc.llvm_type
        self.line(f"store {llvm_type} {value}, ptr %return")
        self.line("br label %cleanup")

    def numeric_call(self, vop) -> None:
        entry = vop.codegen_info
        convention = linked(entry.context.convention, entry, "convention")
        if entry.callee_required:
            callable_ref, *arguments = vop.args
        else:
            callable_ref = None
            arguments = list(vop.args)
        converted = []
        # Keep original operands owned until all fallible extraction and frame
        # entry checks succeed. Their ordinary landing releases them on failure.
        for ref, sc in zip(arguments, convention.parameters, strict=True):
            if sc is DOUBLE:
                converted.append(vops.unbox_float_operand(self, ref))
            elif sc is SIGNED:
                value = self.load(ref)
                converted.append(self.decode(value) if ref.tn.sc in OBJECT else value)
            else:
                converted.append(self.load(ref))
        callee = self.load(callable_ref) if callable_ref is not None else "null"
        needs_error_flag = convention.returned is not DESCRIPTOR
        if not entry.proven:
            if needs_error_flag:
                self.uses_numeric_error = True
                self.line("store i1 false, ptr %numeric.error")
            if callable_ref is not None:
                self.take(callable_ref)
            raw_owned = []
            for ref, sc in zip(arguments, convention.parameters, strict=True):
                taken = self.take(ref)
                if (
                    sc is not DESCRIPTOR
                    and ref.tn.sc in OBJECT
                    and ref.tn.borrowed_from is None
                ):
                    raw_owned.append(taken)
            operands = [
                f"ptr {callee}",
                *(["ptr %numeric.error"] if needs_error_flag else []),
                *(
                    f"{sc.llvm_type} {value}"
                    for sc, value in zip(convention.parameters, converted, strict=True)
                ),
            ]
            helper = self.module.guarded_numeric_call_helper(entry)
            value = self.operation(
                f"call {convention.returned.llvm_type} {helper}({', '.join(operands)})"
            )
            for owned in raw_owned:
                self.release(owned)
            if callable_ref is not None:
                self.release(callee)
            result = vop.results[0]
            if needs_error_flag:
                failed = self.operation("load i1, ptr %numeric.error")
                self.guard(failed)
                if convention.returned is SIGNED and result.tn.sc in OBJECT:
                    self.native_result(result, self.encode(value))
                else:
                    self.store(result, value)
            else:
                if entry.result_contract is not None:
                    self.refine_type(entry.context.return_type, value)
                self.store(result, value)
                self.check_pointer(value)
                self.refine(result.tn, value)
            return
        local_environment = self.module.local_environments.is_local(entry.function)
        same_namespace = runtime_namespace(
            entry.function.namespace
        ) == runtime_namespace(self.lambda_.namespace)
        if local_environment:
            callee_globals = "%globals"
            closure = callee if self.module.closure_of(entry.function) else None
        elif same_namespace and not self.module.closure_of(entry.function):
            callee_globals = "%globals"
            closure = None
        else:
            self.uses_numeric_error = True
            self.operation(
                f"call ptr @mule_method_entry(ptr {callee}, ptr %callee.globals.slot)"
            )
            callee_globals = self.operation("load ptr, ptr %callee.globals.slot")
            closure = (
                self.operation(f"call ptr @mule_function_closure_of(ptr {callee})")
                if self.module.closure_of(entry.function)
                else None
            )
        check_depth = self.module.needs_frame_depth_check(entry.function)
        if check_depth:
            status = self.operation("call i32 @mule_enter_frame()")
            self.check_status(status)
        if needs_error_flag:
            self.uses_numeric_error = True
            self.line("store i1 false, ptr %numeric.error")
        if callable_ref is not None:
            self.take(callable_ref)
        raw_owned = []
        for ref, sc in zip(arguments, convention.parameters, strict=True):
            taken = self.take(ref)
            if (
                sc is not DESCRIPTOR
                and ref.tn.sc in OBJECT
                and ref.tn.borrowed_from is None
            ):
                raw_owned.append(taken)
        operands = [
            f"ptr {callee_globals}",
            *(["ptr %numeric.error"] if needs_error_flag else []),
        ]
        if closure is not None:
            operands.append(f"ptr {closure}")
        operands.extend(
            f"{sc.llvm_type} {value}"
            for sc, value in zip(convention.parameters, converted, strict=True)
        )
        value = self.operation(
            f"call {convention.returned.llvm_type} {entry.symbol}({', '.join(operands)})"
        )
        if check_depth:
            self.line("call void @mule_leave_frame()")
        for owned in raw_owned:
            self.release(owned)
        if not local_environment and callable_ref is not None:
            self.release(callee)
        result = vop.results[0]
        if needs_error_flag:
            failed = self.operation("load i1, ptr %numeric.error")
            self.guard(failed)
            if convention.returned is SIGNED and result.tn.sc in OBJECT:
                self.native_result(result, self.encode(value))
            else:
                self.store(result, value)
        else:
            self.refine_type(entry.context.return_type, value)
            self.store(result, value)
            self.check_pointer(value)
            self.refine(result.tn, value)

    def local_slot(self, name: str) -> str:
        return self.local_slots[name]

    def local_owned(self, name: str) -> bool:
        tn = self.local_tns_by_name[name]
        return tn.sc is not None and tn.sc.owned and tn.borrowed_from is None

    def dependency(self, var) -> str:
        return self.dependencies[var]

    def owner_slot(self, node) -> str:
        return self.owner_slots[node]

    def argv(self, values) -> None:
        self.max_argv = max(self.max_argv, len(values))
        for index, value in enumerate(values):
            slot = self.operation(f"getelementptr ptr, ptr %argv, i64 {index}")
            self.line(f"store ptr {value}, ptr {slot}")

    def decode(self, value: str) -> str:
        bits = self.operation(f"ptrtoint ptr {value} to i64")
        return self.operation(f"ashr i64 {bits}, 1")

    def encode(self, value: str) -> str:
        shifted = self.operation(f"shl nsw i64 {value}, 1")
        tagged = self.operation(f"or disjoint i64 {shifted}, 1")
        return self.operation(f"inttoptr i64 {tagged} to ptr")

    def constant(self, literal) -> str:
        return literal.accept(self._literal_emitter)

    def borrowed_constant(self, literal) -> str:
        return literal.accept(self._borrowed_literal_emitter)

    def truth(self, tn, value: str) -> str:
        """An ``i1`` for the truth of a value in ``tn``'s representation."""
        sc = tn.sc
        if sc is BOOLEAN:
            return value
        if sc is SIGNED:
            return self.operation(f"icmp ne i64 {value}, 0")
        if sc is DOUBLE:
            return self.operation(f"fcmp une double {value}, 0.0")
        if sc is NULL:
            return "false"
        if tn.primitive_type is NONE_T:
            self.release(value)
            return "false"
        if tn.primitive_type is BOOLEAN_T:
            self.release(value)
            return self.operation(f"icmp eq ptr {value}, @mule.true")
        if (tn.type or UNKNOWN).is_fixnum:
            self.release(value)
            return self.operation(f"icmp ne ptr {value}, inttoptr (i64 1 to ptr)")
        truth = self.operation(f"call i32 @mule.fast.truth(ptr {value})")
        self.release(value)
        self.check_status(truth)
        return self.operation(f"icmp ne i32 {truth}, 0")

    def box(self, tn, value: str) -> str:
        """An owned object for a raw value."""
        sc = tn.sc
        if sc is SIGNED:
            return self.operation(f"call ptr @{INT64_BOX_LAYOUT.symbol()}(i64 {value})")
        if sc is BOOLEAN:
            return self.operation(f"select i1 {value}, ptr @mule.true, ptr @mule.false")
        if sc is NULL:
            return "@mule.none"
        if sc is DOUBLE:
            return self.operation(
                f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {value})"
            )
        if sc in (INSTANCE, CLASS, REFERENCE):
            return self.operation(f"call ptr @mule.fast.retain(ptr {value})")
        return value

    def double(self, ref) -> str:
        """A ``double`` for an operand held as a double or a signed word."""
        value = self.take(ref)
        if ref.tn.sc is SIGNED:
            return self.operation(f"sitofp i64 {value} to double")
        return value

    def float_arithmetic(
        self, opcode: str, left: str, right: str, *, check_zero=None
    ) -> str:
        if check_zero is None:
            check_zero = bool(self.lambda_.policy.safety)
        if opcode == "fdiv" and check_zero:
            zero = self.operation(f"fcmp oeq double {right}, 0.0")
            failed, continuation = self.fresh("zero")[1:], self.fresh("nonzero")[1:]
            self.line(
                f"br i1 {zero}, label %{failed}, label %{continuation}, {vops.COLD_BRANCH}"
            )
            self.lines.append(failed + ":")
            self.line("call i32 @mule_native_zero_division() cold noinline")
            self.line(f"br label %{self.failure_target()}")
            self.lines.append(continuation + ":")
        return self.operation(f"{opcode} double {left}, {right}")

    def float_sqrt(self, value: str, *, check_domain=True) -> str:
        if check_domain:
            neg = self.operation(f"fcmp olt double {value}, 0.0")
            failed, continuation = (
                self.fresh("domain_err")[1:],
                self.fresh("sqrt_ok")[1:],
            )
            self.line(
                f"br i1 {neg}, label %{failed}, label %{continuation}, {vops.COLD_BRANCH}"
            )
            self.lines.append(failed + ":")
            self.line("call i32 @mule_math_domain_error() cold noinline")
            self.line(f"br label %{self.failure_target()}")
            self.lines.append(continuation + ":")
        return self.operation(f"call double @llvm.sqrt.f64(double {value})")

    def float_pow(self, left: str, right: str) -> str:
        if right == "0x3FE0000000000000":
            normalized = self.operation(f"fadd double {left}, 0.0")
            return self.operation(f"call double @llvm.sqrt.f64(double {normalized})")
        zero_base = self.operation(f"fcmp oeq double {left}, 0.0")
        neg_exp = self.operation(f"fcmp olt double {right}, 0.0")
        zero_div = self.operation(f"and i1 {zero_base}, {neg_exp}")
        failed_z, check_pow = self.fresh("pow.zero")[1:], self.fresh("pow.eval")[1:]
        self.line(
            f"br i1 {zero_div}, label %{failed_z}, label %{check_pow}, {vops.COLD_BRANCH}"
        )
        self.lines.append(failed_z + ":")
        self.line("call i32 @mule_native_zero_division() cold noinline")
        self.line(f"br label %{self.failure_target()}")
        self.lines.append(check_pow + ":")
        result = self.operation(
            f"call double @llvm.pow.f64(double {left}, double {right})"
        )
        abs_res = self.operation(f"call double @llvm.fabs.f64(double {result})")
        is_inf = self.operation(f"fcmp oeq double {abs_res}, 0x7FF0000000000000")
        abs_left = self.operation(f"call double @llvm.fabs.f64(double {left})")
        left_fin = self.operation(f"fcmp olt double {abs_left}, 0x7FF0000000000000")
        abs_right = self.operation(f"call double @llvm.fabs.f64(double {right})")
        right_fin = self.operation(f"fcmp olt double {abs_right}, 0x7FF0000000000000")
        both_fin = self.operation(f"and i1 {left_fin}, {right_fin}")
        overflow = self.operation(f"and i1 {is_inf}, {both_fin}")
        failed_ovf, ok_label = self.fresh("pow.ovf")[1:], self.fresh("pow.ok")[1:]
        self.line(
            f"br i1 {overflow}, label %{failed_ovf}, label %{ok_label}, {vops.COLD_BRANCH}"
        )
        self.lines.append(failed_ovf + ":")
        self.line("call i32 @mule_native_overflow() cold noinline")
        self.line(f"br label %{self.failure_target()}")
        self.lines.append(ok_label + ":")
        return result

    def field_address(self, tn, owner: str, name: str) -> str:
        layout = tn.primitive_type.layout
        index = layout.info.fields.index(name)
        slot = self.operation(f"getelementptr i8, ptr {owner}, i64 32")
        payload = self.operation(f"load ptr, ptr {slot}, !invariant.load !{{}}")
        return self.operation(f"getelementptr i64, ptr {payload}, i64 {index}")

    def bind_method(self, receiver: str, name: str) -> str:
        """Materialize an attribute now while the caller retains its receiver root."""
        attribute = self.operation(self.constant(ast.TextLiteral(name)))
        self.check_pointer(attribute)
        receiver = self.operation(f"call ptr @mule.fast.retain(ptr {receiver})")
        return self.sequence_call("get_attribute", [receiver, attribute])

    def take_native_result(self, value: str) -> str:
        """Transfer the adapter's owned raw result after fallible preparation."""
        self.line("store ptr null, ptr %native.result.owner")
        return value

    def arithmetic(self, opcode: str, left: str, right: str) -> str:
        if opcode in ("add", "sub", "mul"):
            if self.lambda_.policy.safety:
                checked = self.operation(
                    f"call {{i64, i1}} @llvm.s{opcode}.with.overflow.i64(i64 {left}, i64 {right})"
                )
                value = self.operation(f"extractvalue {{i64, i1}} {checked}, 0")
                overflow = self.operation(f"extractvalue {{i64, i1}} {checked}, 1")
                failed, continuation = (
                    self.fresh("overflow")[1:],
                    self.fresh("bounded")[1:],
                )
                self.line(
                    f"br i1 {overflow}, label %{failed}, label %{continuation}, {vops.COLD_BRANCH}"
                )
                self.lines.append(failed + ":")
                self.line("call i32 @mule_native_overflow() cold noinline")
                self.line(f"br label %{self.failure_target('error')}")
                self.lines.append(continuation + ":")
                return value
            return self.operation(f"{opcode} nsw i64 {left}, {right}")
        return self.operation(f"{opcode} i64 {left}, {right}")

    # Calls

    def compiled_call_named(self, name: str, values) -> str:
        """A compiled call of the protocol function named ``name``."""
        return self.compiled_call(self.module.by_name[name], values)

    def compiled_call(
        self, function: Function, values, globals_value="%globals"
    ) -> str:
        """Transfer owned arguments to a compiled body inside a runtime frame.

        The body owns its parameters on every return. Failed frame entry leaves
        them here, so that arm consumes them before returning the error result.
        """
        helper = self.module.compiled_call_helper(function, len(values))
        operands = ", ".join(
            [f"ptr {globals_value}", *(f"ptr {value}" for value in values)]
        )
        return self.operation(f"call ptr {helper}({operands})")

    def sequence_call(self, name: str, values) -> str:
        helper = self.module.protocol_helper(name, len(values))
        operands = ", ".join(f"ptr {value}" for value in values)
        return self.operation(f"call ptr {helper}({operands})")

    def tuple_of(self, result, refs) -> str | None:
        """Build an owned tuple of the given operands, or None when empty.
        The tuple is parked in ``result``'s slot while it is built so error
        cleanup releases it. The returned pointer borrows that slot until taken."""
        if not refs:
            return None
        taken = [self.take(ref) for ref in refs]
        self.argv(taken)
        built = self.operation(
            f"call ptr @mule_sequence_tuple_owned(i64 {len(taken)}, ptr %argv)"
        )
        self.publish(result, built)
        self.check_pointer(built)
        return self.load(result)

    def call_method(
        self,
        info,
        callable_value,
        receiver,
        arguments,
        unpack_arity: int | None = None,
        argument_refs=(),
        result_contract=None,
    ) -> str:
        """Consume receiver/arguments on direct, fallback, and rejected arms."""
        module = self.module
        if (
            info.target is not None
            and not info.keywords
            and unpack_arity is None
            and info.target.call_order((), len(arguments) + 1) is not None
        ):
            return self.call_resolved_target(
                info,
                callable_value,
                receiver,
                arguments,
                argument_refs,
                result_contract=result_contract,
            )
        candidates = module.method_targets.get(info.name, ())
        functions = tuple(
            target
            for target in candidates
            if any(
                target.call_order(info.keywords, len(arguments), receiver=bound)
                is not None
                for bound in (True, False)
            )
        )
        if not functions:
            if not info.keywords:
                self.argv(arguments)
                buf = "%argv" if arguments else "null"
                return self.operation(
                    f"call ptr @mule_call_resolved_owned(ptr {callable_value}, ptr {receiver}, i64 {len(arguments)}, ptr {buf})"
                )
            helper = module.keyword_call_helper(info.keywords, resolved=True)
            operands = ", ".join(
                [
                    f"ptr {callable_value}",
                    f"ptr {receiver}",
                    *(f"ptr {value}" for value in arguments),
                ]
            )
            return self.operation(f"call ptr {helper}({operands})")
        supplied = (
            info.receiver_type,
            *((ref.tn.type or UNKNOWN) for ref in argument_refs),
        )
        helper = module.method_call_helper(
            functions,
            info.keywords,
            len(arguments),
            supplied,
            result_contract=result_contract,
        )
        operands = ", ".join(
            [
                f"ptr {callable_value}",
                f"ptr {receiver}",
                *(f"ptr {value}" for value in arguments),
            ]
        )
        return self.operation(f"call ptr {helper}({operands})")

    def argument_guard(self, value: str, expected: ValueType) -> str:
        """Check representation/range without touching or invoking an object."""
        return expected.accept(_ArgumentGuard(self, value))

    def call_resolved_target(
        self,
        info,
        callable_value,
        receiver,
        arguments,
        argument_refs,
        *,
        result_contract=None,
    ) -> str:
        """Call a method analysis resolved from the receiver's class: guard
        that resolution produced that very function, then enter its body
        with the receiver as the first argument, choosing an entry context
        by argument types as a guarded global call does. Any other callable
        takes the general resolved-call path."""
        module = self.module
        target = info.target
        values = (receiver, *arguments)
        supplied = (
            info.receiver_type,
            *((ref.tn.type or UNKNOWN) for ref in argument_refs),
        )
        # Parameters the call leaves to their defaults have no argument type;
        # as for global calls, such calls only take the general body.
        order = target.call_order((), len(values))
        if any(index < 0 for index in order):
            steps = ((None, ()),)
        else:
            steps = entry_steps(module, target, (supplied[index] for index in order))
        helper = module.direct_call_helper(
            target,
            (),
            len(values),
            steps,
            receiver_call=True,
            result_contract=result_contract,
        )
        operands = ", ".join(
            [
                "ptr %globals",
                f"ptr {callable_value}",
                *(f"ptr {value}" for value in values),
            ]
        )
        return self.operation(f"call ptr {helper}({operands})")

    def call(
        self,
        site,
        callee: str,
        argument_refs,
        arguments,
        *,
        unpack_arity: int | None = None,
        result_contract=None,
    ) -> str:
        """Dispatch once, consuming owned arguments on every arm.

        Direct entries take parameter ownership. External fallback borrows the
        original evaluation-order buffer, so that arm releases its references.
        The separately owned callable stays with the caller across all arms.
        """
        module = self.module
        keywords = site.keywords
        target = site.target
        function = (
            target
            if target is not None
            and target.call_order(keywords, len(arguments)) is not None
            else None
        )
        if function is None:
            helper = (
                module.keyword_call_helper(keywords, resolved=False)
                if keywords
                else module.positional_call_helper(len(arguments))
            )
            operands = ", ".join(
                [f"ptr {callee}", *(f"ptr {value}" for value in arguments)]
            )
            return self.operation(f"call ptr {helper}({operands})")
        order = function.call_order(keywords, len(arguments))
        default_types = function.default_types
        argument_types = tuple(
            (argument_refs[index].tn.type or UNKNOWN) if index >= 0 else None
            for index in order
        )
        supplied_sources = [source for source in argument_types if source is not None]
        if len(supplied_sources) != len(order):
            omitted_defaults = [
                default_types[def_idx]
                if 0 <= (def_idx := -2 - index) < len(default_types)
                else UNKNOWN
                for index in order
                if index < 0
            ]
            if any(def_t.has_float_specialization() for def_t in omitted_defaults):
                full_types = [
                    (argument_refs[index].tn.type or UNKNOWN)
                    if index >= 0
                    else (
                        default_types[def_idx]
                        if 0 <= (def_idx := -2 - index) < len(default_types)
                        else UNKNOWN
                    )
                    for index in order
                ]
                steps = entry_steps(module, function, full_types)
            else:
                steps = ((None, ()),)
        else:
            steps = entry_steps(module, function, supplied_sources)
        if unpack_arity is not None:
            helper = module.unpacked_direct_call_helper(
                function,
                keywords,
                len(arguments),
                unpack_arity,
                tuple(steps),
                proven=site.proven and not keywords,
                result_contract=result_contract,
                local_environment=site.local_environment,
            )
            operands = ", ".join(
                [
                    "ptr %globals",
                    f"ptr {callee}",
                    *(f"ptr {value}" for value in arguments),
                    "ptr %unpack.out",
                ]
            )
            return self.operation(f"call ptr {helper}({operands})")
        proven = site.proven and (not keywords or site.final_binding)
        helper = module.direct_call_helper(
            function,
            keywords,
            len(arguments),
            tuple(steps),
            proven=proven,
            same_namespace=(
                proven
                and runtime_namespace(function.namespace)
                == runtime_namespace(self.lambda_.namespace)
            ),
            borrowed_arguments=frozenset(
                index
                for index, ref in enumerate(argument_refs)
                if ref.tn.borrowed_from is not None
                and ref.tn.sc.owned
                and index in site.borrowable_arguments
            ),
            local_environment=site.local_environment,
            result_contract=result_contract,
        )
        operands = ", ".join(
            [
                "ptr %globals",
                f"ptr {callee}",
                *(f"ptr {value}" for value in arguments),
            ]
        )
        return self.operation(f"call ptr {helper}({operands})")

    # Function emission

    def function_comment(self, entry: str) -> str:
        return _function_comment(self.module.code(self.lambda_), entry)

    def operation_comment(self, vop) -> str:
        node = vop.node
        source = None if node is None else node.source
        location = "compiler-generated"
        if source is not None and source.span is not None:
            start, end = source.span.start, source.span.end
            location = f"{start.line}:{start.column}-{end.line}:{end.column}"
        operation = None if node is None else node.accept(_PROVENANCE_OPERATION)
        origin = f"source: {location}"
        if operation is not None:
            origin += f"; operation: {operation}"
        return _llvm_comment(f"{origin}; VOP: {vop.info.name}")

    def emit(self) -> str:
        if self.raw:
            return self.emit_raw()
        wrapper = (
            self.wrapper()
            if self.context.id is None
            and not self.module.local_environments.is_local(self.lambda_)
            and not self.unpacked
            and self.convention is None
            and self.context.rejected_fixnum_guard is None
            else ""
        )
        adapter = ""
        # Adapt an object entry only when reboxing preserves the returned
        # object's semantics. Otherwise its ordinary body stays authoritative;
        # the raw entry remains available to native callers.
        linkage = (
            ""
            if self.module.program.exports
            and not self.raw
            and not self.unpacked
            and self.convention is None
            and (
                self.context.id is None
                or self.context.id
                == _declared_protocol_context(
                    self.lambda_, self.module.facts.get(self.lambda_)
                )
            )
            and self.context.rejected_fixnum_guard is None
            else "internal "
        )
        plan = (
            self.module.native_plans.get(self.lambda_)
            if self.convention is None
            and not self.unpacked
            and self.context.rejected_fixnum_guard is None
            else None
        )
        if plan is not None:
            adapt_return = linked(
                plan.returned, plan, "returned representation"
            ).can_adapt_return(self.component, plan)
            adapter = native_adapter(self, plan, linkage, adapt_return=adapt_return)
            self.body = _suffixed(self.body, ".boxed")
            linkage = "internal "
            self.lines = []
        lambda_ = self.lambda_
        parameter_scs = (
            self.convention.parameters
            if self.convention is not None
            else (DESCRIPTOR,) * len(lambda_.vars)
        )
        returned = (
            self.convention.returned if self.convention is not None else DESCRIPTOR
        )
        parameters = ", ".join(
            ["ptr %globals"]
            + (
                ["ptr %error.flag"]
                if self.convention is not None
                and self.convention.returned is not DESCRIPTOR
                else []
            )
            + (["ptr %closure"] if self.module.closure_of(lambda_) else [])
            + [
                f"{sc.llvm_type} %parameter.{index}"
                for index, sc in enumerate(parameter_scs)
            ]
            + (["ptr %tuple.out"] if self.unpacked else [])
        )
        entry = "general body"
        if self.unpacked:
            entry = "unpacked return body"
        elif self.convention is not None:
            entry = "internal numeric body"
        elif self.context.rejected_fixnum_guard is not None:
            entry = "rejected fixnum guard fallback body"
        elif plan is not None:
            entry = "boxed fallback body"
        elif self.context.id is not None:
            entry = "specialized body"
        self.lines.append(
            self.function_comment(f"{entry}; context: {self.context.id or 'general'}")
        )
        fn_attrs = (
            " cold noinline"
            if str(self.context.id or "").startswith("fallback.final.")
            else ""
        )
        self.lines.append(
            f"define {linkage}{returned.llvm_type} {self.body}({parameters}){fn_attrs} {{"
        )
        self.lines.append("entry:")
        self.line(f"%return = alloca {returned.llvm_type}")
        self.line(f"store {returned.llvm_type} {returned.zero}, ptr %return")
        for group in (*self.local_slot_groups, *self.value_slot_groups):
            tn = group[0]
            self.line(f"{tn.slot} = alloca {tn.sc.llvm_type}")
            self.line(f"store {tn.sc.llvm_type} {tn.sc.zero}, ptr {tn.slot}")
        for tn in self.range_tns:
            self.line(f"{tn.slot} = alloca [3 x i64]")
            self.line(f"store [3 x i64] zeroinitializer, ptr {tn.slot}")
        if self.range_tns:
            self.line("%range.buffer = alloca [3 x i64]")
        if self.coroutine_local_tns:
            n_locals = len(self.coroutine_local_tns)
            self.line(f"%coroutine.locals = alloca [{n_locals} x ptr]")
            self.line(
                f"store [{n_locals} x ptr] zeroinitializer, ptr %coroutine.locals"
            )
        records_frame = lambda_.policy.records_frame or (
            lambda_.policy.debug >= 1
            and (
                (lambda_.code is not None and bool(lambda_.code.flags & 0x2A0))
                or lambda_ in self.module.may_call_external
            )
        )
        if records_frame:
            self.line("%frame = alloca {ptr, ptr, ptr}, align 8")
        self.entry_end = len(self.lines)
        if self.context.final_bindings:
            ready = "true"
            for binding in self.context.final_bindings:
                cell = self.module.cell(binding.namespace, binding.name)
                state = self.operation(
                    f"getelementptr {{ptr, i64, ptr, ptr}}, ptr {cell}, i32 0, i32 1"
                )
                flags = self.operation(f"load i64, ptr {state}")
                sealed = self.operation(f"and i64 {flags}, 2")
                present = self.operation(f"icmp ne i64 {sealed}, 0")
                ready = (
                    present
                    if ready == "true"
                    else self.operation(f"and i1 {ready}, {present}")
                )
            self.line(
                f"br i1 {ready}, label %final.ready, label %final.fallback, {vops.HOT_BRANCH}"
            )
            self.lines.append("final.fallback:")
            operands = ["ptr %globals"]
            if (
                self.convention is not None
                and self.convention.returned is not DESCRIPTOR
            ):
                operands.append("ptr %error.flag")
            if self.module.closure_of(lambda_):
                operands.append("ptr %closure")
            operands.extend(
                f"{sc.llvm_type} %parameter.{index}"
                for index, sc in enumerate(parameter_scs)
            )
            if self.unpacked:
                operands.append("ptr %tuple.out")
            fallback = self.module.specialized[lambda_, self.context.final_fallback]
            result = self.operation(
                f"call {returned.llvm_type} {fallback}({', '.join(operands)})"
            )
            self.line(f"ret {returned.llvm_type} {result}")
            self.lines.append("final.ready:")
        if self.context.id is not None:
            for index, fact in enumerate(self.context.parameters):
                if parameter_scs[index] is not DESCRIPTOR:
                    continue
                value = f"%parameter.{index}"
                if fact.is_fixnum or fact.is_immortal:
                    self.immortal_values.add(value)
                self.refine_type(fact, value)
        if self.coroutine_local_tns:
            n_locals = len(self.coroutine_local_tns)
            self.line(
                f"call void @mule_coroutine_set_locals(ptr %coroutine.locals, i64 {n_locals})"
            )
        if records_frame:
            # Publish only a fully initialized stack record, before any operation
            # that can enter cleanup. Globals remain borrowed from the callable.
            self.line("%frame.slot = call ptr @mule_frame_head_slot()")
            self.line(
                "%frame.parent = load atomic ptr, ptr %frame.slot acquire, align 8"
            )
            self.line("store ptr %frame.parent, ptr %frame")
            self.line(
                "%frame.code = getelementptr {ptr, ptr, ptr}, ptr %frame, i32 0, i32 1"
            )
            self.line(
                f"store ptr {self.module.code_descriptor(lambda_)}, ptr %frame.code"
            )
            self.line(
                "%frame.globals = getelementptr {ptr, ptr, ptr}, ptr %frame, i32 0, i32 2"
            )
            self.line("store ptr %globals, ptr %frame.globals")
            self.line("store atomic ptr %frame, ptr %frame.slot release, align 8")
        if lambda_ is self.module.program.entry and self.context.id is None:
            # The runtime creates the main program's globals and enters its
            # top-level body once, before any function it defines can exist.
            self.lines += self.module.link(lambda_.namespace, "%globals", "error", 0)
            self.line(
                f"call void @mule_module_loaded(ptr {self.module.text('__main__')}, ptr %globals)"
            )
        self.line(f"br label %{self.component.blocks[0].label}")
        self.emit_blocks()
        for label, groups in self.unwind_groups_by_target.items():
            self.lines.append("unwind." + label + ":")
            # Values the handler still uses were computed before its region
            # was entered, like the stack below an SBCL catch block; only the
            # temporaries the transfer abandons are released.
            for group in groups:
                tn = group[0]
                value = self.operation(f"load ptr, ptr {tn.slot}")
                self.line(f"store ptr null, ptr {tn.slot}")
                self.record_facts(tn, value)
                if tn.sc.owned:
                    self.release(value)
            self.line(f"br label %{label}")
        self.emit_landings()
        self.lines.append("error:")
        if self.convention is not None and self.convention.returned is not DESCRIPTOR:
            self.line("store i1 true, ptr %error.flag")
        self.line("br label %cleanup")
        self.lines.append("cleanup:")
        cleanup_groups = [
            group
            for group in (*self.local_slot_groups, *self.value_slot_groups)
            if _needs_cleanup_release(group[0])
        ]
        for group in cleanup_groups:
            tn = group[0]
            loaded = self.operation(f"load ptr, ptr {tn.slot}")
            self.record_facts(tn, loaded)
            self.release(loaded)
        if records_frame:
            # Suspension can change both the thread and the bottom record's parent.
            # Neither value may be reused from entry when the activation returns.
            self.line("%frame.exit.slot = call ptr @mule_frame_head_slot()")
            self.line("%frame.exit.parent = load ptr, ptr %frame")
            self.line(
                "store atomic ptr %frame.exit.parent, ptr %frame.exit.slot release, align 8"
            )
        value = self.operation(f"load {returned.llvm_type}, ptr %return")
        self.line(f"ret {returned.llvm_type} {value}")
        self.lines.append("}")
        return wrapper + "\n\n" + adapter + "\n\n" + "\n".join(self.lines)

    @property
    def cold_blocks(self) -> set[str]:
        cold: set[str] = set()
        for block in self.component.blocks:
            if block.vops and block.vops[-1].info.name in (
                "raise",
                "reraise",
                "assertion-failure",
            ):
                cold.add(block.label)
        changed = True
        while changed:
            changed = False
            for block in self.component.blocks:
                if block.label in cold or not block.vops:
                    continue
                last = block.vops[-1]
                if last.info.name == "jump" and last.codegen_info in cold:
                    cold.add(block.label)
                    changed = True
        return cold

    def emit_blocks(self) -> None:
        cold_blocks = self.cold_blocks
        for block in self.component.blocks:
            self.block = block
            self.in_cold_block = block.label in cold_blocks
            self.lines.append(block.label + ":")
            target = block.exception_target
            self.exception_target = (
                self.component.ir2_blocks[target].label
                if target is not None and target in self.component.ir2_blocks
                else None
            )
            self.line(
                _llvm_comment(
                    f"IR2 block: {block.label}; "
                    f"exception target: {self.exception_target or 'function error'}"
                )
            )
            previous_origin = None
            for vop in block.vops:
                self.vop = vop
                origin = self.operation_comment(vop)
                if origin != previous_origin and vop.info.name not in (
                    "load-local",
                    "move",
                    "jump",
                    "branch",
                    "constant",
                    "discard",
                    "move-argument",
                    "insert-safepoint",
                    "return-value",
                    "unbind-local",
                ):
                    self.line(origin)
                    previous_origin = origin
                self.unpacked_remaining_slots = (
                    self.component.tuple_delivery.remaining.get(vop, ())
                )
                vop.info.generator(self, vop)
                for tn in self.expiring.get(vop, ()):
                    value = self.live.pop(tn, None)
                    if value is None:
                        continue
                    if vop.info.name in vops.TERMINATORS:
                        raise ValueError(f"{vop!r} ends its block owning {tn!r}")
                    self.release(value)
            if self.live:
                raise ValueError(f"{block!r} ends owning {list(self.live)!r}")
        self.block = None
        self.in_cold_block = False
        if self.max_unpack_out:
            self.lines.insert(
                self.entry_end, f"  %unpack.out = alloca [{self.max_unpack_out} x ptr]"
            )
        if self.max_argv:
            self.lines.insert(
                self.entry_end, f"  %argv = alloca [{self.max_argv} x ptr]"
            )
        if self.uses_reuse:
            self.lines.insert(self.entry_end, "  %reuse.slot = alloca ptr")
        if self.uses_out:
            self.lines.insert(self.entry_end, "  %out = alloca ptr")
        if self.uses_float_scratch:
            self.lines.insert(self.entry_end, "  %float.scratch = alloca double")
        if self.uses_numeric_error:
            self.lines.insert(self.entry_end, "  %numeric.error = alloca i1")
            self.lines.insert(self.entry_end, "  %callee.globals.slot = alloca ptr")

    def emit_raw(self) -> str:
        """Native entry: borrowed raw slots and scalars, ownership only at edges."""
        plan = self.plan
        parameters = ["ptr %globals", "ptr %error.flag"]
        parameters.extend(
            f"{rep.llvm_type} %parameter.{index}"
            for index, rep in enumerate(plan.parameters)
        )
        parameters.extend(f"ptr {value}" for value in self.dependencies.values())
        returned = plan.returned
        self.lines = [
            self.function_comment("native body; context: native"),
            f"define internal {returned.llvm_type} {plan.entry}({', '.join(parameters)}) {{",
            "entry:",
        ]
        self.line(f"%return = alloca {returned.llvm_type}")
        self.line(f"store {returned.llvm_type} {returned.zero}, ptr %return")
        self.line("%call.error = alloca i1")
        for group in (*self.local_slot_groups, *self.value_slot_groups):
            tn = group[0]
            self.line(f"{tn.slot} = alloca {tn.sc.llvm_type}")
            if tn.sc in OBJECT:
                self.line(f"store ptr null, ptr {tn.slot}")
        for slot in self.owner_slots.values():
            self.line(f"{slot} = alloca ptr")
            self.line(f"store ptr null, ptr {slot}")
        self.entry_end = len(self.lines)
        self.line(f"br label %{self.component.blocks[0].label}")
        self.emit_blocks()
        self.emit_landings()
        self.lines.append("error:")
        self.line("store i1 true, ptr %error.flag")
        self.line("br label %cleanup")
        self.lines.append("cleanup:")
        for slot in self.owner_slots.values():
            owner = self.operation(f"load ptr, ptr {slot}")
            self.line(f"call void @mule.fast.release(ptr {owner})")
        result = self.operation(f"load {returned.llvm_type}, ptr %return")
        self.line(f"ret {returned.llvm_type} {result}")
        self.lines.append("}")
        return "\n".join(self.lines)

    def wrapper_closure(self, lines) -> list[str]:
        """Fetch the running function's closure from the argument view."""
        if not self.module.closure_of(self.lambda_):
            return []
        lines.append("  %closure = call ptr @mule_closure(ptr %args)")
        return ["ptr %closure"]

    def wrapper_body_call(
        self, lines: list[str], param_names: list[str], arguments: str
    ) -> None:
        lambda_ = self.lambda_
        module = self.module
        fixnum_variant = next(
            (v for v in module.variants_of(lambda_) if v.id == "fixnum"), None
        )
        general_body = module.bodies[lambda_]
        if (
            param_names
            and not module.program.bootstrap
            and module.native_plans.get(lambda_) is None
            and fixnum_variant is not None
        ):
            builder = _HelperBuilder(self.module)
            combined = None
            guard = FixnumGuard.from_parameters(fixnum_variant.parameters)
            guard_ok = None if guard is None else guard.emit(builder, param_names)
            for index, (name, expected) in enumerate(
                zip(param_names, fixnum_variant.parameters, strict=True)
            ):
                if expected is UNKNOWN or guard is not None:
                    continue
                if (
                    expected.is_fixnum
                    and expected.minimum == FIXNUM_MIN
                    and expected.maximum == FIXNUM_MAX
                ):
                    bits = f"%fix.bits.{index}"
                    builder.line(f"{bits} = ptrtoint ptr {name} to i64")
                    if combined is None:
                        combined = bits
                    else:
                        next_combined = f"%fix.tags.{index}"
                        builder.line(f"{next_combined} = and i64 {combined}, {bits}")
                        combined = next_combined
                    continue
                checked = expected.accept(_ArgumentGuard(builder, name))
                if checked == "true":
                    continue
                guard_ok = (
                    checked
                    if guard_ok is None
                    else builder.operation(f"and i1 {guard_ok}, {checked}")
                )
            if combined is not None:
                builder.line(f"%fix.tag = and i64 {combined}, 1")
                builder.line("%fix.ok = icmp ne i64 %fix.tag, 0")
                guard_ok = (
                    "%fix.ok"
                    if guard_ok is None
                    else builder.operation(f"and i1 %fix.ok, {guard_ok}")
                )
            if guard_ok is not None:
                if guard is not None:
                    general_body = module.rejected_fixnum_entry(lambda_, guard)
                lines.extend(builder.lines)
                lines.extend(
                    [
                        f"  br i1 {guard_ok}, label %invoke.fixnum, label %invoke.general",
                        "invoke.fixnum:",
                        f"  %result.fixnum = call ptr {module.specialized[lambda_, 'fixnum']}({arguments})",
                        "  ret ptr %result.fixnum",
                        "invoke.general:",
                    ]
                )
        lines.extend(
            [
                f"  %result = call ptr {general_body}({arguments})",
                "  ret ptr %result",
            ]
        )

    def binding_wrapper(self, *, ordinary_view: bool = False) -> str:
        lambda_ = self.lambda_
        module = self.module
        count = len(lambda_.vars)
        kinds = lambda_.signature.kinds
        from mule import ast

        positional = sum(
            kind
            in (
                ast.ParameterKind.POSITIONAL_ONLY,
                ast.ParameterKind.POSITIONAL_OR_KEYWORD,
            )
            for kind in kinds
        )
        default_offsets = {
            index: offset
            for offset, index in enumerate(lambda_.signature.default_parameters)
        }
        rest_idx = next(
            (
                index
                for index, kind in enumerate(kinds)
                if kind == ast.ParameterKind.VAR_POSITIONAL
            ),
            None,
        )
        keywords_idx = next(
            (
                index
                for index, kind in enumerate(kinds)
                if kind == ast.ParameterKind.VAR_KEYWORD
            ),
            None,
        )
        # Without supplied keywords, optional keyword-only parameters use
        # their captured defaults and ``**kwargs`` receives a fresh dictionary.
        fast = all(
            index in default_offsets
            for index, kind in enumerate(kinds)
            if kind == ast.ParameterKind.KEYWORD_ONLY
        )
        required = sum(index not in default_offsets for index in range(positional))
        entry = module.functions[lambda_]
        view_entry = _suffixed(entry, ".view")
        header = (
            f"define internal ptr {view_entry}(ptr %globals, ptr %args) {{"
            if ordinary_view
            else f"define ptr {entry}(ptr %globals, ptr %args) {{"
        )
        lines = [
            self.function_comment(
                "ordinary argument view"
                if ordinary_view
                else "external argument wrapper"
            ),
            header,
            "entry:",
        ]
        binding = _suffixed(module.functions[lambda_], ".binding")
        if fast:
            if not ordinary_view:
                lines += [
                    "  %ordinary = call i1 @mule.argument_view.is_view(ptr %args)",
                    f"  br i1 %ordinary, label %view, label %other, {vops.HOT_BRANCH}",
                    "view:",
                    f"  %view.result = tail call ptr {view_entry}(ptr %globals, ptr %args)",
                    "  ret ptr %view.result",
                    "other:",
                ]
            count_helper = (
                "mule.argument_view.positional"
                if ordinary_view
                else "mule_binding_positional"
            )
            take_helper = "mule.argument_view.take" if ordinary_view else "mule_arg"
            default_helper = (
                "mule.argument_view.arg_or_default"
                if ordinary_view
                else "mule_arg_or_default"
            )
            lines += [
                f"  %plain.count = call i32 @{count_helper}(ptr %args, i32 {required}, i32 {positional}, i32 {int(rest_idx is not None)})",
                "  %fits = icmp sge i32 %plain.count, 0",
                f"  br i1 %fits, label %plain, label %binding, {vops.HOT_BRANCH}",
                "plain:",
            ]
            if rest_idx is not None:
                rest_var = lambda_.vars[rest_idx]
                if rest_var.refs or rest_var.sets or rest_var.indirect:
                    lines += [
                        f"  %rest.more = icmp sgt i32 %plain.count, {positional}",
                        f"  %rest.start = select i1 %rest.more, i32 {positional}, i32 %plain.count",
                        f"  %plain.{rest_idx} = call ptr @mule_rest_args(ptr %args, i32 %rest.start)",
                        f"  %plain.missing = icmp eq ptr %plain.{rest_idx}, null",
                        f"  br i1 %plain.missing, label %failed, label %plain.ok, {vops.COLD_BRANCH}",
                        "plain.ok:",
                    ]
                else:
                    lines.append(
                        f"  %plain.{rest_idx} = getelementptr i8, ptr null, i64 0"
                    )
            if keywords_idx is not None:
                kw_var = lambda_.vars[keywords_idx]
                if kw_var.refs or kw_var.sets or kw_var.indirect:
                    build_dict = module.protocol_helper("build_dict", 1)
                    lines += [
                        f"  %plain.empty = {module.constant_tuple(ast.Tuple([]))}",
                        "  %plain.empty.missing = icmp eq ptr %plain.empty, null",
                        f"  br i1 %plain.empty.missing, label %plain.cleanup, label %plain.dictionary, {vops.COLD_BRANCH}",
                        "plain.dictionary:",
                        f"  %plain.{keywords_idx} = call ptr {build_dict}(ptr %plain.empty)",
                        f"  %plain.dictionary.missing = icmp eq ptr %plain.{keywords_idx}, null",
                        f"  br i1 %plain.dictionary.missing, label %plain.cleanup, label %plain.ready, {vops.COLD_BRANCH}",
                        "plain.cleanup:",
                    ]
                    if rest_idx is not None:
                        lines.append(
                            f"  call void @mule.fast.release(ptr %plain.{rest_idx})"
                        )
                    lines += [
                        "  br label %failed",
                        "plain.ready:",
                    ]
                else:
                    lines.append(
                        f"  %plain.{keywords_idx} = getelementptr i8, ptr null, i64 0"
                    )
            for index, kind in enumerate(kinds):
                if kind in (
                    ast.ParameterKind.VAR_POSITIONAL,
                    ast.ParameterKind.VAR_KEYWORD,
                ):
                    continue
                if kind == ast.ParameterKind.KEYWORD_ONLY:
                    lines.append(
                        f"  %plain.{index} = call ptr @mule_binding_default_at(ptr %args, i32 {default_offsets[index]})"
                    )
                elif index < required:
                    lines.append(
                        f"  %plain.{index} = call ptr @{take_helper}(ptr %args, i32 {index})"
                    )
                else:
                    lines.append(
                        f"  %plain.{index} = call ptr @{default_helper}(ptr %args, i32 {index}, i32 {default_offsets[index]})"
                    )
            lines.append("  br label %invoke")
        else:
            lines.append("  br label %binding")
        lines += [
            "binding:",
            f"  %bound.result = tail call ptr {binding}(ptr %globals, ptr %args)",
            "  ret ptr %bound.result",
        ]
        if fast:
            lines.append("invoke:")
            param_names = [f"%plain.{index}" for index in range(count)]
            arguments = ", ".join(
                [
                    "ptr %globals",
                    *self.wrapper_closure(lines),
                    *(f"ptr {name}" for name in param_names),
                ]
            )
            self.wrapper_body_call(lines, param_names, arguments)
        lines += [
            "failed:",
            "  ret ptr null",
            "}",
        ]
        # Keep keyword buffers and variable-sized scratch out of the ordinary
        # positional entry. Do not assume keywords themselves are infrequent.
        if ordinary_view:
            return "\n".join(lines)
        bound_lines = [
            self.function_comment("keyword binding wrapper"),
            f"define internal ptr {binding}(ptr %globals, ptr %args) noinline {{",
            "entry:",
            "  br label %binding",
            *self.keyword_entry(positional, default_offsets, rest_idx, keywords_idx),
            "invoke:",
        ]
        param_names = [f"%bound.{index}" for index in range(count)]
        arguments = ", ".join(
            [
                "ptr %globals",
                *self.wrapper_closure(bound_lines),
                *(f"ptr {name}" for name in param_names),
            ]
        )
        self.wrapper_body_call(bound_lines, param_names, arguments)
        bound_lines += ["failed:", "  ret ptr null", "}"]
        view = self.binding_wrapper(ordinary_view=True) if fast else ""
        return "\n".join(lines + [""] + bound_lines + [view])

    def keyword_entry(
        self,
        positional: int,
        default_offsets: dict[int, int],
        rest_idx: int | None,
        keywords_idx: int | None,
    ) -> list[str]:
        """The ``binding:`` blocks of the external entry: this function's own
        keyword parser, after SBCL's ``convert-more-entry``. Each supplied
        keyword is matched against this function's parameter names, first by
        the address of the unit's shared name constant and then by text, and
        stored straight into its parameter slot. Unmatched keywords go to
        ``**kwargs``. Ends defining ``%bound.N`` for every parameter, owned,
        and branching from ``%bound.ok`` to ``%invoke``."""
        lambda_ = self.lambda_
        module = self.module
        from mule import ast

        kinds = lambda_.signature.kinds
        names = lambda_.parameters
        count = len(names)
        keyword_params = [
            index
            for index, kind in enumerate(kinds)
            if kind
            in (ast.ParameterKind.POSITIONAL_OR_KEYWORD, ast.ParameterKind.KEYWORD_ONLY)
        ]
        table = module.names(tuple(names[index] for index in keyword_params))
        variadic = {rest_idx, keywords_idx} - {None}
        lines = [
            "binding:",
            f"  %kw.slots = alloca [{count} x ptr]",
            f"  %kw.positional = alloca [{max(positional, 1)} x ptr]",
            "  %kw.count.slot = alloca i64",
            "  %kw.names.slot = alloca ptr",
            "  %kw.values.slot = alloca ptr",
            (
                "  %kw.count = call i64 @mule_binding_view(ptr %args, ptr %kw.positional, "
                f"i64 {positional}, ptr %kw.count.slot, ptr %kw.names.slot, "
                "ptr %kw.values.slot)"
            ),
            "  %kw.view.bad = icmp slt i64 %kw.count, 0",
            f"  br i1 %kw.view.bad, label %failed, label %kw.view.ok, {vops.COLD_BRANCH}",
            "kw.view.ok:",
            "  %kw.npos = load i64, ptr %kw.count.slot",
            "  %kw.names = load ptr, ptr %kw.names.slot",
            "  %kw.values = load ptr, ptr %kw.values.slot",
            "  %kw.extra = alloca i8, i64 %kw.count",
        ]
        if rest_idx is None:
            lines += [
                f"  %kw.too.many = icmp sgt i64 %kw.npos, {positional}",
                f"  br i1 %kw.too.many, label %kw.too.many.error, label %kw.slots.init, {vops.COLD_BRANCH}",
                "kw.too.many.error:",
                "  call void @mule_binding_error(i32 0, ptr null)",
                "  br label %failed",
            ]
        else:
            lines.append("  br label %kw.slots.init")
        lines.append("kw.slots.init:")
        for index in range(count):
            lines.append(
                f"  %kw.slot.{index} = getelementptr [{count} x ptr], ptr %kw.slots, i64 0, i64 {index}"
            )
            if index < positional:
                lines += [
                    f"  %kw.given.{index} = icmp sgt i64 %kw.npos, {index}",
                    f"  %kw.pos.addr.{index} = getelementptr ptr, ptr %kw.positional, i64 {index}",
                    f"  %kw.pos.{index} = load ptr, ptr %kw.pos.addr.{index}",
                    f"  %kw.init.{index} = select i1 %kw.given.{index}, ptr %kw.pos.{index}, ptr null",
                    f"  store ptr %kw.init.{index}, ptr %kw.slot.{index}",
                ]
            else:
                lines.append(f"  store ptr null, ptr %kw.slot.{index}")
        # Each supplied keyword: find its parameter, then bind or reject it.
        fast = []
        previous = "-1"
        for position, index in enumerate(keyword_params):
            fast += [
                f"  %kw.is.{position} = icmp eq ptr %kw.name, {module.text(names[index])}",
                f"  %kw.fast.{position} = select i1 %kw.is.{position}, i64 {position}, i64 {previous}",
            ]
            previous = f"%kw.fast.{position}"
        cases = " ".join(
            f"i64 {position}, label %kw.bind.{position}"
            for position in range(len(keyword_params))
        )
        lines += [
            "  br label %kw.head",
            "kw.head:",
            "  %kw.k = phi i64 [0, %kw.slots.init], [%kw.k.next, %kw.next]",
            "  %kw.done = icmp eq i64 %kw.k, %kw.count",
            "  br i1 %kw.done, label %kw.end, label %kw.body",
            "kw.body:",
            "  %kw.name.addr = getelementptr ptr, ptr %kw.names, i64 %kw.k",
            "  %kw.name = load ptr, ptr %kw.name.addr",
            "  %kw.extra.addr = getelementptr i8, ptr %kw.extra, i64 %kw.k",
            "  store i8 0, ptr %kw.extra.addr",
            *fast,
            f"  %kw.fast = add i64 {previous}, 0",
            "  %kw.miss = icmp eq i64 %kw.fast, -1",
            f"  br i1 %kw.miss, label %kw.slow, label %kw.dispatch, {vops.COLD_BRANCH}",
            "kw.slow:",
            (
                "  %kw.slow.index = call i64 @mule_binding_name_index(ptr %kw.name, "
                f"ptr {table}, i64 {len(keyword_params)})"
            ),
            "  br label %kw.dispatch",
            "kw.dispatch:",
            "  %kw.j = phi i64 [%kw.fast, %kw.body], [%kw.slow.index, %kw.slow]",
            f"  switch i64 %kw.j, label %kw.unknown [{cases}]",
        ]
        for position, index in enumerate(keyword_params):
            lines += [
                f"kw.bind.{position}:",
                f"  %kw.old.{position} = load ptr, ptr %kw.slot.{index}",
                f"  %kw.bound.{position} = icmp ne ptr %kw.old.{position}, null",
                f"  br i1 %kw.bound.{position}, label %kw.duplicate, label %kw.store.{position}, {vops.COLD_BRANCH}",
                f"kw.store.{position}:",
                f"  %kw.value.addr.{position} = getelementptr ptr, ptr %kw.values, i64 %kw.k",
                f"  %kw.value.{position} = load ptr, ptr %kw.value.addr.{position}",
                f"  store ptr %kw.value.{position}, ptr %kw.slot.{index}",
                "  br label %kw.next",
            ]
        lines += [
            "kw.duplicate:",
            "  call void @mule_binding_error(i32 2, ptr %kw.name)",
            "  br label %failed",
            "kw.unknown:",
        ]
        if keywords_idx is not None:
            lines += ["  store i8 1, ptr %kw.extra.addr", "  br label %kw.next"]
        else:
            lines += [
                "  call void @mule_binding_error(i32 1, ptr %kw.name)",
                "  br label %failed",
            ]
        lines += [
            "kw.next:",
            "  %kw.k.next = add i64 %kw.k, 1",
            "  br label %kw.head",
            "kw.end:",
        ]
        # Required parameters nothing supplied.
        for index in range(count):
            if index in variadic or index in default_offsets:
                continue
            lines += [
                f"  %kw.check.{index} = load ptr, ptr %kw.slot.{index}",
                f"  %kw.missing.{index} = icmp eq ptr %kw.check.{index}, null",
                f"  br i1 %kw.missing.{index}, label %kw.missing.error.{index}, label %kw.present.{index}, {vops.COLD_BRANCH}",
                f"kw.missing.error.{index}:",
                f"  call void @mule_binding_error(i32 3, ptr {module.text(names[index])})",
                "  br label %failed",
                f"kw.present.{index}:",
            ]
        # Binding succeeded: acquire owned references.
        owned: list[str] = []

        def fail_block(label: str) -> list[str]:
            return [
                f"{label}:",
                *(f"  call void @mule.fast.release(ptr {value})" for value in owned),
                "  br label %failed",
            ]

        if rest_idx is not None:
            rest_var = lambda_.vars[rest_idx]
            if rest_var.refs or rest_var.sets or rest_var.indirect:
                lines += [
                    f"  %kw.rest.many = icmp sgt i64 %kw.npos, {positional}",
                    f"  %kw.rest.from = select i1 %kw.rest.many, i64 {positional}, i64 %kw.npos",
                    "  %kw.rest.start = trunc i64 %kw.rest.from to i32",
                    f"  %bound.{rest_idx} = call ptr @mule_rest_args(ptr %args, i32 %kw.rest.start)",
                    f"  %kw.rest.bad = icmp eq ptr %bound.{rest_idx}, null",
                    f"  br i1 %kw.rest.bad, label %failed, label %kw.rest.ok, {vops.COLD_BRANCH}",
                    "kw.rest.ok:",
                ]
                owned.append(f"%bound.{rest_idx}")
            else:
                lines.append(f"  %bound.{rest_idx} = getelementptr i8, ptr null, i64 0")
        if keywords_idx is not None:
            kw_var = lambda_.vars[keywords_idx]
            if kw_var.refs or kw_var.sets or kw_var.indirect:
                build_dict = module.protocol_helper("build_dict", 1)
                setitem = module.protocol_helper("setitem", 3)
                extra = f"%bound.{keywords_idx}"
                lines += [
                    f"  %kw.empty = {module.constant_tuple(ast.Tuple([]))}",
                    "  %kw.empty.bad = icmp eq ptr %kw.empty, null",
                    f"  br i1 %kw.empty.bad, label %kw.dict.fail, label %kw.dict.make, {vops.COLD_BRANCH}",
                    "kw.dict.make:",
                    f"  {extra} = call ptr {build_dict}(ptr %kw.empty)",
                    f"  %kw.dict.bad = icmp eq ptr {extra}, null",
                    f"  br i1 %kw.dict.bad, label %kw.dict.fail, label %kw.dict.ok, {vops.COLD_BRANCH}",
                    *fail_block("kw.dict.fail"),
                    "kw.dict.ok:",
                ]
                owned.append(extra)
                lines += [
                    "  br label %kw.extra.head",
                    "kw.extra.head:",
                    "  %kw.e = phi i64 [0, %kw.dict.ok], [%kw.e.next, %kw.extra.next]",
                    "  %kw.e.done = icmp eq i64 %kw.e, %kw.count",
                    "  br i1 %kw.e.done, label %kw.extra.end, label %kw.extra.test",
                    "kw.extra.test:",
                    "  %kw.e.flag.addr = getelementptr i8, ptr %kw.extra, i64 %kw.e",
                    "  %kw.e.flag = load i8, ptr %kw.e.flag.addr",
                    "  %kw.e.set = icmp ne i8 %kw.e.flag, 0",
                    "  br i1 %kw.e.set, label %kw.extra.insert, label %kw.extra.next",
                    "kw.extra.insert:",
                    "  %kw.e.key = call ptr @mule_binding_keyword_text(ptr %args, i64 %kw.e)",
                    "  %kw.e.key.bad = icmp eq ptr %kw.e.key, null",
                    f"  br i1 %kw.e.key.bad, label %kw.extra.fail, label %kw.extra.store, {vops.COLD_BRANCH}",
                    "kw.extra.store:",
                    "  %kw.e.value.addr = getelementptr ptr, ptr %kw.values, i64 %kw.e",
                    "  %kw.e.value.borrowed = load ptr, ptr %kw.e.value.addr",
                    "  %kw.e.value = call ptr @mule.fast.retain(ptr %kw.e.value.borrowed)",
                    f"  %kw.e.dict = call ptr @mule.fast.retain(ptr {extra})",
                    f"  %kw.e.result = call ptr {setitem}(ptr %kw.e.dict, ptr %kw.e.key, ptr %kw.e.value)",
                    "  %kw.e.result.bad = icmp eq ptr %kw.e.result, null",
                    f"  br i1 %kw.e.result.bad, label %kw.extra.fail, label %kw.extra.done, {vops.COLD_BRANCH}",
                    "kw.extra.done:",
                    "  call void @mule.fast.release(ptr %kw.e.result)",
                    "  br label %kw.extra.next",
                    "kw.extra.next:",
                    "  %kw.e.next = add i64 %kw.e, 1",
                    "  br label %kw.extra.head",
                    *fail_block("kw.extra.fail"),
                    "kw.extra.end:",
                ]
            else:
                lines.append(
                    f"  %bound.{keywords_idx} = getelementptr i8, ptr null, i64 0"
                )
        for index in range(count):
            if index in variadic:
                continue
            if index not in default_offsets:
                lines.append(
                    f"  %bound.{index} = call ptr @mule.fast.retain(ptr %kw.check.{index})"
                )
                owned.append(f"%bound.{index}")
                continue
            offset = default_offsets[index]
            lines += [
                f"  %kw.given.value.{index} = load ptr, ptr %kw.slot.{index}",
                f"  %kw.defaulted.{index} = icmp eq ptr %kw.given.value.{index}, null",
                f"  br i1 %kw.defaulted.{index}, label %kw.default.{index}, label %kw.supplied.{index}",
                f"kw.supplied.{index}:",
                f"  %kw.owned.{index} = call ptr @mule.fast.retain(ptr %kw.given.value.{index})",
                f"  br label %kw.acquired.{index}",
                f"kw.default.{index}:",
                f"  %kw.default.value.{index} = call ptr @mule_binding_default_at(ptr %args, i32 {offset})",
                f"  %kw.default.bad.{index} = icmp eq ptr %kw.default.value.{index}, null",
                f"  br i1 %kw.default.bad.{index}, label %kw.default.fail.{index}, label %kw.acquired.{index}, {vops.COLD_BRANCH}",
                *fail_block(f"kw.default.fail.{index}"),
                f"kw.acquired.{index}:",
                (
                    f"  %bound.{index} = phi ptr [%kw.owned.{index}, %kw.supplied.{index}], "
                    f"[%kw.default.value.{index}, %kw.default.{index}]"
                ),
            ]
            owned.append(f"%bound.{index}")
        lines += ["  br label %bound.ok", "bound.ok:", "  br label %invoke"]
        return lines

    def wrapper(self) -> str:
        """General entry validates an argument view and transfers owned arguments."""
        lambda_ = self.lambda_
        module = self.module
        if lambda_.binding is not None:
            return self.binding_wrapper()
        count = len(lambda_.vars)
        variadic = lambda_.signature.variadic
        fixed = count - int(variadic)
        check = "mule_check_rest_args" if variadic else "mule_check_args"
        lines = [
            self.function_comment("external argument wrapper"),
            f"define ptr {module.functions[lambda_]}(ptr %globals, ptr %args) {{",
            "entry:",
            f"  %status = call i32 @{check}(ptr %args, i32 {fixed})",
            "  %wrong = icmp slt i32 %status, 0",
            f"  br i1 %wrong, label %failed, label %checked, {vops.COLD_BRANCH}",
            "checked:",
        ]
        if variadic:
            lines.extend(
                [
                    f"  %arg.{fixed} = call ptr @mule_rest_args(ptr %args, i32 {fixed})",
                    f"  %missing = icmp eq ptr %arg.{fixed}, null",
                    f"  br i1 %missing, label %failed, label %rest.ok, {vops.COLD_BRANCH}",
                    "rest.ok:",
                ]
            )
        for index in range(fixed):
            lines.append(f"  %arg.{index} = call ptr @mule_arg(ptr %args, i32 {index})")
        param_names = [f"%arg.{index}" for index in range(count)]
        arguments = ", ".join(
            [
                "ptr %globals",
                *self.wrapper_closure(lines),
                *(f"ptr {name}" for name in param_names),
            ]
        )
        self.wrapper_body_call(lines, param_names, arguments)
        lines.extend(
            [
                "failed:",
                "  ret ptr null",
                "}",
            ]
        )
        return "\n".join(lines)


def native_adapter(
    gen: FunctionGenerator, plan, linkage: str, *, adapt_return=True
) -> str:
    """Boxed entry checks physical boundaries once, then calls a raw entry."""
    parameters = ", ".join(
        ["ptr %globals", *(f"ptr %parameter.{i}" for i in range(len(plan.parameters)))]
    )
    gen.lines = [
        gen.function_comment("boxed-to-native adapter"),
        f"define {linkage}ptr {gen.body}({parameters}) {{",
        "entry:",
    ]
    gen.line("%result = alloca ptr")
    gen.line("store ptr null, ptr %result")
    gen.line("%native.error = alloca i1")
    gen.line("store i1 false, ptr %native.error")
    if plan.returned.owns_result:
        gen.line("%native.result.owner = alloca ptr")
        gen.line("store ptr null, ptr %native.result.owner")
    dependencies = []
    slots = []
    for index in range(len(plan.dependencies)):
        slot = f"%dependency.{index}"
        slots.append(slot)
        gen.line(f"{slot} = alloca ptr")
        gen.line(f"store ptr null, ptr {slot}")
    arguments = [
        rep.parameter(gen, f"%parameter.{i}") for i, rep in enumerate(plan.parameters)
    ]
    for index, (var, prediction) in enumerate(plan.dependencies.items()):
        value = gen.operation(
            gen.module.load_global(var, gen.lambda_.policy.safety, speculative=True)
        )
        gen.line(f"store ptr {value}, ptr {slots[index]}")
        gen.check_pointer(value)
        matched = gen.operation(
            f"call i32 @mule_native_class_matches(ptr {value}, ptr {prediction.layout.schema}, ptr %globals)"
        )
        wrong = gen.operation(f"icmp eq i32 {matched}, 0")
        continuation = gen.fresh("layout.ok")[1:]
        gen.line(
            f"br i1 {wrong}, label %fallback, label %{continuation}, {vops.COLD_BRANCH}"
        )
        gen.lines.append(continuation + ":")
        dependencies.append(value)
    if not adapt_return:
        gen.line("br label %fallback")
        gen.lines.append("fallback:")
        for slot in slots:
            gen.release(gen.operation(f"load ptr, ptr {slot}"))
        boxed = gen.operation(f"call ptr {_suffixed(gen.body, '.boxed')}({parameters})")
        gen.line(f"ret ptr {boxed}")
        gen.emit_landings()
        gen.landings.clear()
        gen.landing_sites.clear()
        gen.lines.extend(["error:", "  br label %cleanup", "cleanup:"])
        for slot in slots:
            gen.release(gen.operation(f"load ptr, ptr {slot}"))
        for index in range(len(plan.parameters)):
            gen.release(f"%parameter.{index}")
        gen.line("ret ptr null")
        gen.lines.append("}")
        return "\n".join(gen.lines)
    call = ["ptr %globals", "ptr %native.error"]
    call.extend(
        f"{rep.llvm_type} {value}"
        for rep, value in zip(plan.parameters, arguments, strict=True)
    )
    call.extend(f"ptr {value}" for value in dependencies)
    result = gen.operation(
        f"call {plan.returned.llvm_type} {plan.entry}({', '.join(call)})"
    )
    if plan.returned.owns_result:
        gen.line(f"store ptr {result}, ptr %native.result.owner")
    failed = gen.operation("load i1, ptr %native.error")
    gen.guard(failed)
    boxed = plan.returned.box(gen, result)
    gen.line(f"store ptr {boxed}, ptr %result")
    gen.line("br label %cleanup")
    if plan.dependencies:
        gen.lines.append("fallback:")
        for slot in slots:
            gen.release(gen.operation(f"load ptr, ptr {slot}"))
        result = gen.operation(
            f"call ptr {_suffixed(gen.body, '.boxed')}({parameters})"
        )
        gen.line(f"ret ptr {result}")
    # The adapter is its own function: its failure edges land here, and the
    # body generated next starts without them.
    gen.emit_landings()
    gen.landings.clear()
    gen.landing_sites.clear()
    gen.lines.append("error:")
    gen.line("br label %cleanup")
    gen.lines.append("cleanup:")
    if plan.returned.owns_result:
        gen.release(gen.operation("load ptr, ptr %native.result.owner"))
    for slot in slots:
        gen.release(gen.operation(f"load ptr, ptr {slot}"))
    for i in range(len(plan.parameters)):
        gen.release(f"%parameter.{i}")
    result = gen.operation("load ptr, ptr %result")
    gen.line(f"ret ptr {result}")
    gen.lines.append("}")
    return "\n".join(gen.lines)

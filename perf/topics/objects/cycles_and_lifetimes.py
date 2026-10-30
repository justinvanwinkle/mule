"""Cyclic object graph churn and heterogeneous slot/dict attribute mutation workloads.

Covers doubly-linked parent/child/sibling DOM/CFG trees with callback closures,
subtree splicing and detachment, and high-rate attribute access across slotted
and __dict__-backed objects with dynamic setattr/getattr/hasattr/delattr.
"""

import sys
import time
from typing import Any, Callable

DEFAULT_SIZE = 500
DEFAULT_LOOPS = 1

_TAGS = ("div", "section", "article", "span", "p", "ul", "li", "header")
_DYNAMIC_ATTRS = (
    "ext_token",
    "ext_region",
    "ext_trace_id",
    "ext_priority",
    "ext_retry_budget",
    "ext_flags",
)


class CyclicTreeNode:
    def __init__(self, node_id: int, tag: str, weight: int) -> None:
        self.node_id = node_id
        self.tag = tag
        self.weight = weight
        self.parent: CyclicTreeNode | None = None
        self.children: list[CyclicTreeNode] = []
        self.prev_sibling: CyclicTreeNode | None = None
        self.next_sibling: CyclicTreeNode | None = None
        self.peer_link: CyclicTreeNode | None = None
        self.callbacks: list[Callable[[int], int]] = []

    def append_child(self, child: "CyclicTreeNode") -> None:
        child.parent = self
        if self.children:
            last = self.children[-1]
            last.next_sibling = child
            child.prev_sibling = last
        else:
            child.prev_sibling = None
        child.next_sibling = None
        self.children.append(child)

    def detach(self) -> None:
        parent = self.parent
        if parent is None:
            return
        prev_s = self.prev_sibling
        next_s = self.next_sibling
        if prev_s is not None:
            prev_s.next_sibling = next_s
        if next_s is not None:
            next_s.prev_sibling = prev_s
        parent.children.remove(self)
        self.parent = None
        self.prev_sibling = None
        self.next_sibling = None

    def register_default_closure(self, factor: int) -> None:
        node_ref = self

        def _on_visit(delta: int) -> int:
            parent_w = node_ref.parent.weight if node_ref.parent is not None else 0
            peer_w = node_ref.peer_link.weight if node_ref.peer_link is not None else 0
            node_ref.weight = (node_ref.weight + delta + factor) & 0xFFFF
            return node_ref.weight + parent_w + (peer_w & 0xFF)

        self.callbacks.append(_on_visit)


class SlottedTelemetryPoint:
    __slots__ = ("entity_id", "counter", "latency_us", "state_code", "_observers")

    def __init__(self, entity_id: int, counter: int, latency_us: int, state_code: int) -> None:
        self.entity_id = entity_id
        self.counter = counter
        self.latency_us = latency_us
        self.state_code = state_code
        self._observers: list[Callable[[int, int, int], None]] = []

    def add_observer(self, cb: Callable[[int, int, int], None]) -> None:
        self._observers.append(cb)

    def transition(self, new_state: int, latency_delta: int) -> int:
        old_state = self.state_code
        self.counter += 1
        self.latency_us = max(1, self.latency_us + latency_delta)
        self.state_code = new_state
        if old_state != new_state:
            for cb in self._observers:
                cb(self.entity_id, old_state, new_state)
        return self.counter + self.latency_us


class DynamicSessionContext:
    def __init__(self, entity_id: int, counter: int, latency_us: int, state_code: int) -> None:
        self.entity_id = entity_id
        self.counter = counter
        self.latency_us = latency_us
        self.state_code = state_code
        self._observers: list[Callable[[int, int, int], None]] = []

    def add_observer(self, cb: Callable[[int, int, int], None]) -> None:
        self._observers.append(cb)

    def transition(self, new_state: int, latency_delta: int) -> int:
        old_state = self.state_code
        self.counter += 1
        self.latency_us = max(1, self.latency_us + latency_delta)
        self.state_code = new_state
        if old_state != new_state:
            for cb in self._observers:
                cb(self.entity_id, old_state, new_state)
        return self.counter + self.latency_us


NodeSpec = tuple[int, str, int, int, int]
EntitySpec = tuple[int, int, int, int, str, int]


def prepare(size: int) -> tuple[list[NodeSpec], list[EntitySpec]]:
    node_specs: list[NodeSpec] = []
    entity_specs: list[EntitySpec] = []
    state = 419283741

    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        tag = _TAGS[state % len(_TAGS)]
        weight = ((state >> 4) & 0x3FF) + 1
        parent_idx = (state >> 14) % i if i > 0 else -1
        peer_idx = (state >> 20) % size
        node_specs.append((i, tag, weight, parent_idx, peer_idx))

        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        counter = (state >> 3) & 0xFF
        latency = ((state >> 11) & 0x7FF) + 10
        scode = (state >> 22) & 0x7
        dyn_attr = _DYNAMIC_ATTRS[state % len(_DYNAMIC_ATTRS)]
        dyn_val = ((state >> 7) & 0xFFF) + i
        entity_specs.append((i, counter, latency, scode, dyn_attr, dyn_val))

    return node_specs, entity_specs


def scenario_cyclic_graph_churn(node_specs: list[NodeSpec]) -> tuple[int, int, int, int]:
    """Repeatedly construct, splice, traverse, and drop cyclic DOM/CFG node graphs."""
    callback_accum = 0
    spliced_count = 0
    sibling_hops = 0
    survived_weight = 0

    n = len(node_specs)
    for round_idx in range(12300):
        nodes: list[CyclicTreeNode] = []
        for nid, tag, weight, parent_idx, _peer_idx in node_specs:
            node = CyclicTreeNode(nid, tag, weight + round_idx)
            node.register_default_closure((nid + round_idx) & 15)
            if parent_idx >= 0:
                nodes[parent_idx].append_child(node)
            nodes.append(node)

        for idx, (_nid, _tag, _weight, _parent_idx, peer_idx) in enumerate(node_specs):
            target_idx = (peer_idx + round_idx) % n
            nodes[idx].peer_link = nodes[target_idx]

        # Splice and re-parent a subset of subtrees
        for idx in range(1, n):
            if (idx + round_idx) % 5 == 0:
                moving = nodes[idx]
                new_parent_idx = (idx + round_idx + 1) % idx
                moving.detach()
                nodes[new_parent_idx].append_child(moving)
                spliced_count += 1

        # Traverse roots and sibling chains while firing bound closures
        for node in nodes:
            for cb in node.callbacks:
                callback_accum += cb((round_idx & 7) + 1)
            cur = node.children[0] if node.children else None
            while cur is not None:
                sibling_hops += 1
                survived_weight += cur.weight & 0xFF
                cur = cur.next_sibling

        # Explicitly detach every 4th node before releasing the graph
        for idx in range(n - 1, 0, -4):
            nodes[idx].detach()

    return callback_accum, spliced_count, sibling_hops, survived_weight


def scenario_heterogeneous_slot_mutation(
    entity_specs: list[EntitySpec],
) -> tuple[int, int, int, int]:
    """Churn slotted and dict-backed attributes, dynamic attrs, and state observers."""
    transition_total = 0
    observer_events = 0
    dynamic_hits = 0
    dynamic_deletions = 0

    audit_state = [0, 0]

    def _observer(entity_id: int, old_state: int, new_state: int) -> None:
        audit_state[0] += 1
        audit_state[1] += (entity_id + old_state * 3 + new_state * 5) & 0xFF

    objects: list[Any] = []
    for eid, counter, latency, scode, dyn_attr, dyn_val in entity_specs:
        if (eid & 1) == 0:
            obj: Any = SlottedTelemetryPoint(eid, counter, latency, scode)
        else:
            obj = DynamicSessionContext(eid, counter, latency, scode)
            setattr(obj, dyn_attr, dyn_val)
        obj.add_observer(_observer)
        objects.append(obj)

    for step in range(52000):
        delta = ((step & 7) - 3) * 4
        for idx, obj in enumerate(objects):
            next_state = (obj.state_code + step + (idx & 3)) & 7
            transition_total += obj.transition(next_state, delta)

            attr_name = _DYNAMIC_ATTRS[(idx + step) % len(_DYNAMIC_ATTRS)]
            if isinstance(obj, DynamicSessionContext):
                if hasattr(obj, attr_name):
                    val = getattr(obj, attr_name)
                    dynamic_hits += val & 0x7F
                    if (step + idx) % 3 == 0:
                        delattr(obj, attr_name)
                        dynamic_deletions += 1
                    else:
                        setattr(obj, attr_name, val + step + 1)
                else:
                    setattr(obj, attr_name, (idx * 17 + step) & 0x3FF)
            else:
                # Read slotted fields via both direct attribute access and getattr
                slot_val = getattr(obj, "latency_us") + obj.counter
                dynamic_hits += slot_val & 0x3F

    observer_events = audit_state[0] + audit_state[1]
    return transition_total, observer_events, dynamic_hits, dynamic_deletions


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
    node_specs, entity_specs = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    cyclic_s = 0.0
    slot_s = 0.0
    for _ in range(loops):
        r1: tuple[int, ...] = ()
        r2: tuple[int, ...] = ()
        if scenario in ("all", "cyclic_graph_churn"):
            s0 = time.perf_counter()
            r1 = scenario_cyclic_graph_churn(node_specs)
            cyclic_s += time.perf_counter() - s0
        if scenario in ("all", "heterogeneous_slot_mutation"):
            s1 = time.perf_counter()
            r2 = scenario_heterogeneous_slot_mutation(entity_specs)
            slot_s += time.perf_counter() - s1
        combined = r1 + r2
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (cyclic_s + slot_s) / loops
    print("topic\tobjects/cycles_and_lifetimes")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "cyclic_graph_churn"):
        print(f"cyclic_graph_churn\t{cyclic_s / loops:.6f}")
    if scenario in ("all", "heterogeneous_slot_mutation"):
        print(f"heterogeneous_slot_mutation\t{slot_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()

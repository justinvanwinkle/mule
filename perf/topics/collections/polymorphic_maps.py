"""Polymorphic dictionary key probing, collision resolution, and composite-key LRU caching.

Covers heterogeneous dictionary keys (str, fixnum int, >64-bit bigint, tuples,
and custom __hash__/__eq__ objects with controlled hash collisions), dict merge
and comprehension operations, and a doubly-linked LRU + TTL cache wrapper.
"""

import sys
import time
from typing import Any

DEFAULT_SIZE = 500
DEFAULT_LOOPS = 1

_NAMESPACES = ("core", "ir1", "ir2", "runtime", "linker", "gc")
_SYMBOLS = ("alloc_cell", "unbox_i64", "guard_type", "patch_ic", "lookup_slot", "emit_phi")
_ENDPOINTS = ("/v1/search", "/v1/compile", "/v1/metrics", "/v2/resolve")


class SymbolKey:
    __slots__ = ("namespace", "name", "version", "_hash")

    def __init__(self, namespace: str, name: str, version: int, collision_group: int) -> None:
        self.namespace = namespace
        self.name = name
        self.version = version
        # Group keys into controlled hash buckets so probing must run __eq__
        if collision_group > 0:
            self._hash = (collision_group * 0x9E3779B1) & 0x7FFFFFFF
        else:
            self._hash = hash((namespace, name, version))

    def __hash__(self) -> int:
        return self._hash

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, SymbolKey):
            return False
        return (
            self.version == other.version
            and self.name == other.name
            and self.namespace == other.namespace
        )


class _LRUNode:
    __slots__ = ("key", "value", "expires_at", "prev", "next")

    def __init__(self, key: Any, value: int, expires_at: int) -> None:
        self.key = key
        self.value = value
        self.expires_at = expires_at
        self.prev: _LRUNode | None = None
        self.next: _LRUNode | None = None


class CompositeLRUCache:
    def __init__(self, capacity: int, ttl_ticks: int) -> None:
        self.capacity = max(4, capacity)
        self.ttl_ticks = ttl_ticks
        self.table: dict[Any, _LRUNode] = {}
        self.head = _LRUNode(None, 0, 0)
        self.tail = _LRUNode(None, 0, 0)
        self.head.next = self.tail
        self.tail.prev = self.head
        self.hits = 0
        self.misses = 0
        self.evictions = 0

    def _unlink(self, node: _LRUNode) -> None:
        prev_n = node.prev
        next_n = node.next
        if prev_n is not None:
            prev_n.next = next_n
        if next_n is not None:
            next_n.prev = prev_n
        node.prev = None
        node.next = None

    def _insert_front(self, node: _LRUNode) -> None:
        first = self.head.next
        node.next = first
        node.prev = self.head
        self.head.next = node
        if first is not None:
            first.prev = node

    def call_cached(
        self,
        now_tick: int,
        endpoint: str,
        shard_id: int,
        flags: int,
        **kwargs: Any,
    ) -> int:
        kw_part = tuple(sorted(kwargs.items()))
        composite_key = ((endpoint, shard_id, flags), kw_part)

        node = self.table.get(composite_key)
        if node is not None:
            if node.expires_at >= now_tick:
                self.hits += 1
                self._unlink(node)
                self._insert_front(node)
                return node.value
            # Expired entry: remove before recomputing
            self._unlink(node)
            del self.table[composite_key]
            self.evictions += 1

        self.misses += 1
        computed = len(endpoint) * 19 + shard_id * 31 + flags * 7
        for k, v in kw_part:
            if isinstance(v, int):
                computed += len(k) * 3 + v
            elif isinstance(v, str):
                computed += len(k) * 5 + len(v) * 11
            else:
                computed += len(k) * 2

        if len(self.table) >= self.capacity:
            lru = self.tail.prev
            if lru is not None and lru is not self.head:
                self._unlink(lru)
                self.table.pop(lru.key, None)
                self.evictions += 1

        fresh = _LRUNode(composite_key, computed & 0xFFFFF, now_tick + self.ttl_ticks)
        self.table[composite_key] = fresh
        self._insert_front(fresh)
        return fresh.value


CallSpec = tuple[str, int, int, str, int, str, int]


def prepare(size: int) -> tuple[list[tuple[Any, int]], list[CallSpec]]:
    mixed_entries: list[tuple[Any, int]] = []
    call_specs: list[CallSpec] = []
    state = 618033988
    big_base = 1 << 80

    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        ns = _NAMESPACES[state % len(_NAMESPACES)]
        sym = _SYMBOLS[(state >> 4) % len(_SYMBOLS)]
        ver = (state >> 8) & 15
        val = ((state >> 12) & 0xFFF) + 1

        kind = i % 5
        if kind == 0:
            key: Any = f"{ns}::{sym}:v{ver}:{i & 63}"
        elif kind == 1:
            key = (i & 127) - 32
        elif kind == 2:
            key = big_base + ((state >> 6) & 0x3FFFF) + (i & 31)
        elif kind == 3:
            key = (ns, i & 63, (state & 1) == 0)
        else:
            # Half of SymbolKey instances share a small collision group
            cgroup = ((i & 7) + 1) if (i & 2) == 0 else 0
            key = SymbolKey(ns, sym, ver, cgroup)

        mixed_entries.append((key, val))

        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        ep = _ENDPOINTS[state % len(_ENDPOINTS)]
        shard = (state >> 3) & 15
        flags = (state >> 7) & 7
        region = _NAMESPACES[(state >> 10) % len(_NAMESPACES)]
        limit = ((state >> 14) & 3) * 25 + 10
        mode = "fast" if (state & 16) else "strict"
        retries = (state >> 18) & 3
        call_specs.append((ep, shard, flags, region, limit, mode, retries))

    return mixed_entries, call_specs


def scenario_mixed_key_dict_churn(
    mixed_entries: list[tuple[Any, int]],
) -> tuple[int, int, int, int]:
    """Churn dicts with heterogeneous keys, hash collisions, merges, and comprehensions."""
    lookup_sum = 0
    popped_sum = 0
    merged_keys = 0
    comp_total = 0

    for round_idx in range(58200):
        primary: dict[Any, int] = {}
        overlay: dict[Any, int] = {}

        for idx, (key, val) in enumerate(mixed_entries):
            adjusted = val + round_idx
            if (idx + round_idx) & 3 == 0:
                overlay[key] = adjusted * 2
                prev = primary.setdefault(key, adjusted)
                lookup_sum += prev & 0xFF
            else:
                primary[key] = primary.get(key, 0) + adjusted

        for idx in range(0, len(mixed_entries), 3):
            probe_key = mixed_entries[idx][0]
            got = primary.get(probe_key, -1)
            if got >= 0:
                lookup_sum += got & 0x3FF
            if (idx + round_idx) % 6 == 0:
                removed = primary.pop(probe_key, 0)
                popped_sum += removed & 0x3FF

        primary.update(overlay)
        combined = primary | overlay
        merged_keys += len(combined)

        filtered = {
            k: (v + round_idx) & 0x7FFF
            for k, v in combined.items()
            if (v & 1) == (round_idx & 1)
        }
        for _k, fv in filtered.items():
            comp_total += fv & 0x1FF

    return lookup_sum, popped_sum, merged_keys, comp_total


def scenario_composite_key_lru_cache(
    call_specs: list[CallSpec],
) -> tuple[int, int, int, int]:
    """Exercise a composite-key LRU + TTL cache wrapping multi-arg/kwarg calls."""
    capacity = max(16, len(call_specs) // 5)
    cache = CompositeLRUCache(capacity=capacity, ttl_ticks=24)
    value_accum = 0
    tick = 100

    for epoch in range(12480):
        for idx, (ep, shard, flags, region, limit, mode, retries) in enumerate(call_specs):
            tick += 1 if (idx & 1) == 0 else 2
            eff_shard = (shard + (epoch & 3)) & 15
            res = cache.call_cached(
                tick,
                ep,
                eff_shard,
                flags,
                region=region,
                limit=limit,
                mode=mode,
                retries=retries,
            )
            value_accum += res & 0x3FF

            # Immediate locality repeat to exercise LRU promotion on hit
            if (idx + epoch) % 3 == 0:
                res_hit = cache.call_cached(
                    tick,
                    ep,
                    eff_shard,
                    flags,
                    mode=mode,
                    retries=retries,
                    limit=limit,
                    region=region,
                )
                value_accum += res_hit & 0xFF

    return value_accum, cache.hits, cache.misses, cache.evictions


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
    mixed_entries, call_specs = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    mixed_s = 0.0
    lru_s = 0.0
    for _ in range(loops):
        r1: tuple[int, ...] = ()
        r2: tuple[int, ...] = ()
        if scenario in ("all", "mixed_key_dict_churn"):
            s0 = time.perf_counter()
            r1 = scenario_mixed_key_dict_churn(mixed_entries)
            mixed_s += time.perf_counter() - s0
        if scenario in ("all", "composite_key_lru_cache"):
            s1 = time.perf_counter()
            r2 = scenario_composite_key_lru_cache(call_specs)
            lru_s += time.perf_counter() - s1
        combined = r1 + r2
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (mixed_s + lru_s) / loops
    print("topic\tcollections/polymorphic_maps")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "mixed_key_dict_churn"):
        print(f"mixed_key_dict_churn\t{mixed_s / loops:.6f}")
    if scenario in ("all", "composite_key_lru_cache"):
        print(f"composite_key_lru_cache\t{lru_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()

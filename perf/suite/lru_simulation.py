"""Simulate an LRU cache backed by a dict and an intrusive doubly-linked list.

Exercises instance attribute reads and writes on linked nodes alongside
dictionary lookups, insertions, and deletions.
"""

from perfutils import run_case

NAME = "lru_simulation"
DESCRIPTION = "Doubly-linked list and dict LRU cache simulation"
DEFAULT_SIZE = 80_000
DEFAULT_SEED = 42


class _Node:
    def __init__(self, key: int, value: int):
        self.key = key
        self.value = value
        self.prev: _Node | None = None
        self.next: _Node | None = None


class LRUCache:
    def __init__(self, capacity: int):
        self.capacity = capacity
        self.table: dict[int, _Node] = {}
        head = _Node(-1, -1)
        tail = _Node(-1, -1)
        head.next = tail
        tail.prev = head
        self.head = head
        self.tail = tail

    def _unlink(self, node: _Node) -> None:
        prev_node = node.prev
        next_node = node.next
        if prev_node is not None:
            prev_node.next = next_node
        if next_node is not None:
            next_node.prev = prev_node

    def _push_front(self, node: _Node) -> None:
        first = self.head.next
        node.prev = self.head
        node.next = first
        self.head.next = node
        if first is not None:
            first.prev = node

    def get(self, key: int) -> int:
        node = self.table.get(key)
        if node is None:
            return -1
        self._unlink(node)
        self._push_front(node)
        return node.value

    def put(self, key: int, value: int) -> int:
        node = self.table.get(key)
        if node is not None:
            node.value = value
            self._unlink(node)
            self._push_front(node)
            return 0
        evicted = 0
        if len(self.table) >= self.capacity:
            victim = self.tail.prev
            if victim is not None and victim is not self.head:
                self._unlink(victim)
                del self.table[victim.key]
                evicted = victim.value
        fresh = _Node(key, value)
        self.table[key] = fresh
        self._push_front(fresh)
        return evicted


def prepare(size: int, seed: int) -> list[tuple[int, int, int]]:
    if size < 0:
        raise ValueError("size must be nonnegative")
    state = seed & 0x7FFFFFFF
    ops: list[tuple[int, int, int]] = []
    for _ in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        is_put = state & 1
        key = (state >> 4) % 192
        value = ((state >> 12) % 1000) + 1
        ops.append((is_put, key, value))
    return ops


def run(data: list[tuple[int, int, int]]) -> tuple[int, int, int]:
    cache = LRUCache(64)
    rounds = max(1, len(data) // 330)
    hits = 0
    evicted_sum = 0
    value_checksum = 0
    for round_idx in range(rounds):
        shift = round_idx & 31
        for is_put, key, value in data:
            adj_key = (key + shift) % 192
            if is_put == 1:
                evicted_sum = (
                    evicted_sum + cache.put(adj_key, value + shift)
                ) & 0x3FFFFFFF
            else:
                found = cache.get(adj_key)
                if found >= 0:
                    hits += 1
                    value_checksum = (value_checksum + found) & 0x3FFFFFFF
    return hits, evicted_sum, value_checksum


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()

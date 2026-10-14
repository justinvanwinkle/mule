"""Dijkstra traversal of a seeded, connected sparse graph.

Size counts vertices. Preparation creates an undirected chain plus three
seeded edges per vertex; every weight is positive and at most 97. Preparation
is separate from traversal so timed work includes the heap, relaxation, and
the reduction of all shortest-path distances, but not graph construction.
"""

import heapq

from perfutils import run_case

NAME = "graph_paths"
DESCRIPTION = "Sparse-graph Dijkstra traversal with heap operations"
DEFAULT_SIZE = 20_000
DEFAULT_SEED = 42


def prepare(size, seed):
    if size < 1:
        raise ValueError("size must be positive")
    graph = [[] for _ in range(size)]
    state = seed & 0x7FFFFFFF
    for vertex in range(size - 1):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        weight = (state >> 16) % 97 + 1
        graph[vertex].append((vertex + 1, weight))
        graph[vertex + 1].append((vertex, weight))
    for vertex in range(size):
        for _ in range(3):
            state = (state * 1103515245 + 12345) & 0x7FFFFFFF
            target = (state >> 8) % size
            state = (state * 1103515245 + 12345) & 0x7FFFFFFF
            weight = (state >> 16) % 97 + 1
            if target != vertex:
                graph[vertex].append((target, weight))
                graph[target].append((vertex, weight))
    return graph


def run(graph):
    infinity = len(graph) * 97 + 1
    distances = [infinity] * len(graph)
    distances[0] = 0
    pending = [(0, 0)]
    while pending:
        distance, vertex = heapq.heappop(pending)
        if distance != distances[vertex]:
            continue
        for target, weight in graph[vertex]:
            candidate = distance + weight
            if candidate < distances[target]:
                distances[target] = candidate
                heapq.heappush(pending, (candidate, target))
    checksum = 0
    reachable = 0
    for vertex in range(len(graph)):
        if distances[vertex] < infinity:
            checksum += (vertex + 1) * distances[vertex]
            reachable += 1
    return checksum, reachable


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()

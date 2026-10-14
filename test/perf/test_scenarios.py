"""Independent correctness checks for the initial standalone workloads."""

import copy
import importlib
import subprocess
import sys
from pathlib import Path

import pytest

SUITE = Path(__file__).resolve().parents[2] / "perf" / "suite"
SCENARIOS = (
    "integer_accumulation",
    "graph_paths",
    "log_aggregation",
    "http_routing",
)


@pytest.fixture
def load_case(monkeypatch):
    monkeypatch.syspath_prepend(str(SUITE))
    return importlib.import_module


@pytest.mark.parametrize("name", SCENARIOS)
def test_seeded_inputs_and_repeatable_results(load_case, name):
    case = load_case(name)
    data = case.prepare(31, 42)
    assert data == case.prepare(31, 42)
    assert data != case.prepare(31, 43)
    snapshot = copy.deepcopy(data)
    result = case.run(data)
    assert data == snapshot
    assert case.run(data) == result
    assert data == snapshot


def test_integer_accumulation(load_case):
    kernel = load_case("integer_accumulation").run
    values = [-9, 0, 7, -3, 11]
    assert kernel(values) == (6, 2)
    assert values == [-9, 0, 7, -3, 11]
    assert kernel([]) == (0, 0)


def test_log_aggregation(load_case):
    kernel = load_case("log_aggregation").run
    records = [
        "10.0.0.4 GET /api/items/0 200 10 5",
        "10.0.0.7 POST /api/items/1 500 20 8",
        "10.0.0.4 GET /api/items/0 200 3 2",
    ]
    snapshot = list(records)
    assert kernel(records) == (3, 33, 15, 8, 48, 900, 22)
    assert records == snapshot
    assert kernel([]) == (0, 0, 0, 0, 0, 0, 0)


def test_http_routes_and_aggregation(load_case):
    case = load_case("http_routing")
    requests = [
        "GET /health HTTP/1.1\r\nHost: localhost\r\n\r\n",
        "GET /items?view=compact&id=17 HTTP/1.1\r\n\r\n",
        "POST /events HTTP/1.1\r\ncontent-length: 3\r\n\r\nabc",
        "POST /events HTTP/1.1\r\nContent-Length: 4\r\n\r\nabc",
        "GET /missing/3 HTTP/1.1\r\n\r\n",
        "DELETE /items?id=17 HTTP/1.1\r\n\r\n",
    ]
    expected = [
        (1, 200, "ok\n"),
        (2, 200, '{"id":17}\n'),
        (3, 202, '{"accepted":3}\n'),
        (3, 400, "invalid content length\n"),
        (4, 404, "not found\n"),
        (2, 405, "method not allowed\n"),
    ]
    handle = case.handle
    assert [handle(request) for request in requests] == expected
    kernel = case.run
    snapshot = list(requests)
    assert kernel(requests) == (15, 1811, 80)
    assert requests == snapshot
    assert kernel([]) == (0, 0, 0)


def test_graph_known_paths_and_unreachable_vertices(load_case):
    kernel = load_case("graph_paths").run
    graph = [
        [(1, 5), (2, 20)],
        [(0, 5), (2, 2)],
        [(0, 20), (1, 2), (3, 1)],
        [(2, 1)],
    ]
    snapshot = copy.deepcopy(graph)
    assert kernel(graph) == (63, 4)
    assert graph == snapshot
    assert kernel([[(1, 5)], [(0, 5)], [], []]) == (10, 2)
    assert kernel([[]]) == (0, 1)


def bellman_ford_result(graph):
    """An independent relaxation algorithm; no heap or Dijkstra traversal."""
    distances = [float("inf")] * len(graph)
    distances[0] = 0
    for _ in range(len(graph) - 1):
        previous = list(distances)
        for source, edges in enumerate(graph):
            for target, weight in edges:
                distances[target] = min(distances[target], previous[source] + weight)
        if distances == previous:
            break
    reachable = [
        (index, distance)
        for index, distance in enumerate(distances)
        if distance != float("inf")
    ]
    return sum((index + 1) * distance for index, distance in reachable), len(reachable)


@pytest.mark.parametrize("size,seed", ((1, 0), (2, 42), (7, -7), (25, 999)))
def test_generated_graph_against_independent_algorithm(load_case, size, seed):
    case = load_case("graph_paths")
    graph = case.prepare(size, seed)
    expected = bellman_ford_result(graph)
    assert expected[1] == size
    assert case.run(graph) == expected


def test_standalone_cli_protocol(load_case, tmp_path):
    completed = subprocess.run(
        [
            sys.executable,
            str(SUITE / "integer_accumulation.py"),
            "--size",
            "23",
            "--seed",
            "17",
            "--loops",
            "2",
            "--warmups",
            "0",
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=True,
    )
    lines = [line.split("\t", 1) for line in completed.stdout.splitlines()]
    fields = dict(lines)
    assert len(lines) == len(fields) == 7
    assert fields["CASE"] == "integer_accumulation"
    assert fields["SIZE"] == "23"
    assert fields["SEED"] == "17"
    assert fields["LOOPS"] == "2"
    case = load_case("integer_accumulation")
    assert fields["RESULT"] == repr(case.run(case.prepare(23, 17)))
    assert float(fields["PREPARE"]) >= 0
    assert float(fields["KERNEL"]) > 0
    assert completed.stderr == ""


def test_invalid_cli_rejects_unknown_option(tmp_path):
    completed = subprocess.run(
        [
            sys.executable,
            str(SUITE / "integer_accumulation.py"),
            "--variant",
            "unrecognized",
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )
    assert completed.returncode != 0
    assert "unknown option: --variant" in completed.stderr
    assert completed.stdout == ""

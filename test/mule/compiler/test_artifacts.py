import os

import pytest

from mule.compiler.artifacts import Graph, ToolchainError, physical_core_count


@pytest.mark.parametrize(
    ("siblings", "allowed", "expected"),
    [
        (["0,2", "1,3", "0,2", "1,3"], {0, 1, 2, 3}, 2),
        (["0,2", "1,3", "0,2", "1,3"], {2, 3}, 2),
        (["0,2", "1,3", "0,2", "1,3"], {0, 2}, 1),
        (["0", "1", "2", "3"], {0, 1, 2, 3}, 4),
        (["0-3", "0-3", "0-3", "0-3"], {1, 3}, 1),
    ],
)
def test_physical_core_count_counts_available_sibling_groups(
    tmp_path, siblings, allowed, expected
):
    for cpu, group in enumerate(siblings):
        topology = tmp_path / f"cpu{cpu}" / "topology"
        topology.mkdir(parents=True)
        (topology / "thread_siblings_list").write_text(group + "\n")
    assert physical_core_count(allowed, topology_root=tmp_path) == expected


def test_physical_core_count_missing_topology_reports_cpu(tmp_path):
    with pytest.raises(ToolchainError, match="CPU 3"):
        physical_core_count({3}, topology_root=tmp_path)


def test_physical_core_count_empty_topology_reports_cpu(tmp_path):
    topology = tmp_path / "cpu3" / "topology"
    topology.mkdir(parents=True)
    (topology / "thread_siblings_list").write_text("\n")
    with pytest.raises(ToolchainError, match="CPU 3"):
        physical_core_count({3}, topology_root=tmp_path)


def test_physical_core_count_empty_affinity_reports_error(tmp_path):
    with pytest.raises(ToolchainError, match="No CPUs"):
        physical_core_count(set(), topology_root=tmp_path)


def test_graph_default_workers_match_available_physical_cores():
    with Graph() as graph:
        assert len(graph._workers) == physical_core_count(os.sched_getaffinity(0))


def test_graph_explicit_workers_override_default():
    with Graph(workers=2) as graph:
        assert len(graph._workers) == 2

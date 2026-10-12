import os

import pytest

from mule.compiler.artifacts import (
    Graph,
    Step,
    ToolchainError,
    cache_directory,
    physical_core_count,
)


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


@pytest.fixture
def private_cache(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_CACHE_HOME", str(tmp_path / "cache"))


def run_copy_step(action):
    """Build ``copy.txt`` from a text input; the step for ``action``."""
    with Graph(1) as graph:
        source = graph.text("source", "contents")
        step = graph.step(
            "copy",
            lambda: Step(
                graph,
                "copy",
                inputs=[source],
                settings={"step": "copy"},
                outputs=("copy.txt",),
                action=action,
            ),
        )
        graph.run(step)
    return step


def copy_action(runs):
    def action(inputs, directory):
        runs.append(directory)
        (directory / "copy.txt").write_text(inputs[0].text)

    return action


def truncate_output(entry):
    (entry / "copy.txt").write_text("cont")


def remove_output(entry):
    (entry / "copy.txt").unlink()


def truncate_manifest(entry):
    manifest = entry / "manifest.json"
    manifest.write_bytes(manifest.read_bytes()[:10])


def remove_manifest(entry):
    (entry / "manifest.json").unlink()


def test_intact_cache_entry_is_reused(private_cache):
    runs = []
    run_copy_step(copy_action(runs))
    run_copy_step(copy_action(runs))
    assert len(runs) == 1


@pytest.mark.parametrize(
    "damage", [truncate_output, remove_output, truncate_manifest, remove_manifest]
)
def test_damaged_cache_entry_is_rebuilt(private_cache, damage):
    runs = []
    damage(run_copy_step(copy_action(runs)).directory)
    rebuilt = run_copy_step(copy_action(runs))
    assert len(runs) == 2
    assert rebuilt.path("copy.txt").read_text() == "contents"


def test_failed_action_leaves_no_cache_entry(private_cache):
    def partial(inputs, directory):
        (directory / "copy.txt").write_text("cont")
        raise RuntimeError("interrupted")

    with pytest.raises(RuntimeError, match="interrupted"):
        run_copy_step(partial)
    steps = cache_directory() / "steps"
    assert [path for path in steps.iterdir() if path.is_dir()] == []
    runs = []
    built = run_copy_step(copy_action(runs))
    assert len(runs) == 1
    assert built.path("copy.txt").read_text() == "contents"

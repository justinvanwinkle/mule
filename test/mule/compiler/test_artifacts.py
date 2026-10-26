import logging
import os
import threading

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
    cores = physical_core_count(os.sched_getaffinity(0))
    with Graph() as graph:
        assert graph.worker_count == cores
        assert graph._workers == ()


def test_graph_explicit_workers_override_default():
    with Graph(workers=2) as graph:
        assert graph.worker_count == 2
        assert graph._workers == ()


def test_empty_graph_does_not_start_threads(monkeypatch):
    def forbidden_start(self):
        raise AssertionError("empty graph started an OS thread")

    monkeypatch.setattr(threading.Thread, "start", forbidden_start)
    with Graph(workers=2) as graph:
        source = graph.text("source", "contents")
        assert graph.run(source) is source
        assert graph.worker_count == 2
        assert graph._workers == ()


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


def test_serial_graph_runs_dependency_chain_on_caller_without_starting_threads(
    private_cache, monkeypatch
):
    def forbidden_start(self):
        raise AssertionError("serial build started an OS thread")

    monkeypatch.setattr(threading.Thread, "start", forbidden_start)
    caller = threading.get_ident()
    calls = []

    def first_action(inputs, directory):
        calls.append(("first", threading.get_ident()))
        (directory / "first.txt").write_text(inputs[0].text)

    def second_action(inputs, directory):
        calls.append(("second", threading.get_ident()))
        text = inputs[0].path("first.txt").read_text()
        (directory / "second.txt").write_text(text.upper())

    with Graph(1) as graph:
        source = graph.text("source", "contents")
        first = graph.step(
            "first",
            lambda: Step(
                graph,
                "first",
                inputs=[source],
                settings={},
                outputs=("first.txt",),
                action=first_action,
            ),
        )
        second = graph.step(
            "second",
            lambda: Step(
                graph,
                "second",
                inputs=[first],
                settings={},
                outputs=("second.txt",),
                action=second_action,
            ),
        )
        assert calls == []
        assert graph._workers == ()
        assert graph.run(second) is second
        assert graph.run(second) is second
    assert calls == [("first", caller), ("second", caller)]
    assert second.path("second.txt").read_text() == "CONTENTS"


@pytest.mark.parametrize("workers", [1, 2])
def test_graph_propagates_dependency_failure_without_running_consumer(
    private_cache, workers
):
    failure = RuntimeError("producer failed")
    calls = []

    def fail(inputs, directory):
        raise failure

    with Graph(workers) as graph:
        first = graph.step(
            "first",
            lambda: Step(
                graph,
                "first",
                inputs=[],
                settings={},
                outputs=(),
                action=fail,
            ),
        )
        second = graph.step(
            "second",
            lambda: Step(
                graph,
                "second",
                inputs=[first],
                settings={},
                outputs=(),
                action=lambda *args: calls.append(1),
            ),
        )
        with pytest.raises(RuntimeError) as caught:
            graph.run(second)
    assert caught.value is failure
    assert second.error is failure
    assert calls == []


@pytest.mark.parametrize("workers", [1, 2])
def test_graph_reports_original_error_before_independent_work_finishes(
    tmp_path, workers
):
    reported = threading.Event()
    messages = []
    failure = SyntaxError("unsupported operation", ("example.py", 7, 1, "bad()"))

    class FailureLog(logging.Handler):
        def emit(self, record):
            messages.append(self.format(record))
            reported.set()

    def fail(inputs, directory):
        raise failure

    def independent(inputs, directory):
        # Completion must not be required before the error becomes visible.
        if not reported.wait(5):
            raise RuntimeError("build error was not reported while work remained")
        (directory / "finished.txt").write_text("finished")

    logger = logging.getLogger("mule")
    handler = FailureLog()
    logger.addHandler(handler)
    try:
        with Graph(workers) as graph:
            producer = graph.step(
                "compile example.py",
                lambda: Step(
                    graph,
                    "compile example.py",
                    inputs=[],
                    settings={"test": str(tmp_path), "role": "failure"},
                    outputs=(),
                    action=fail,
                ),
            )
            other = graph.step(
                "independent",
                lambda: Step(
                    graph,
                    "independent",
                    inputs=[],
                    settings={"test": str(tmp_path), "role": "independent"},
                    outputs=("finished.txt",),
                    action=independent,
                ),
            )
            target = graph.step(
                "link",
                lambda: Step(
                    graph,
                    "link",
                    inputs=[producer, other],
                    settings={},
                    outputs=(),
                    action=lambda *args: pytest.fail("failed dependency was ignored"),
                ),
            )
            with pytest.raises(SyntaxError) as caught:
                graph.run(target)
        assert caught.value is failure
        assert other.error is None
        assert other.path("finished.txt").read_text() == "finished"
        assert len(messages) == 1
        assert "compile example.py" in messages[0]
        assert "example.py" in messages[0]
        assert "line 7" in messages[0]
        assert "unsupported operation" in messages[0]
    finally:
        logger.removeHandler(handler)


@pytest.mark.parametrize("workers", [1, 2])
def test_graph_discovery_adds_dependencies_before_consumer_can_start(
    private_cache, workers
):
    calls = []

    def produce(inputs, directory):
        calls.append("produce")
        (directory / "first.txt").write_text("first")

    def expand(first):
        calls.append("discover")
        discovered = graph.step(
            "discovered",
            lambda: Step(
                graph,
                "discovered",
                inputs=[first],
                settings={},
                outputs=("extra.txt",),
                action=produce_extra,
            ),
        )
        target.add_input(discovered)

    def produce_extra(inputs, directory):
        calls.append("extra")
        (directory / "extra.txt").write_text(
            inputs[0].path("first.txt").read_text() + " extra"
        )

    def consume(inputs, directory):
        calls.append("consume")
        (directory / "result.txt").write_text(inputs[1].path("extra.txt").read_text())

    with Graph(workers) as graph:
        first = graph.step(
            "first",
            lambda: Step(
                graph,
                "first",
                inputs=[],
                settings={},
                outputs=("first.txt",),
                action=produce,
                discover=expand,
            ),
        )
        target = graph.step(
            "target",
            lambda: Step(
                graph,
                "target",
                inputs=[first],
                settings={},
                outputs=("result.txt",),
                action=consume,
            ),
        )
        graph.run(target)
    assert calls == ["produce", "discover", "extra", "consume"]
    assert target.path("result.txt").read_text() == "first extra"


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


def test_partial_worker_startup_joins_started_workers_and_retains_backlog(
    private_cache, monkeypatch
):
    original_start = threading.Thread.start
    started = []

    def fail_second(worker):
        if worker.name == "mule-step-1":
            raise RuntimeError("worker startup failed")
        original_start(worker)
        started.append(worker)

    monkeypatch.setattr(threading.Thread, "start", fail_second)
    runs = []
    with Graph(workers=2) as graph:
        source = graph.text("source", "contents")
        step = graph.step(
            "copy",
            lambda: Step(
                graph,
                "copy",
                inputs=[source],
                settings={},
                outputs=("copy.txt",),
                action=copy_action(runs),
            ),
        )
        with pytest.raises(RuntimeError, match="worker startup failed"):
            graph.run(step)
        assert runs == []
        assert graph._workers == ()
        assert all(not worker.is_alive() for worker in started)
        monkeypatch.setattr(threading.Thread, "start", original_start)
        assert graph.run(step) is step
        assert len(graph._workers) == graph.worker_count == 2
        assert len(runs) == 1
    assert all(not worker.is_alive() for worker in graph._workers)


@pytest.mark.parametrize("workers", [1, 2])
def test_step_dependency_orders_execution_without_changing_cache_key(
    private_cache, workers
):
    order = []

    def build_dep(inputs, directory):
        order.append("dep")
        (directory / "dep.txt").write_text("dep")

    def build_consumer(inputs, directory):
        order.append("consumer")
        (directory / "out.txt").write_text(inputs[0].text)

    with Graph(workers=workers) as graph:
        source = graph.text("source", "hello")
        dep = graph.step(
            "dep",
            lambda: Step(
                graph,
                "dep",
                inputs=[],
                settings={"step": "dep"},
                outputs=("dep.txt",),
                action=build_dep,
            ),
        )
        consumer = graph.step(
            "consumer",
            lambda: Step(
                graph,
                "consumer",
                inputs=[source],
                settings={"step": "consumer"},
                outputs=("out.txt",),
                action=build_consumer,
            ),
        )
        key_before = consumer.key
        consumer.add_dependency(dep)
        assert consumer.key == key_before
        assert not consumer.try_reuse_cache()
        graph.run(consumer)

    assert order == ["dep", "consumer"]

    with Graph(workers=workers) as graph:
        source = graph.text("source", "hello")
        cached = graph.step(
            "consumer",
            lambda: Step(
                graph,
                "consumer",
                inputs=[source],
                settings={"step": "consumer"},
                outputs=("out.txt",),
                action=lambda *args: pytest.fail("cache entry was not reused"),
            ),
        )
        assert cached.try_reuse_cache()
        assert cached.path("out.txt").read_text() == "hello"

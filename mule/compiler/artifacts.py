"""Build artifacts as a graph of cached steps.

Every artifact's key is the SHA3-256 digest of one JSON object holding the
digest of every file of the Mule package, the settings applied to the step
that builds the artifact, and the digest of each of its inputs. JSON with
sorted keys makes the object deterministic regardless of the order in which
settings and inputs were collected. A step whose key has a cache entry reuses
its outputs; otherwise it runs. An input produced by another step is
represented by the digest of that step's outputs, so a step whose outputs did
not change leaves everything downstream cached.

A step can also read files it discovers while running, such as the module
search probes of an import. Those are recorded in the cache entry's manifest,
and the entry is valid only while each still has the recorded state.

A cache entry is written whole or not at all: a step builds into a private
directory, records the digest of each output in its manifest, flushes it to
disk and renames it into place. Reusing an entry validates every output digest
against its manifest, rehashing changed file versions, so a corrupt, truncated
or incomplete entry is rebuilt.

Steps run on the caller for one worker, or on threads for larger pools
(the interpreter is free-threaded). Each input notifies
the steps that consume it when it resolves; a step starts once all of its
inputs have resolved, so independent steps run in parallel.
"""

from __future__ import annotations

import fcntl
import json
import os
import queue
import shutil
import subprocess
import tempfile
import threading
from collections.abc import Callable, Iterable
from contextlib import contextmanager
from functools import cache, cached_property
from pathlib import Path

from .inputs import digest, file_digest, recording, unchanged

MULE_PACKAGE = Path(__file__).resolve().parents[1]
# Conversion recurses over the syntax tree; compile threads need room.
STEP_STACK_SIZE = 256 << 20
MANIFEST = "manifest.json"


class ToolchainError(RuntimeError):
    """The native toolchain or target runtime could not produce an artifact."""


def physical_core_count(
    cpus: Iterable[int] | None = None,
    *,
    topology_root: Path = Path("/sys/devices/system/cpu"),
) -> int:
    """Count Linux physical cores with at least one CPU in the affinity mask.

    Sysfs gives every SMT sibling the same canonical CPU list, including
    siblings outside the mask. This also distinguishes cores across sockets.
    """
    if cpus is None:
        cpus = os.sched_getaffinity(0)
    cores = set()
    for cpu in cpus:
        path = topology_root / f"cpu{cpu}" / "topology/thread_siblings_list"
        try:
            siblings = path.read_text().strip()
        except OSError as error:
            raise ToolchainError(
                f"Could not read physical-core topology for CPU {cpu}: {path}"
            ) from error
        if not siblings:
            raise ToolchainError(f"Empty physical-core topology for CPU {cpu}: {path}")
        cores.add(siblings)
    if not cores:
        raise ToolchainError("No CPUs are available for compilation")
    return len(cores)


@cache
def mule_hash() -> str:
    """The digest of every file of the Mule package, by relative path."""
    files = {
        str(path.relative_to(MULE_PACKAGE)): file_digest(path)
        for path in sorted(MULE_PACKAGE.rglob("*"))
        if path.is_file() and "__pycache__" not in path.parts
    }
    return digest(json.dumps(files, sort_keys=True).encode())


@cache
def tool_identity(command: tuple[str, ...], *, tool: str, failure: str) -> str:
    """A toolchain program's version report, part of the settings of every
    step that runs it, so an upgrade rebuilds what it produced. ``tool``
    names the program and ``failure`` the work that cannot proceed without
    it, for the error when it is missing or broken."""
    try:
        result = subprocess.run(command, check=False, capture_output=True, text=True)
    except FileNotFoundError as error:
        raise ToolchainError(f"{tool} '{command[0]}' was not found") from error
    if result.returncode:
        raise ToolchainError(
            f"{failure}: '{' '.join(command)}' exited with status "
            f"{result.returncode}:\n{result.stderr.strip()}"
        )
    return result.stdout


def artifact_key(settings: dict, inputs: dict[str, str | None]) -> str:
    """The cache key of an artifact built with ``settings`` from ``inputs``,
    a mapping of each input to its digest (None for a probed absent file)."""
    identity = {"mule": mule_hash(), "settings": settings, "inputs": inputs}
    return digest(json.dumps(identity, sort_keys=True, separators=(",", ":")).encode())


def cache_directory() -> Path:
    base = os.environ.get("XDG_CACHE_HOME") or Path.home() / ".cache"
    return Path(base) / "mule"


@contextmanager
def _locked(entry: Path):
    """Serialize building ``entry`` across threads and processes. The lock
    lives beside the entry, which a rebuild replaces."""
    entry.parent.mkdir(parents=True, exist_ok=True)
    with open(entry.with_name(entry.name + ".lock"), "w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        try:
            yield
        finally:
            fcntl.flock(lock, fcntl.LOCK_UN)


def _sync(path: Path) -> None:
    descriptor = os.open(path, os.O_RDONLY)
    try:
        os.fsync(descriptor)
    finally:
        os.close(descriptor)


class Node:
    """Something whose outputs steps consume."""

    def __init__(self, graph: Graph, name: str):
        self.graph = graph
        self.name = name
        self.dependents: list[Step] = []
        self.error: BaseException | None = None
        self.done = False

    @cached_property
    def output_hash(self) -> str | None:
        raise NotImplementedError

    def start(self) -> None:
        """Begin producing this node's outputs; a source has them already."""
        self.resolved()

    def resolved(self) -> None:
        """Notify every consumer that this node's outputs exist, or that it
        failed."""
        with self.graph.lock:
            self.done = True
            dependents = list(self.dependents)
        for dependent in dependents:
            dependent.notify(self)
        self.graph.finished(self)


class File(Node):
    """A source file, or a probe for one: its digest is None when absent."""

    def __init__(self, graph: Graph, path: Path):
        super().__init__(graph, str(path))
        self.path = path

    @cached_property
    def output_hash(self) -> str | None:
        try:
            return file_digest(self.path)
        except FileNotFoundError, NotADirectoryError, IsADirectoryError:
            return None


class Text(Node):
    """Source text that is not a file, such as ``-c`` or standard input."""

    def __init__(self, graph: Graph, name: str, text: str):
        super().__init__(graph, name)
        self.text = text

    @cached_property
    def output_hash(self) -> str:
        return digest(self.text.encode())


class Step(Node):
    """Produces ``outputs`` from its inputs by running ``action``.

    ``action(inputs, directory)`` receives the resolved inputs and writes
    each output, by name, into ``directory``. ``discover(step)`` runs after
    the outputs exist, before any consumer is notified, so it can create
    further steps and add them to consumers' inputs. ``validate(entry)``
    replaces the check of the files the action read: it decides whether a
    cache entry's outputs still hold, for a step whose reads depend on more
    than its key, such as module search probes.
    """

    def __init__(
        self,
        graph: Graph,
        name: str,
        *,
        inputs: Iterable[Node],
        settings: dict,
        outputs: tuple[str, ...],
        action: Callable[[list[Node], Path], None],
        discover: Callable[[Step], None] | None = None,
        validate: Callable[[Path], bool] | None = None,
    ):
        super().__init__(graph, name)
        self.inputs: list[Node] = []
        self.settings = settings
        self.outputs = outputs
        self.action = action
        self.discover = discover
        self.validate = validate
        self.directory: Path | None = None
        self.output_digests: dict[str, str] | None = None
        self._waiting: set[Node] = set()
        self._started = False
        for node in inputs:
            self.add_input(node)

    def start(self) -> None:
        """Run once every input has resolved, which may already be so."""
        self.notify(None)

    def add_input(self, node: Node) -> None:
        """Make ``node`` an input. A step adds inputs to a consumer before it
        notifies that consumer, so the consumer cannot start early."""
        with self.graph.lock:
            if node in self.inputs:
                return
            if self._started:
                raise RuntimeError(f"{self.name} already started")
            self.inputs.append(node)
            node.dependents.append(self)
            if not node.done:
                self._waiting.add(node)

    def notify(self, node: Node | None) -> None:
        """``node`` resolved; None only starts a step whose inputs resolved
        before it was created."""
        with self.graph.lock:
            self._waiting.discard(node)
            ready = not self._waiting and not self._started
            if ready:
                self._started = True
        if ready:
            self.graph.submit(self)

    def path(self, output: str) -> Path:
        if self.directory is None:
            raise RuntimeError(f"{self.name} has not produced its outputs")
        return self.directory / output

    @cached_property
    def key(self) -> str:
        return artifact_key(
            self.settings,
            {node.name: node.output_hash for node in self.inputs},
        )

    @cached_property
    def output_hash(self) -> str:
        if self.output_digests is None:
            raise RuntimeError(f"{self.name} has not produced its outputs")
        return digest(
            "".join(self.output_digests[output] for output in self.outputs).encode()
        )

    def execute(self) -> None:
        """Reuse the cache entry for this step's key or build it."""
        failed = next((node for node in self.inputs if node.error is not None), None)
        if failed is not None:
            self.error = failed.error
            return
        entry = cache_directory() / "steps" / self.key
        outputs = self._verified(entry)
        if outputs is None:
            with _locked(entry):
                outputs = self._verified(entry)
                if outputs is None:
                    outputs = self._build(entry)
        self.directory = entry
        self.output_digests = outputs
        if self.discover is not None:
            self.discover(self)

    def _verified(self, entry: Path) -> dict[str, str] | None:
        """The digest of each output when ``entry`` holds this step's outputs
        intact, as its manifest recorded them, and they still hold; None
        when the entry must be rebuilt."""
        try:
            manifest = json.loads((entry / MANIFEST).read_bytes())
            recorded = manifest["outputs"]
            discovered = manifest["discovered"]
        except FileNotFoundError, ValueError, KeyError, TypeError:
            return None
        outputs = {}
        for output in self.outputs:
            try:
                outputs[output] = file_digest(entry / output)
            except FileNotFoundError:
                return None
        if outputs != recorded:
            return None
        if self.validate is not None:
            valid = self.validate(entry)
        else:
            valid = unchanged(discovered)
        return outputs if valid else None

    def _build(self, entry: Path) -> dict[str, str]:
        """Run the action into a private directory and rename it into place
        as ``entry``, replacing an entry that failed verification."""
        built = Path(tempfile.mkdtemp(prefix=".build-", dir=entry.parent))
        try:
            with recording() as discovered:
                self.action(self.inputs, built)
            outputs = {output: file_digest(built / output) for output in self.outputs}
            manifest = {"outputs": outputs, "discovered": discovered}
            (built / MANIFEST).write_text(json.dumps(manifest))
            for path in built.iterdir():
                _sync(path)
            _sync(built)
            self._install(built, entry)
        finally:
            shutil.rmtree(built, ignore_errors=True)
        return outputs

    @staticmethod
    def _install(built: Path, entry: Path) -> None:
        # A directory cannot be renamed over a nonempty one, so a rejected
        # entry moves aside first. Readers open outputs by path: they see the
        # old entry, no entry (a miss, which waits on the lock), or the new one.
        discarded = Path(tempfile.mkdtemp(prefix=".discard-", dir=entry.parent))
        try:
            try:
                os.rename(entry, discarded / entry.name)
            except FileNotFoundError:
                pass
            os.rename(built, entry)
            _sync(entry.parent)
        finally:
            shutil.rmtree(discarded, ignore_errors=True)


class _Scheduler:
    def __init__(self, graph):
        self.graph = graph

    def prepare(self):
        raise NotImplementedError("build scheduler must prepare its workers")

    @property
    def worker_count(self):
        raise NotImplementedError(
            "build scheduler must report its configured parallelism"
        )

    def wait(self):
        raise NotImplementedError("build scheduler must resolve pending steps")

    def close(self):
        raise NotImplementedError("build scheduler must stop its workers")

    def threads(self):
        raise NotImplementedError("build scheduler must report its threads")


class _SerialScheduler(_Scheduler):
    def prepare(self):
        pass

    @property
    def worker_count(self):
        return 1

    def wait(self):
        self.graph._drain()

    def close(self):
        pass

    def threads(self):
        return ()


class _ThreadedScheduler(_Scheduler):
    def __init__(self, graph, workers):
        super().__init__(graph)
        self._count = workers
        self._threads = []
        self._start_lock = threading.Lock()

    @property
    def worker_count(self):
        return self._count

    def prepare(self):
        with self._start_lock:
            if self._threads:
                return
            previous = threading.stack_size(STEP_STACK_SIZE)
            try:
                for index in range(self._count):
                    worker = threading.Thread(
                        target=self.graph._work, name=f"mule-step-{index}", daemon=True
                    )
                    worker.start()
                    self._threads.append(worker)
            except BaseException:
                self.graph._stop_workers(len(self._threads))
                for worker in self._threads:
                    worker.join()
                self._threads.clear()
                raise
            finally:
                threading.stack_size(previous)

    def wait(self):
        self.graph._wait()

    def close(self):
        self.graph._stop_workers(len(self._threads))
        for worker in self._threads:
            worker.join()

    def threads(self):
        return tuple(self._threads)


class Graph:
    """Steps and files, each created once per name, executed as their inputs
    resolve."""

    def __init__(self, workers: int | None = None):
        self.lock = threading.RLock()
        self._files: dict[str, File] = {}
        self._texts: dict[str, Text] = {}
        self._steps: dict[str, Step] = {}
        self._pending = 0
        self._idle = threading.Condition(self.lock)
        self._ready: queue.SimpleQueue[Step | None] = queue.SimpleQueue()
        # Steps that became ready before ``run``: nothing executes until the
        # initial graph is complete.
        self._backlog: list[Step] | None = []
        workers = workers or physical_core_count()
        self._scheduler = (
            _SerialScheduler(self)
            if workers == 1
            else _ThreadedScheduler(self, workers)
        )

    @property
    def worker_count(self):
        return self._scheduler.worker_count

    @property
    def _workers(self):
        return self._scheduler.threads()

    def file(self, path: Path) -> File:
        return self._intern(self._files, str(path), lambda: File(self, path))

    def text(self, name: str, text: str) -> Text:
        return self._intern(self._texts, name, lambda: Text(self, name, text))

    def step(self, name: str, create: Callable[[], Step]) -> Step:
        """The step named ``name``, created by ``create`` the first time."""
        return self._intern(self._steps, name, create)

    def _intern[T: Node](
        self, nodes: dict[str, T], name: str, create: Callable[[], T]
    ) -> T:
        with self.lock:
            existing = nodes.get(name)
            if existing is not None:
                return existing
            node = create()
            nodes[name] = node
            self._pending += 1
        node.start()
        return node

    def submit(self, step: Step) -> None:
        with self.lock:
            if self._backlog is not None:
                self._backlog.append(step)
                return
        self._scheduler.prepare()
        self._ready.put(step)

    def _work(self) -> None:
        while (step := self._ready.get()) is not None:
            self._execute(step)

    def _execute(self, step: Step) -> None:
        try:
            step.execute()
        # Graph.run raises a step's failure; dependents fail without running.
        except Exception as error:  # noqa: BLE001
            step.error = error
        step.resolved()

    def _drain(self) -> None:
        while self._pending:
            self._execute(self._ready.get_nowait())

    def _wait(self) -> None:
        with self.lock:
            while self._pending:
                self._idle.wait()

    def _stop_workers(self, count: int) -> None:
        for _ in range(count):
            self._ready.put(None)

    def finished(self, node: Node) -> None:
        with self.lock:
            self._pending -= 1
            self._idle.notify_all()

    def run(self, target: Node) -> Node:
        """Execute the graph, waiting until every node has resolved; raise
        the target's failure."""
        with self.lock:
            pending = self._pending
        if pending:
            self._scheduler.prepare()
        with self.lock:
            backlog, self._backlog = self._backlog or [], None
        for step in backlog:
            self._ready.put(step)
        self._scheduler.wait()
        if target.error is not None:
            raise target.error
        return target

    def close(self) -> None:
        self._scheduler.close()

    def __enter__(self):
        return self

    def __exit__(self, *exception):
        self.close()

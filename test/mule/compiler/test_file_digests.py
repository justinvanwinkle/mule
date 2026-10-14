"""File version reuse still detects content changes and filesystem races."""

import os

from mule.compiler import inputs


def test_unchanged_file_is_hashed_once_and_source_reads_still_record_it(
    tmp_path, monkeypatch
):
    path = tmp_path / "source.py"
    path.write_text("answer = 42\n")
    calls = []
    original = inputs.digest

    def counted(data):
        calls.append(data)
        return original(data)

    monkeypatch.setattr(inputs, "digest", counted)
    first = inputs.file_digest(path)
    assert inputs.file_digest(path) == first
    with inputs.recording() as recorded:
        assert inputs.read_source(path) == "answer = 42\n"
    assert recorded == {str(path): first}
    assert inputs.unchanged(recorded)
    assert calls == [b"answer = 42\n"]


def test_same_size_rewrite_with_restored_mtime_invalidates_digest(tmp_path):
    path = tmp_path / "source.py"
    path.write_bytes(b"first")
    before = path.stat()
    first = inputs.file_digest(path)
    path.write_bytes(b"other")
    os.utime(path, ns=(before.st_atime_ns, before.st_mtime_ns))
    assert inputs.file_digest(path) == inputs.digest(b"other")
    assert inputs.file_digest(path) != first


def test_atomic_replacement_and_missing_file_invalidate_digest(tmp_path):
    path = tmp_path / "source.py"
    path.write_bytes(b"first")
    first = inputs.file_digest(path)
    replacement = tmp_path / "replacement.py"
    replacement.write_bytes(b"other")
    before = path.stat()
    os.utime(replacement, ns=(before.st_atime_ns, before.st_mtime_ns))
    replacement.replace(path)
    assert inputs.file_digest(path) != first
    path.unlink()
    assert inputs.file_state(path) is None
    path.write_bytes(b"third")
    assert inputs.file_state(path) == inputs.digest(b"third")


def test_change_during_hashing_is_not_memoized(tmp_path, monkeypatch):
    path = tmp_path / "source.py"
    path.write_bytes(b"first")
    original = inputs.digest
    calls = []

    def changed(data):
        calls.append(data)
        if len(calls) == 1:
            path.write_bytes(b"other")
        return original(data)

    monkeypatch.setattr(inputs, "digest", changed)
    assert inputs.file_digest(path) == original(b"first")
    assert inputs.file_digest(path) == original(b"other")
    assert inputs.file_digest(path) == original(b"other")
    assert calls == [b"first", b"other"]

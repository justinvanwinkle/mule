"""Vendor Python 3.15.0 library sources and select their Python fallbacks.

Usage: uv run python -m tools.vendor_stdlib path/to/Python-3.15.0.tar.xz
"""

import argparse
import hashlib
import json
import tarfile
from pathlib import Path, PurePosixPath

from .stdlib_io import adapt_io

VERSION = "3.15.0"
ARCHIVE_SHA256 = "ba4bed1ba346b916890b76d9e320451420aa69f6408997d33c66482eeae3d575"
SOURCE_URL = f"https://www.python.org/ftp/python/{VERSION}/Python-{VERSION}.tar.xz"
EXCLUDED_DIRECTORIES = {"test", "tests", "idle_test"}


def replace_once(source, old, new):
    if source.count(old) != 1:
        raise ValueError(f"upstream source changed: expected one {old!r}")
    return source.replace(old, new, 1)


def fallback(source, marker):
    """Select the except suite of one audited accelerator-import try block.

    This is a source splice, not a Python parser. It only handles the precise
    four-space-indented try blocks in the checksum-pinned release. Keeping the
    original fallback text avoids rewriting its algorithms or relying on the
    host's AST, which does not support all of the release's syntax.
    """
    lines = source.splitlines(keepends=True)
    matches = [index for index, line in enumerate(lines) if marker in line]
    if len(matches) != 1:
        raise ValueError(f"expected one accelerator import: {marker}")
    imported = matches[0]
    indentation = len(lines[imported]) - len(lines[imported].lstrip()) - 4
    prefix = " " * indentation
    start = next(
        index
        for index in range(imported - 1, -1, -1)
        if lines[index].startswith(prefix + "try:")
        and lines[index].split("#", 1)[0].rstrip() == prefix + "try:"
    )
    handler = next(
        index
        for index in range(imported + 1, len(lines))
        if lines[index].startswith(prefix + "except ")
    )
    end = handler + 1
    while end < len(lines):
        line = lines[end]
        if line.strip() and not line.startswith(prefix + "    "):
            break
        end += 1
    selected = [
        line[:indentation] + line[indentation + 4 :] if line.strip() else line
        for line in lines[handler + 1 : end]
    ]
    if "".join(selected).strip() == "pass":
        selected = []
    if end < len(lines) and lines[end] == prefix + "else:\n":
        end += 1
        while end < len(lines):
            line = lines[end]
            if line.strip() and not line.startswith(prefix + "    "):
                break
            end += 1
    return "".join(lines[:start] + selected + lines[end:])


def adapt(sources):
    imports = {
        "abc.py": ["from _abc import"],
        "asyncio/events.py": ["from _asyncio import"],
        "asyncio/futures.py": ["    import _asyncio"],
        "asyncio/tasks.py": ["    import _asyncio", "from _asyncio import"],
        "bisect.py": ["from _bisect import"],
        "collections/__init__.py": [
            "from _collections import OrderedDict",
            "from _collections import _tuplegetter",
            "from _collections import _count_elements",
        ],
        "datetime.py": ["from _datetime import"],
        "decimal.py": ["from _decimal import *"],
        "functools.py": [
            "from _functools import cmp_to_key",
            "from _functools import partial",
            "from _functools import _lru_cache_wrapper",
            "from _functools import reduce",
        ],
        "heapq.py": ["from _heapq import"],
        "hmac.py": ["import _hashlib as _hashopenssl", "    import _hmac"],
        "json/decoder.py": ["from _json import"],
        "json/encoder.py": [
            "from _json import encode_basestring_ascii",
            "from _json import encode_basestring as",
            "from _json import make_encoder",
        ],
        "json/scanner.py": ["from _json import"],
        "mimetypes.py": ["from _winapi import _mimetypes_read_windows_registry"],
        "ntpath.py": [
            "from nt import _path_splitroot_ex",
            "from nt import _path_normpath",
            "from nt import _path_isdir",
        ],
        "operator.py": ["from _operator import *"],
        "pickle.py": ["from _pickle import PickleBuffer", "from _pickle import ("],
        "posixpath.py": [
            "from posix import _path_splitroot_ex",
            "from posix import _path_normpath",
        ],
        "queue.py": ["from _queue import SimpleQueue", "from _queue import Empty"],
        "stat.py": ["from _stat import"],
        "statistics.py": ["from _statistics import"],
        "traceback.py": [
            "            import _suggestions",
            "        import _suggestions",
        ],
        "types.py": ["from _types import"],
        "uuid.py": ["    import _uuid"],
        "warnings.py": ["from _warnings import"],
        "xml/etree/ElementTree.py": ["from _elementtree import *"],
        "zoneinfo/__init__.py": ["from _zoneinfo import ZoneInfo"],
        "_pylong.py": ["    import _decimal"],
    }
    for filename, markers in imports.items():
        for marker in markers:
            sources[filename] = fallback(sources[filename], marker)
    sources["xml/etree/ElementTree.py"] += "\n_Element_Py = Element\n"
    adapt_io(sources)
    for filename in (
        "zipimport.py",
        "importlib/_bootstrap_external.py",
        "site.py",
        "unittest/mock.py",
    ):
        sources[filename] = sources[filename].replace("import _io", "import io as _io")
    # Preserve site's original alias rather than producing a double 'as'.
    sources["site.py"] = sources["site.py"].replace(
        "import io as _io as io", "import io"
    )
    sources["importlib/_bootstrap_external.py"] = replace_once(
        sources["importlib/_bootstrap_external.py"],
        "import _warnings\n",
        "import warnings as _warnings\n",
    )
    for marker in (
        "import _frozen_importlib as",
        "import _frozen_importlib_external as",
    ):
        sources["importlib/__init__.py"] = fallback(
            sources["importlib/__init__.py"], marker
        )
    for marker in (
        "    import _frozen_importlib\n",
        "    import _frozen_importlib_external\n",
    ):
        sources["importlib/abc.py"] = fallback(sources["importlib/abc.py"], marker)
    sources["importlib/abc.py"] = replace_once(
        sources["importlib/abc.py"],
        "if exc.name != '_frozen_importlib':\n    raise\n",
        "",
    )
    sources["zipimport.py"] = (
        sources["zipimport.py"]
        .replace(
            "import _frozen_importlib_external as",
            "from importlib import _bootstrap_external as",
        )
        .replace(
            "from _frozen_importlib_external import",
            "from importlib._bootstrap_external import",
        )
        .replace("import _frozen_importlib as", "from importlib import _bootstrap as")
    )
    sources["importlib/_bootstrap.py"] = replace_once(
        sources["importlib/_bootstrap.py"],
        "    import _frozen_importlib_external\n"
        "    _bootstrap_external = _frozen_importlib_external\n"
        "    _frozen_importlib_external._install(sys.modules[__name__])\n",
        "    from importlib import _bootstrap_external\n"
        "    _bootstrap_external._install(sys.modules[__name__])\n",
    )
    sources["importlib/_bootstrap.py"] = replace_once(
        sources["importlib/_bootstrap.py"],
        "for builtin_name in ('_thread', '_warnings', '_weakref'):",
        "for builtin_name in ('_thread', '_weakref'):",
    )
    sources["importlib/_bootstrap.py"] = replace_once(
        sources["importlib/_bootstrap.py"],
        "    self_module = sys.modules[__name__]\n",
        "    self_module = sys.modules[__name__]\n"
        "    import warnings\n"
        "    self_module._warnings = warnings\n",
    )
    sources["threading.py"] = fallback(
        sources["threading.py"], "from _collections import deque"
    )
    sources["threading.py"] = fallback(
        sources["threading.py"], "from _thread import (_excepthook"
    )
    sources["threading.py"] = fallback(
        sources["threading.py"], "from _thread import _local"
    )
    # The fallback imports threading.current_thread/RLock, which must exist first.
    sources["threading.py"] = replace_once(
        sources["threading.py"], "from _threading_local import local\n", ""
    )
    # Creating the Python local accesses current_thread(), which also needs the
    # main-thread registration to have finished. Its original C version did not.
    sources["threading.py"] = (
        replace_once(sources["threading.py"], "_thread_local_info = local()\n", "")
        + "\nfrom _threading_local import local\n_thread_local_info = local()\n"
    )
    sources["threading.py"] = fallback(
        sources["threading.py"], "    _CRLock = _thread.RLock"
    )


def vendor(archive, destination):
    checksum = hashlib.sha256(archive.read_bytes()).hexdigest()
    if checksum != ARCHIVE_SHA256:
        raise ValueError(
            "archive does not match the official Python 3.15.0 release checksum"
        )
    sources = {}
    prefix = f"Python-{VERSION}/Lib/"
    with tarfile.open(archive, "r:xz") as release:
        license_text = release.extractfile(f"Python-{VERSION}/LICENSE").read()
        for member in release.getmembers():
            if not member.isfile() or not member.name.startswith(prefix):
                continue
            relative = PurePosixPath(member.name.removeprefix(prefix))
            if relative.suffix != ".py" or EXCLUDED_DIRECTORIES.intersection(
                relative.parts[:-1]
            ):
                continue
            if relative.is_absolute() or ".." in relative.parts:
                raise ValueError(f"unsafe archive member: {member.name}")
            sources[str(relative)] = release.extractfile(member).read().decode("utf-8")
    original_hashes = {
        name: hashlib.sha256(text.encode()).hexdigest()
        for name, text in sources.items()
    }
    adapt(sources)
    destination.mkdir(parents=True, exist_ok=True)
    for name, text in sources.items():
        path = destination / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")
    (destination / "LICENSE.cpython").write_bytes(license_text)
    manifest = {
        "version": VERSION,
        "source": SOURCE_URL,
        "archive_sha256": checksum,
        "excluded_directories": sorted(EXCLUDED_DIRECTORIES),
        "files": {
            name: {
                "upstream_sha256": original_hashes[name],
                "vendored_sha256": hashlib.sha256(text.encode()).hexdigest(),
            }
            for name, text in sorted(sources.items())
        },
    }
    (destination / "UPSTREAM.json").write_text(json.dumps(manifest, indent=2) + "\n")
    modified = sum(
        details["upstream_sha256"] != details["vendored_sha256"]
        for details in manifest["files"].values()
    )
    print(
        f"Vendored {len(sources)} Python files; selected Python fallbacks in {modified} files."
    )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("archive", type=Path)
    parser.add_argument(
        "--destination",
        type=Path,
        default=Path(__file__).resolve().parents[1] / "stdlib",
    )
    arguments = parser.parse_args()
    vendor(arguments.archive, arguments.destination)


if __name__ == "__main__":
    main()

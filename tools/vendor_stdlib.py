"""Vendor Python 3.14.7 library sources and select their Python fallbacks.

Usage: uv run python -m tools.vendor_stdlib path/to/Python-3.14.7.tar.xz
"""

import argparse
import hashlib
import tarfile
from pathlib import Path, PurePosixPath

from .stdlib_io import adapt_io

# 3.14.7 matches the host CPython used as the conformance oracle.
VERSION = "3.14.7"
ARCHIVE_SHA256 = "3b48dac8fb59f62eaa67ac83c1eb12bda1b7a08406dd286e252c11a66be27f81"
SOURCE_URL = f"https://www.python.org/ftp/python/{VERSION}/Python-{VERSION}.tar.xz"
TEST_DIRECTORIES = {"test", "tests"}
# Library trees Mule does not ship: tkinter and the modules that exist only to
# drive it, the pip installer and the venv creator that runs it, and their tests.
EXCLUDED_PATHS = (
    "tkinter/",
    "idlelib/",
    "turtledemo/",
    "turtle.py",
    "ensurepip/",
    "venv/",
    "test/test_ensurepip.py",
    "test/test_venv.py",
    "test/test_tkinter/",
    "test/test_ttk/",
    "test/tkinterdata/",
    "test/test_ttk_textonly.py",
    "test/test_tcl.py",
    "test/test_idle.py",
    "test/test_turtle.py",
)


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


def read_member(release, member):
    file = release.extractfile(member)
    if file is None:
        raise ValueError(f"archive member is not a regular file: {member}")
    return file.read()


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
            f"archive does not match the official Python {VERSION} release checksum"
        )
    sources = {}
    test_files = {}
    resources = {}
    directories = []
    prefix = f"Python-{VERSION}/Lib/"
    with tarfile.open(archive, "r:xz") as release:
        license_text = read_member(release, f"Python-{VERSION}/LICENSE")
        for member in release.getmembers():
            if not member.name.startswith(prefix):
                continue
            relative = PurePosixPath(member.name.removeprefix(prefix))
            if relative.is_absolute() or ".." in relative.parts:
                raise ValueError(f"unsafe archive member: {member.name}")
            if str(relative).startswith(EXCLUDED_PATHS):
                continue
            if member.isdir():
                directories.append(str(relative))
                continue
            if not member.isfile():
                raise ValueError(f"unsupported archive member: {member.name}")
            content = read_member(release, member)
            if TEST_DIRECTORIES.intersection(relative.parts[:-1]):
                # Tests contain intentionally invalid source/encoding fixtures;
                # preserve their bytes and native-extension references exactly.
                test_files[str(relative)] = content
            elif relative.suffix == ".py":
                sources[str(relative)] = content.decode("utf-8")
            else:
                resources[str(relative)] = content
    original = dict(sources)
    adapt(sources)
    destination.mkdir(parents=True, exist_ok=True)
    for name in directories:
        (destination / name).mkdir(parents=True, exist_ok=True)
    for name, text in sources.items():
        path = destination / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")
    for name, content in (test_files | resources).items():
        path = destination / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(content)
    (destination / "LICENSE.cpython").write_bytes(license_text)
    modified = sum(sources[name] != text for name, text in original.items())
    print(
        f"Vendored {len(sources)} Python files; selected Python fallbacks in {modified} files."
    )
    print(
        f"Included all {len(test_files)} upstream test/support/fixture files "
        f"and {len(resources)} library resource files."
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

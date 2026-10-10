"""Select the released standard library's Python stream implementations."""


def _replace_once(source: str, old: str, new: str) -> str:
    if source.count(old) != 1:
        raise ValueError(f"upstream io source changed: expected one {old!r}")
    return source.replace(old, new, 1)


def _section(source: str, start: str, end: str) -> str:
    if source.count(start) != 1 or source.count(end) != 1:
        raise ValueError("upstream io source section boundaries changed")
    return source[source.index(start) : source.index(end)]


def adapt_io(sources: dict[str, str]) -> None:
    """Remove ``_io`` and the fallback's dependency on its public wrapper.

    Public protocols and export names come directly from upstream ``io.py``.
    The fallback's own classes already inherit its Python base classes, so
    registering them with a second hierarchy is unnecessary. File operations
    still require Mule's OS primitives; this adaptation does not supply them.
    """
    public = sources["io.py"]
    fallback = sources["_pyio.py"]
    exports = _section(public, "__all__ = [", "\n\n\nimport _io\n")
    seeks = _section(public, "# for seek()\n", "# Declaring ABCs in C")
    protocol_marker = "#\n# Static Typing Support\n#\n"
    if public.count(protocol_marker) != 1:
        raise ValueError("upstream io protocol boundary changed")
    protocols = public[public.index(protocol_marker) :]
    fallback = _replace_once(
        fallback,
        "import io\n"
        "from io import (__all__, SEEK_SET, SEEK_CUR, SEEK_END, Reader, Writer)"
        "  # noqa: F401\n",
        "from _collections_abc import _check_methods\n\n"
        + exports
        + "\n\n"
        + seeks
        + protocols
        + "\n",
    )
    fallback = _replace_once(
        fallback,
        '# NOTE: Base classes defined here are registered with the "official" ABCs\n'
        "# defined in io.py. We don't use real inheritance though, because we don't want\n"
        "# to inherit the C implementations.\n",
        "# Mule exports this Python class hierarchy directly through io.py.\n",
    )
    fallback = _replace_once(
        fallback,
        "try:\n"
        "    open_code = io.open_code\n"
        "except AttributeError:\n"
        "    open_code = _open_code_with_warning\n",
        "open_code = _open_code_with_warning\n",
    )
    fallback = _replace_once(
        fallback,
        "# In normal operation, both `UnsupportedOperation`s should be bound to the\n"
        "# same object.\n"
        "try:\n"
        "    UnsupportedOperation = io.UnsupportedOperation\n"
        "except AttributeError:\n"
        "    class UnsupportedOperation(OSError, ValueError):\n"
        "        pass\n",
        "class UnsupportedOperation(OSError, ValueError):\n    pass\n",
    )
    for name in ("IOBase", "RawIOBase", "BufferedIOBase", "TextIOBase"):
        fallback = _replace_once(fallback, f"io.{name}.register({name})\n", "")
    if public.count("\nimport _io\n") != 1:
        raise ValueError("upstream io implementation boundary changed")
    sources["io.py"] = public[: public.index("\nimport _io\n")] + (
        "\n# Mule uses the standard library's Python stream implementations.\n"
        "from _pyio import *\n"
    )
    sources["_pyio.py"] = fallback

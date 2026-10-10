"""Compiled print rules for the initial native stdout boundary."""

# Compiler-provided physical primitives and shared conversion protocol.
# ruff: noqa: F821


def print(*objects, end="\n"):
    if end is None:
        end = "\n"
    elif not _text_storage(end):
        raise TypeError("end must be None or a string")
    index = 0
    count = _storage_length(objects)
    while index < count:
        if index > 0:
            _stdout_write(" ")
        _stdout_write(_string_protocol(_storage_read(objects, index)))
        index += 1
    _stdout_write(end)


_install_builtin("print", print)

"""Compiled print conversion, stream selection and ending rules."""

# Compiler-provided physical primitives and shared conversion protocol.
# ruff: noqa: F821


def print(*objects, sep=" ", end="\n", file=None, flush=False):
    flush = True if flush else False
    if file is None:
        import sys
        file = sys.stdout
        if file is None:
            return
    if sep is None:
        sep = " "
    elif not _text_storage(sep):
        raise TypeError("sep must be None or a string, not " + _type_name(sep))
    if end is None:
        end = "\n"
    elif not _text_storage(end):
        raise TypeError("end must be None or a string, not " + _type_name(end))
    index = 0
    count = _storage_length(objects)
    while index < count:
        if index > 0:
            file.write(sep)
        file.write(_string_protocol(_storage_read(objects, index)))
        index += 1
    file.write(end)
    if flush:
        file.flush()


_install_builtin("print", print)

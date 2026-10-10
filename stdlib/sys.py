"""Initial native system module: process arguments owned by compiled Python."""

from _mule_codecs import utf8_decode as _decode_argv
from _mule_native import argc as _argc
from _mule_native import argv as _argv
from _mule_native import read_c_string as _read_c_string
from _mule_native import read_pointer as _read_pointer


def _make_argv():
    arguments = []
    index = 0
    while index < _argc:
        pointer = _read_pointer(_argv, index)
        if pointer is None:
            raise SystemError("NULL pointer inside native argument vector")
        arguments.append(_decode_argv(_read_c_string(pointer)))
        index += 1
    return arguments


argv = _make_argv()

"""Initial native system module: process arguments owned by compiled Python."""

from _mule_os import argument, argument_count


def _make_argv():
    arguments = []
    count = argument_count()
    index = 0
    while index < count:
        arguments.append(argument(index))
        index += 1
    return arguments


argv = _make_argv()

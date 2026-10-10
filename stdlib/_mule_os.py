"""Compiler-private access to physical OS state, compiled by Mule."""

# ruff: noqa: F821


def argument_count():
    return _process_argument_count()


def argument(index):
    return _process_argument(index)

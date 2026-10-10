"""Compiler-private native class protocols, compiled through Mule's frontend.

The underscored primitive bindings are supplied by the compiler, not imported
from the Python host. Constructor signatures are expanded for each layout.
"""

# These bindings are physical compiler primitives, supplied during lowering.
# ruff: noqa: F821


def construct(cls, PARAMETERS):
    instance = _allocate(cls)
    result = _initialize(instance, PARAMETERS)
    if result is not None:
        _invalid_initializer()
    return instance


def get_field(instance, index):
    return _read_int64(instance, index)


def set_field(instance, index, value):
    _write_int64(instance, index, value)

"""Compiler annotation markers and policy directives for Mule programs.

The numeric classes describe contracts to Mule's compiler. They are annotation
markers, not numeric constructors or an implementation of fixed-width arithmetic.
The host decorator preserves functions so annotated comparison programs can also
run on a standard Python interpreter.
"""

__all__ = (
    "final",
    "float16",
    "float32",
    "float64",
    "float128",
    "int8",
    "int16",
    "int32",
    "int64",
    "int128",
    "optimize",
    "uint8",
    "uint16",
    "uint32",
    "uint64",
    "uint128",
)


class int8:
    """Signed 8-bit integer annotation."""


class int16:
    """Signed 16-bit integer annotation."""


class int32:
    """Signed 32-bit integer annotation."""


class int64:
    """Signed 64-bit integer annotation."""


class int128:
    """Signed 128-bit integer annotation."""


class uint8:
    """Unsigned 8-bit integer annotation."""


class uint16:
    """Unsigned 16-bit integer annotation."""


class uint32:
    """Unsigned 32-bit integer annotation."""


class uint64:
    """Unsigned 64-bit integer annotation."""


class uint128:
    """Unsigned 128-bit integer annotation."""


class float16:
    """IEEE binary16 float annotation; native storage uses binary64 only."""


class float32:
    """IEEE binary32 float annotation; native storage uses binary64 only."""


class float64:
    """IEEE binary64 float annotation."""


class float128:
    """IEEE binary128 float annotation; storage lowering is not implemented yet."""


def optimize(*, speed=None, safety=None, debug=None):
    """Set a Mule function's compiler policy; preserve it on the host interpreter.

    Each level is an integer between zero and three; an omitted level keeps
    the enclosing policy. Mule's compiler recognizes this decorator through its
    imported binding and applies the policy itself. A module sets its own
    policy by assigning ``__speed__``, ``__safety__`` or ``__debug_level__``.
    """
    for option, level in (("speed", speed), ("safety", safety), ("debug", debug)):
        if level is None:
            continue
        if not 0 <= level <= 3:
            raise ValueError(f"{option} must be between 0 and 3")
        # Subscription applies the integer index protocol without inspecting types.
        (0, 1, 2, 3)[level]

    def preserve(function):
        return function

    return preserve


def final(obj):
    """Declare a function, method, or class as final."""
    try:
        obj.__final__ = True
    except AttributeError, TypeError:
        pass
    return obj

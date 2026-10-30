"""Physical argument-view access for generated external entries.

SBCL's ``init-xep-environment`` verifies argument counts before moving values
from standard argument locations. Mule's entry receives an existing borrowed
``Data::ArgumentView`` instead. Its tag selects that physical layout once;
the signature-specific entry then checks counts and acquires values directly.

The Rust layout test in ``runtime/arguments.rs`` checks these offsets against
real ``repr(C)`` objects. The buffer, count, defaults and closure stay immutable
through an invocation. Only the ownership mask changes as references transfer.
"""

from mule import procedures

DATA_OFFSET = 16
ARGUMENT_VIEW_TAG = 10
ARGUMENT_VALUES_OFFSET = 24
ARGUMENT_COUNT_OFFSET = 32
ARGUMENT_DEFAULTS_OFFSET = 40
ARGUMENT_CLOSURE_OFFSET = 48
ARGUMENT_OWNED_MASK_OFFSET = 56


def helpers() -> str:
    """LLVM physical primitives, all inlined into generated binding entries."""
    return procedures.load_category("argument_view")

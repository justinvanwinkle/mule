"""The known-function database for Python operations, following ``fndb.lisp``.

Names beginning with ``%`` are compiler-internal operations that IR1
conversion emits for Python syntax and for Mule's runtime protocols. Their
attributes are deliberately conservative: nearly every Python operator can run
user code and raise, so almost nothing is flushable or foldable until the
optimizer proves operand types.
"""

from __future__ import annotations

from mule import ast

from .knownfun import defknown, fun_info

# Binary operators: one known per operator and one per in-place form.
BINARY = {
    operator: defknown(f"%{operator.name.lower()}", "call")
    for operator in ast.BinaryOperator
}
INPLACE_BINARY = {
    operator: defknown(f"%i{operator.name.lower()}", "call", "unsafe")
    for operator in ast.BinaryOperator
}
UNARY = {
    operator: defknown(f"%unary_{operator.name.lower()}", "call")
    for operator in ast.UnaryOperator
}
COMPARE = {
    operator: defknown(f"%compare_{operator.name.lower()}", "call")
    for operator in ast.ComparisonOperator
}

# Attribute and method protocols.
GETATTR = defknown("%getattr", "call")
SETATTR = defknown("%setattr", "call", "unsafe", result=False)
RESOLVE_METHOD = defknown("%resolve_method", "call")
CALL_RESOLVED = defknown("%call_resolved", "call", "unsafe")

# Definitions executed when a module runs.
MAKE_FUNCTION = defknown("%make_function", "flushable")
MAKE_CLASS = defknown("%make_class", "call", "unsafe")
# A class body's namespace: creation, and reads, stores and deletions of the
# names the body binds. Reads fall back to the module globals and builtins.
CLASS_NAMESPACE = defknown("%class_namespace", "flushable")
CLASS_LOAD = defknown("%class_load", "call")
CLASS_STORE = defknown("%class_store", "unsafe", result=False)
CLASS_DELETE = defknown("%class_delete", "unsafe", result=False)
IMPORT = defknown("%import", "call", "unsafe")
# ``from module import *``: bind the module's public names in the globals.
IMPORT_STAR = defknown("%import_star", "unsafe", result=False)
HAS_BINDING = defknown("%has_binding")

# Displays and formatting.
BUILD_LIST = defknown("%build_list", "flushable")
BUILD_TUPLE = defknown("%build_tuple", "flushable", "foldable")
UNPACK_REF = defknown("%unpack_ref")
FORMAT = defknown("%format", "call")

# Guarded counted iteration over a builtin range.
RANGE_PREPARE = defknown("%range_prepare")
RANGE_AVAILABLE = defknown("%range_available", "flushable")
RANGE_ADVANCE = defknown("%range_advance", "unsafe")

# Compiled stdlib protocols, keyed by the name the sequence module exports.
PROTOCOLS = {
    name: defknown(f"%{name}", "call", "unsafe", protocol=name)
    for name in (
        "iterate",
        "next_item",
        "getitem",
        "setitem",
        "unpack",
        "exception_matches",
        "iteration_exhausted",
        "context_exit",
        "cell_new",
        "cell_get",
        "cell_set",
        "cell_clear",
        "exception_instance",
        "set_exception_cause",
        "ellipsis_value",
    )
}

# Exception state.
CATCH_EXCEPTION = defknown("%catch_exception", "unsafe")
ENTER_HANDLER = defknown("%enter_handler", "unsafe", result=False)
LEAVE_HANDLER = defknown("%leave_handler", "unsafe", result=False)
ASSERTION_FAILURE = defknown("%assertion_failure", "unsafe", result=False)
RAISE = defknown("%raise", "unsafe", "never_returns", result=False)
RERAISE = defknown("%reraise", "unsafe", "never_returns", result=False)
UNWIND = defknown("%unwind", "unsafe", "never_returns", result=False)

__all__ = [
    "ASSERTION_FAILURE",
    "BINARY",
    "BUILD_LIST",
    "BUILD_TUPLE",
    "CALL_RESOLVED",
    "CATCH_EXCEPTION",
    "CLASS_DELETE",
    "CLASS_LOAD",
    "CLASS_NAMESPACE",
    "CLASS_STORE",
    "COMPARE",
    "ENTER_HANDLER",
    "FORMAT",
    "GETATTR",
    "HAS_BINDING",
    "IMPORT",
    "IMPORT_STAR",
    "INPLACE_BINARY",
    "LEAVE_HANDLER",
    "MAKE_CLASS",
    "MAKE_FUNCTION",
    "PROTOCOLS",
    "RAISE",
    "RANGE_ADVANCE",
    "RANGE_AVAILABLE",
    "RANGE_PREPARE",
    "RERAISE",
    "RESOLVE_METHOD",
    "SETATTR",
    "UNARY",
    "UNPACK_REF",
    "UNWIND",
    "fun_info",
]

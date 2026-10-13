"""Opcode predicates and tables, as CPython's builtin ``_opcode`` module.

Mule does not execute CPython bytecode; these describe the CPython opcode
set for modules such as ``dis`` and ``inspect`` that consult the tables.
"""

from _opcode_metadata import opmap as _opmap

ENABLE_SPECIALIZATION = 1
ENABLE_SPECIALIZATION_FT = 0

_ARG = {
    "BINARY_OP", "BUILD_CONST_KEY_MAP", "BUILD_LIST", "BUILD_MAP",
    "BUILD_SET", "BUILD_SLICE", "BUILD_STRING", "CALL", "CALL_FUNCTION_EX",
    "CALL_INTRINSIC_1", "CALL_INTRINSIC_2", "CALL_KW", "COMPARE_OP",
    "CONTAINS_OP", "CONVERT_VALUE", "COPY", "DELETE_FAST", "DELETE_GLOBAL",
    "DELETE_NAME", "DICT_MERGE", "DICT_UPDATE", "EXTENDED_ARG", "FORMAT_VALUE",
    "FOR_ITER", "FORMAT_WITH_SPEC", "GET_AWAITABLE", "GET_AITER", "GET_ANEXT",
    "IS_OP", "JUMP_BACKWARD", "JUMP_BACKWARD_NO_INTERRUPT", "JUMP_FORWARD",
    "KW_NAMES", "LIST_APPEND", "LIST_EXTEND", "LOAD_ATTR", "LOAD_BUILD_CLASS",
    "LOAD_CONST", "LOAD_DEREF", "LOAD_FAST", "LOAD_FAST_AND_CLEAR",
    "LOAD_FAST_CHECK", "LOAD_FROM_DICT_OR_DEREF", "LOAD_FROM_DICT_OR_GLOBALS",
    "LOAD_GLOBAL", "LOAD_METHOD", "LOAD_NAME", "LOAD_SUPER_ATTR", "MAKE_CELL",
    "MAKE_FUNCTION", "MAP_ADD", "POP_JUMP_IF_FALSE", "POP_JUMP_IF_NONE",
    "POP_JUMP_IF_NOT_NONE", "POP_JUMP_IF_TRUE", "RAISE_VARARGS", "RESUME",
    "RETURN_VALUE", "SEND", "SET_ADD", "SET_FUNCTION_ATTRIBUTE", "SET_UPDATE",
    "STORE_ATTR", "STORE_DEREF", "STORE_FAST", "STORE_GLOBAL", "STORE_NAME",
    "SWAP", "UNPACK_EX", "UNPACK_SEQUENCE",
}
_NAME = {
    "DELETE_GLOBAL", "DELETE_NAME", "LOAD_ATTR", "LOAD_GLOBAL", "LOAD_METHOD",
    "LOAD_NAME", "LOAD_SUPER_ATTR", "STORE_ATTR", "STORE_GLOBAL", "STORE_NAME",
}
_CONST = {"LOAD_CONST"}
_JUMP = {
    "FOR_ITER", "JUMP_BACKWARD", "JUMP_BACKWARD_NO_INTERRUPT", "JUMP_FORWARD",
    "POP_JUMP_IF_FALSE", "POP_JUMP_IF_NONE", "POP_JUMP_IF_NOT_NONE",
    "POP_JUMP_IF_TRUE", "SEND",
}
_FREE = {
    "DELETE_DEREF", "LOAD_CLASSDEREF", "LOAD_DEREF", "LOAD_FROM_DICT_OR_DEREF",
    "MAKE_CELL", "STORE_DEREF",
}
_LOCAL = {
    "DELETE_FAST", "LOAD_FAST", "LOAD_FAST_AND_CLEAR", "LOAD_FAST_CHECK",
    "STORE_FAST",
}
_EXC = {
    "CALL", "CALL_FUNCTION_EX", "CALL_KW", "CHECK_EG_MATCH", "CHECK_EXC_MATCH",
    "FOR_ITER", "LOAD_ATTR", "LOAD_GLOBAL", "PUSH_EXC_INFO", "RAISE_VARARGS",
    "RERAISE", "WITH_EXCEPT_START",
}


def _values(names):
    return {_opmap[name] for name in names if name in _opmap}


_ARG_VALUES = _values(_ARG)
_NAME_VALUES = _values(_NAME)
_CONST_VALUES = _values(_CONST)
_JUMP_VALUES = _values(_JUMP)
_FREE_VALUES = _values(_FREE)
_LOCAL_VALUES = _values(_LOCAL)
_EXC_VALUES = _values(_EXC)


def has_arg(op):
    return op in _ARG_VALUES


def has_const(op):
    return op in _CONST_VALUES


def has_name(op):
    return op in _NAME_VALUES


def has_jump(op):
    return op in _JUMP_VALUES


def has_free(op):
    return op in _FREE_VALUES


def has_local(op):
    return op in _LOCAL_VALUES


def has_exc(op):
    return op in _EXC_VALUES


def is_valid(op):
    return op in _opmap.values()


def stack_effect(op, oparg=None, *, jump=None):
    # Without a bytecode stack there is no consumer for exact effects in
    # Mule; CPython's table is large, so this reports the neutral value
    # for opcodes that don't pop or push net values and 0 otherwise.
    return 0


def get_intrinsic1_descs():
    return [
        "INTRINSIC_1_INVALID", "INTRINSIC_PRINT", "INTRINSIC_IMPORT_STAR",
        "INTRINSIC_STOPITERATION_ERROR", "INTRINSIC_ASYNC_GEN_WRAP",
        "INTRINSIC_UNARY_POSITIVE", "INTRINSIC_UNARY_NEGATIVE",
        "INTRINSIC_UNARY_NOT", "INTRINSIC_UNARY_INVERT",
        "INTRINSIC_LIST_TO_TUPLE", "INTRINSIC_TYPEVAR", "INTRINSIC_PARAMSPEC",
        "INTRINSIC_TYPEVARTUPLE", "INTRINSIC_SUBSCRIPT_GENERIC",
        "INTRINSIC_TYPEALIAS",
    ]


def get_intrinsic2_descs():
    return [
        "INTRINSIC_2_INVALID", "INTRINSIC_PREP_RERAISE_STAR",
        "INTRINSIC_TYPEVAR_WITH_BOUND", "INTRINSIC_TYPEVAR_WITH_CONSTRAINTS",
        "INTRINSIC_SET_FUNCTION_TYPE_PARAMS", "INTRINSIC_SET_TYPEPARAM_DEFAULTS",
    ]


def get_special_method_names():
    return ["__enter__", "__exit__", "__aenter__", "__aexit__"]


def get_nb_ops():
    return [
        ("NB_ADD", "+"), ("NB_AND", "&"), ("NB_FLOOR_DIVIDE", "//"),
        ("NB_LSHIFT", "<<"), ("NB_MATRIX_MULTIPLY", "@"), ("NB_MULTIPLY", "*"),
        ("NB_REMAINDER", "%"), ("NB_OR", "|"), ("NB_POWER", "**"),
        ("NB_RSHIFT", ">>"), ("NB_SUBTRACT", "-"), ("NB_TRUE_DIVIDE", "/"),
        ("NB_XOR", "^"), ("NB_INPLACE_ADD", "+="), ("NB_INPLACE_AND", "&="),
        ("NB_INPLACE_FLOOR_DIVIDE", "//="), ("NB_INPLACE_LSHIFT", "<<="),
        ("NB_INPLACE_MATRIX_MULTIPLY", "@="), ("NB_INPLACE_MULTIPLY", "*="),
        ("NB_INPLACE_REMAINDER", "%="), ("NB_INPLACE_OR", "|="),
        ("NB_INPLACE_POWER", "**="), ("NB_INPLACE_RSHIFT", ">>="),
        ("NB_INPLACE_SUBTRACT", "-="), ("NB_INPLACE_TRUE_DIVIDE", "/="),
        ("NB_INPLACE_XOR", "^="),
    ]


def get_executor(code, offset):
    return None


def get_specialization_stats():
    return None

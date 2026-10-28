"""Final module functions keep a fixed binding and a known callable target."""

import re

import pytest

from mule.compiler import emit_llvm
from test.mule.compiler.test_modules import execute

TYPING_SOURCE = """\
def final(function):
    function.__final__ = True
    return function
"""


def _llvm_function(llvm, name):
    declaration = re.search(
        rf'^define [^\n]*@"?{re.escape(name)}"?\([^\n]*\)[^\n]*\{{',
        llvm,
        re.MULTILINE,
    )
    assert declaration is not None, f"LLVM is missing function {name}"
    end = llvm.find("\n}", declaration.start())
    assert end >= 0, f"LLVM function {name} has no closing brace"
    return llvm[declaration.start() : end + 2]


def test_final_function_calls_retain_target_and_bytearray_specialization(tmp_path):
    (tmp_path / "typing.py").write_text(TYPING_SOURCE, encoding="utf-8")
    source = """\
from typing import final as fixed

@fixed
def append_varint(output, value):
    if value < 0:
        raise ValueError('negative')
    while value >= 128:
        output.append((value & 127) | 128)
        value >>= 7
    output.append(value)

def encode(data):
    output = bytearray()
    for byte in data:
        append_varint(output, byte & 127)
    return output
"""
    llvm = emit_llvm(source, str(tmp_path / "main.py"), roots=(tmp_path,))
    encode = _llvm_function(llvm, "mule_fn_encode.body")
    direct_call = re.search(
        r'call ptr @"(mule\.direct\.mule_fn_append_varint\.[^"]+)"\(',
        encode,
    )
    assert direct_call is not None, "the final helper needs a guarded direct call"

    direct_adapter = _llvm_function(llvm, direct_call.group(1))
    observed_entry = re.search(
        r'call ptr @"(mule_fn_append_varint\.spec\.[^"]+)"\(', direct_adapter
    )
    assert observed_entry is not None, "the adapter must select an observed entry"
    specialized_body = _llvm_function(llvm, observed_entry.group(1))
    assert "@mule_bytearray_append_fast(" in specialized_body


@pytest.mark.parametrize(
    "mutation",
    ["read = replacement\n", "del read\n"],
    ids=["reassignment", "deletion"],
)
def test_final_function_rejects_module_reassignment_and_deletion(tmp_path, mutation):
    with pytest.raises(SyntaxError, match="final"):
        execute(
            tmp_path,
            {"typing.py": TYPING_SOURCE},
            "from typing import final\n"
            "@final\n"
            "def read():\n    return 1\n"
            "def replacement():\n    return 2\n" + mutation,
        )


def test_final_function_rejects_dynamic_global_reassignment_and_keeps_value(tmp_path):
    result = execute(
        tmp_path,
        {"typing.py": TYPING_SOURCE},
        """\
from typing import final

@final
def read():
    return 17

def replacement():
    return 99

try:
    globals()['read'] = replacement
except TypeError:
    print('binding protected')
print(read())
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "binding protected\n17\n"


def test_final_function_import_alias_and_qualified_form_keep_their_own_targets(
    tmp_path,
):
    (tmp_path / "typing.py").write_text(TYPING_SOURCE, encoding="utf-8")
    source = """\
from typing import final as imported_final
import typing as typing_alias

@imported_final
def first(value):
    return value + 1

@typing_alias.final
def second(value):
    return value + 2

def call_first(value):
    return first(value)

def call_second(value):
    return second(value)
"""
    llvm = emit_llvm(source, str(tmp_path / "main.py"), roots=(tmp_path,))
    for caller, callee in (("call_first", "first"), ("call_second", "second")):
        body = _llvm_function(llvm, f"mule_fn_{caller}.body")
        assert re.search(
            rf'call ptr @"mule\.direct\.mule_fn_{callee}\.[^"]+"\(', body
        ), f"{caller} must retain the target of {callee}"


def test_other_identity_decorator_does_not_make_a_binding_final(tmp_path):
    result = execute(
        tmp_path,
        {},
        """\
def identity(function):
    return function

@identity
def read():
    return 1

def replacement():
    return 2

read = replacement
print(read())
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "2\n"


def test_final_module_function_preserves_recursive_and_closure_behavior(tmp_path):
    result = execute(
        tmp_path,
        {"typing.py": TYPING_SOURCE},
        """\
from typing import final

@final
def parity(value):
    if value == 0:
        return True
    return not parity(value - 1)

@final
def make_offsetter(offset):
    def add(value):
        return value + offset
    return add

left = make_offsetter(3)
right = make_offsetter(8)
print(parity.__final__, make_offsetter.__final__, parity(14), left(10), right(10))
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True True True 13 18\n"


def test_final_outside_another_decorator_marks_and_seals_decorated_result(tmp_path):
    result = execute(
        tmp_path,
        {"typing.py": TYPING_SOURCE},
        """\
from typing import final

def add_ten(function):
    def wrapper():
        return function() + 10
    return wrapper

@final
@add_ten
def value():
    return 1

print(value(), value.__final__)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "11 True\n"


def test_shadowed_typing_final_name_does_not_seal_another_function(tmp_path):
    result = execute(
        tmp_path,
        {"typing.py": TYPING_SOURCE},
        """\
from typing import final

def identity(function):
    return function

final = identity

@final
def read():
    return 1

def replacement():
    return 2

read = replacement
print(read())
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "2\n"


def test_final_before_import_still_evaluates_the_unbound_name(tmp_path):
    result = execute(
        tmp_path,
        {"typing.py": TYPING_SOURCE},
        """\
try:
    @final
    def early():
        return 1
except NameError:
    print('not imported yet')

from typing import final

@final
def later():
    return 2

print(later())
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "not imported yet\n2\n"


def test_final_binding_blocks_global_dict_and_module_mutation_operations(tmp_path):
    result = execute(
        tmp_path,
        {"typing.py": TYPING_SOURCE},
        """\
from typing import final
import __main__ as module

@final
def read():
    return 17

def replacement():
    return 99

namespace = globals()
for mutate in (
    lambda: namespace.pop('read'),
    lambda: namespace.update({'read': replacement}),
    lambda: namespace.__delitem__('read'),
    lambda: setattr(module, 'read', replacement),
    lambda: delattr(module, 'read'),
):
    try:
        mutate()
    except TypeError:
        print(read())

saved_read, saved_print, saved_type_error = read, print, TypeError
try:
    namespace.clear()
except saved_type_error:
    saved_print(saved_read())
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "17\n" * 6


def test_unrelated_final_attribute_does_not_disable_final_binding(tmp_path):
    result = execute(
        tmp_path,
        {"typing.py": TYPING_SOURCE},
        """\
from typing import final

def other():
    pass
other.final = 0

@final
def read():
    return 17

try:
    globals()['read'] = other
except TypeError:
    print(read(), read.__final__)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "17 True\n"


def test_replaced_qualified_final_decorator_keeps_runtime_semantics(tmp_path):
    result = execute(
        tmp_path,
        {"typing.py": TYPING_SOURCE},
        """\
import typing as annotation_module
alias = annotation_module

def identity(function):
    return function

alias.final = identity

@annotation_module.final
def read():
    return 1

def replacement():
    return 2

read = replacement
print(read())
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "2\n"


def test_final_function_named_doc_preserves_generated_module_initialization(tmp_path):
    result = execute(
        tmp_path,
        {"typing.py": TYPING_SOURCE},
        """\
from typing import final

@final
def __doc__():
    return 17

print(__doc__(), __doc__.__final__)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "17 True\n"


@pytest.mark.parametrize("mutation", ["read = None", "del read"])
def test_final_function_rejects_global_mutation_inside_earlier_function(
    tmp_path, mutation
):
    with pytest.raises(SyntaxError, match="final"):
        execute(
            tmp_path,
            {"typing.py": TYPING_SOURCE},
            "from typing import final\n"
            "def change():\n"
            "    global read\n"
            f"    {mutation}\n"
            "@final\n"
            "def read():\n"
            "    return 17\n",
        )

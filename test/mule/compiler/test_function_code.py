"""Immutable native code/signature metadata without a bytecode evaluator."""

import pytest

from test.mule.compiler.test_exception_groups import compare


def test_parameter_layout_defaults_flags_and_code_identity(tmp_path):
    compare(
        tmp_path,
        """
def function(first, second=2, /, third=3, *rest, required, optional=4, **extra):
    local = first
    return local
info = function.__code__
print(info.co_name, info.co_qualname, info.co_firstlineno)
print(info.co_argcount, info.co_posonlyargcount, info.co_kwonlyargcount, info.co_flags)
print(info.co_varnames, info.co_nlocals, info.co_cellvars, info.co_freevars)
print(function.__defaults__, function.__kwdefaults__)
print(function.__defaults__ is function.__defaults__, function.__kwdefaults__ is function.__kwdefaults__)
print(info is function.__code__)
try:
    info.co_argcount = 0
except AttributeError:
    print('readonly')
def factory(value):
    def inner(default=value, *, keyword=value):
        return value
    return inner
first, second = factory(1), factory(2)
print(first.__code__ is second.__code__)
print(first.__defaults__, second.__defaults__, first.__kwdefaults__, second.__kwdefaults__)
print(first.__code__.co_freevars, factory.__code__.co_cellvars)
def generator():
    yield 1
async def coroutine():
    return 1
async def async_generator():
    yield 1
for func in (generator, coroutine, async_generator):
    print(func.__code__.co_flags)
left, right = (lambda one: one), (lambda two, three: two)
print(left.__code__ is right.__code__, left.__code__.co_varnames, right.__code__.co_varnames)
def broken():
    raise ValueError('broken')
try:
    broken()
except ValueError as error:
    print(error.__traceback__.tb_next.tb_frame.f_code is broken.__code__)
""",
    )


@pytest.mark.integration
def test_inspect_signature_reads_native_metadata(tmp_path):
    compare(
        tmp_path,
        """
import inspect
def function(first: int, second=2, /, third: str='value', *rest, required, optional=4, **extra) -> bool:
    return True
class Example:
    def method(self, value=3, *, extra=4):
        return value
print(inspect.signature(function))
print(inspect.signature(Example.method), inspect.signature(Example().method))
""",
    )

"""Leading function documentation survives native definition and decoration."""

from mule.compiler import emit_llvm
from test.mule.compiler.test_exception_groups import compare


def test_function_docstrings_definitions_decorators_and_mutation(tmp_path):
    result = compare(
        tmp_path,
        """
' module\\n  documentation\\n\\n'
print(repr(__doc__))
def decorator(function):
    print('decorating', repr(function.__doc__))
    return function
@decorator
def documented():
    'literal\\n  second line —'
    return 3
def missing():
    pass
def late():
    value = 2
    'not documentation'
    return value
def nested():
    def inner():
        'nested'
        return 5
    return inner
async def asynchronous():
    'async documentation'
class Example:
    ' class\\n  documentation'
    def method(self):
        'method documentation'
    @property
    def value(self):
        'property documentation'
        return 4
for function in (documented, missing, late, nested(), asynchronous, lambda: 1):
    print(repr(function.__doc__))
print(Example.method.__doc__, Example.value.__doc__)
def new_getter(self):
    'replacement documentation'
    return 6
print(Example.value.getter(new_getter).__doc__)
print(property(new_getter, doc='explicit').getter(Example.value.fget).__doc__)
print(repr(Example.__doc__))
print(documented())
documented.__doc__ = ['mutable']
print(documented.__doc__)
documented.__doc__ = None
print(documented.__doc__)
def whitespace():
    '\\n\\n  first\\n\\tsecond\\n\\n'
print(repr(whitespace.__doc__))
""",
    )
    assert result.stderr == ""


def test_docstring_and_literal_statements_emit_no_code_in_the_body():
    """``__doc__`` is set when the function is defined; the body itself must
    not build the docstring, or any other literal statement, on each call."""
    llvm = emit_llvm(
        "def run(value):\n"
        " 'documentation that is long enough to be distinctive'\n"
        " 42\n"
        " 'not documentation'\n"
        " return value + 1\n"
        "print(run(1), run.__doc__)\n"
    )
    body = llvm.split('define internal ptr @"mule_fn_run.body"(')[1].split("\n}")[0]
    assert "mule_literal_text" not in body
    assert "documentation" not in body

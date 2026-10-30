"""Builtin calls and protocol reads that compile to direct primitives."""

from mule.compiler import emit_llvm
from mule.compiler.codegen import ModuleGenerator, Program
from mule.compiler.modules import ModuleCompiler
from mule.compiler.protocols import PRIMITIVES, compile_protocol
from test.mule.compiler.test_exception_groups import compare
from test.mule.compiler.test_llvm_boxing import function_body


def test_type_of_one_argument_reads_the_class_without_a_call():
    llvm = emit_llvm("""
import sys

def kind(value):
    return type(value) is str

print(kind(sys.argv[0]))
""")
    body = function_body(llvm, '"mule_fn_kind.body"')
    assert "call ptr @mule_type_of(" in body
    assert "mule.call.1" not in body


def test_type_builtin_preserves_metaclass_and_class_creation(tmp_path):
    compare(
        tmp_path,
        """
class Meta(type):
    pass

class Base(metaclass=Meta):
    pass

def kinds(*values):
    return [type(value).__name__ for value in values]

print(kinds(1, True, None, 'text', Base, Base(), type))
print(type(*[2.5]).__name__)
Made = type('Made', (Base,), {'field': 7})
print(Made.__name__, Made.field, type(Made).__name__)
try:
    type(1, 2)
except TypeError as error:
    print(type(error).__name__)

def shadowed():
    type = len
    return type('four')

print(shadowed())
""",
    )


def test_text_containment_searches_without_protocol_dispatch():
    llvm = emit_llvm("""
import sys

def flags(pattern: str, flag: str) -> int:
    count = 0
    if flag in pattern:
        count += 1
    if flag not in "-+ #0":
        count += 2
    return count

print(flags(sys.argv[0], sys.argv[0][:1]))
""")
    body = function_body(llvm, '"mule_fn_flags.spec.0"')
    assert '@"mule.protocol.contains"' in body
    assert '@"mule.protocol.not_contains"' in body


def test_text_containment_matches_python(tmp_path):
    compare(
        tmp_path,
        """
def probe(item: str, text: str):
    return item in text, item not in text

for item, text in [('', ''), ('', 'abc'), ('a', ''), ('bc', 'abc'), ('cb', 'abc'),
                   ('é', 'café'), ('abcd', 'abc'), ('\\x00', 'a\\x00b')]:
    print(repr(item), repr(text), probe(item, text))
""",
    )


def test_named_field_of_unproven_receiver_probes_the_site_cache():
    compilation = compile_protocol(
        """
def used(table):
    _set_instance_field(table, '_used', _instance_field(table, '_used') + 1)
""",
        "<named-field-cache>",
        namespace="$mule.named_field_cache",
    )
    function = next(
        function
        for function in compilation.lambdas
        if function.code is not None and function.code.qualname == "used"
    )
    module = ModuleGenerator(Program(compilation.lambdas, None, compilation.facts))
    general, _ = module.contexts(function)
    body = module.emit_body(function, general)
    assert "@mule.attr.hit(" in body
    assert "@mule.attr.field(" in body
    assert "@mule_instance_field_name_cached(" in body
    assert "@mule_set_instance_field_name_cached(" in body
    assert "@mule_instance_field_name(" not in body
    assert "@mule_set_instance_field_name(" not in body


def test_builtin_function_call_enters_the_builtins_unit_directly():
    llvm = emit_llvm("""
import sys

def width(value):
    return len(repr(value))

print(width(sys.argv))
""")
    body = function_body(llvm, '"mule_fn_width.body"')
    assert "mule_fn_repr" in body
    assert "@mule.call.1(" not in body


def test_builtin_function_call_follows_runtime_rebinding(tmp_path):
    compare(
        tmp_path,
        """
import builtins

def show(value):
    return repr(value)

print(show(1))
original = builtins.repr
builtins.repr = lambda value: 'patched'
print(show(1))
builtins.repr = original
print(show([2]))
""",
    )


def test_constant_percent_pattern_formats_without_the_modulo_protocol():
    llvm = emit_llvm("""
import sys

def item(key, value):
    return '%s: %r (100%%)' % (key, value)

print(item(sys.argv[0], len(sys.argv)))
""")
    body = function_body(llvm, '"mule_fn_item.body"')
    assert "mule_format_modulo" not in body
    assert "mule_format_representation" in body


def test_constant_percent_pattern_matches_python(tmp_path):
    compare(
        tmp_path,
        """
class Loud:
    def __repr__(self):
        print('repr')
        return 'Loud()'
    def __str__(self):
        print('str')
        return 'loud'

def side(value):
    print('evaluate', value)
    return value

print('%s: %s' % (side('key'), side(2)))
print('%r=%s, 100%%' % (Loud(), Loud()))
print('%a|%r' % ('\\xe9', '\\xe9'))
print('abc%%' % ())
print('%s%s' % ((1, 2), [3]))
print('%5s|%-3d' % ('a', 4))
for operands in [(1,), (1, 2, 3)]:
    try:
        print('%s %s' % tuple(operands))
    except TypeError as error:
        print('TypeError', error)
class Bad:
    def __str__(self):
        raise ValueError('bad str')
try:
    print('%s %s' % (side('first'), Bad()))
except ValueError as error:
    print('ValueError', error)
""",
    )


def test_class_attribute_reads_track_class_and_metaclass_changes(tmp_path):
    compare(
        tmp_path,
        """
class Meta(type):
    @property
    def tag(cls):
        return 'meta-tag'
    shared = 'meta-shared'

class A(metaclass=Meta):
    tag = 'class-tag'
    x = 1
    @classmethod
    def cm(cls):
        return cls.__name__
    @staticmethod
    def sm():
        return 'sm'
    def m(self):
        return 'm'

class B(A):
    pass

def read(c):
    return (c.tag, c.x, c.cm(), c.sm(), c.m.__name__, c.shared,
            getattr(c, 'missing', 'default'))

for c in (A, B, A, B):
    print(read(c))
A.x = 2
print(read(B))
B.x = 3
print(read(A), read(B))
del B.x
print(read(B))
Meta.shared = 'changed'
print(read(A))
A.cm = classmethod(lambda cls: 'replaced')
print(read(B))
print(getattr(int, '__repr__', None) is int.__repr__)
""",
    )


def test_native_export_call_compiles_to_its_primitive():
    compilation = compile_protocol(
        """
from _mule_native import type_of
from _mule_native import is_class

def classify(value):
    if is_class(value):
        return value
    return type_of(value)

def passes(function):
    return function

def handle():
    return passes(type_of)
""",
        "<native-exports>",
        namespace="$mule.native_exports",
        imports=ModuleCompiler("<native-exports>", primitives=PRIMITIVES),
    )
    module = ModuleGenerator(Program(compilation.lambdas, None, compilation.facts))

    def body(name):
        function = next(
            function
            for function in compilation.lambdas
            if function.code is not None and function.code.qualname == name
        )
        general, _ = module.contexts(function)
        return module.emit_body(function, general)

    classify = body("classify")
    assert "@mule_type_of(" in classify
    assert "@mule_is_class(" in classify
    assert "@mule.call.1(" not in classify
    # A reference that is not a call still reads the imported function.
    assert "@mule_type_of(" not in body("handle")

"""Builtin calls and protocol reads that compile to direct primitives."""

from mule.compiler import emit_llvm
from mule.compiler.codegen import ModuleGenerator, Program
from mule.compiler.protocols import compile_protocol
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
    assert '@"mule.protocol.text_contains"' in body
    assert '@"mule.protocol.text_not_contains"' in body
    assert '@"mule.protocol.contains"' not in body
    assert '@"mule.protocol.not_contains"' not in body


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

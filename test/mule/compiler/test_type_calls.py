"""Metaclass super calls share the compiled constructor protocol."""

from test.mule.compiler.test_exception_groups import compare


def test_type_call_and_metaclass_super_preserve_creation_and_initialization(tmp_path):
    compare(
        tmp_path,
        """
class Meta(type):
    def __call__(cls, *args, **kwargs):
        print('meta', cls.__name__)
        return super().__call__(*args, **kwargs)
    def create(cls, *args, **kwargs):
        return super().__call__(*args, **kwargs)
class Example(metaclass=Meta):
    def __new__(cls, value, *, extra=0):
        print('new', value, extra)
        return object.__new__(cls)
    def __init__(self, value, *, extra=0):
        print('init', value, extra)
        self.value = value + extra
print(Example(3, extra=4).value)
print(Example.create(5, extra=6).value)
print(type.__call__(Example, 7, extra=8).value)
class Foreign(metaclass=Meta):
    def __new__(cls, value):
        return value
    def __init__(self, value):
        raise AssertionError('foreign result was initialized')
print(Foreign.create(42))
""",
    )

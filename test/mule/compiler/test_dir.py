"""Attribute names come from each class family, without executing properties."""

from test.mule.compiler.test_exception_groups import compare


def test_dir_modules_classes_instances_hooks_and_descriptors(tmp_path):
    compare(
        tmp_path,
        """
import types
module = types.ModuleType('example')
module.zebra = 1
module.apple = 2
print(dir(module))
module.__dir__ = lambda: ('zebra', 'apple', 'zebra')
print(dir(module))
class Meta(type):
    meta_only = 1
class Base(metaclass=Meta):
    inherited = 1
    @property
    def broken(self):
        raise RuntimeError('property must not run')
class Child(Base):
    own = 2
instance = Child()
instance.dynamic = 3
for value in (Child, instance):
    names = dir(value)
    print(names == sorted(names), len(names) == len(set(names)))
    print([name for name in names if name in ('inherited', 'own', 'dynamic', 'broken', 'meta_only')])
del instance.dynamic
Child.later = 4
print('dynamic' in dir(instance), 'later' in dir(Child), 'later' in dir(instance))
class Custom:
    def __dir__(self):
        return (name for name in ('zebra', 'apple'))
print(dir(Custom()))
class Static:
    @staticmethod
    def __dir__():
        return ['zebra', 'apple']
print(dir(Static()))
class Bad:
    def __dir__(self):
        return 1
try:
    dir(Bad())
except TypeError:
    print('invalid iterable')
""",
    )

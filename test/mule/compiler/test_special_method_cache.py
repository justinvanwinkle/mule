"""Special-method dispatch follows class attribute changes made after use."""

from test.mule.compiler.test_exception_groups import compare


def test_special_methods_follow_later_class_and_base_assignments(tmp_path):
    compare(
        tmp_path,
        """
class Base:
    pass

class Derived(Base):
    pass

instance = Derived()
instance.value = 1
print(instance.value)

def logging_setattr(self, name, value):
    print("set", name, value)
    object.__setattr__(self, name, value)

Base.__setattr__ = logging_setattr
instance.value = 2
print(instance.value)
del Base.__setattr__
instance.value = 3
print(instance.value)

class Counter:
    def __init__(self):
        self.calls = 0

counter = Counter()
print(len.__name__, hasattr(counter, "__len__"))
Counter.__len__ = lambda self: 7
print(len(counter))
del Counter.__len__
try:
    len(counter)
except TypeError as error:
    print(type(error).__name__)

class Items:
    def __iter__(self):
        return iter([1, 2])

print(list(Items()))
Items.__iter__ = lambda self: iter([3])
print(list(Items()))

class Shadowed(Base):
    pass

Base.__repr__ = lambda self: "base repr"
print(repr(Shadowed()))
Shadowed.__repr__ = lambda self: "shadowed repr"
print(repr(Shadowed()))
del Shadowed.__repr__
print(repr(Shadowed()))
""",
    )

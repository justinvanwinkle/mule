"""Subclasses receive complete empty dictionary storage before their initializer."""

from test.mule.compiler.test_exception_groups import compare


def test_dict_new_and_subclass_initializers_match_python(tmp_path):
    compare(
        tmp_path,
        """
class Empty(dict):
    def __init__(self, *args, **kwargs):
        self.seen = len(self)
value = Empty({'ignored': 1}, ignored=2)
print(value.seen, len(value), dict(value))
class Custom(dict):
    def __init__(self, other=()):
        self['first'] = 1
        dict.update(self, other)
value = Custom({'second': 2})
print(dict(value))
dict.__init__(value, {'third': 3}, fourth=4)
print(dict(value))
dict.__init__(value)
print(dict(value))
empty = dict.__new__(dict, {'ignored': 1}, ignored=2)
print(empty, len(empty))
dict.__init__(empty, [('item', 5)])
print(empty)
for cls in (object, list):
    try:
        dict.__new__(cls)
    except TypeError:
        print('not a dictionary subtype')
""",
    )


def test_ordered_dict_uses_base_storage_and_noncallable_weak_links(tmp_path):
    compare(
        tmp_path,
        """
from collections import OrderedDict
from weakref import proxy, CallableProxyType, ProxyType
value = OrderedDict((('first', 1), ('second', 2)))
value['third'] = 3
print(list(value.items()), value['second'])
value.move_to_end('first')
print(list(value.items()))
del value['second']
print(value.popitem(), list(value.items()))
class Plain:
    pass
class Callable:
    def __call__(self):
        return 42
plain, callable_value = Plain(), Callable()
plain_proxy, callable_proxy = proxy(plain), proxy(callable_value)
print(type(plain_proxy) is ProxyType, type(callable_proxy) is CallableProxyType, callable_proxy())
""",
    )

"""Weak-reference eligibility and borrowed lifetimes match Python 3.14."""

from test.mule.compiler.test_exception_groups import compare


def test_weakref_subclass_slots_and_lifetimes(tmp_path):
    compare(
        tmp_path,
        """
import weakref
for base in (object, list, dict, str, int, tuple, bytes, float, set, Exception):
    for slots in (None, ()):
        namespace = {} if slots is None else {'__slots__': slots}
        cls = type('Sub', (base,), namespace)
        value = cls()
        try:
            reference = weakref.ref(value)
            print(base.__name__, slots, reference() is value)
        except TypeError:
            print(base.__name__, slots, 'TypeError')
class WeakList(list):
    __slots__ = ('__weakref__',)
class Child(WeakList):
    __slots__ = ()
for cls in (WeakList, Child):
    events = []
    value = cls([1, 2])
    reference = weakref.ref(value, lambda ref: events.append(ref() is None))
    print(reference() is value, list(reference()))
    del value
    print(reference() is None, events)
for value in (object(), [], {}, '', 1, (), b'', 1.0, Exception()):
    try:
        weakref.ref(value)
    except TypeError:
        print('builtin rejected', type(value).__name__)
""",
    )

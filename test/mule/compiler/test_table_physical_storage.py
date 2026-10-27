"""Builtin table algorithms use their physical storage, not subclass attributes."""

from test.mule.compiler.rewrite_support import build_case


def test_replaced_value_can_clear_its_dictionary_during_release(tmp_path):
    case = build_case(
        tmp_path,
        """
import weakref
events = []
mapping = {}
class Value:
    pass
def released(reference):
    events.append("released")
    mapping.clear()
value = Value()
watch = weakref.ref(value, released)
mapping["key"] = value
del value
mapping["key"] = None
print(events, len(mapping))
""",
    )
    case.compare()


def test_table_subclass_descriptors_do_not_replace_builtin_storage(tmp_path):
    case = build_case(
        tmp_path,
        """
events = []
class Trap:
    def __get__(self, instance, owner=None):
        events.append("get")
        raise AssertionError("private storage descriptor was read")
    def __set__(self, instance, value):
        events.append("set")
        raise AssertionError("private storage descriptor was written")
class HiddenDict(dict):
    _indices = _hashes = _keys = _values = _used = Trap()
class HiddenSet(set):
    _indices = _hashes = _keys = _values = _used = Trap()
class HiddenFrozen(frozenset):
    _indices = _hashes = _keys = _values = _used = Trap()
mapping = HiddenDict(a=1, b=2)
mapping["a"] = 3
mapping.update({"c": 4})
print(mapping.get("a"), mapping["b"], len(mapping), list(mapping))
print(list(mapping.values()), list(mapping.items()), list(reversed(mapping)))
print(len(mapping.keys()), ("a", 3) in mapping.items())
print(mapping.pop("b"), mapping.setdefault("d", 5), mapping.copy())
mapping.clear()
mapping["next"] = 6
print(mapping, len(mapping))
items = HiddenSet([3, 1, 2])
items.add(4)
items.discard(2)
items.remove(1)
print(sorted(items), len(items), sorted(items.union({7})))
items.update([5, 3])
print(sorted(items), items == {3, 4, 5}, items.issubset({3, 4, 5, 7}))
items.clear()
print(len(items))
frozen = HiddenFrozen([3, 2, 3])
print(sorted(frozen), len(frozen), hash(frozen) == hash(frozenset([2, 3])))
print(events)
""",
        optimize=1,
    )
    case.compare()

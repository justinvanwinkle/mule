"""Manual cycles preserve roots and clear weak targets like Python 3.14."""

from test.mule.compiler.test_exception_groups import compare


def test_unreachable_instance_container_closure_and_class_cycles(tmp_path):
    compare(
        tmp_path,
        """
import gc
import weakref
gc.disable()
gc.collect()
class Node:
    pass
events = []
def callback(ref):
    events.append(ref() is None)
def make_cycle(kind):
    obj = Node()
    if kind == 'self':
        obj.link = obj
    elif kind == 'dict':
        obj.link = {'node': obj, 'duplicate': obj}
    elif kind == 'list':
        obj.link = [obj, (obj,)]
    elif kind == 'method':
        obj.link = obj.__repr__
    else:
        def closed():
            return obj
        obj.link = closed
    return weakref.ref(obj, callback)
refs = [make_cycle(kind) for kind in ('self', 'dict', 'list', 'method', 'closure')]
print('before', all(ref() is not None for ref in refs))
print('collected', gc.collect() > 0)
print('after', all(ref() is None for ref in refs), len(events), all(events))
live = Node()
live.link = live
ref = weakref.ref(live, callback)
gc.collect()
print('root', ref() is live, live.link is live)
del live
gc.collect()
print('released', ref() is None)
def make_type():
    class Local:
        pass
    Local.loop = Local
    return weakref.ref(Local, callback)
type_ref = make_type()
gc.collect()
print('class', type_ref() is None)
class Number(float):
    pass
class Text(str):
    pass
def typed_cycle(cls, value):
    obj = cls(value)
    obj.link = obj
    return weakref.ref(obj)
typed_refs = [typed_cycle(Number, 7.0), typed_cycle(Text, 'hello')]
gc.collect()
print('typed', all(ref() is None for ref in typed_refs))
gc.collect()
print('stable', gc.collect() == 0)
for generation in (-1, 3):
    try:
        gc.collect(generation)
    except ValueError:
        print('generation')
""",
    )


def test_collection_callbacks_and_unreachable_weak_callbacks(tmp_path):
    compare(
        tmp_path,
        """
import gc
import weakref
gc.disable()
gc.collect()
class Node:
    pass
events = []
def callback(ref):
    events.append('weak')
def make():
    obj = Node()
    obj.self = obj
    obj.ref = weakref.ref(obj, callback)
make()
gc.collect()
print('unreachable callback', events)
def notification(phase, info):
    print(phase, info['generation'], info['uncollectable'], info['collected'] >= 0)
    print('nested', gc.collect())
gc.callbacks.append(notification)
gc.collect(0)
gc.callbacks.remove(notification)
import sys
def failing(phase, info):
    raise ValueError(phase)
def report(unraisable):
    print('ignored', unraisable.exc_type.__name__, str(unraisable.exc_value), unraisable.object is failing,
          unraisable.exc_traceback is not None)
original = sys.unraisablehook
sys.unraisablehook = report
gc.callbacks.append(failing)
gc.collect()
gc.callbacks.remove(failing)
sys.unraisablehook = original
obj = Node()
obj.items = [obj, obj]
refs = gc.get_referents(obj.items)
print('owned aliases', refs.count(obj))
print('tracked', gc.is_tracked(obj), gc.is_tracked(obj.items), gc.is_tracked(7), gc.is_tracked('text'))
print('snapshot root', any(value is obj for value in gc.get_objects()))
del refs
ref = weakref.ref(obj)
del obj
gc.collect()
print('gone', ref() is None)
""",
    )

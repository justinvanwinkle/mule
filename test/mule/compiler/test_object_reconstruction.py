"""Copy reconstruction allocates first, then restores Python state."""

from test.mule.compiler.test_exception_groups import compare


def test_copy_restores_required_dataclasses_slots_aliases_and_cycles(tmp_path):
    compare(
        tmp_path,
        """
import copy
from dataclasses import dataclass
calls = []
class Required:
    def __init__(self, value):
        calls.append('init')
        self.value = value
original = Required([1, 2])
for protocol in range(6):
    recipe = original.__reduce_ex__(protocol)
    allocated = recipe[0](*recipe[1])
    allocated.__dict__.update(recipe[2])
    print(protocol, allocated.value, calls)
original.alias = original.value
original.cycle = original
shallow = copy.copy(original)
deep = copy.deepcopy(original)
print(calls, shallow is not original, deep is not original)
print(shallow.value is original.value, shallow.cycle is original)
print(deep.value, deep.value is not original.value, deep.alias is deep.value, deep.cycle is deep)
Slotted = type('Slotted', (), {'__slots__': ('value', '__hidden', 'unset')})
slotted = Slotted()
slotted.value = [3]
object.__setattr__(slotted, '_Slotted__hidden', 'private')
print(sorted(object.__getstate__(slotted)[1].items()))
cloned = copy.deepcopy(slotted)
print(cloned.value, cloned.value is not slotted.value, cloned._Slotted__hidden, hasattr(cloned, 'unset'))
class Mixed:
    __slots__ = ('value', '__dict__')
    def __init__(self, value): self.value = value
mixed = Mixed([4])
mixed.extra = mixed.value
cloned = copy.deepcopy(mixed)
print(cloned.value, cloned.extra is cloned.value, cloned.value is not mixed.value)
@dataclass(slots=True, frozen=True)
class Frozen:
    value: list
frozen = Frozen([5])
cloned = copy.deepcopy(frozen)
print(cloned == frozen, cloned.value is not frozen.value)
print(object.__getstate__(object()) is None)
""",
    )


def test_reconstruction_new_arguments_state_hooks_and_reduction_overrides(tmp_path):
    compare(
        tmp_path,
        """
import copy
calls = []
class Keyword:
    def __new__(cls, value, *, flag):
        result = object.__new__(cls)
        result.value, result.flag = value, flag
        calls.append(('new', value, flag))
        return result
    def __init__(self, value, *, flag): calls.append('init')
    def __getnewargs_ex__(self): return (self.value,), {'flag': self.flag}
    def __getstate__(self): return {'extra': [1]}
    def __setstate__(self, state): self.extra = state['extra']
original = Keyword('key', flag=True)
cloned = copy.deepcopy(original)
print(calls, cloned.value, cloned.flag, cloned.extra)
class Positional:
    def __new__(cls, value):
        result = object.__new__(cls)
        result.value = value
        return result
    def __getnewargs__(self): return (self.value,)
print(copy.copy(Positional(7)).value)
class Override:
    def __reduce__(self): return str, ('override',)
print(copy.copy(Override()), copy.deepcopy(Override()))
for value in ([], (), ([], {}), ((), []), ((), {}, None)):
    class Invalid:
        def __getnewargs_ex__(self): return value
    try:
        object.__reduce_ex__(Invalid(), 4)
    except (TypeError, ValueError) as error:
        print(type(error).__name__, str(error))
class Invalid:
    def __getnewargs__(self): return []
try:
    copy.copy(Invalid())
except TypeError as error:
    print(type(error).__name__, str(error))
""",
    )

"""Compiled exception group behavior, compared with Python 3.14."""

import subprocess
import sys

from test.mule.compiler.test_modules import execute


def compare(tmp_path, source):
    path = tmp_path / "oracle.py"
    path.write_text(source)
    oracle = subprocess.run(
        [sys.executable, path], capture_output=True, text=True, timeout=10, check=False
    )
    assert oracle.returncode == 0, oracle.stderr
    native = execute(tmp_path, {}, source)
    assert native.returncode == 0, native.stderr
    assert native.stdout == oracle.stdout
    return native


def test_construction_selection_snapshots_aliases_and_validation(tmp_path):
    compare(
        tmp_path,
        """
values = [ValueError('one'), TypeError('two')]
group = BaseExceptionGroup('message', values)
print(type(group).__name__, str(group), repr(group))
print(group.args[1] is values, isinstance(group.exceptions, tuple))
values.append(RuntimeError('later'))
print(len(group.exceptions), len(group.args[1]))
base = BaseExceptionGroup('base', [KeyboardInterrupt()])
print(type(base).__name__, isinstance(base, Exception))
print(issubclass(ExceptionGroup, Exception), issubclass(ExceptionGroup, BaseExceptionGroup))
print(BaseExceptionGroup[ValueError].__origin__ is BaseExceptionGroup)
print(ExceptionGroup[ValueError].__args__ == (ValueError,))
for cls, message, items in (
    (BaseExceptionGroup, 1, values),
    (BaseExceptionGroup, 'empty', []),
    (BaseExceptionGroup, 'bad item', [1]),
    (BaseExceptionGroup, 'bad sequence', iter(values)),
    (ExceptionGroup, 'base child', [KeyboardInterrupt()]),
):
    try:
        cls(message, items)
    except (TypeError, ValueError) as error:
        print(type(error).__name__, str(error))
for name in ('message', 'exceptions'):
    try:
        setattr(group, name, None)
    except AttributeError:
        print('readonly', name)
try:
    raise group
except Exception as error:
    print('ordinary', error is group)
try:
    raise base
except Exception:
    print('wrong')
except BaseExceptionGroup as error:
    print('base group', error is base)
class Mixin:
    pass
class Mixed(Mixin, Exception):
    pass
try:
    raise Mixed('secondary base')
except Exception as error:
    print('secondary', str(error))
""",
    )


def test_recursive_selection_metadata_identity_and_predicates(tmp_path):
    compare(
        tmp_path,
        """
inner = ExceptionGroup('inner', [TypeError('t'), ValueError('v')])
group = BaseExceptionGroup('outer', [ValueError('first'), inner, KeyboardInterrupt()])
try:
    raise group
except BaseExceptionGroup as caught:
    group = caught
group.__cause__ = RuntimeError('cause')
group.__context__ = KeyError('context')
group.add_note('note')
match, rest = group.split(ValueError)
print(repr(match), repr(rest))
for part in (match, rest, match.exceptions[1]):
    print(type(part).__name__, part.message)
    if part is not match.exceptions[1]:
        print(part.__traceback__ is group.__traceback__, part.__cause__ is group.__cause__,
              part.__context__ is group.__context__, part.__suppress_context__)
        print(part.__notes__, part.__notes__ is group.__notes__)
print(group.subgroup(BaseException) is group, group.subgroup(OSError) is None)
print(group.split(BaseException)[0] is group, group.split(OSError)[1] is group)
print(repr(group.subgroup(lambda exception: isinstance(exception, ExceptionGroup))))
visits = []
def predicate(exception):
    visits.append(type(exception).__name__)
    return isinstance(exception, ValueError)
print(repr(group.subgroup(predicate)), visits)
print(type(inner.derive([KeyboardInterrupt()])).__name__)
for condition in (int, (ValueError, 1), 42):
    try:
        group.split(condition)
    except TypeError as error:
        print(str(error))
""",
    )


def test_custom_derivation_preserves_subclass_and_rejects_invalid_results(tmp_path):
    compare(
        tmp_path,
        """
class Custom(BaseExceptionGroup):
    def derive(self, exceptions):
        return Custom(self.message, exceptions)
group = Custom('custom', [ValueError('v'), TypeError('t')])
match, rest = group.split(ValueError)
print(type(group).__name__, type(match).__name__, type(rest).__name__)
print(repr(match), repr(rest))
class Bad(BaseExceptionGroup):
    def derive(self, exceptions):
        return ValueError('wrong return')
try:
    Bad('bad', [ValueError(), TypeError()]).split(ValueError)
except TypeError as error:
    print(str(error))
class Sequence:
    def __getitem__(self, index):
        if index == 0:
            return ValueError('sequence')
        raise IndexError
print(BaseExceptionGroup('sequence', Sequence()))
""",
    )

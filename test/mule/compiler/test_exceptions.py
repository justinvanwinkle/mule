"""Native exception transfers preserve locals, ownership, and handler state."""

from test.mule.compiler.test_exception_groups import compare
from test.mule.compiler.test_modules import execute


def test_key_error_formats_single_argument_with_repr(tmp_path):
    compare(
        tmp_path,
        """
class Key:
    def __str__(self): return 'string'
    def __repr__(self): return '<key>'
class Custom(KeyError): pass
for cls in (KeyError, Custom):
    for args in ((), ('missing',), (\"can't\",), (42,), (None,), ((1, 2),),
                 ('one', 'two'), (Key(),)):
        error = cls(*args)
        print(str(error), repr(error), error.args)
try:
    {}['missing']
except KeyError as error:
    print(str(error), repr(error))
""",
    )


def test_handlers_else_nested_reraise_and_binding_cleanup(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
def reraiser():
    raise
try:
    try:
        raise ValueError('broken')
    except IndexError:
        print('wrong')
    except Exception as error:
        print(error.args[0])
        reraiser()
    else:
        print('wrong')
except ValueError as outer:
    print(outer.args[0])
try:
    print(outer)
except NameError:
    print('deleted')
try:
    print('body')
except Exception:
    print('wrong')
else:
    print('else')
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "broken\nbroken\ndeleted\nbody\nelse\n"


def test_failed_expression_and_store_preserve_prior_locals(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
def run():
    value = 'old owned value'
    try:
        value = 1 / 0
    except ZeroDivisionError:
        print(value)
    try:
        print(missing)
    except NameError:
        value = 42
    print(value)
run()
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "old owned value\n42\n"


def test_handler_return_break_continue_and_bare_raise_without_handler(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
def run():
    try:
        raise IndexError('return')
    except Exception as error:
        return error.args[0]
print(run())
for i in range(2):
    try:
        raise ValueError
    except ValueError as error:
        if i == 0:
            continue
        break
try:
    raise
except RuntimeError:
    print('no active exception')
try:
    print(error)
except NameError:
    print('deleted')
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "return\nno active exception\ndeleted\n"


def test_custom_exception_hierarchy_and_condition_failure(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
class Custom(ValueError):
    pass
class Condition:
    def __bool__(self):
        raise Custom('truth failed')
try:
    if Condition():
        print('wrong')
except LookupError:
    print('wrong')
except ValueError as error:
    print(error.args)
try:
    raise Custom('custom')
except (IndexError, Exception):
    print('hierarchy')
try:
    raise 42
except TypeError:
    print('invalid raise')
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "('truth failed',)\nhierarchy\ninvalid raise\n"


def test_handler_does_not_cover_statements_before_the_try(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
def f():
    x = 1 // 0
    try:
        y = 2
    except ZeroDivisionError:
        print('caught')
    return 5
print(f())
""",
    )
    assert result.returncode != 0
    assert result.stdout == ""


def test_finally_runs_on_every_exit(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
def through(flag):
    try:
        if flag:
            return 'returned'
        raise ValueError('raised')
    finally:
        print('cleanup', flag)
print(through(True))
try:
    through(False)
except ValueError as error:
    print('propagated', error.args[0])
for i in range(3):
    try:
        if i == 1:
            continue
        if i == 2:
            break
        print('body', i)
    finally:
        print('finally', i)
def override():
    try:
        raise ValueError('lost')
    finally:
        return 'finally wins'
print(override())
def nested():
    try:
        try:
            raise KeyError('inner')
        finally:
            print('inner finally')
    except KeyError as error:
        print('caught', error.args[0])
    finally:
        print('outer finally')
nested()
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "cleanup True\nreturned\ncleanup False\npropagated raised\n"
        "body 0\nfinally 0\nfinally 1\nfinally 2\nfinally wins\n"
        "inner finally\ncaught inner\nouter finally\n"
    )


def test_with_statement_calls_enter_and_exit(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
class Manager:
    def __init__(self, name, suppress):
        self.name = name
        self.suppress = suppress
    def __enter__(self):
        print('enter', self.name)
        return self.name
    def __exit__(self, kind, value, traceback):
        print('exit', self.name, kind is None, value is None, traceback is None)
        return self.suppress
with Manager('plain', False) as value:
    print('body', value)
with Manager('swallow', True):
    raise ValueError('ignored')
print('after swallow')
try:
    with Manager('raise', False):
        raise ValueError('escapes')
except ValueError as error:
    print('caught', error.args[0])
def early():
    with Manager('return', False):
        return 'early'
print(early())
with Manager('outer', False), Manager('inner', False):
    print('nested body')
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "enter plain\nbody plain\nexit plain True True True\n"
        "enter swallow\nexit swallow False False False\nafter swallow\n"
        "enter raise\nexit raise False False False\ncaught escapes\n"
        "enter return\nexit return True True True\nearly\n"
        "enter outer\nenter inner\nnested body\nexit inner True True True\nexit outer True True True\n"
    )


def test_exception_chaining_records_cause_and_context(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
def explicit():
    try:
        raise KeyError('inner')
    except KeyError as inner:
        raise ValueError('outer') from inner
try:
    explicit()
except ValueError as error:
    print(error.args[0], error.__cause__.args[0], error.__context__.args[0], error.__suppress_context__)
def implicit():
    try:
        raise KeyError('first')
    except KeyError:
        raise ValueError('second')
try:
    implicit()
except ValueError as error:
    print(error.__cause__, error.__context__.args[0], error.__suppress_context__)
def suppressed():
    try:
        raise KeyError('hidden')
    except KeyError:
        raise ValueError('shown') from None
try:
    suppressed()
except ValueError as error:
    print(error.__cause__, error.__context__.args[0], error.__suppress_context__)
try:
    raise ValueError from KeyError
except ValueError as error:
    print(error.args, error.__cause__.args)
try:
    raise ValueError('bad cause') from 42
except TypeError as error:
    print('cause type error')
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "outer inner inner True\nNone first False\nNone hidden True\n() ()\ncause type error\n"
    )

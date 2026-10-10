"""Native exception transfers preserve locals, ownership, and handler state."""

from test.mule.compiler.test_modules import execute


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

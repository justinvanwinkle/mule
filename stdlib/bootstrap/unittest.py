"""Initial compiled assertion API; discovery belongs to the native test harness.

The vendored Python 3.15 unittest implementation remains in stdlib/unittest/.
This bootstrap provides the assertion methods and SkipTest exception below. General
unittest loading, fixtures, skip accounting, decorators and exception contexts need further
compiler/runtime support; they must not be treated as executed tests.
"""


class SkipTest(Exception):
    """Signal that a test cannot run; runners decide how to account for it."""


class TestCase:
    def skipTest(self, reason):
        raise SkipTest(reason)

    def assertEqual(self, first, second):
        assert first == second, repr(first) + " != " + repr(second)

    def assertTrue(self, expression):
        assert expression, repr(expression) + " is not true"

    def assertFalse(self, expression):
        assert not expression, repr(expression) + " is not false"

    def fail(self, message):
        assert False, message


def main():
    assert False, (
        "General unittest loading is not implemented; use tools.run_stdlib_tests"
    )

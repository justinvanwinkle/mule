"""The bootstrap skip signal remains an ordinary exception for native runners."""

from test.mule.compiler.test_exception_groups import compare


def test_skip_exception_identity_and_case_method(tmp_path):
    result = compare(
        tmp_path,
        """
import unittest
print(issubclass(unittest.SkipTest, Exception))
error = unittest.SkipTest('reason')
print(error.args, str(error))
try:
    unittest.TestCase().skipTest('case reason')
except unittest.SkipTest as error:
    print(type(error).__name__, error.args, str(error))
""",
    )
    assert result.stderr == ""

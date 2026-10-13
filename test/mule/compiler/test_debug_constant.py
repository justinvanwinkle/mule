"""The debug flag is a language constant, independent of native optimization."""

import pytest

from mule.compiler import emit_llvm
from mule.compiler.main import compile_source
from test.mule.compiler.test_exception_groups import compare


def test_debug_reads_are_constants_even_after_namespace_mutation(tmp_path):
    compare(
        tmp_path,
        """
import builtins
print(__debug__, builtins.__debug__)
globals()['__debug__'] = False
setattr(builtins, '__debug__', False)
def function():
    return __debug__
print(__debug__, function(), builtins.__debug__)
from builtins import __debug__ as imported
print(imported, __debug__)
""",
    )
    llvm = emit_llvm("print(__debug__)\n")
    assert 'c"__debug__\\00"' not in llvm


@pytest.mark.parametrize(
    "source",
    [
        "__debug__ = False\n",
        "__debug__: bool\n",
        "__debug__ += 1\n",
        "del __debug__\n",
        "def function(__debug__):\n    pass\n",
        "def __debug__():\n    pass\n",
        "class __debug__:\n    pass\n",
        "import sys as __debug__\n",
        "from builtins import __debug__\n",
        "for __debug__ in []:\n    pass\n",
        "def function():\n    return\n    __debug__ = False\n",
    ],
)
def test_debug_name_cannot_be_bound(source):
    with pytest.raises(SyntaxError, match="__debug__"):
        compile_source(source, "debug_constant.py")

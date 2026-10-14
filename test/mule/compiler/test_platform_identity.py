"""Platform reports the native implementation independently of the host."""

from test.mule.compiler.test_modules import execute


def test_mule_platform_version_build_and_compiler(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
import platform
import sys
print(sys.implementation.name)
print(platform.python_implementation())
print(platform.python_version())
print(platform.python_version_tuple())
print(platform.python_build())
print(platform.python_compiler())
print(platform._sys_version('3.14.0 (Mule native build) [LLVM]'))
print(platform._sys_version('3.14.0 (reference) [Other Compiler]'))
try:
    platform._sys_version('invalid Mule')
except ValueError as error:
    print(str(error))
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stderr == ""
    assert result.stdout == (
        "mule\nMule\n3.14.0\n('3', '14', '0')\n('Mule native build', '')\nLLVM\n"
        "('Mule', '3.14.0', '', '', 'Mule native build', '', 'LLVM')\n"
        "('CPython', '3.14.0', '', '', 'reference', '', 'Other Compiler')\n"
        "failed to parse Mule sys.version: 'invalid Mule'\n"
    )

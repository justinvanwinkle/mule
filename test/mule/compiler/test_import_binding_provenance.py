"""Import-origin analysis distinguishes real module aliases from IR cycles."""

from test.mule.compiler.test_modules import execute


def test_ignored_parameter_annotation_preserves_attribute_error(tmp_path):
    result = execute(
        tmp_path,
        {},
        "from mule import int32\n"
        "def read(value: int32):\n"
        " return value.missing\n"
        "try:\n"
        " read('wrong')\n"
        "except AttributeError:\n"
        " print('attribute error')\n",
    )

    assert result.returncode == 0, result.stderr
    assert result.stdout == "attribute error\n"


def test_cyclic_local_alias_receiver_preserves_unbound_error(tmp_path):
    result = execute(
        tmp_path,
        {},
        "def read():\n"
        " left = right\n"
        " right = left\n"
        " return left.member\n"
        "try:\n"
        " read()\n"
        "except UnboundLocalError:\n"
        " print('unbound error')\n",
    )

    assert result.returncode == 0, result.stderr
    assert result.stdout == "unbound error\n"


def test_repeated_module_aliases_keep_imported_member_calls(tmp_path):
    result = execute(
        tmp_path,
        {"support.py": "def member():\n return 42\n"},
        "def read():\n"
        " import support\n"
        " left = support\n"
        " right = left\n"
        " first = left.member\n"
        " second = right.member\n"
        " return first(), second()\n"
        "print(read())\n"
        "print(read())\n",
    )

    assert result.returncode == 0, result.stderr
    assert result.stdout == "(42, 42)\n(42, 42)\n"

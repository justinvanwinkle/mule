"""Native construction of lists and tuples from evaluated expressions."""

from test.mule.compiler.test_modules import execute


def test_sequence_displays_preserve_order_identity_and_fresh_allocations(tmp_path):
    result = execute(
        tmp_path,
        {},
        "def item(value):\n print(value)\n return value\n"
        "values = [item(1), item(2), (item(3),)]\n"
        "print(values, len(values), (), [])\n"
        "a = [42]\nb = [42]\nprint(a is b)\n"
        "print((values, values))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "1\n2\n3\n[1, 2, (3,)] 3 () []\nFalse\n([1, 2, (3,)], [1, 2, (3,)])\n"
    )


def test_sequence_element_failure_stops_later_evaluation(tmp_path):
    result = execute(
        tmp_path,
        {},
        "def before():\n print('before')\n return 'owned'\n"
        "def after():\n print('after')\n return 3\n"
        "print([before(), missing, after()])\n",
    )
    assert result.returncode != 0
    assert result.stdout == "before\n"
    assert "NameError" in result.stderr

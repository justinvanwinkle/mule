"""The compiled environment mapping owns its dictionary updates."""

from test.mule.compiler.test_modules import execute


def test_environment_mapping_updates_deletes_and_restores_after_error(tmp_path):
    result = execute(
        tmp_path,
        {},
        "import os\n"
        "key = 'MULE_ENVIRONMENT_REGRESSION_A71E'\n"
        "print(os.environ.get(key))\n"
        "os.environ[key] = 'first'\n"
        "print(os.environ[key])\n"
        "os.environ[key] = 'second'\n"
        "print(os.environ.pop(key, None))\n"
        "print(os.environ.pop(key, None))\n"
        "os.environ[key] = 'third'\n"
        "del os.environ[key]\n"
        "print(key in os.environ)\n"
        "try:\n"
        " del os.environ[key]\n"
        "except KeyError as error:\n"
        " print(error.args == (key,))\n"
        "try:\n"
        " try:\n"
        "  os.environ[key] = 'temporary'\n"
        "  raise ValueError('original')\n"
        " finally:\n"
        "  os.environ.pop(key, None)\n"
        "except ValueError as error:\n"
        " print(str(error))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "None\nfirst\nsecond\nNone\nFalse\nTrue\noriginal\n"


def test_direct_posix_environment_changes_do_not_mutate_python_mapping(tmp_path):
    result = execute(
        tmp_path,
        {},
        "import os\n"
        "import posix\n"
        "from _posix import environ_items\n"
        "key = 'MULE_ENVIRONMENT_REGRESSION_A71E'\n"
        "encoded = key.encode()\n"
        "posix.putenv(encoded, b'native')\n"
        "print(os.environ.get(key), posix.environ.get(encoded))\n"
        "print(dict(environ_items()).get(encoded))\n"
        "posix.unsetenv(encoded)\n"
        "os.environ[key] = 'mapped'\n"
        "posix.putenv(encoded, b'replaced')\n"
        "print(os.environ[key], posix.environ[encoded])\n"
        "posix.unsetenv(encoded)\n"
        "print(os.environ[key], posix.environ[encoded])\n"
        "print(dict(environ_items()).get(encoded))\n"
        "del os.environ[key]\n"
        "print(encoded in posix.environ)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "None None\nb'native'\nmapped b'mapped'\nmapped b'mapped'\nNone\nFalse\n"
    )

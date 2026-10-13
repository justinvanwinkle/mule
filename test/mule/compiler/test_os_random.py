"""OS entropy is native storage behind the compiled integer-index wrapper."""

from test.mule.compiler.test_modules import execute


def test_urandom_returns_requested_bytes_and_obeys_index_protocol(tmp_path):
    result = execute(
        tmp_path,
        {},
        "import os\n"
        "import posix\n"
        "class Size:\n def __index__(self):\n  return 32\n"
        "for size in (0, 1, 32, 4097, False, True, Size()):\n"
        " data = os.urandom(size)\n"
        " print(type(data) is bytes, len(data))\n"
        "print(os.urandom is posix.urandom)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "True 0\nTrue 1\nTrue 32\nTrue 4097\nTrue 0\nTrue 1\nTrue 32\nTrue\n"
    )


def test_urandom_rejects_invalid_lengths_and_recovers(tmp_path):
    result = execute(
        tmp_path,
        {},
        "import os\n"
        "class BadSize:\n def __index__(self):\n  return 1.5\n"
        "for size in (-1, 1.5, '3', BadSize(), 2 ** 100):\n"
        " try:\n  os.urandom(size)\n"
        " except (ValueError, TypeError, OverflowError) as error:\n"
        "  print(type(error).__name__)\n"
        "print(len(os.urandom(3)))\n",
    )
    assert result.returncode == 0, result.stderr
    assert (
        result.stdout
        == "ValueError\nTypeError\nTypeError\nTypeError\nOverflowError\n3\n"
    )

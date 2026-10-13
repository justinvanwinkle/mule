"""MT19937 streams and state transitions compared with the CPython oracle."""

from test.mule.compiler.test_math_native import assert_matches_python


def test_random_integer_seeds_streams_and_state_roundtrips_match_python(tmp_path):
    assert_matches_python(
        tmp_path,
        "import _random\n"
        "for seed in (0, 1, -123, 2 ** 100 + 42):\n"
        " generator = _random.Random(seed)\n"
        " print([generator.random() for _ in range(4)])\n"
        " print([generator.getrandbits(k) for k in (0, 1, 31, 32, 33, 64, 100)])\n"
        " saved = generator.getstate()\n"
        " values = [generator.getrandbits(32) for _ in range(1300)]\n"
        " print(values[610:620], values[-4:])\n"
        " generator.setstate(saved)\n"
        " print(values == [generator.getrandbits(32) for _ in range(1300)])\n",
    )


def test_random_invalid_state_does_not_replace_current_state(tmp_path):
    assert_matches_python(
        tmp_path,
        "import _random\n"
        "generator = _random.Random(123)\n"
        "saved = generator.getstate()\n"
        "for state in ((), saved[:-1] + (625,), saved[:-1] + (-1,)):\n"
        " try:\n  generator.setstate(state)\n"
        " except ValueError:\n  print('state')\n"
        " print(generator.getstate() == saved)\n"
        "try:\n generator.getrandbits(-1)\n"
        "except ValueError:\n print('negative')\n"
        "try:\n generator.getrandbits(1.5)\n"
        "except TypeError:\n print('index')\n"
        "print(generator.random())\n",
    )

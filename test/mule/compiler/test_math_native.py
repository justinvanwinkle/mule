"""Exercise the initial compiled math package against Python references."""

import subprocess

from test.mule.compiler.test_native import run_native
from test.python_host import python_host


def assert_matches_python(tmp_path, source):
    reference = subprocess.run(
        [python_host(), "-c", source],
        capture_output=True,
        text=True,
        check=False,
        timeout=10,
    )
    assert reference.returncode == 0, reference.stderr
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == reference.stdout
    assert result.stderr == ""


def test_math_benchmark_functions_match_python(tmp_path):
    assert_matches_python(
        tmp_path,
        "from math import sqrt, factorial\n"
        "print(sqrt(9), factorial(7), factorial(50))\n",
    )


def test_math_constants_and_square_root_special_values_match_python(tmp_path):
    assert_matches_python(
        tmp_path,
        "import math\n"
        "print(math.pi, math.e, math.tau)\n"
        "print(math.sqrt(0.0), math.sqrt(-0.0), math.sqrt(True))\n"
        "print(math.sqrt(math.inf) == math.inf)\n"
        "value = math.sqrt(math.nan)\nprint(value != value)\n",
    )


def test_math_integer_operations_match_python(tmp_path):
    assert_matches_python(
        tmp_path,
        "import math\n"
        "print(math.factorial(0), math.factorial(True), math.factorial(False))\n"
        "print(math.comb(0, 0), math.comb(30, 0), math.comb(30, 30))\n"
        "print(math.comb(6, 7), math.comb(100, 50))\n"
        "print(math.perm(7), math.perm(7, None), math.perm(7, 3))\n"
        "print(math.perm(7, 0), math.perm(7, 8))\n"
        "print(math.gcd(), math.gcd(-42), math.gcd(0, -42, 84))\n"
        "print(math.gcd(2 ** 100, 3 * 2 ** 100))\n"
        "print(math.lcm(), math.lcm(-7), math.lcm(0, 42), math.lcm(-6, 15))\n"
        "print(math.lcm(2 ** 100, 3 * 2 ** 100))\n",
    )


def test_math_integer_square_root_boundaries_match_python(tmp_path):
    assert_matches_python(
        tmp_path,
        "import math\n"
        "for value in range(101):\n print(math.isqrt(value), end=' ')\n"
        "print()\n"
        "for value in (2 ** 62 - 1, 2 ** 62, 2 ** 63 - 1, 2 ** 63, "
        "2 ** 64 - 1, 2 ** 64, 2 ** 64 + 1, 2 ** 65 - 1, 2 ** 65):\n"
        " print(math.isqrt(value))\n"
        "for root in (2 ** 31, 2 ** 32 - 1, 2 ** 32, 2 ** 32 + 1):\n"
        " square = root * root\n"
        " print(math.isqrt(square - 1), math.isqrt(square), math.isqrt(square + 1))\n"
        "root = 2 ** 100 + 123\nsquare = root * root\n"
        "print(math.isqrt(square - 1), math.isqrt(square), math.isqrt(square + 1))\n"
        "print(math.isqrt(2 ** 1024), math.isqrt(2 ** 1024 - 1))\n",
    )


def test_math_integer_square_root_seed_intervals_match_python(tmp_path):
    assert_matches_python(
        tmp_path,
        "import math\n"
        "for prefix in range(64, 256):\n"
        " lower = prefix << 56\n"
        " upper = lower + (2 ** 56 - 1)\n"
        " print(math.isqrt(lower - 1), math.isqrt(lower), math.isqrt(upper))\n",
    )


def test_math_conversion_protocols_match_python(tmp_path):
    assert_matches_python(
        tmp_path,
        "import math\n"
        "class Index:\n def __index__(self):\n  print('index')\n  return 9\n"
        "class Real:\n def __float__(self):\n  print('float')\n  return 4.0\n"
        " def __index__(self):\n  print('unexpected index')\n  return 9\n"
        "print(math.sqrt(Index()), math.sqrt(Real()))\n"
        "print(math.factorial(Index()), math.comb(Index(), 2), math.perm(Index(), 2))\n"
        "print(math.gcd(Index(), 6), math.lcm(Index(), 6), math.isqrt(Index()))\n",
    )


def test_math_errors_and_protocol_result_validation_recover_like_python(tmp_path):
    assert_matches_python(
        tmp_path,
        "import math\n"
        "class BadIndex:\n def __index__(self):\n  return 1.5\n"
        "class BadFloat:\n def __float__(self):\n  return 9\n"
        " def __index__(self):\n  print('unexpected fallback')\n  return 9\n"
        "class FailingFloat:\n def __float__(self):\n  raise RuntimeError('failed')\n"
        "for value in (-1, -math.inf):\n"
        " try:\n  math.sqrt(value)\n"
        " except ValueError:\n  print('domain')\n"
        "try:\n math.sqrt(10 ** 1000)\n"
        "except OverflowError:\n print('overflow')\n"
        "for value in ('9', b'9', BadFloat(), BadIndex()):\n"
        " try:\n  math.sqrt(value)\n"
        " except TypeError:\n  print('type')\n"
        "try:\n math.sqrt(FailingFloat())\n"
        "except RuntimeError:\n print('conversion error')\n"
        "for function in (math.factorial, math.isqrt):\n"
        " try:\n  function(-1)\n"
        " except ValueError:\n  print('negative')\n"
        " for value in (3.0, BadIndex()):\n"
        "  try:\n   function(value)\n"
        "  except TypeError:\n   print('integer type')\n"
        "for function in (math.comb, math.perm):\n"
        " for pair in ((-1, 0), (1, -1)):\n"
        "  try:\n   function(pair[0], pair[1])\n"
        "  except ValueError:\n   print('negative pair')\n"
        "for function in (math.gcd, math.lcm):\n"
        " try:\n  function(0, 0, BadIndex())\n"
        " except TypeError:\n  print('all operands converted')\n"
        "print(math.sqrt(16), math.factorial(5))\n",
    )


def test_math_positional_only_and_arity_errors_match_python(tmp_path):
    assert_matches_python(
        tmp_path,
        "import math\n"
        "for function in (math.sqrt, math.factorial, math.isqrt):\n"
        " try:\n  function()\n"
        " except TypeError:\n  print('missing')\n"
        " try:\n  function(x=1)\n"
        " except TypeError:\n  print('keyword')\n"
        "try:\n math.comb(n=3, k=1)\n"
        "except TypeError:\n print('comb keyword')\n"
        "try:\n math.perm(3, k=1)\n"
        "except TypeError:\n print('perm keyword')\n"
        "try:\n math.gcd(value=1)\n"
        "except TypeError:\n print('gcd keyword')\n"
        "print(math.perm(3, None))\n",
    )


def test_math_integer_module_reexports_share_function_objects(tmp_path):
    result = run_native(
        tmp_path,
        "import math\nimport math.integer as integer\n"
        "print(math.integer is integer, math.factorial is integer.factorial)\n"
        "print(integer.comb(10, 4), integer.perm(10, 4), integer.isqrt(12345))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True True\n210 5040 111\n"
    assert result.stderr == ""


def test_math_rounding_dispatch_and_integer_results_match_python(tmp_path):
    assert_matches_python(
        tmp_path,
        "import math\n"
        "class Integer(int):\n pass\n"
        "class CustomInteger(int):\n"
        " def __floor__(self):\n  return 'custom floor'\n"
        " def __ceil__(self):\n  return 'custom ceil'\n"
        "class Rounded:\n"
        " def __floor__(self):\n  return 'floor'\n"
        " def __ceil__(self):\n  return 'ceil'\n"
        "class Real:\n def __float__(self):\n  return -1.25\n"
        "for function in (math.floor, math.ceil):\n"
        " for value in (True, False, 3, 2 ** 100, Integer(3), 1.25, -1.25):\n"
        "  result = function(value)\n"
        "  print(result, type(result) is int)\n"
        " print(function(CustomInteger(3)), function(Rounded()), function(Real()))\n",
    )


def test_math_transcendentals_predicates_and_large_logs_match_python(tmp_path):
    assert_matches_python(
        tmp_path,
        "import math\n"
        "for function in (math.log, math.log2, math.exp, math.sin, math.cos, math.acos, math.lgamma):\n"
        " print(round(function(0.5), 12))\n"
        "print(math.log(8, 2), math.log2(2 ** 4096))\n"
        "print(round(math.log(10 ** 1000), 10))\n"
        "for value in (-math.inf, -0.0, 0, 1, math.inf, math.nan):\n"
        " print(math.isfinite(value), math.isinf(value), math.isnan(value))\n"
        "print(math.fabs(-0.0), math.fabs(-3))\n"
        "print(math.exp(-math.inf), math.exp(math.inf), math.lgamma(-math.inf))\n"
        "class Real:\n def __float__(self):\n  return 0.5\n"
        "class Index:\n def __index__(self):\n  return 1\n"
        "print(round(math.log(Real()), 12), math.log(Index()), math.isfinite(Real()))\n"
        "print(math.isnan(math.log(math.nan)), math.isnan(math.sin(math.nan)))\n",
    )


def test_math_transcendental_errors_preserve_python_boundaries(tmp_path):
    assert_matches_python(
        tmp_path,
        "import math\n"
        "for function, value in ((math.log, 0), (math.log2, -1), (math.sin, math.inf),\n"
        " (math.cos, -math.inf), (math.acos, 2), (math.lgamma, 0), (math.lgamma, -2),\n"
        " (math.exp, 1000), (math.lgamma, 1e308), (math.ceil, math.inf), (math.floor, math.nan)):\n"
        " try:\n  function(value)\n"
        " except (ValueError, OverflowError) as error:\n  print(type(error).__name__)\n"
        "try:\n math.log(2, 1)\n"
        "except ZeroDivisionError:\n print('base one')\n"
        "for function in (math.log, math.exp, math.isfinite):\n"
        " try:\n  function('1')\n"
        " except TypeError:\n  print('type')\n"
        "try:\n math.log(x=1)\n"
        "except TypeError:\n print('keyword')\n",
    )

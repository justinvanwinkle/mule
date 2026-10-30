"""Exercise selected upstream algorithms on the external Python test host.

These checks do not claim Mule can compile the imported standard library yet.
"""

import shutil
import subprocess
import sys
from pathlib import Path

import pytest

STDLIB = Path(__file__).resolve().parents[2] / "stdlib"


def _external_python_host() -> str:
    if sys.implementation.name == "mule":
        return shutil.which("python3") or "python3"
    return sys.executable


@pytest.mark.parametrize(
    ("filename", "program", "output"),
    [
        (
            "bisect.py",
            (
                "items = [1, 3, 3, 7]\n"
                "print(module.bisect_left(items, 3), module.bisect_right(items, 3))\n"
                "module.insort(items, 5)\nprint(items)\n"
            ),
            "1 3\n[1, 3, 3, 5, 7]\n",
        ),
        (
            "heapq.py",
            (
                "items = [5, 1, 4, 2]\nmodule.heapify(items)\n"
                "print([module.heappop(items) for _ in range(4)])\n"
                "items = [2, 9, 4]\nmodule.heapify_max(items)\n"
                "print(module.heappop_max(items))\n"
            ),
            "[1, 2, 4, 5]\n9\n",
        ),
        (
            "operator.py",
            "print(module.add(19, 23))\nprint(module.itemgetter(1, 0)(['a', 'b']))\n",
            "42\n('b', 'a')\n",
        ),
        (
            "abc.py",
            (
                "class Base(module.ABC):\n"
                "    @module.abstractmethod\n"
                "    def value(self): pass\n"
                "class Concrete(Base):\n"
                "    def value(self): return 42\n"
                "try:\n    Base()\nexcept TypeError:\n    print('abstract')\n"
                "print(Concrete().value())\n"
            ),
            "abstract\n42\n",
        ),
        (
            "_pyio.py",
            (
                "text = module.StringIO('hello')\nprint(text.read())\n"
                "stream = module.BytesIO()\n"
                "writer = module.TextIOWrapper(stream, encoding='utf-8')\n"
                "writer.write('雪')\nwriter.flush()\nprint(stream.getvalue().hex())\n"
                "print(module.Reader is not None, module.Writer is not None)\n"
            ),
            "hello\ne99baa\nTrue True\n",
        ),
        (
            "hmac.py",
            (
                "print(module.new(b'key', b'data', 'sha256').hexdigest())\n"
                "print(module._hashopenssl, module._hmac)\n"
            ),
            (
                "5031fe3d989c6d1537a013fa6e739da23463fdaec3b70137d828e36ace221bd0\n"
                "None None\n"
            ),
        ),
        (
            "threading.py",
            (
                "print(module._CRLock)\n"
                "storage = module.local()\nstorage.value = 'main'\nvalues = []\n"
                "def worker():\n"
                "    storage.value = 'worker'\n"
                "    values.append(storage.value)\n"
                "thread = module.Thread(target=worker)\n"
                "thread.start()\nthread.join()\nprint(storage.value, values)\n"
            ),
            "None\nmain ['worker']\n",
        ),
    ],
)
def test_python_algorithm(filename, program, output, tmp_path):
    # Load just the selected source/package, keeping this external host's own
    # platform services. Prepending the entire 3.15 tree to a 3.14 host would
    # incorrectly mix bootstrap libraries and unsupported 3.15 syntax.
    name = "threading" if filename == "threading.py" else "stdlib_under_test"
    loader = (
        "import importlib.util\nimport sys\n"
        f"mt_spec = importlib.util.spec_from_file_location('mule_types', {str(STDLIB / 'mule_types.py')!r})\n"
        "mt_mod = importlib.util.module_from_spec(mt_spec)\n"
        "sys.modules['mule_types'] = mt_mod\n"
        "mt_spec.loader.exec_module(mt_mod)\n"
        f"spec = importlib.util.spec_from_file_location({name!r}, {str(STDLIB / filename)!r})\n"
        "module = importlib.util.module_from_spec(spec)\n"
        "sys.modules[spec.name] = module\n"
        "spec.loader.exec_module(module)\n"
    )
    result = subprocess.run(
        [_external_python_host(), "-I", "-S", "-B", "-c", loader + program],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        encoding="utf-8",
        timeout=10,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == output

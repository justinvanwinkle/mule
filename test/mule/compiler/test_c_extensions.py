"""CPython C-API source compatibility, shared-library loading, and heap types."""

from pathlib import Path
import shutil
import subprocess

import pytest

from mule.compiler import build


REPO_ROOT = Path(__file__).resolve().parents[3]
INCLUDE_DIR = REPO_ROOT / "mule" / "include"
WRAPT_SRC = Path("/tmp/ext-src/wrapt-2.5.1/src/wrapt")


def _compile_c_extension(c_source: Path, so_path: Path) -> None:
    cc = shutil.which("clang") or shutil.which("cc") or "cc"
    cmd = [
        cc,
        "-shared",
        "-fPIC",
        "-O2",
        "-I",
        str(INCLUDE_DIR),
        str(c_source),
        "-o",
        str(so_path),
    ]
    res = subprocess.run(cmd, capture_output=True, text=True, check=False)
    assert res.returncode == 0, res.stderr


def test_c_extension_heap_types_descriptors_and_protocols(tmp_path):
    c_file = tmp_path / "_cext_demo.c"
    c_file.write_text(
        r"""
#include <Python.h>
#include <structmember.h>

typedef struct {
    PyObject_HEAD
    PyObject *wrapped;
    Py_ssize_t tag;
} BoxObject;

static void
Box_dealloc(PyObject *self)
{
    BoxObject *box = (BoxObject *)self;
    PyTypeObject *tp = Py_TYPE(self);
    Py_XDECREF(box->wrapped);
    tp->tp_free(self);
    Py_DECREF(tp);
}

static int
Box_init(PyObject *self, PyObject *args, PyObject *kwds)
{
    static char *kwlist[] = {"wrapped", "tag", NULL};
    BoxObject *box = (BoxObject *)self;
    PyObject *wrapped = NULL;
    Py_ssize_t tag = 0;
    if (!PyArg_ParseTupleAndKeywords(args, kwds, "O|n:Box", kwlist, &wrapped, &tag)) {
        return -1;
    }
    Py_INCREF(wrapped);
    Py_XSETREF(box->wrapped, wrapped);
    box->tag = tag;
    return 0;
}

static PyObject *
Box_get_wrapped(PyObject *self, void *Py_UNUSED(closure))
{
    BoxObject *box = (BoxObject *)self;
    if (!box->wrapped) {
        PyErr_SetString(PyExc_AttributeError, "wrapped is not set");
        return NULL;
    }
    return Py_NewRef(box->wrapped);
}

static int
Box_set_wrapped(PyObject *self, PyObject *value, void *Py_UNUSED(closure))
{
    BoxObject *box = (BoxObject *)self;
    if (value == NULL) {
        Py_CLEAR(box->wrapped);
        return 0;
    }
    Py_INCREF(value);
    Py_XSETREF(box->wrapped, value);
    return 0;
}

static PyObject *
Box_add_fast(PyObject *self, PyObject *const *args, Py_ssize_t nargs)
{
    BoxObject *box = (BoxObject *)self;
    if (nargs != 1) {
        PyErr_SetString(PyExc_TypeError, "add_fast takes 1 argument");
        return NULL;
    }
    return PyNumber_Add(box->wrapped, args[0]);
}

static PyObject *
Box_nb_add(PyObject *left, PyObject *right)
{
    PyObject *w = NULL;
    if (PyObject_GetOptionalAttrString(left, "__wrapped__", &w) > 0) {
        PyObject *res = PyNumber_Add(w, right);
        Py_DECREF(w);
        return res;
    }
    Py_RETURN_NOTIMPLEMENTED;
}

static PyObject *
Box_descr_get(PyObject *self, PyObject *inst, PyObject *owner)
{
    BoxObject *box = (BoxObject *)self;
    if (inst == NULL || inst == Py_None) {
        return Py_NewRef(self);
    }
    return Py_BuildValue("(OO)", box->wrapped, inst);
}

static PyMemberDef Box_members[] = {
    {"tag", T_PYSSIZET, offsetof(BoxObject, tag), 0, "tag value"},
    {NULL}
};

static PyGetSetDef Box_getset[] = {
    {"__wrapped__", Box_get_wrapped, Box_set_wrapped, "wrapped value", NULL},
    {NULL}
};

static PyMethodDef Box_methods[] = {
    {"add_fast", _PyCFunction_CAST(Box_add_fast), METH_FASTCALL, "add value"},
    {NULL}
};

static PyType_Slot Box_slots[] = {
    {Py_tp_dealloc, (void *)Box_dealloc},
    {Py_tp_init, (void *)Box_init},
    {Py_tp_members, Box_members},
    {Py_tp_getset, Box_getset},
    {Py_tp_methods, Box_methods},
    {Py_nb_add, (void *)Box_nb_add},
    {Py_tp_descr_get, (void *)Box_descr_get},
    {0, NULL},
};

static PyType_Spec Box_spec = {
    .name = "_cext_demo.Box",
    .basicsize = sizeof(BoxObject),
    .flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_HEAPTYPE,
    .slots = Box_slots,
};

static int
demo_exec(PyObject *m)
{
    PyObject *box_type = PyType_FromModuleAndSpec(m, &Box_spec, NULL);
    if (!box_type) return -1;
    if (PyModule_AddObjectRef(m, "Box", box_type) < 0) {
        Py_DECREF(box_type);
        return -1;
    }
    Py_DECREF(box_type);
    PyObject *err = PyErr_NewException("_cext_demo.DemoError", PyExc_ValueError, NULL);
    if (!err) return -1;
    if (PyModule_AddObject(m, "DemoError", err) < 0) {
        return -1;
    }
    return 0;
}

static PyModuleDef_Slot demo_slots[] = {
    {Py_mod_exec, (void *)demo_exec},
    {0, NULL}
};

static struct PyModuleDef demo_module = {
    PyModuleDef_HEAD_INIT,
    .m_name = "_cext_demo",
    .m_size = 0,
    .m_slots = demo_slots,
};

PyMODINIT_FUNC
PyInit__cext_demo(void)
{
    return PyModuleDef_Init(&demo_module);
}
"""
    )
    so_path = tmp_path / "_cext_demo.mule-314-x86_64-linux-gnu.so"
    _compile_c_extension(c_file, so_path)

    main_py = tmp_path / "main.py"
    main_py.write_text(
        """import _cext_demo

for i in range(5):
    b = _cext_demo.Box(10, tag=i)
    assert b.__wrapped__ == 10
    assert b.tag == i
    assert b.add_fast(5) == 15
    assert b + 7 == 17
    b.tag = 99
    assert b.tag == 99
    del b.__wrapped__
    try:
        _ = b.__wrapped__
        assert False
    except AttributeError:
        pass

class Holder:
    attr = _cext_demo.Box("payload", tag=1)

h = Holder()
assert Holder.attr.__wrapped__ == "payload"
assert h.attr == ("payload", h)
assert issubclass(_cext_demo.DemoError, ValueError)
print("ok")
"""
    )
    exe = build(main_py, tmp_path / "prog", optimize=False, workers=1)
    res = subprocess.run([exe], capture_output=True, text=True, check=False, timeout=30)
    assert res.returncode == 0, res.stderr
    assert res.stdout == "ok\n"


@pytest.mark.skipif(not WRAPT_SRC.exists(), reason="wrapt C extension source not present")
def test_wrapt_c_extension(tmp_path):
    pkg_dir = tmp_path / "wrapt"
    pkg_dir.mkdir()
    for py_file in WRAPT_SRC.glob("*.py"):
        (pkg_dir / py_file.name).write_text(py_file.read_text())

    so_path = pkg_dir / "_wrappers.mule-314-x86_64-linux-gnu.so"
    _compile_c_extension(WRAPT_SRC / "_wrappers.c", so_path)

    main_py = pkg_dir / "main.py"
    main_py.write_text(
        """import sys
import exceptions
sys.modules["wrapt.exceptions"] = exceptions
import _wrappers

p = _wrappers.ObjectProxy({"a": 1, "b": 2})
assert p == {"a": 1, "b": 2}
assert len(p) == 2
assert p["a"] == 1
p["c"] = 3
assert p["c"] == 3
assert "c" in p
del p["b"]
assert "b" not in p
assert list(_wrappers.ObjectProxy([10, 20, 30])) == [10, 20, 30]
assert isinstance(p, dict)
assert p.__class__ is dict

num = _wrappers.ObjectProxy(10)
assert num + 5 == 15
assert 5 + num == 15
assert divmod(num, 3) == (3, 1)
assert divmod(25, num) == (2, 5)

calls = []
def wrapper(wrapped, instance, args, kwargs):
    calls.append((wrapped.__name__, instance, args, kwargs))
    return wrapped(*args, **kwargs)

class Greeter:
    def __init__(self, prefix):
        self.prefix = prefix
    def say(self, name):
        return f"{self.prefix} {name}"
    @classmethod
    def cm(cls, x):
        return (cls.__name__, x)
    @staticmethod
    def sm(x):
        return x * 2
    def cm2(cls, x):
        return (cls.__name__, x + 1)
    cm2 = _wrappers.FunctionWrapper(classmethod(cm2), wrapper)

Greeter.say = _wrappers.FunctionWrapper(Greeter.say, wrapper)
Greeter.cm = _wrappers.FunctionWrapper(Greeter.cm, wrapper)
Greeter.sm = _wrappers.FunctionWrapper(staticmethod(Greeter.sm), wrapper)
g = Greeter("Hi")
assert g.say("Alice") == "Hi Alice"
assert Greeter.cm(42) == ("Greeter", 42)
assert g.cm(42) == ("Greeter", 42)
assert Greeter.sm(21) == 42
assert g.sm(21) == 42
assert Greeter.cm2(42) == ("Greeter", 43)
assert g.cm2(42) == ("Greeter", 43)
print("wrapt-ok")
"""
    )
    exe = build(main_py, tmp_path / "wrapt_prog", optimize=False, workers=1)
    res = subprocess.run([exe], capture_output=True, text=True, check=False, timeout=60)
    assert res.returncode == 0, res.stderr
    assert res.stdout == "wrapt-ok\n"

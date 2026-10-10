"""Compile and exercise the native object runtime without Python evaluation."""

import shlex
import shutil
import subprocess
import sys
import sysconfig
from pathlib import Path

import pytest

RUNTIME = Path(__file__).resolve().parents[3] / "mule/compiler/runtime.c"

PROBE = r"""
#include <stdio.h>
#include <string.h>
#include "runtime.c"

static const char *scenario;

static int show(PyObject *value) {
    if (!value) return -1;
    int status = PyObject_Print(value, stdout, 0);
    putchar('\n');
    mule_release(value);
    return status;
}

static PyObject *add_bias(PyObject *globals, PyObject *args) {
    if (mule_check_args(args, 1) < 0) return NULL;
    PyObject *value = mule_arg(args, 0);
    PyObject *bias = mule_load_global(globals, "bias");
    if (!value || !bias) {
        mule_release(value); mule_release(bias); return NULL;
    }
    PyObject *result = mule_binary(0, value, bias, 0);
    mule_release(value); mule_release(bias);
    return result;
}

static void destructor_error(PyObject *capsule) {
    (void)capsule;
    PyErr_SetString(PyExc_RuntimeError, "cleanup failure");
}

static PyObject *double_value(PyObject *globals, PyObject *args) {
    (void)globals;
    if (mule_check_args(args, 1) < 0) return NULL;
    PyObject *value = mule_arg(args, 0);
    if (!value) return NULL;
    PyObject *result = mule_binary(0, value, value, 0);
    mule_release(value);
    return result;
}

static PyObject *recurse(PyObject *globals, PyObject *args) {
    static int depth;
    if (mule_check_args(args, 0) < 0) return NULL;
    if (++depth >= 100000) {
        PyErr_SetString(PyExc_RuntimeError, "recursion guard missing");
        return NULL;
    }
    PyObject *function = mule_load_global(globals, "recurse");
    if (!function) return NULL;
    PyObject *result = mule_call(function, 0, NULL);
    mule_release(function);
    return result;
}

static PyObject *module_entry(PyObject *globals, PyObject *args) {
    if (mule_check_args(args, 0) < 0) return NULL;
    if (!strcmp(scenario, "literals")) {
        if (show(mule_literal_int("-123456789012345678901234567890")) < 0 ||
            show(mule_literal_float(1.25)) < 0 ||
            show(mule_literal_text("snow \xe2\x98\x83\0end", 12)) < 0 ||
            show(mule_literal_bytes("a\0b", 3)) < 0 ||
            show(mule_none()) < 0 || show(mule_bool(0)) < 0 ||
            show(mule_bool(1)) < 0) return NULL;
    } else if (!strcmp(scenario, "huge_int")) {
        char decimal[6002];
        decimal[0] = '1'; memset(decimal + 1, '0', 6000); decimal[6001] = 0;
        PyObject *actual = mule_literal_int(decimal);
        PyObject *ten = mule_literal_int("10"), *exponent = mule_literal_int("6000");
        PyObject *expected = PyNumber_Power(ten, exponent, Py_None);
        mule_release(ten); mule_release(exponent);
        if (!actual || !expected) { mule_release(actual); mule_release(expected); return NULL; }
        PyObject *equal = mule_compare(0, actual, expected);
        mule_release(actual); mule_release(expected);
        if (show(equal) < 0) return NULL;
        decimal[0] = '-'; decimal[1] = '1';
        PyObject *negative = mule_literal_int(decimal);
        if (!negative) return NULL;
        PyObject *zero = mule_literal_int("0");
        equal = mule_compare(2, negative, zero);
        mule_release(negative); mule_release(zero);
        if (show(equal) < 0) return NULL;
    } else if (!strcmp(scenario, "recursion")) {
        PyObject *function = mule_function(globals, (void *)recurse, "recurse");
        if (!function) return NULL;
        if (mule_store_global(globals, "recurse", function) < 0) {
            mule_release(function); return NULL;
        }
        PyObject *result = mule_call(function, 0, NULL);
        mule_release(function); return result;
    } else if (!strcmp(scenario, "binary")) {
        PyObject *left = mule_literal_int("7"), *right = mule_literal_int("2");
        for (int op = 0; op < 13; op++) {
            if (op == 6) continue;
            if (show(mule_binary(op, left, right, 0)) < 0) {
                mule_release(left); mule_release(right); return NULL;
            }
        }
        mule_release(left); mule_release(right);
    } else if (!strcmp(scenario, "unary")) {
        PyObject *value = mule_literal_int("7");
        for (int op = 0; op < 4; op++) {
            if (show(mule_unary(op, value)) < 0) { mule_release(value); return NULL; }
        }
        mule_release(value);
    } else if (!strcmp(scenario, "compare")) {
        PyObject *left = mule_literal_int("1000"), *right = mule_literal_int("1000");
        for (int op = 0; op < 8; op++) {
            if (show(mule_compare(op, left, right)) < 0) goto compare_error;
        }
        PyObject *items = PyTuple_Pack(1, left);
        if (!items) goto compare_error;
        int status = show(mule_compare(8, left, items));
        if (status == 0) status = show(mule_compare(9, left, items));
        mule_release(items);
        if (status < 0) goto compare_error;
        mule_release(left); mule_release(right);
        goto done;
        compare_error:
        mule_release(left); mule_release(right); return NULL;
    } else if (!strcmp(scenario, "inplace")) {
        PyObject *left = PyList_New(0), *right = PyList_New(0);
        PyObject *value = mule_literal_int("5");
        if (!left || !right || !value || PyList_Append(right, value) < 0) return NULL;
        PyObject *result = mule_binary(0, left, right, 1);
        if (!result) return NULL;
        printf("same=%d\n", result == left);
        show(result); mule_release(left); mule_release(right); mule_release(value);
    } else if (!strcmp(scenario, "globals")) {
        PyObject *value = mule_literal_int("42");
        if (!value || mule_store_global(globals, "answer", value) < 0) return NULL;
        mule_release(value);
        if (show(mule_load_global(globals, "answer")) < 0) return NULL;
        PyObject *builtin = mule_load_global(globals, "print");
        if (!builtin) return NULL;
        printf("callable=%d\n", PyCallable_Check(builtin)); mule_release(builtin);
        value = mule_none();
        if (mule_store_global(globals, "print", value) < 0) return NULL;
        mule_release(value);
        if (show(mule_load_global(globals, "print")) < 0) return NULL;
        if (show(mule_load_global(globals, "__name__")) < 0) return NULL;
    } else if (!strcmp(scenario, "function_metadata")) {
        PyObject *bias = mule_literal_int("40");
        if (!bias || mule_store_global(globals, "bias", bias) < 0) return NULL;
        mule_release(bias);
        PyObject *first = mule_function(globals, (void *)add_bias, "first");
        if (!first) return NULL;
        for (int index = 0; index < 10000; index++) {
            PyObject *replacement = mule_function(globals, (void *)add_bias, "first");
            if (!replacement) { mule_release(first); return NULL; }
            if (replacement == first) {
                PyErr_SetString(PyExc_RuntimeError, "definition reused callable identity");
                mule_release(replacement); mule_release(first); return NULL;
            }
            mule_release(replacement);
        }
        PyObject *second = mule_function(globals, (void *)add_bias, "second");
        if (!second) { mule_release(first); return NULL; }
        PyObject *other_entry = mule_function(globals, (void *)double_value, "first");
        if (!other_entry) { mule_release(first); mule_release(second); return NULL; }
        int records = 0;
        for (MuleMethodRecord *record = mule_methods; record; record = record->next) records++;
        printf("records=%d\n", records);
        int status = show(PyObject_GetAttrString(first, "__name__"));
        if (status == 0) status = show(PyObject_GetAttrString(second, "__name__"));
        PyObject *value = mule_literal_int("2");
        PyObject *values[] = {value};
        if (status == 0) status = show(mule_call(first, 1, values));
        if (status == 0) status = show(mule_call(second, 1, values));
        if (status == 0) status = show(mule_call(other_entry, 1, values));
        mule_release(first); mule_release(second); mule_release(other_entry); mule_release(value);
        if (status < 0) return NULL;
    } else if (!strcmp(scenario, "function") || !strcmp(scenario, "arity")) {
        PyObject *bias = mule_literal_int("40");
        if (!bias || mule_store_global(globals, "bias", bias) < 0) return NULL;
        mule_release(bias);
        PyObject *function = mule_function(globals, (void *)add_bias, "add_bias");
        if (!function) return NULL;
        if (!strcmp(scenario, "arity")) {
            PyObject *result = mule_call(function, 0, NULL);
            mule_release(function); return result;
        }
        PyObject *value = mule_literal_int("2");
        PyObject *values[] = {value};
        PyObject *result = mule_call(function, 1, values);
        mule_release(value); mule_release(function);
        if (show(result) < 0) return NULL;
    } else if (!strcmp(scenario, "builtin_call")) {
        PyObject *function = mule_load_global(globals, "print");
        PyObject *value = mule_literal_text("native callback", 15);
        PyObject *values[] = {value};
        if (!function || !value) return NULL;
        PyObject *result = mule_call(function, 1, values);
        mule_release(function); mule_release(value);
        if (!result) return NULL;
        mule_release(result);
    } else if (!strcmp(scenario, "truth")) {
        PyObject *zero = mule_literal_int("0"), *one = mule_literal_int("1");
        printf("%d %d\n", mule_truth(zero), mule_truth(one));
        mule_release(zero); mule_release(one);
    } else if (!strcmp(scenario, "zero_division")) {
        PyObject *left = mule_literal_int("1"), *right = mule_literal_int("0");
        PyObject *result = mule_binary(3, left, right, 0);
        mule_release(left); mule_release(right); return result;
    } else if (!strcmp(scenario, "name_error")) {
        return mule_load_global(globals, "absent_name");
    } else if (!strcmp(scenario, "local_error")) {
        return mule_load_local(NULL, "absent_local");
    } else if (!strcmp(scenario, "not_callable")) {
        PyObject *value = mule_literal_int("1");
        PyObject *result = mule_call(value, 0, NULL);
        mule_release(value); return result;
    } else if (!strcmp(scenario, "release_error")) {
        static int data;
        PyObject *value = PyCapsule_New(&data, "probe", destructor_error);
        if (!value) return NULL;
        PyErr_SetString(PyExc_ValueError, "original failure");
        mule_release(value); return NULL;
    } else if (!strcmp(scenario, "null_without_error")) {
        return NULL;
    } else {
        PyErr_SetString(PyExc_RuntimeError, "unknown probe"); return NULL;
    }
    done:
    return mule_none();
}

int main(int argc, char **argv) {
    if (argc != 2) return 99;
    scenario = argv[1];
    return mule_run((void *)module_entry);
}
"""


@pytest.fixture(scope="module")
def runtime_probe(tmp_path_factory):
    clang = shutil.which("clang")
    assert clang is not None, "native runtime tests require clang"
    directory = tmp_path_factory.mktemp("runtime-probe")
    source = directory / "probe.c"
    executable = directory / "probe"
    source.write_text(PROBE)
    libdir = sysconfig.get_config_var("LIBDIR")
    library = sysconfig.get_config_var("LDLIBRARY")
    assert libdir and library
    command = [
        clang,
        "-std=c11",
        "-Wall",
        "-Wextra",
        "-Werror",
        f"-I{sysconfig.get_path('include')}",
        f"-I{RUNTIME.parent}",
        f'-DMULE_PYTHON_HOME="{sys.base_prefix}"',
        str(source),
        str(Path(libdir) / library),
        f"-Wl,-rpath,{libdir}",
        *shlex.split(sysconfig.get_config_var("LIBS") or ""),
        *shlex.split(sysconfig.get_config_var("SYSLIBS") or ""),
        "-o",
        str(executable),
    ]
    subprocess.run(command, check=True, capture_output=True, text=True)
    return executable


@pytest.mark.parametrize(
    ("scenario", "output"),
    [
        (
            "literals",
            "-123456789012345678901234567890\n1.25\n'snow ☃\\x00end'\nb'a\\x00b'\nNone\nFalse\nTrue\n",
        ),
        ("binary", "9\n5\n14\n3.5\n3\n1\n49\n28\n1\n2\n5\n7\n"),
        ("huge_int", "True\nTrue\n"),
        ("unary", "7\n-7\n-8\nFalse\n"),
        (
            "compare",
            "True\nFalse\nFalse\nTrue\nFalse\nTrue\nFalse\nTrue\nTrue\nFalse\n",
        ),
        ("inplace", "same=1\n[5]\n"),
        ("globals", "42\ncallable=1\nNone\n'__main__'\n"),
        ("function", "42\n"),
        ("function_metadata", "records=3\n'first'\n'second'\n42\n42\n4\n"),
        ("builtin_call", "native callback\n"),
        ("truth", "0 1\n"),
    ],
)
def test_runtime_values_and_direct_native_calls(runtime_probe, scenario, output):
    result = subprocess.run(
        [runtime_probe, scenario], check=False, capture_output=True, text=True
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == output
    assert result.stderr == ""


@pytest.mark.parametrize(
    ("scenario", "error"),
    [
        ("zero_division", "ZeroDivisionError"),
        ("name_error", "NameError: name 'absent_name' is not defined"),
        ("local_error", "UnboundLocalError"),
        ("not_callable", "TypeError"),
        ("arity", "TypeError"),
        ("release_error", "ValueError: original failure"),
        ("null_without_error", "SystemError"),
        ("recursion", "RecursionError"),
    ],
)
def test_runtime_errors_exit_nonzero(runtime_probe, scenario, error):
    result = subprocess.run(
        [runtime_probe, scenario], check=False, capture_output=True, text=True
    )
    assert result.returncode != 0
    assert error in result.stderr
    assert "cleanup failure" not in result.stderr

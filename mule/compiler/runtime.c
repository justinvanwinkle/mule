/* Object services for LLVM-generated Mule code. No source or bytecode runner. */
#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* PyCFunction borrows its PyMethodDef. Keep these records alive through
 * interpreter finalization, including callbacks during cyclic collection. */
typedef struct MuleMethodRecord {
    struct MuleMethodRecord *next;
    PyMethodDef definition;
    char name[];
} MuleMethodRecord;

static MuleMethodRecord *mule_methods;

PyObject *mule_literal_int(const char *decimal) {
    /* Decimal conversion limits protect untrusted text parsing. This string
     * describes an already-parsed integer constant, including hex literals
     * whose exact value can exceed that limit. Build it in bounded chunks. */
    int negative = *decimal == '-';
    if (*decimal == '-' || *decimal == '+') decimal++;
    size_t length = strlen(decimal);
    if (!length) {
        PyErr_SetString(PyExc_ValueError, "invalid decimal Mule integer literal");
        return NULL;
    }
    for (size_t index = 0; index < length; index++) {
        if (decimal[index] < '0' || decimal[index] > '9') {
            PyErr_SetString(PyExc_ValueError, "invalid decimal Mule integer literal");
            return NULL;
        }
    }
    size_t width = length % 9;
    if (!width) width = 9;
    unsigned long chunk = 0;
    for (size_t index = 0; index < width; index++) {
        chunk = chunk * 10 + (unsigned long)(decimal[index] - '0');
    }
    PyObject *result = PyLong_FromUnsignedLong(chunk);
    if (!result) return NULL;
    PyObject *base = length > width ? PyLong_FromLong(1000000000L) : NULL;
    if (length > width && !base) { Py_DECREF(result); return NULL; }
    for (size_t offset = width; offset < length; offset += 9) {
        chunk = 0;
        for (size_t index = offset; index < offset + 9; index++) {
            chunk = chunk * 10 + (unsigned long)(decimal[index] - '0');
        }
        PyObject *part = PyLong_FromUnsignedLong(chunk);
        PyObject *scaled = part ? PyNumber_Multiply(result, base) : NULL;
        PyObject *next = scaled ? PyNumber_Add(scaled, part) : NULL;
        Py_XDECREF(part); Py_XDECREF(scaled); Py_DECREF(result);
        result = next;
        if (!result) { Py_DECREF(base); return NULL; }
    }
    Py_XDECREF(base);
    if (negative) {
        PyObject *signed_result = PyNumber_Negative(result);
        Py_DECREF(result);
        result = signed_result;
    }
    return result;
}

PyObject *mule_literal_float(double value) {
    return PyFloat_FromDouble(value);
}

PyObject *mule_literal_text(const char *data, intptr_t length) {
    return PyUnicode_DecodeUTF8(data, (Py_ssize_t)length, "surrogatepass");
}

PyObject *mule_literal_bytes(const char *data, intptr_t length) {
    return PyBytes_FromStringAndSize(data, (Py_ssize_t)length);
}

PyObject *mule_none(void) {
    return Py_NewRef(Py_None);
}

PyObject *mule_bool(int value) {
    return PyBool_FromLong(value != 0);
}

PyObject *mule_load_global(PyObject *globals, const char *name) {
    PyObject *value = PyDict_GetItemString(globals, name);
    if (value) return Py_NewRef(value);
    if (PyErr_Occurred()) return NULL;

    PyObject *builtins = PyDict_GetItemString(globals, "__builtins__");
    if (!builtins) {
        if (PyErr_Occurred()) return NULL;
        builtins = PyImport_AddModule("builtins");
        if (!builtins) return NULL;
    }
    if (PyModule_Check(builtins)) builtins = PyModule_GetDict(builtins);
    if (!PyDict_Check(builtins)) {
        PyErr_SetString(PyExc_TypeError, "__builtins__ must be a module or dictionary");
        return NULL;
    }
    value = PyDict_GetItemString(builtins, name);
    if (value) return Py_NewRef(value);
    if (!PyErr_Occurred()) {
        PyErr_Format(PyExc_NameError, "name '%s' is not defined", name);
    }
    return NULL;
}

int mule_store_global(PyObject *globals, const char *name, PyObject *value) {
    return PyDict_SetItemString(globals, name, value);
}

PyObject *mule_load_local(PyObject *value, const char *name) {
    if (value) return Py_NewRef(value);
    PyErr_Format(PyExc_UnboundLocalError,
                 "cannot access local variable '%s' where it is not associated with a value",
                 name);
    return NULL;
}

PyObject *mule_binary(int op, PyObject *left, PyObject *right, int inplace) {
    switch (op) {
        case 0: return inplace ? PyNumber_InPlaceAdd(left, right) : PyNumber_Add(left, right);
        case 1: return inplace ? PyNumber_InPlaceSubtract(left, right) : PyNumber_Subtract(left, right);
        case 2: return inplace ? PyNumber_InPlaceMultiply(left, right) : PyNumber_Multiply(left, right);
        case 3: return inplace ? PyNumber_InPlaceTrueDivide(left, right) : PyNumber_TrueDivide(left, right);
        case 4: return inplace ? PyNumber_InPlaceFloorDivide(left, right) : PyNumber_FloorDivide(left, right);
        case 5: return inplace ? PyNumber_InPlaceRemainder(left, right) : PyNumber_Remainder(left, right);
        case 6: return inplace ? PyNumber_InPlaceMatrixMultiply(left, right) : PyNumber_MatrixMultiply(left, right);
        case 7: return inplace ? PyNumber_InPlacePower(left, right, Py_None) : PyNumber_Power(left, right, Py_None);
        case 8: return inplace ? PyNumber_InPlaceLshift(left, right) : PyNumber_Lshift(left, right);
        case 9: return inplace ? PyNumber_InPlaceRshift(left, right) : PyNumber_Rshift(left, right);
        case 10: return inplace ? PyNumber_InPlaceAnd(left, right) : PyNumber_And(left, right);
        case 11: return inplace ? PyNumber_InPlaceXor(left, right) : PyNumber_Xor(left, right);
        case 12: return inplace ? PyNumber_InPlaceOr(left, right) : PyNumber_Or(left, right);
        default:
            PyErr_SetString(PyExc_SystemError, "unknown Mule binary operation");
            return NULL;
    }
}

PyObject *mule_unary(int op, PyObject *value) {
    switch (op) {
        case 0: return PyNumber_Positive(value);
        case 1: return PyNumber_Negative(value);
        case 2: return PyNumber_Invert(value);
        case 3: {
            int truth = PyObject_IsTrue(value);
            return truth < 0 ? NULL : mule_bool(!truth);
        }
        default:
            PyErr_SetString(PyExc_SystemError, "unknown Mule unary operation");
            return NULL;
    }
}

PyObject *mule_compare(int op, PyObject *left, PyObject *right) {
    static const int rich_operations[] = {Py_EQ, Py_NE, Py_LT, Py_LE, Py_GT, Py_GE};
    if (op >= 0 && op < 6) {
        return PyObject_RichCompare(left, right, rich_operations[op]);
    }
    if (op == 6) return mule_bool(left == right);
    if (op == 7) return mule_bool(left != right);
    if (op == 8 || op == 9) {
        int contains = PySequence_Contains(right, left);
        return contains < 0 ? NULL : mule_bool(op == 8 ? contains : !contains);
    }
    PyErr_SetString(PyExc_SystemError, "unknown Mule comparison operation");
    return NULL;
}

int mule_truth(PyObject *value) {
    return PyObject_IsTrue(value);
}

void mule_release(PyObject *value) {
    if (!value) return;
    /* Deallocation can execute callbacks. Preserve the exception that caused
     * generated code to enter its cleanup path. */
    PyObject *exception = PyErr_GetRaisedException();
    Py_DECREF(value);
    if (exception) PyErr_SetRaisedException(exception);
}

PyObject *mule_call(PyObject *callable, intptr_t argc, PyObject **argv) {
    if (argc < 0) {
        PyErr_SetString(PyExc_SystemError, "negative Mule argument count");
        return NULL;
    }
    PyObject *args = PyTuple_New((Py_ssize_t)argc);
    if (!args) return NULL;
    for (intptr_t index = 0; index < argc; index++) {
        PyTuple_SET_ITEM(args, index, Py_NewRef(argv[index]));
    }
    if (Py_EnterRecursiveCall(" while calling a Mule native function")) {
        mule_release(args);
        return NULL;
    }
    PyObject *result = PyObject_Call(callable, args, NULL);
    Py_LeaveRecursiveCall();
    mule_release(args);
    return result;
}

PyObject *mule_function(PyObject *globals, void *nativecode, const char *name) {
    /* Reexecuting a definition creates a fresh callable, while its immutable
     * method metadata is shared by native entry and public name. */
    PyCFunction entry = (PyCFunction)nativecode;
    for (MuleMethodRecord *record = mule_methods; record; record = record->next) {
        if (record->definition.ml_meth == entry && !strcmp(record->name, name)) {
            return PyCFunction_NewEx(&record->definition, globals, NULL);
        }
    }
    size_t length = strlen(name);
    MuleMethodRecord *record = malloc(sizeof(*record) + length + 1);
    if (!record) return PyErr_NoMemory();
    memcpy(record->name, name, length + 1);
    record->definition = (PyMethodDef){record->name, entry, METH_VARARGS, NULL};
    PyObject *function = PyCFunction_NewEx(&record->definition, globals, NULL);
    if (!function) {
        free(record);
        return NULL;
    }
    record->next = mule_methods;
    mule_methods = record;
    return function;
}

int mule_check_args(PyObject *args, int count) {
    if (!PyTuple_Check(args)) {
        PyErr_SetString(PyExc_SystemError, "Mule native function arguments must be a tuple");
        return -1;
    }
    Py_ssize_t actual = PyTuple_GET_SIZE(args);
    if (actual != count) {
        PyErr_Format(PyExc_TypeError, "expected %d positional arguments, got %zd", count, actual);
        return -1;
    }
    return 0;
}

PyObject *mule_arg(PyObject *args, int index) {
    PyObject *value = PyTuple_GetItem(args, index);
    return value ? Py_NewRef(value) : NULL;
}

static void mule_free_methods(void) {
    while (mule_methods) {
        MuleMethodRecord *record = mule_methods;
        mule_methods = record->next;
        free(record);
    }
}

int mule_run(void *native_module_init) {
    PyConfig config;
    PyConfig_InitPythonConfig(&config);
    config.parse_argv = 0;
    config.site_import = 0;
    PyStatus status = PyStatus_Ok();
#ifdef MULE_PYTHON_HOME
    status = PyConfig_SetBytesString(&config, &config.home, MULE_PYTHON_HOME);
#endif
    if (!PyStatus_Exception(status)) status = Py_InitializeFromConfig(&config);
    if (PyStatus_Exception(status)) {
        fprintf(stderr, "Mule runtime initialization failed: %s\n",
                status.err_msg ? status.err_msg : "unknown error");
        PyConfig_Clear(&config);
        return 1;
    }
    PyConfig_Clear(&config);

    int failed = 0;
    PyObject *globals = PyDict_New();
    PyObject *name = mule_literal_text("__main__", 8);
    PyObject *builtins = PyImport_AddModule("builtins");
    if (!globals || !name || !builtins ||
        mule_store_global(globals, "__name__", name) < 0 ||
        mule_store_global(globals, "__builtins__", PyModule_GetDict(builtins)) < 0) {
        failed = 1;
    } else {
        PyCFunction entry = (PyCFunction)native_module_init;
        PyObject *args = PyTuple_New(0);
        PyObject *result = args ? entry(globals, args) : NULL;
        mule_release(args);
        if (!result) {
            if (!PyErr_Occurred()) {
                PyErr_SetString(PyExc_SystemError, "Mule native module returned NULL without an exception");
            }
            failed = 1;
        }
        mule_release(result);
    }
    mule_release(name);
    mule_release(globals);
    if (PyErr_Occurred()) {
        PyErr_PrintEx(0);
        failed = 1;
    }
    if (Py_FinalizeEx() < 0) failed = 1;
    mule_free_methods();
    return failed;
}

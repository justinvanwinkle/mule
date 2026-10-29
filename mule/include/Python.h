/* Mule's CPython C API headers.
 *
 * These declarations let unmodified C extension sources compile against
 * Mule. They contain declarations and macros only; every function and
 * data object is implemented by Mule's runtime (mule/runtime/capi.rs and
 * stdlib/_capi.py). They do not include or link CPython.
 *
 * Objects crossing into C are proxies: each Mule object has at most one
 * proxy, a PyObject whose ob_mule field refers back to the Mule object.
 * Field-access macros that CPython implements by reading object storage
 * (tuple items, string lengths, reference counts) call runtime functions
 * instead. See spec/cpython-interface.md.
 */

#ifndef Py_PYTHON_H
#define Py_PYTHON_H

#include <limits.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <assert.h>
#include <errno.h>

#define PY_MAJOR_VERSION 3
#define PY_MINOR_VERSION 14
#define PY_MICRO_VERSION 0
#define PY_RELEASE_LEVEL 0xF
#define PY_RELEASE_SERIAL 0
#define PY_VERSION "3.14.0"
#define PY_VERSION_HEX 0x030E00F0

/* Mule programs run on one thread; extensions see GIL-build semantics. */
#define MULE_CAPI 1

#ifdef __cplusplus
#  define PyAPI_FUNC(RTYPE) extern "C" RTYPE
#  define PyAPI_DATA(RTYPE) extern "C" RTYPE
#  define PyMODINIT_FUNC extern "C" __attribute__((visibility("default"))) PyObject *
#else
#  define PyAPI_FUNC(RTYPE) extern RTYPE
#  define PyAPI_DATA(RTYPE) extern RTYPE
#  define PyMODINIT_FUNC __attribute__((visibility("default"))) PyObject *
#endif

#define Py_UNUSED(name) _unused_ ## name __attribute__((unused))
#define Py_ARRAY_LENGTH(array) (sizeof(array) / sizeof((array)[0]))
#define Py_MIN(x, y) (((x) > (y)) ? (y) : (x))
#define Py_MAX(x, y) (((x) > (y)) ? (x) : (y))
#define Py_ABS(x) ((x) < 0 ? -(x) : (x))
#define Py_STRINGIFY(x) _Py_XSTRINGIFY(x)
#define _Py_XSTRINGIFY(x) #x
#define Py_LOCAL_INLINE(type) static inline type
#define Py_ALWAYS_INLINE __attribute__((always_inline))
#define Py_NO_INLINE __attribute__((noinline))
#define Py_DEPRECATED(version) __attribute__((deprecated))
#define Py_GCC_ATTRIBUTE(x) __attribute__(x)
#define Py_UNREACHABLE() __builtin_unreachable()
#define Py_CHARMASK(c) ((unsigned char)((c) & 0xff))
#define _Py_STATIC_CAST(type, expr) ((type)(expr))
#define _Py_CAST(type, expr) ((type)(expr))
#define _Py_NULL NULL

typedef ssize_t Py_ssize_t;
typedef Py_ssize_t Py_hash_t;
typedef size_t Py_uhash_t;
typedef uintptr_t Py_uintptr_t;
typedef intptr_t Py_intptr_t;
typedef uint32_t Py_UCS4;
typedef uint16_t Py_UCS2;
typedef uint8_t Py_UCS1;
#define PY_SSIZE_T_MAX ((Py_ssize_t)(((size_t)-1) >> 1))
#define PY_SSIZE_T_MIN (-PY_SSIZE_T_MAX - 1)
#define SIZEOF_VOID_P 8
#define SIZEOF_SIZE_T 8
#define SIZEOF_LONG 8

/* Objects */

struct _typeobject;
typedef struct _object {
    Py_ssize_t ob_refcnt;
    struct _typeobject *ob_type;
    /* Mule-private: the Mule object this proxy represents. */
    void *ob_mule;
} PyObject;

typedef struct {
    PyObject ob_base;
    Py_ssize_t ob_size;
} PyVarObject;

typedef struct _typeobject PyTypeObject;

#define PyObject_HEAD PyObject ob_base;
#define PyObject_VAR_HEAD PyVarObject ob_base;
#define PyObject_HEAD_INIT(type) { 1, (type), NULL },
#define PyVarObject_HEAD_INIT(type, size) { PyObject_HEAD_INIT(type) (size) },

#define _PyObject_CAST(op) ((PyObject *)(op))
#define _PyVarObject_CAST(op) ((PyVarObject *)(op))

PyAPI_FUNC(PyTypeObject *) Py_TYPE(PyObject *op);
#define Py_TYPE(op) Py_TYPE(_PyObject_CAST(op))
PyAPI_FUNC(Py_ssize_t) Py_REFCNT(PyObject *op);
#define Py_REFCNT(op) Py_REFCNT(_PyObject_CAST(op))
PyAPI_FUNC(Py_ssize_t) Py_SIZE(PyObject *op);
#define Py_SIZE(op) Py_SIZE(_PyObject_CAST(op))
#define Py_IS_TYPE(op, type) (Py_TYPE(op) == (type))
#define Py_Is(x, y) ((x) == (y))
#define Py_IsNone(x) Py_Is((x), Py_None)
#define Py_IsTrue(x) Py_Is((x), Py_True)
#define Py_IsFalse(x) Py_Is((x), Py_False)

PyAPI_FUNC(void) Py_IncRef(PyObject *op);
PyAPI_FUNC(void) Py_DecRef(PyObject *op);
#define Py_INCREF(op) Py_IncRef(_PyObject_CAST(op))
#define Py_DECREF(op) Py_DecRef(_PyObject_CAST(op))
#define Py_XINCREF(op) Py_IncRef(_PyObject_CAST(op))
#define Py_XDECREF(op) Py_DecRef(_PyObject_CAST(op))

static inline PyObject *_Py_NewRef(PyObject *obj) { Py_INCREF(obj); return obj; }
static inline PyObject *_Py_XNewRef(PyObject *obj) { Py_XINCREF(obj); return obj; }
#define Py_NewRef(obj) _Py_NewRef(_PyObject_CAST(obj))
#define Py_XNewRef(obj) _Py_XNewRef(_PyObject_CAST(obj))

#define Py_CLEAR(op) \
    do { \
        PyObject **_py_tmp_ptr = (PyObject **)&(op); \
        PyObject *_py_tmp = *_py_tmp_ptr; \
        if (_py_tmp != NULL) { \
            *_py_tmp_ptr = NULL; \
            Py_DECREF(_py_tmp); \
        } \
    } while (0)

#define Py_SETREF(dst, src) \
    do { \
        PyObject **_tmp_dst_ptr = (PyObject **)&(dst); \
        PyObject *_tmp_old_dst = *_tmp_dst_ptr; \
        *_tmp_dst_ptr = _PyObject_CAST(src); \
        Py_DECREF(_tmp_old_dst); \
    } while (0)

#define Py_XSETREF(dst, src) \
    do { \
        PyObject **_tmp_dst_ptr = (PyObject **)&(dst); \
        PyObject *_tmp_old_dst = *_tmp_dst_ptr; \
        *_tmp_dst_ptr = _PyObject_CAST(src); \
        Py_XDECREF(_tmp_old_dst); \
    } while (0)

#define Py_VISIT(op) \
    do { \
        if (op) { \
            int vret = visit(_PyObject_CAST(op), arg); \
            if (vret) \
                return vret; \
        } \
    } while (0)

/* Function types */

typedef PyObject *(*unaryfunc)(PyObject *);
typedef PyObject *(*binaryfunc)(PyObject *, PyObject *);
typedef PyObject *(*ternaryfunc)(PyObject *, PyObject *, PyObject *);
typedef int (*inquiry)(PyObject *);
typedef Py_ssize_t (*lenfunc)(PyObject *);
typedef PyObject *(*ssizeargfunc)(PyObject *, Py_ssize_t);
typedef PyObject *(*ssizessizeargfunc)(PyObject *, Py_ssize_t, Py_ssize_t);
typedef int (*ssizeobjargproc)(PyObject *, Py_ssize_t, PyObject *);
typedef int (*ssizessizeobjargproc)(PyObject *, Py_ssize_t, Py_ssize_t, PyObject *);
typedef int (*objobjargproc)(PyObject *, PyObject *, PyObject *);
typedef int (*objobjproc)(PyObject *, PyObject *);
typedef int (*visitproc)(PyObject *, void *);
typedef int (*traverseproc)(PyObject *, visitproc, void *);
typedef void (*freefunc)(void *);
typedef void (*destructor)(PyObject *);
typedef PyObject *(*getattrfunc)(PyObject *, char *);
typedef PyObject *(*getattrofunc)(PyObject *, PyObject *);
typedef int (*setattrfunc)(PyObject *, char *, PyObject *);
typedef int (*setattrofunc)(PyObject *, PyObject *, PyObject *);
typedef PyObject *(*reprfunc)(PyObject *);
typedef Py_hash_t (*hashfunc)(PyObject *);
typedef PyObject *(*richcmpfunc)(PyObject *, PyObject *, int);
typedef PyObject *(*getiterfunc)(PyObject *);
typedef PyObject *(*iternextfunc)(PyObject *);
typedef PyObject *(*descrgetfunc)(PyObject *, PyObject *, PyObject *);
typedef int (*descrsetfunc)(PyObject *, PyObject *, PyObject *);
typedef int (*initproc)(PyObject *, PyObject *, PyObject *);
typedef PyObject *(*newfunc)(struct _typeobject *, PyObject *, PyObject *);
typedef PyObject *(*allocfunc)(struct _typeobject *, Py_ssize_t);
typedef PyObject *(*vectorcallfunc)(PyObject *, PyObject *const *, size_t, PyObject *);
typedef PyObject *(*getter)(PyObject *, void *);
typedef int (*setter)(PyObject *, PyObject *, void *);
typedef struct bufferinfo Py_buffer;
typedef int (*getbufferproc)(PyObject *, Py_buffer *, int);
typedef void (*releasebufferproc)(PyObject *, Py_buffer *);
typedef PyObject *(*sendfunc_unused)(PyObject *);

typedef struct {
    binaryfunc nb_add;
    binaryfunc nb_subtract;
    binaryfunc nb_multiply;
    binaryfunc nb_remainder;
    binaryfunc nb_divmod;
    ternaryfunc nb_power;
    unaryfunc nb_negative;
    unaryfunc nb_positive;
    unaryfunc nb_absolute;
    inquiry nb_bool;
    unaryfunc nb_invert;
    binaryfunc nb_lshift;
    binaryfunc nb_rshift;
    binaryfunc nb_and;
    binaryfunc nb_xor;
    binaryfunc nb_or;
    unaryfunc nb_int;
    void *nb_reserved;
    unaryfunc nb_float;
    binaryfunc nb_inplace_add;
    binaryfunc nb_inplace_subtract;
    binaryfunc nb_inplace_multiply;
    binaryfunc nb_inplace_remainder;
    ternaryfunc nb_inplace_power;
    binaryfunc nb_inplace_lshift;
    binaryfunc nb_inplace_rshift;
    binaryfunc nb_inplace_and;
    binaryfunc nb_inplace_xor;
    binaryfunc nb_inplace_or;
    binaryfunc nb_floor_divide;
    binaryfunc nb_true_divide;
    binaryfunc nb_inplace_floor_divide;
    binaryfunc nb_inplace_true_divide;
    unaryfunc nb_index;
    binaryfunc nb_matrix_multiply;
    binaryfunc nb_inplace_matrix_multiply;
} PyNumberMethods;

typedef struct {
    lenfunc sq_length;
    binaryfunc sq_concat;
    ssizeargfunc sq_repeat;
    ssizeargfunc sq_item;
    void *was_sq_slice;
    ssizeobjargproc sq_ass_item;
    void *was_sq_ass_slice;
    objobjproc sq_contains;
    binaryfunc sq_inplace_concat;
    ssizeargfunc sq_inplace_repeat;
} PySequenceMethods;

typedef struct {
    lenfunc mp_length;
    binaryfunc mp_subscript;
    objobjargproc mp_ass_subscript;
} PyMappingMethods;

typedef struct {
    unaryfunc am_await;
    unaryfunc am_aiter;
    unaryfunc am_anext;
    void *am_send;
} PyAsyncMethods;

typedef struct {
    getbufferproc bf_getbuffer;
    releasebufferproc bf_releasebuffer;
} PyBufferProcs;

typedef PyObject *(*PyCFunction)(PyObject *, PyObject *);
typedef PyObject *(*PyCFunctionWithKeywords)(PyObject *, PyObject *, PyObject *);
typedef PyObject *(*PyCFunctionFast)(PyObject *, PyObject *const *, Py_ssize_t);
typedef PyObject *(*PyCFunctionFastWithKeywords)(PyObject *, PyObject *const *, Py_ssize_t, PyObject *);
typedef PyObject *(*PyCMethod)(PyObject *, PyTypeObject *, PyObject *const *, size_t, PyObject *);
#define _PyCFunction_CAST(func) ((PyCFunction)(void (*)(void))(func))

struct PyMethodDef {
    const char *ml_name;
    PyCFunction ml_meth;
    int ml_flags;
    const char *ml_doc;
};
typedef struct PyMethodDef PyMethodDef;

#define METH_VARARGS 0x0001
#define METH_KEYWORDS 0x0002
#define METH_NOARGS 0x0004
#define METH_O 0x0008
#define METH_CLASS 0x0010
#define METH_STATIC 0x0020
#define METH_COEXIST 0x0040
#define METH_FASTCALL 0x0080
#define METH_METHOD 0x0200

typedef struct PyGetSetDef {
    const char *name;
    getter get;
    setter set;
    const char *doc;
    void *closure;
} PyGetSetDef;

typedef struct PyMemberDef {
    const char *name;
    int type;
    Py_ssize_t offset;
    int flags;
    const char *doc;
} PyMemberDef;

#define Py_T_SHORT 0
#define Py_T_INT 1
#define Py_T_LONG 2
#define Py_T_FLOAT 3
#define Py_T_DOUBLE 4
#define Py_T_STRING 5
#define _Py_T_OBJECT 6
#define Py_T_CHAR 7
#define Py_T_BYTE 8
#define Py_T_UBYTE 9
#define Py_T_UINT 11
#define Py_T_USHORT 12
#define Py_T_ULONG 13
#define Py_T_STRING_INPLACE 14
#define Py_T_BOOL 15
#define Py_T_OBJECT_EX 16
#define Py_T_LONGLONG 17
#define Py_T_ULONGLONG 18
#define Py_T_PYSSIZET 19
#define _Py_T_NONE 20
#define Py_READONLY 1
#define Py_AUDIT_READ 2
#define _Py_WRITE_RESTRICTED 4
#define Py_RELATIVE_OFFSET 8

struct _typeobject {
    PyObject_VAR_HEAD
    const char *tp_name;
    Py_ssize_t tp_basicsize, tp_itemsize;
    destructor tp_dealloc;
    Py_ssize_t tp_vectorcall_offset;
    getattrfunc tp_getattr;
    setattrfunc tp_setattr;
    PyAsyncMethods *tp_as_async;
    reprfunc tp_repr;
    PyNumberMethods *tp_as_number;
    PySequenceMethods *tp_as_sequence;
    PyMappingMethods *tp_as_mapping;
    hashfunc tp_hash;
    ternaryfunc tp_call;
    reprfunc tp_str;
    getattrofunc tp_getattro;
    setattrofunc tp_setattro;
    PyBufferProcs *tp_as_buffer;
    unsigned long tp_flags;
    const char *tp_doc;
    traverseproc tp_traverse;
    inquiry tp_clear;
    richcmpfunc tp_richcompare;
    Py_ssize_t tp_weaklistoffset;
    getiterfunc tp_iter;
    iternextfunc tp_iternext;
    struct PyMethodDef *tp_methods;
    struct PyMemberDef *tp_members;
    struct PyGetSetDef *tp_getset;
    PyTypeObject *tp_base;
    PyObject *tp_dict;
    descrgetfunc tp_descr_get;
    descrsetfunc tp_descr_set;
    Py_ssize_t tp_dictoffset;
    initproc tp_init;
    allocfunc tp_alloc;
    newfunc tp_new;
    freefunc tp_free;
    inquiry tp_is_gc;
    PyObject *tp_bases;
    PyObject *tp_mro;
    PyObject *tp_cache;
    void *tp_subclasses;
    PyObject *tp_weaklist;
    destructor tp_del;
    unsigned int tp_version_tag;
    destructor tp_finalize;
    vectorcallfunc tp_vectorcall;
    unsigned char tp_watched;
    uint16_t tp_versions_used;
};

#define Py_TPFLAGS_MANAGED_DICT (1UL << 4)
#define Py_TPFLAGS_SEQUENCE (1UL << 5)
#define Py_TPFLAGS_MAPPING (1UL << 6)
#define Py_TPFLAGS_DISALLOW_INSTANTIATION (1UL << 7)
#define Py_TPFLAGS_IMMUTABLETYPE (1UL << 8)
#define Py_TPFLAGS_HEAPTYPE (1UL << 9)
#define Py_TPFLAGS_BASETYPE (1UL << 10)
#define Py_TPFLAGS_HAVE_VECTORCALL (1UL << 11)
#define Py_TPFLAGS_READY (1UL << 12)
#define Py_TPFLAGS_READYING (1UL << 13)
#define Py_TPFLAGS_HAVE_GC (1UL << 14)
#define Py_TPFLAGS_HAVE_STACKLESS_EXTENSION 0
#define Py_TPFLAGS_METHOD_DESCRIPTOR (1UL << 17)
#define Py_TPFLAGS_VALID_VERSION_TAG (1UL << 19)
#define Py_TPFLAGS_IS_ABSTRACT (1UL << 20)
#define Py_TPFLAGS_MATCH_SELF (1UL << 22)
#define Py_TPFLAGS_ITEMS_AT_END (1UL << 23)
#define Py_TPFLAGS_LONG_SUBCLASS (1UL << 24)
#define Py_TPFLAGS_LIST_SUBCLASS (1UL << 25)
#define Py_TPFLAGS_TUPLE_SUBCLASS (1UL << 26)
#define Py_TPFLAGS_BYTES_SUBCLASS (1UL << 27)
#define Py_TPFLAGS_UNICODE_SUBCLASS (1UL << 28)
#define Py_TPFLAGS_DICT_SUBCLASS (1UL << 29)
#define Py_TPFLAGS_BASE_EXC_SUBCLASS (1UL << 30)
#define Py_TPFLAGS_TYPE_SUBCLASS (1UL << 31)
#define Py_TPFLAGS_DEFAULT (Py_TPFLAGS_HAVE_STACKLESS_EXTENSION | 0)
#define Py_TPFLAGS_HAVE_FINALIZE (1UL << 0)
#define Py_TPFLAGS_HAVE_VERSION_TAG (1UL << 18)

#define PyType_HasFeature(t, f) ((((PyTypeObject *)(t))->tp_flags & (f)) != 0)
#define PyType_FastSubclass(type, flag) PyType_HasFeature((type), (flag))

typedef struct {
    int slot;
    void *pfunc;
} PyType_Slot;

typedef struct {
    const char *name;
    int basicsize;
    int itemsize;
    unsigned int flags;
    PyType_Slot *slots;
} PyType_Spec;

/* Slot identifiers from CPython's Include/typeslots.h. */
#define Py_bf_getbuffer 1
#define Py_bf_releasebuffer 2
#define Py_mp_ass_subscript 3
#define Py_mp_length 4
#define Py_mp_subscript 5
#define Py_nb_absolute 6
#define Py_nb_add 7
#define Py_nb_and 8
#define Py_nb_bool 9
#define Py_nb_divmod 10
#define Py_nb_float 11
#define Py_nb_floor_divide 12
#define Py_nb_index 13
#define Py_nb_inplace_add 14
#define Py_nb_inplace_and 15
#define Py_nb_inplace_floor_divide 16
#define Py_nb_inplace_lshift 17
#define Py_nb_inplace_multiply 18
#define Py_nb_inplace_or 19
#define Py_nb_inplace_power 20
#define Py_nb_inplace_remainder 21
#define Py_nb_inplace_rshift 22
#define Py_nb_inplace_subtract 23
#define Py_nb_inplace_true_divide 24
#define Py_nb_inplace_xor 25
#define Py_nb_int 26
#define Py_nb_invert 27
#define Py_nb_lshift 28
#define Py_nb_multiply 29
#define Py_nb_negative 30
#define Py_nb_or 31
#define Py_nb_positive 32
#define Py_nb_power 33
#define Py_nb_remainder 34
#define Py_nb_rshift 35
#define Py_nb_subtract 36
#define Py_nb_true_divide 37
#define Py_nb_xor 38
#define Py_sq_ass_item 39
#define Py_sq_concat 40
#define Py_sq_contains 41
#define Py_sq_inplace_concat 42
#define Py_sq_inplace_repeat 43
#define Py_sq_item 44
#define Py_sq_length 45
#define Py_sq_repeat 46
#define Py_tp_alloc 47
#define Py_tp_base 48
#define Py_tp_bases 49
#define Py_tp_call 50
#define Py_tp_clear 51
#define Py_tp_dealloc 52
#define Py_tp_del 53
#define Py_tp_descr_get 54
#define Py_tp_descr_set 55
#define Py_tp_doc 56
#define Py_tp_getattr 57
#define Py_tp_getattro 58
#define Py_tp_hash 59
#define Py_tp_init 60
#define Py_tp_is_gc 61
#define Py_tp_iter 62
#define Py_tp_iternext 63
#define Py_tp_methods 64
#define Py_tp_new 65
#define Py_tp_repr 66
#define Py_tp_richcompare 67
#define Py_tp_setattr 68
#define Py_tp_setattro 69
#define Py_tp_str 70
#define Py_tp_traverse 71
#define Py_tp_members 72
#define Py_tp_getset 73
#define Py_tp_free 74
#define Py_nb_matrix_multiply 75
#define Py_nb_inplace_matrix_multiply 76
#define Py_am_await 77
#define Py_am_aiter 78
#define Py_am_anext 79
#define Py_tp_finalize 80
#define Py_am_send 81
#define Py_tp_vectorcall 82
#define Py_tp_token 83

/* Singletons and builtin types */

PyAPI_DATA(PyObject) _Py_NoneStruct;
PyAPI_DATA(PyObject) _Py_NotImplementedStruct;
PyAPI_DATA(PyObject) _Py_TrueStruct;
PyAPI_DATA(PyObject) _Py_FalseStruct;
PyAPI_DATA(PyObject) _Py_EllipsisObject;
#define Py_None (&_Py_NoneStruct)
#define Py_NotImplemented (&_Py_NotImplementedStruct)
#define Py_True (&_Py_TrueStruct)
#define Py_False (&_Py_FalseStruct)
#define Py_Ellipsis (&_Py_EllipsisObject)
#define Py_RETURN_NONE return Py_NewRef(Py_None)
#define Py_RETURN_NOTIMPLEMENTED return Py_NewRef(Py_NotImplemented)
#define Py_RETURN_TRUE return Py_NewRef(Py_True)
#define Py_RETURN_FALSE return Py_NewRef(Py_False)

PyAPI_DATA(PyTypeObject) PyType_Type;
PyAPI_DATA(PyTypeObject) PyBaseObject_Type;
PyAPI_DATA(PyTypeObject) PyLong_Type;
PyAPI_DATA(PyTypeObject) PyBool_Type;
PyAPI_DATA(PyTypeObject) PyFloat_Type;
PyAPI_DATA(PyTypeObject) PyComplex_Type;
PyAPI_DATA(PyTypeObject) PyUnicode_Type;
PyAPI_DATA(PyTypeObject) PyBytes_Type;
PyAPI_DATA(PyTypeObject) PyTuple_Type;
PyAPI_DATA(PyTypeObject) PyList_Type;
PyAPI_DATA(PyTypeObject) PyDict_Type;
PyAPI_DATA(PyTypeObject) PyFunction_Type;
PyAPI_DATA(PyTypeObject) PyMethod_Type;
PyAPI_DATA(PyTypeObject) PyCFunction_Type;
PyAPI_DATA(PyTypeObject) PyStaticMethod_Type;
PyAPI_DATA(PyTypeObject) PyClassMethod_Type;
PyAPI_DATA(PyTypeObject) PyReversed_Type;
PyAPI_DATA(PyTypeObject) PyModule_Type;

PyAPI_FUNC(int) PyType_IsSubtype(PyTypeObject *a, PyTypeObject *b);
#define PyObject_TypeCheck(ob, type) (Py_IS_TYPE((ob), (type)) || PyType_IsSubtype(Py_TYPE(ob), (type)))
#define PyType_Check(op) PyType_FastSubclass(Py_TYPE(op), Py_TPFLAGS_TYPE_SUBCLASS)
#define PyType_CheckExact(op) Py_IS_TYPE((op), &PyType_Type)
#define PyLong_Check(op) PyType_FastSubclass(Py_TYPE(op), Py_TPFLAGS_LONG_SUBCLASS)
#define PyLong_CheckExact(op) Py_IS_TYPE((op), &PyLong_Type)
#define PyBool_Check(op) Py_IS_TYPE((op), &PyBool_Type)
#define PyFloat_Check(op) PyObject_TypeCheck((op), &PyFloat_Type)
#define PyUnicode_Check(op) PyType_FastSubclass(Py_TYPE(op), Py_TPFLAGS_UNICODE_SUBCLASS)
#define PyUnicode_CheckExact(op) Py_IS_TYPE((op), &PyUnicode_Type)
#define PyBytes_Check(op) PyType_FastSubclass(Py_TYPE(op), Py_TPFLAGS_BYTES_SUBCLASS)
#define PyTuple_Check(op) PyType_FastSubclass(Py_TYPE(op), Py_TPFLAGS_TUPLE_SUBCLASS)
#define PyTuple_CheckExact(op) Py_IS_TYPE((op), &PyTuple_Type)
#define PyList_Check(op) PyType_FastSubclass(Py_TYPE(op), Py_TPFLAGS_LIST_SUBCLASS)
#define PyDict_Check(op) PyType_FastSubclass(Py_TYPE(op), Py_TPFLAGS_DICT_SUBCLASS)
#define PyDict_CheckExact(op) Py_IS_TYPE((op), &PyDict_Type)
#define PyExceptionClass_Check(x) (PyType_Check(x) && PyType_FastSubclass((PyTypeObject *)(x), Py_TPFLAGS_BASE_EXC_SUBCLASS))
#define PyExceptionInstance_Check(x) PyType_FastSubclass(Py_TYPE(x), Py_TPFLAGS_BASE_EXC_SUBCLASS)
#define PyFunction_Check(op) Py_IS_TYPE((op), &PyFunction_Type)
#define PyMethod_Check(op) Py_IS_TYPE((op), &PyMethod_Type)
#define PyCFunction_Check(op) PyObject_TypeCheck((op), &PyCFunction_Type)
#define PyModule_Check(op) PyObject_TypeCheck((op), &PyModule_Type)

/* Generic object operations */

#define Py_LT 0
#define Py_LE 1
#define Py_EQ 2
#define Py_NE 3
#define Py_GT 4
#define Py_GE 5

PyAPI_FUNC(PyObject *) PyObject_Repr(PyObject *);
PyAPI_FUNC(PyObject *) PyObject_Str(PyObject *);
PyAPI_FUNC(PyObject *) PyObject_Format(PyObject *obj, PyObject *format_spec);
PyAPI_FUNC(PyObject *) PyObject_RichCompare(PyObject *, PyObject *, int);
PyAPI_FUNC(int) PyObject_RichCompareBool(PyObject *, PyObject *, int);
PyAPI_FUNC(PyObject *) PyObject_GetAttr(PyObject *, PyObject *);
PyAPI_FUNC(PyObject *) PyObject_GetAttrString(PyObject *, const char *);
PyAPI_FUNC(int) PyObject_GetOptionalAttr(PyObject *, PyObject *, PyObject **);
PyAPI_FUNC(int) PyObject_GetOptionalAttrString(PyObject *, const char *, PyObject **);
PyAPI_FUNC(int) PyObject_SetAttr(PyObject *, PyObject *, PyObject *);
PyAPI_FUNC(int) PyObject_SetAttrString(PyObject *, const char *, PyObject *);
PyAPI_FUNC(int) PyObject_DelAttr(PyObject *, PyObject *);
PyAPI_FUNC(int) PyObject_HasAttrWithError(PyObject *, PyObject *);
PyAPI_FUNC(PyObject *) PyObject_GenericGetAttr(PyObject *, PyObject *);
PyAPI_FUNC(int) PyObject_GenericSetAttr(PyObject *, PyObject *, PyObject *);
PyAPI_FUNC(Py_hash_t) PyObject_Hash(PyObject *);
PyAPI_FUNC(int) PyObject_IsTrue(PyObject *);
PyAPI_FUNC(int) PyObject_Not(PyObject *);
PyAPI_FUNC(int) PyCallable_Check(PyObject *);
PyAPI_FUNC(PyObject *) PyObject_Dir(PyObject *);
PyAPI_FUNC(int) PyObject_IsInstance(PyObject *object, PyObject *typeorclass);
PyAPI_FUNC(int) PyObject_IsSubclass(PyObject *object, PyObject *typeorclass);
PyAPI_FUNC(PyObject *) PyObject_GetItem(PyObject *o, PyObject *key);
PyAPI_FUNC(int) PyObject_SetItem(PyObject *o, PyObject *key, PyObject *v);
PyAPI_FUNC(int) PyObject_DelItem(PyObject *o, PyObject *key);
PyAPI_FUNC(Py_ssize_t) PyObject_Size(PyObject *o);
PyAPI_FUNC(Py_ssize_t) PyObject_Length(PyObject *o);
PyAPI_FUNC(PyObject *) PyObject_GetIter(PyObject *);
PyAPI_FUNC(PyObject *) PyIter_Next(PyObject *);
PyAPI_FUNC(PyObject *) PyObject_Type(PyObject *o);
PyAPI_FUNC(void) PyObject_ClearWeakRefs(PyObject *);

PyAPI_FUNC(PyObject *) PyObject_Call(PyObject *callable, PyObject *args, PyObject *kwargs);
PyAPI_FUNC(PyObject *) PyObject_CallObject(PyObject *callable, PyObject *args);
PyAPI_FUNC(PyObject *) PyObject_CallNoArgs(PyObject *func);
PyAPI_FUNC(PyObject *) PyObject_CallOneArg(PyObject *func, PyObject *arg);
PyAPI_FUNC(PyObject *) PyObject_CallFunctionObjArgs(PyObject *callable, ...);
PyAPI_FUNC(PyObject *) PyObject_CallMethodObjArgs(PyObject *obj, PyObject *name, ...);

/* Allocation and collection */

PyAPI_FUNC(void *) PyMem_Malloc(size_t size);
PyAPI_FUNC(void *) PyMem_Calloc(size_t nelem, size_t elsize);
PyAPI_FUNC(void *) PyMem_Realloc(void *ptr, size_t new_size);
PyAPI_FUNC(void) PyMem_Free(void *ptr);
PyAPI_FUNC(void *) PyObject_Malloc(size_t size);
PyAPI_FUNC(void) PyObject_Free(void *ptr);
PyAPI_FUNC(PyObject *) PyType_GenericAlloc(PyTypeObject *, Py_ssize_t);
PyAPI_FUNC(PyObject *) PyType_GenericNew(PyTypeObject *, PyObject *, PyObject *);
PyAPI_FUNC(void) PyObject_GC_Track(void *);
PyAPI_FUNC(void) PyObject_GC_UnTrack(void *);
PyAPI_FUNC(void) PyObject_GC_Del(void *);

/* Types */

struct PyModuleDef;
PyAPI_FUNC(PyObject *) PyType_FromSpec(PyType_Spec *);
PyAPI_FUNC(PyObject *) PyType_FromSpecWithBases(PyType_Spec *, PyObject *);
PyAPI_FUNC(PyObject *) PyType_FromModuleAndSpec(PyObject *, PyType_Spec *, PyObject *);
PyAPI_FUNC(PyObject *) PyType_GetModule(PyTypeObject *);
PyAPI_FUNC(void *) PyType_GetModuleState(PyTypeObject *);
PyAPI_FUNC(PyObject *) PyType_GetModuleByDef(PyTypeObject *, struct PyModuleDef *);
PyAPI_FUNC(void *) PyType_GetSlot(PyTypeObject *, int);
PyAPI_FUNC(int) PyType_Ready(PyTypeObject *);
PyAPI_FUNC(PyObject *) PyType_GetName(PyTypeObject *);
PyAPI_FUNC(PyObject *) PyType_GetQualName(PyTypeObject *);
PyAPI_FUNC(PyObject *) Py_GenericAlias(PyObject *, PyObject *);

/* Critical sections are blocks on builds with a global interpreter lock. */
#define Py_BEGIN_CRITICAL_SECTION(op) {
#define Py_END_CRITICAL_SECTION() }
#define Py_BEGIN_CRITICAL_SECTION2(a, b) {
#define Py_END_CRITICAL_SECTION2() }

/* Numbers */

PyAPI_FUNC(PyObject *) PyNumber_Add(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_Subtract(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_Multiply(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_MatrixMultiply(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_FloorDivide(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_TrueDivide(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_Remainder(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_Divmod(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_Power(PyObject *o1, PyObject *o2, PyObject *o3);
PyAPI_FUNC(PyObject *) PyNumber_Negative(PyObject *o);
PyAPI_FUNC(PyObject *) PyNumber_Positive(PyObject *o);
PyAPI_FUNC(PyObject *) PyNumber_Absolute(PyObject *o);
PyAPI_FUNC(PyObject *) PyNumber_Invert(PyObject *o);
PyAPI_FUNC(PyObject *) PyNumber_Lshift(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_Rshift(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_And(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_Xor(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_Or(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_Index(PyObject *o);
PyAPI_FUNC(PyObject *) PyNumber_Long(PyObject *o);
PyAPI_FUNC(PyObject *) PyNumber_Float(PyObject *o);
PyAPI_FUNC(PyObject *) PyNumber_InPlaceAdd(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_InPlaceSubtract(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_InPlaceMultiply(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_InPlaceMatrixMultiply(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_InPlaceFloorDivide(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_InPlaceTrueDivide(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_InPlaceRemainder(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_InPlacePower(PyObject *o1, PyObject *o2, PyObject *o3);
PyAPI_FUNC(PyObject *) PyNumber_InPlaceLshift(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_InPlaceRshift(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_InPlaceAnd(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_InPlaceXor(PyObject *o1, PyObject *o2);
PyAPI_FUNC(PyObject *) PyNumber_InPlaceOr(PyObject *o1, PyObject *o2);

PyAPI_FUNC(PyObject *) PyBool_FromLong(long);
PyAPI_FUNC(PyObject *) PyLong_FromLong(long);
PyAPI_FUNC(PyObject *) PyLong_FromSsize_t(Py_ssize_t);
PyAPI_FUNC(PyObject *) PyLong_FromVoidPtr(void *);
PyAPI_FUNC(long) PyLong_AsLong(PyObject *);
PyAPI_FUNC(Py_ssize_t) PyLong_AsSsize_t(PyObject *);
PyAPI_FUNC(PyObject *) PyFloat_FromDouble(double);
PyAPI_FUNC(double) PyFloat_AsDouble(PyObject *);

/* Sequences and mappings */

PyAPI_FUNC(int) PySequence_Contains(PyObject *seq, PyObject *ob);
PyAPI_FUNC(PyObject *) PySequence_Tuple(PyObject *o);
PyAPI_FUNC(PyObject *) PyMapping_Keys(PyObject *o);

PyAPI_FUNC(PyObject *) PyTuple_New(Py_ssize_t size);
PyAPI_FUNC(Py_ssize_t) PyTuple_Size(PyObject *);
PyAPI_FUNC(PyObject *) PyTuple_GetItem(PyObject *, Py_ssize_t);
PyAPI_FUNC(int) PyTuple_SetItem(PyObject *, Py_ssize_t, PyObject *);
PyAPI_FUNC(PyObject *) PyTuple_GetSlice(PyObject *, Py_ssize_t, Py_ssize_t);
PyAPI_FUNC(PyObject *) PyTuple_Pack(Py_ssize_t, ...);
#define PyTuple_GET_SIZE(op) PyTuple_Size(_PyObject_CAST(op))
#define PyTuple_GET_ITEM(op, index) PyTuple_GetItem(_PyObject_CAST(op), (index))
#define PyTuple_SET_ITEM(op, index, value) ((void)PyTuple_SetItem(_PyObject_CAST(op), (index), _PyObject_CAST(value)))

PyAPI_FUNC(PyObject *) PyList_New(Py_ssize_t size);
PyAPI_FUNC(Py_ssize_t) PyList_Size(PyObject *);
PyAPI_FUNC(PyObject *) PyList_GetItem(PyObject *, Py_ssize_t);
PyAPI_FUNC(int) PyList_Append(PyObject *, PyObject *);

PyAPI_FUNC(PyObject *) PyDict_New(void);
PyAPI_FUNC(int) PyDict_SetItem(PyObject *mp, PyObject *key, PyObject *item);
PyAPI_FUNC(int) PyDict_SetItemString(PyObject *dp, const char *key, PyObject *item);
PyAPI_FUNC(int) PyDict_DelItem(PyObject *mp, PyObject *key);
PyAPI_FUNC(PyObject *) PyDict_GetItemWithError(PyObject *mp, PyObject *key);
PyAPI_FUNC(int) PyDict_GetItemRef(PyObject *mp, PyObject *key, PyObject **result);
PyAPI_FUNC(int) PyDict_Contains(PyObject *mp, PyObject *key);
PyAPI_FUNC(int) PyDict_Update(PyObject *mp, PyObject *other);
PyAPI_FUNC(Py_ssize_t) PyDict_Size(PyObject *mp);

/* Text and bytes */

PyAPI_FUNC(PyObject *) PyUnicode_FromString(const char *u);
PyAPI_FUNC(PyObject *) PyUnicode_FromStringAndSize(const char *u, Py_ssize_t size);
PyAPI_FUNC(PyObject *) PyUnicode_InternFromString(const char *u);
PyAPI_FUNC(PyObject *) PyUnicode_FromFormat(const char *format, ...);
PyAPI_FUNC(PyObject *) PyUnicode_FromFormatV(const char *format, va_list vargs);
PyAPI_FUNC(const char *) PyUnicode_AsUTF8(PyObject *unicode);
PyAPI_FUNC(const char *) PyUnicode_AsUTF8AndSize(PyObject *unicode, Py_ssize_t *size);
PyAPI_FUNC(Py_ssize_t) PyUnicode_GetLength(PyObject *unicode);
PyAPI_FUNC(int) PyUnicode_Compare(PyObject *left, PyObject *right);
PyAPI_FUNC(int) PyUnicode_CompareWithASCIIString(PyObject *left, const char *right);
PyAPI_FUNC(int) PyUnicode_EqualToUTF8(PyObject *, const char *);
PyAPI_FUNC(Py_ssize_t) PyUnicode_Tailmatch(PyObject *str, PyObject *substr, Py_ssize_t start, Py_ssize_t end, int direction);
PyAPI_FUNC(PyObject *) PyUnicode_Concat(PyObject *left, PyObject *right);
#define PyUnicode_GET_LENGTH(op) PyUnicode_GetLength(_PyObject_CAST(op))

PyAPI_FUNC(PyObject *) PyBytes_FromStringAndSize(const char *, Py_ssize_t);
PyAPI_FUNC(char *) PyBytes_AsString(PyObject *);
PyAPI_FUNC(Py_ssize_t) PyBytes_Size(PyObject *);

/* Errors */

PyAPI_DATA(PyObject *) PyExc_BaseException;
PyAPI_DATA(PyObject *) PyExc_Exception;
PyAPI_DATA(PyObject *) PyExc_AttributeError;
PyAPI_DATA(PyObject *) PyExc_TypeError;
PyAPI_DATA(PyObject *) PyExc_ValueError;
PyAPI_DATA(PyObject *) PyExc_KeyError;
PyAPI_DATA(PyObject *) PyExc_IndexError;
PyAPI_DATA(PyObject *) PyExc_LookupError;
PyAPI_DATA(PyObject *) PyExc_RuntimeError;
PyAPI_DATA(PyObject *) PyExc_NotImplementedError;
PyAPI_DATA(PyObject *) PyExc_SystemError;
PyAPI_DATA(PyObject *) PyExc_MemoryError;
PyAPI_DATA(PyObject *) PyExc_OverflowError;
PyAPI_DATA(PyObject *) PyExc_ZeroDivisionError;
PyAPI_DATA(PyObject *) PyExc_StopIteration;
PyAPI_DATA(PyObject *) PyExc_ImportError;
PyAPI_DATA(PyObject *) PyExc_DeprecationWarning;
PyAPI_DATA(PyObject *) PyExc_RuntimeWarning;

PyAPI_FUNC(void) PyErr_SetString(PyObject *exception, const char *string);
PyAPI_FUNC(void) PyErr_SetObject(PyObject *exception, PyObject *value);
PyAPI_FUNC(void) PyErr_SetNone(PyObject *exception);
PyAPI_FUNC(PyObject *) PyErr_Format(PyObject *exception, const char *format, ...);
PyAPI_FUNC(PyObject *) PyErr_FormatV(PyObject *exception, const char *format, va_list vargs);
PyAPI_FUNC(PyObject *) PyErr_Occurred(void);
PyAPI_FUNC(void) PyErr_Clear(void);
PyAPI_FUNC(int) PyErr_ExceptionMatches(PyObject *);
PyAPI_FUNC(int) PyErr_GivenExceptionMatches(PyObject *, PyObject *);
PyAPI_FUNC(void) PyErr_Fetch(PyObject **, PyObject **, PyObject **);
PyAPI_FUNC(void) PyErr_Restore(PyObject *, PyObject *, PyObject *);
PyAPI_FUNC(void) PyErr_NormalizeException(PyObject **, PyObject **, PyObject **);
PyAPI_FUNC(PyObject *) PyErr_GetRaisedException(void);
PyAPI_FUNC(void) PyErr_SetRaisedException(PyObject *);
PyAPI_FUNC(void) PyErr_WriteUnraisable(PyObject *);
PyAPI_FUNC(PyObject *) PyErr_NoMemory(void);
PyAPI_FUNC(int) PyErr_BadArgument(void);
PyAPI_FUNC(void) PyErr_BadInternalCall(void);
PyAPI_FUNC(PyObject *) PyErr_NewException(const char *name, PyObject *base, PyObject *dict);
PyAPI_FUNC(int) PyErr_WarnEx(PyObject *category, const char *message, Py_ssize_t stack_level);

/* Argument parsing and value building */

PyAPI_FUNC(int) PyArg_ParseTuple(PyObject *, const char *, ...);
PyAPI_FUNC(int) PyArg_ParseTupleAndKeywords(PyObject *, PyObject *, const char *, char *const *, ...);
PyAPI_FUNC(int) PyArg_UnpackTuple(PyObject *, const char *, Py_ssize_t, Py_ssize_t, ...);
PyAPI_FUNC(PyObject *) Py_BuildValue(const char *, ...);

/* Modules */

typedef struct PyModuleDef_Base {
    PyObject_HEAD
    PyObject *(*m_init)(void);
    Py_ssize_t m_index;
    PyObject *m_copy;
} PyModuleDef_Base;

#define PyModuleDef_HEAD_INIT { PyObject_HEAD_INIT(NULL) NULL, 0, NULL }

typedef struct PyModuleDef_Slot {
    int slot;
    void *value;
} PyModuleDef_Slot;

#define Py_mod_create 1
#define Py_mod_exec 2
#define Py_mod_multiple_interpreters 3
#define Py_mod_gil 4
#define Py_MOD_MULTIPLE_INTERPRETERS_NOT_SUPPORTED ((void *)0)
#define Py_MOD_MULTIPLE_INTERPRETERS_SUPPORTED ((void *)1)
#define Py_MOD_PER_INTERPRETER_GIL_SUPPORTED ((void *)2)
#define Py_MOD_GIL_USED ((void *)0)
#define Py_MOD_GIL_NOT_USED ((void *)1)

typedef struct PyModuleDef {
    PyModuleDef_Base m_base;
    const char *m_name;
    const char *m_doc;
    Py_ssize_t m_size;
    PyMethodDef *m_methods;
    PyModuleDef_Slot *m_slots;
    traverseproc m_traverse;
    inquiry m_clear;
    freefunc m_free;
} PyModuleDef;

PyAPI_FUNC(PyObject *) PyModuleDef_Init(PyModuleDef *);
PyAPI_FUNC(PyModuleDef *) PyModule_GetDef(PyObject *);
PyAPI_FUNC(void *) PyModule_GetState(PyObject *);
PyAPI_FUNC(PyObject *) PyModule_GetDict(PyObject *);
PyAPI_FUNC(PyObject *) PyModule_GetNameObject(PyObject *);
PyAPI_FUNC(int) PyModule_AddObjectRef(PyObject *mod, const char *name, PyObject *value);
PyAPI_FUNC(int) PyModule_AddObject(PyObject *mod, const char *name, PyObject *value);
PyAPI_FUNC(int) PyModule_Add(PyObject *mod, const char *name, PyObject *value);
PyAPI_FUNC(int) PyModule_AddIntConstant(PyObject *mod, const char *name, long value);
PyAPI_FUNC(int) PyModule_AddStringConstant(PyObject *mod, const char *name, const char *value);
PyAPI_FUNC(int) PyModule_AddType(PyObject *mod, PyTypeObject *type);
PyAPI_FUNC(int) PyUnstable_Module_SetGIL(PyObject *module, void *gil);
PyAPI_FUNC(PyObject *) PyImport_ImportModule(const char *name);
PyAPI_FUNC(PyObject *) PyImport_Import(PyObject *name);

#endif /* Py_PYTHON_H */

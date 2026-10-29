#![allow(
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    clippy::missing_safety_doc,
    unused_unsafe
)]

use crate::classes::{self, Builtin};
use crate::object::{self, Data, FunctionKind, Native, Object, Value};
use crate::sequences::mule_protocol_globals;
use std::collections::HashMap;
use std::ffi::{
    CStr, CString, c_char, c_double, c_float, c_int, c_long, c_longlong, c_short, c_uchar, c_uint,
    c_ulong, c_ulonglong, c_ushort, c_void,
};
use std::path::Path;
use std::ptr;
use std::sync::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};

unsafe extern "C" {
    fn calloc(nmemb: usize, size: usize) -> *mut c_void;
    fn malloc(size: usize) -> *mut c_void;
    fn realloc(ptr: *mut c_void, size: usize) -> *mut c_void;
    fn free(ptr: *mut c_void);
    fn dlopen(filename: *const c_char, flags: c_int) -> *mut c_void;
    fn dlsym(handle: *mut c_void, symbol: *const c_char) -> *mut c_void;
    fn dlerror() -> *mut c_char;
}

const RTLD_NOW: c_int = 2;
const RTLD_LOCAL: c_int = 0;

#[inline]
fn has_pending_exception() -> bool {
    object::has_error()
}

#[inline]
fn mule_none() -> Value {
    unsafe { object::none_value() }
}

#[inline]
fn mule_true() -> Value {
    unsafe { object::bool_value(true) }
}

#[inline]
fn mule_false() -> Value {
    unsafe { object::bool_value(false) }
}

#[inline]
fn mule_bool(v: i64) -> Value {
    unsafe { object::bool_value(v != 0) }
}

#[inline]
fn mule_not_implemented() -> Value {
    classes::not_implemented()
}

#[inline]
fn mule_ellipsis() -> Value {
    unsafe {
        let g = mule_protocol_globals();
        if g.is_null() {
            return object::none_value();
        }
        let f = object::lookup(g, "ellipsis_value");
        if f.is_null() {
            object::none_value()
        } else {
            object::call(f, &[])
        }
    }
}

#[inline]
fn mule_missing() -> Value {
    unsafe { crate::arguments::mule_binding_missing() }
}

#[inline]
fn mule_int(v: i64) -> Value {
    unsafe { crate::numeric::integer_from_i64(v) }
}

#[inline]
fn mule_float(v: f64) -> Value {
    unsafe { crate::numeric::mule_literal_float(v) }
}

#[inline]
fn mule_string(ptr: *const u8, len: usize) -> Value {
    if ptr.is_null() || len == 0 {
        object::empty_text()
    } else {
        unsafe { object::text_value(std::slice::from_raw_parts(ptr, len)) }
    }
}

#[inline]
fn mule_bytes(ptr: *const u8, len: usize) -> Value {
    let slice = if ptr.is_null() || len == 0 {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(ptr, len) }
    };
    unsafe { object::new_value(Data::Bytes(slice.to_vec(), ptr::null_mut(), ptr::null_mut())) }
}

#[inline]
fn mule_tuple(ptr: *const Value, len: usize) -> Value {
    unsafe { object::mule_sequence_tuple(len as isize, ptr) }
}

#[inline]
fn mule_list(ptr: *const Value, len: usize) -> Value {
    unsafe { object::mule_sequence_list(len as isize, ptr) }
}

#[inline]
fn mule_list_push(lst: Value, item: Value) {
    unsafe {
        object::list_append(lst, item);
    }
}

#[inline]
fn mule_raise_value(val: Value) {
    unsafe {
        crate::sequences::mule_raise_pending(val);
    }
}

#[inline]
fn current_exception() -> Value {
    unsafe { crate::sequences::mule_catch_exception() }
}

#[inline]
fn take_pending_exception() -> Value {
    unsafe { crate::sequences::mule_catch_exception() }
}

#[inline]
fn string_value(val: Value) -> String {
    if val.is_null() || object::is_fixnum(val) {
        return String::new();
    }
    unsafe {
        match &(*val).data {
            Data::Text(t, ..) => t.to_rust_string().unwrap_or_default(),
            _ => String::new(),
        }
    }
}

#[inline]
fn int_value(val: Value) -> i64 {
    if val.is_null() {
        return 0;
    }
    if object::is_fixnum(val) {
        return unsafe { object::fixnum(val) };
    }
    unsafe {
        match &(*val).data {
            Data::Bool(b) => i64::from(*b),
            Data::BigInt(bi, ..) => bi.to_i64().unwrap_or(-1),
            _ => 0,
        }
    }
}

#[inline]
fn is_truthy(val: Value) -> bool {
    unsafe { object::truth(val).unwrap_or(false) }
}

#[inline]
fn ref_count(val: Value) -> usize {
    if val.is_null() || object::is_fixnum(val) {
        1
    } else {
        unsafe { (*val).references.load(Ordering::Relaxed) }
    }
}

#[inline]
fn protocol_lookup(name: &str) -> Value {
    unsafe {
        let g = mule_protocol_globals();
        if !g.is_null() {
            let v = object::lookup(g, name);
            if !v.is_null() {
                return v;
            }
        }
        let b = crate::builtins::namespace();
        if !b.is_null() {
            object::lookup(b, name)
        } else {
            ptr::null_mut()
        }
    }
}

#[inline]
fn get_attr_str(receiver: Value, name: &str) -> Value {
    let Ok(c) = CString::new(name) else {
        return ptr::null_mut();
    };
    unsafe { classes::mule_get_attr(receiver, c.as_ptr()) }
}

#[inline]
fn set_attr_str(receiver: Value, name: &str, val: Value) -> i32 {
    let Ok(c) = CString::new(name) else {
        return -1;
    };
    unsafe { classes::mule_set_attr(receiver, c.as_ptr(), val) }
}

#[inline]
fn del_attr_str(receiver: Value, name: &str) -> Value {
    unsafe { classes::delete_attribute_with_pub(receiver, name, true) }
}

#[inline]
fn call_with_keywords(callable: Value, pos: &[Value], kws: &[(&str, Value)]) -> Value {
    let mut c_names: Vec<CString> = Vec::with_capacity(kws.len());
    let mut kw_vals: Vec<Value> = Vec::with_capacity(kws.len());
    for &(k, v) in kws {
        let Ok(ck) = CString::new(k) else {
            return ptr::null_mut();
        };
        c_names.push(ck);
        kw_vals.push(v);
    }
    let ptrs: Vec<*const c_char> = c_names.iter().map(|c| c.as_ptr()).collect();
    unsafe {
        crate::arguments::mule_call_keywords(
            callable,
            ptr::null_mut(),
            pos.len() as isize,
            pos.as_ptr(),
            kw_vals.len() as isize,
            ptrs.as_ptr(),
            kw_vals.as_ptr(),
        )
    }
}

pub type Py_ssize_t = isize;
pub type Py_hash_t = isize;

#[repr(C)]
pub struct PyObject {
    pub ob_refcnt: Py_ssize_t,
    pub ob_type: *mut PyTypeObject,
    pub ob_mule: Value,
}
unsafe impl Send for PyObject {}
unsafe impl Sync for PyObject {}

#[repr(C)]
pub struct PyVarObject {
    pub ob_base: PyObject,
    pub ob_size: Py_ssize_t,
}
unsafe impl Send for PyVarObject {}
unsafe impl Sync for PyVarObject {}

pub type PyCFunction = Option<unsafe extern "C" fn(*mut PyObject, *mut PyObject) -> *mut PyObject>;
pub type PyCFunctionWithKeywords = Option<
    unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> *mut PyObject,
>;
pub type PyCFunctionFast = Option<
    unsafe extern "C" fn(*mut PyObject, *const *mut PyObject, Py_ssize_t) -> *mut PyObject,
>;
pub type PyCFunctionFastWithKeywords = Option<
    unsafe extern "C" fn(
        *mut PyObject,
        *const *mut PyObject,
        Py_ssize_t,
        *mut PyObject,
    ) -> *mut PyObject,
>;

pub type inquiry = Option<unsafe extern "C" fn(*mut PyObject) -> c_int>;
pub type unaryfunc = Option<unsafe extern "C" fn(*mut PyObject) -> *mut PyObject>;
pub type binaryfunc = Option<unsafe extern "C" fn(*mut PyObject, *mut PyObject) -> *mut PyObject>;
pub type ternaryfunc = Option<
    unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> *mut PyObject,
>;
pub type lenfunc = Option<unsafe extern "C" fn(*mut PyObject) -> Py_ssize_t>;
pub type ssizeargfunc = Option<unsafe extern "C" fn(*mut PyObject, Py_ssize_t) -> *mut PyObject>;
pub type ssizeobjargproc =
    Option<unsafe extern "C" fn(*mut PyObject, Py_ssize_t, *mut PyObject) -> c_int>;
pub type objobjargproc =
    Option<unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> c_int>;
pub type objobjproc = Option<unsafe extern "C" fn(*mut PyObject, *mut PyObject) -> c_int>;
pub type visitproc = Option<unsafe extern "C" fn(*mut PyObject, *mut c_void) -> c_int>;
pub type traverseproc =
    Option<unsafe extern "C" fn(*mut PyObject, visitproc, *mut c_void) -> c_int>;
pub type freefunc = Option<unsafe extern "C" fn(*mut c_void)>;
pub type destructor = Option<unsafe extern "C" fn(*mut PyObject)>;
pub type getattrfunc = Option<unsafe extern "C" fn(*mut PyObject, *mut c_char) -> *mut PyObject>;
pub type getattrofunc =
    Option<unsafe extern "C" fn(*mut PyObject, *mut PyObject) -> *mut PyObject>;
pub type setattrfunc =
    Option<unsafe extern "C" fn(*mut PyObject, *mut c_char, *mut PyObject) -> c_int>;
pub type setattrofunc =
    Option<unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> c_int>;
pub type reprfunc = Option<unsafe extern "C" fn(*mut PyObject) -> *mut PyObject>;
pub type hashfunc = Option<unsafe extern "C" fn(*mut PyObject) -> Py_hash_t>;
pub type richcmpfunc =
    Option<unsafe extern "C" fn(*mut PyObject, *mut PyObject, c_int) -> *mut PyObject>;
pub type getiterfunc = Option<unsafe extern "C" fn(*mut PyObject) -> *mut PyObject>;
pub type iternextfunc = Option<unsafe extern "C" fn(*mut PyObject) -> *mut PyObject>;
pub type descrgetfunc = Option<
    unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> *mut PyObject,
>;
pub type descrsetfunc =
    Option<unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> c_int>;
pub type initproc =
    Option<unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> c_int>;
pub type newfunc = Option<
    unsafe extern "C" fn(*mut PyTypeObject, *mut PyObject, *mut PyObject) -> *mut PyObject,
>;
pub type allocfunc = Option<unsafe extern "C" fn(*mut PyTypeObject, Py_ssize_t) -> *mut PyObject>;
pub type vectorcallfunc = Option<
    unsafe extern "C" fn(
        *mut PyObject,
        *const *mut PyObject,
        usize,
        *mut PyObject,
    ) -> *mut PyObject,
>;
pub type getter = Option<unsafe extern "C" fn(*mut PyObject, *mut c_void) -> *mut PyObject>;
pub type setter =
    Option<unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut c_void) -> c_int>;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct PyMethodDef {
    pub ml_name: *const c_char,
    pub ml_meth: PyCFunction,
    pub ml_flags: c_int,
    pub ml_doc: *const c_char,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct PyMemberDef {
    pub name: *const c_char,
    pub r#type: c_int,
    pub offset: Py_ssize_t,
    pub flags: c_int,
    pub doc: *const c_char,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct PyGetSetDef {
    pub name: *const c_char,
    pub get: getter,
    pub set: setter,
    pub doc: *const c_char,
    pub closure: *mut c_void,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct PyNumberMethods {
    pub nb_add: binaryfunc,
    pub nb_subtract: binaryfunc,
    pub nb_multiply: binaryfunc,
    pub nb_remainder: binaryfunc,
    pub nb_divmod: binaryfunc,
    pub nb_power: ternaryfunc,
    pub nb_negative: unaryfunc,
    pub nb_positive: unaryfunc,
    pub nb_absolute: unaryfunc,
    pub nb_bool: inquiry,
    pub nb_invert: unaryfunc,
    pub nb_lshift: binaryfunc,
    pub nb_rshift: binaryfunc,
    pub nb_and: binaryfunc,
    pub nb_xor: binaryfunc,
    pub nb_or: binaryfunc,
    pub nb_int: unaryfunc,
    pub nb_reserved: *mut c_void,
    pub nb_float: unaryfunc,
    pub nb_inplace_add: binaryfunc,
    pub nb_inplace_subtract: binaryfunc,
    pub nb_inplace_multiply: binaryfunc,
    pub nb_inplace_remainder: binaryfunc,
    pub nb_inplace_power: ternaryfunc,
    pub nb_inplace_lshift: binaryfunc,
    pub nb_inplace_rshift: binaryfunc,
    pub nb_inplace_and: binaryfunc,
    pub nb_inplace_xor: binaryfunc,
    pub nb_inplace_or: binaryfunc,
    pub nb_floor_divide: binaryfunc,
    pub nb_true_divide: binaryfunc,
    pub nb_inplace_floor_divide: binaryfunc,
    pub nb_inplace_true_divide: binaryfunc,
    pub nb_index: unaryfunc,
    pub nb_matrix_multiply: binaryfunc,
    pub nb_inplace_matrix_multiply: binaryfunc,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct PySequenceMethods {
    pub sq_length: lenfunc,
    pub sq_concat: binaryfunc,
    pub sq_repeat: ssizeargfunc,
    pub sq_item: ssizeargfunc,
    pub was_sq_slice: *mut c_void,
    pub sq_ass_item: ssizeobjargproc,
    pub was_sq_ass_slice: *mut c_void,
    pub sq_contains: objobjproc,
    pub sq_inplace_concat: binaryfunc,
    pub sq_inplace_repeat: ssizeargfunc,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct PyMappingMethods {
    pub mp_length: lenfunc,
    pub mp_subscript: binaryfunc,
    pub mp_ass_subscript: objobjargproc,
}

#[repr(C)]
pub struct PyTypeObject {
    pub ob_base: PyVarObject,
    pub tp_name: *const c_char,
    pub tp_basicsize: Py_ssize_t,
    pub tp_itemsize: Py_ssize_t,
    pub tp_dealloc: destructor,
    pub tp_vectorcall_offset: Py_ssize_t,
    pub tp_getattr: getattrfunc,
    pub tp_setattr: setattrfunc,
    pub tp_as_async: *mut c_void,
    pub tp_repr: reprfunc,
    pub tp_as_number: *mut PyNumberMethods,
    pub tp_as_sequence: *mut PySequenceMethods,
    pub tp_as_mapping: *mut PyMappingMethods,
    pub tp_hash: hashfunc,
    pub tp_call: ternaryfunc,
    pub tp_str: reprfunc,
    pub tp_getattro: getattrofunc,
    pub tp_setattro: setattrofunc,
    pub tp_as_buffer: *mut c_void,
    pub tp_flags: c_ulong,
    pub tp_doc: *const c_char,
    pub tp_traverse: traverseproc,
    pub tp_clear: inquiry,
    pub tp_richcompare: richcmpfunc,
    pub tp_weaklistoffset: Py_ssize_t,
    pub tp_iter: getiterfunc,
    pub tp_iternext: iternextfunc,
    pub tp_methods: *mut PyMethodDef,
    pub tp_members: *mut PyMemberDef,
    pub tp_getset: *mut PyGetSetDef,
    pub tp_base: *mut PyTypeObject,
    pub tp_dict: *mut PyObject,
    pub tp_descr_get: descrgetfunc,
    pub tp_descr_set: descrsetfunc,
    pub tp_dictoffset: Py_ssize_t,
    pub tp_init: initproc,
    pub tp_alloc: allocfunc,
    pub tp_new: newfunc,
    pub tp_free: freefunc,
    pub tp_is_gc: inquiry,
    pub tp_bases: *mut PyObject,
    pub tp_mro: *mut PyObject,
    pub tp_cache: *mut PyObject,
    pub tp_subclasses: *mut c_void,
    pub tp_weaklist: *mut PyObject,
    pub tp_del: destructor,
    pub tp_version_tag: c_uint,
    pub tp_finalize: destructor,
    pub tp_vectorcall: vectorcallfunc,
    pub tp_watched: c_uchar,
    pub tp_versions_used: c_ushort,
}
unsafe impl Send for PyTypeObject {}
unsafe impl Sync for PyTypeObject {}

#[repr(C)]
pub struct PyType_Slot {
    pub slot: c_int,
    pub pfunc: *mut c_void,
}

#[repr(C)]
pub struct PyType_Spec {
    pub name: *const c_char,
    pub basicsize: c_int,
    pub itemsize: c_int,
    pub flags: c_uint,
    pub slots: *mut PyType_Slot,
}

#[repr(C)]
pub struct PyModuleDef_Base {
    pub ob_base: PyObject,
    pub m_init: Option<unsafe extern "C" fn() -> *mut PyObject>,
    pub m_index: Py_ssize_t,
    pub m_copy: *mut PyObject,
}

#[repr(C)]
pub struct PyModuleDef_Slot {
    pub slot: c_int,
    pub value: *mut c_void,
}

#[repr(C)]
pub struct PyModuleDef {
    pub m_base: PyModuleDef_Base,
    pub m_name: *const c_char,
    pub m_doc: *const c_char,
    pub m_size: Py_ssize_t,
    pub m_methods: *mut PyMethodDef,
    pub m_slots: *mut PyModuleDef_Slot,
    pub m_traverse: traverseproc,
    pub m_clear: inquiry,
    pub m_free: freefunc,
}

pub const METH_VARARGS: c_int = 0x0001;
pub const METH_KEYWORDS: c_int = 0x0002;
pub const METH_NOARGS: c_int = 0x0004;
pub const METH_O: c_int = 0x0008;
pub const METH_CLASS: c_int = 0x0010;
pub const METH_STATIC: c_int = 0x0020;
pub const METH_COEXIST: c_int = 0x0040;
pub const METH_FASTCALL: c_int = 0x0080;
pub const METH_METHOD: c_int = 0x0200;

pub const Py_LT: c_int = 0;
pub const Py_LE: c_int = 1;
pub const Py_EQ: c_int = 2;
pub const Py_NE: c_int = 3;
pub const Py_GT: c_int = 4;
pub const Py_GE: c_int = 5;

pub const Py_TPFLAGS_HEAPTYPE: c_ulong = 1 << 9;
pub const Py_TPFLAGS_BASETYPE: c_ulong = 1 << 10;
pub const Py_TPFLAGS_READY: c_ulong = 1 << 12;
pub const Py_TPFLAGS_HAVE_GC: c_ulong = 1 << 14;
pub const Py_TPFLAGS_LONG_SUBCLASS: c_ulong = 1 << 24;
pub const Py_TPFLAGS_LIST_SUBCLASS: c_ulong = 1 << 25;
pub const Py_TPFLAGS_TUPLE_SUBCLASS: c_ulong = 1 << 26;
pub const Py_TPFLAGS_BYTES_SUBCLASS: c_ulong = 1 << 27;
pub const Py_TPFLAGS_UNICODE_SUBCLASS: c_ulong = 1 << 28;
pub const Py_TPFLAGS_DICT_SUBCLASS: c_ulong = 1 << 29;
pub const Py_TPFLAGS_BASE_EXC_SUBCLASS: c_ulong = 1 << 30;
pub const Py_TPFLAGS_TYPE_SUBCLASS: c_ulong = 1 << 31;

pub const Py_mod_create: c_int = 1;
pub const Py_mod_exec: c_int = 2;

pub const Py_bf_getbuffer: c_int = 1;
pub const Py_bf_releasebuffer: c_int = 2;
pub const Py_mp_ass_subscript: c_int = 3;
pub const Py_mp_length: c_int = 4;
pub const Py_mp_subscript: c_int = 5;
pub const Py_nb_absolute: c_int = 6;
pub const Py_nb_add: c_int = 7;
pub const Py_nb_and: c_int = 8;
pub const Py_nb_bool: c_int = 9;
pub const Py_nb_divmod: c_int = 10;
pub const Py_nb_float: c_int = 11;
pub const Py_nb_floor_divide: c_int = 12;
pub const Py_nb_index: c_int = 13;
pub const Py_nb_inplace_add: c_int = 14;
pub const Py_nb_inplace_and: c_int = 15;
pub const Py_nb_inplace_floor_divide: c_int = 16;
pub const Py_nb_inplace_lshift: c_int = 17;
pub const Py_nb_inplace_multiply: c_int = 18;
pub const Py_nb_inplace_or: c_int = 19;
pub const Py_nb_inplace_power: c_int = 20;
pub const Py_nb_inplace_remainder: c_int = 21;
pub const Py_nb_inplace_rshift: c_int = 22;
pub const Py_nb_inplace_subtract: c_int = 23;
pub const Py_nb_inplace_true_divide: c_int = 24;
pub const Py_nb_inplace_xor: c_int = 25;
pub const Py_nb_int: c_int = 26;
pub const Py_nb_invert: c_int = 27;
pub const Py_nb_lshift: c_int = 28;
pub const Py_nb_multiply: c_int = 29;
pub const Py_nb_negative: c_int = 30;
pub const Py_nb_or: c_int = 31;
pub const Py_nb_positive: c_int = 32;
pub const Py_nb_power: c_int = 33;
pub const Py_nb_remainder: c_int = 34;
pub const Py_nb_rshift: c_int = 35;
pub const Py_nb_subtract: c_int = 36;
pub const Py_nb_true_divide: c_int = 37;
pub const Py_nb_xor: c_int = 38;
pub const Py_sq_ass_item: c_int = 39;
pub const Py_sq_concat: c_int = 40;
pub const Py_sq_contains: c_int = 41;
pub const Py_sq_inplace_concat: c_int = 42;
pub const Py_sq_inplace_repeat: c_int = 43;
pub const Py_sq_item: c_int = 44;
pub const Py_sq_length: c_int = 45;
pub const Py_sq_repeat: c_int = 46;
pub const Py_tp_alloc: c_int = 47;
pub const Py_tp_base: c_int = 48;
pub const Py_tp_bases: c_int = 49;
pub const Py_tp_call: c_int = 50;
pub const Py_tp_clear: c_int = 51;
pub const Py_tp_dealloc: c_int = 52;
pub const Py_tp_del: c_int = 53;
pub const Py_tp_descr_get: c_int = 54;
pub const Py_tp_descr_set: c_int = 55;
pub const Py_tp_doc: c_int = 56;
pub const Py_tp_getattr: c_int = 57;
pub const Py_tp_getattro: c_int = 58;
pub const Py_tp_hash: c_int = 59;
pub const Py_tp_init: c_int = 60;
pub const Py_tp_is_gc: c_int = 61;
pub const Py_tp_iter: c_int = 62;
pub const Py_tp_iternext: c_int = 63;
pub const Py_tp_methods: c_int = 64;
pub const Py_tp_new: c_int = 65;
pub const Py_tp_repr: c_int = 66;
pub const Py_tp_richcompare: c_int = 67;
pub const Py_tp_setattr: c_int = 68;
pub const Py_tp_setattro: c_int = 69;
pub const Py_tp_str: c_int = 70;
pub const Py_tp_traverse: c_int = 71;
pub const Py_tp_members: c_int = 72;
pub const Py_tp_getset: c_int = 73;
pub const Py_tp_free: c_int = 74;
pub const Py_nb_matrix_multiply: c_int = 75;
pub const Py_nb_inplace_matrix_multiply: c_int = 76;
pub const Py_am_await: c_int = 77;
pub const Py_am_aiter: c_int = 78;
pub const Py_am_anext: c_int = 79;
pub const Py_tp_finalize: c_int = 80;
pub const Py_am_send: c_int = 81;
pub const Py_tp_token: c_int = 82;

pub const Py_T_SHORT: c_int = 0;
pub const Py_T_INT: c_int = 1;
pub const Py_T_LONG: c_int = 2;
pub const Py_T_FLOAT: c_int = 3;
pub const Py_T_DOUBLE: c_int = 4;
pub const Py_T_STRING: c_int = 5;
pub const _Py_T_OBJECT: c_int = 6;
pub const Py_T_CHAR: c_int = 7;
pub const Py_T_BYTE: c_int = 8;
pub const Py_T_UBYTE: c_int = 9;
pub const Py_T_USHORT: c_int = 10;
pub const Py_T_UINT: c_int = 11;
pub const Py_T_ULONG: c_int = 12;
pub const Py_T_STRING_INPLACE: c_int = 13;
pub const Py_T_BOOL: c_int = 14;
pub const Py_T_OBJECT_EX: c_int = 16;
pub const Py_T_LONGLONG: c_int = 17;
pub const Py_T_ULONGLONG: c_int = 18;
pub const Py_T_PYSSIZET: c_int = 19;
pub const Py_READONLY: c_int = 1;

const fn blank_type(name: &'static [u8], flags: c_ulong) -> PyTypeObject {
    PyTypeObject {
        ob_base: PyVarObject {
            ob_base: PyObject {
                ob_refcnt: isize::MAX / 2,
                ob_type: ptr::null_mut(),
                ob_mule: ptr::null_mut(),
            },
            ob_size: 0,
        },
        tp_name: name.as_ptr() as *const c_char,
        tp_basicsize: std::mem::size_of::<PyObject>() as Py_ssize_t,
        tp_itemsize: 0,
        tp_dealloc: None,
        tp_vectorcall_offset: 0,
        tp_getattr: None,
        tp_setattr: None,
        tp_as_async: ptr::null_mut(),
        tp_repr: None,
        tp_as_number: ptr::null_mut(),
        tp_as_sequence: ptr::null_mut(),
        tp_as_mapping: ptr::null_mut(),
        tp_hash: None,
        tp_call: None,
        tp_str: None,
        tp_getattro: Some(PyObject_GenericGetAttr),
        tp_setattro: Some(PyObject_GenericSetAttr),
        tp_as_buffer: ptr::null_mut(),
        tp_flags: flags | Py_TPFLAGS_READY | Py_TPFLAGS_BASETYPE,
        tp_doc: ptr::null(),
        tp_traverse: None,
        tp_clear: None,
        tp_richcompare: None,
        tp_weaklistoffset: 0,
        tp_iter: None,
        tp_iternext: None,
        tp_methods: ptr::null_mut(),
        tp_members: ptr::null_mut(),
        tp_getset: ptr::null_mut(),
        tp_base: ptr::null_mut(),
        tp_dict: ptr::null_mut(),
        tp_descr_get: None,
        tp_descr_set: None,
        tp_dictoffset: 0,
        tp_init: None,
        tp_alloc: Some(PyType_GenericAlloc),
        tp_new: Some(PyType_GenericNew),
        tp_free: Some(PyObject_GC_Del),
        tp_is_gc: None,
        tp_bases: ptr::null_mut(),
        tp_mro: ptr::null_mut(),
        tp_cache: ptr::null_mut(),
        tp_subclasses: ptr::null_mut(),
        tp_weaklist: ptr::null_mut(),
        tp_del: None,
        tp_version_tag: 0,
        tp_finalize: None,
        tp_vectorcall: None,
        tp_watched: 0,
        tp_versions_used: 0,
    }
}

#[unsafe(no_mangle)]
pub static mut PyBaseObject_Type: PyTypeObject = blank_type(b"object\0", 0);
#[unsafe(no_mangle)]
pub static mut PyType_Type: PyTypeObject = blank_type(b"type\0", Py_TPFLAGS_TYPE_SUBCLASS);
#[unsafe(no_mangle)]
pub static mut PyLong_Type: PyTypeObject = blank_type(b"int\0", Py_TPFLAGS_LONG_SUBCLASS);
#[unsafe(no_mangle)]
pub static mut PyBool_Type: PyTypeObject = blank_type(b"bool\0", Py_TPFLAGS_LONG_SUBCLASS);
#[unsafe(no_mangle)]
pub static mut PyFloat_Type: PyTypeObject = blank_type(b"float\0", 0);
#[unsafe(no_mangle)]
pub static mut PyUnicode_Type: PyTypeObject = blank_type(b"str\0", Py_TPFLAGS_UNICODE_SUBCLASS);
#[unsafe(no_mangle)]
pub static mut PyBytes_Type: PyTypeObject = blank_type(b"bytes\0", Py_TPFLAGS_BYTES_SUBCLASS);
#[unsafe(no_mangle)]
pub static mut PyByteArray_Type: PyTypeObject = blank_type(b"bytearray\0", 0);
#[unsafe(no_mangle)]
pub static mut PyTuple_Type: PyTypeObject = blank_type(b"tuple\0", Py_TPFLAGS_TUPLE_SUBCLASS);
#[unsafe(no_mangle)]
pub static mut PyList_Type: PyTypeObject = blank_type(b"list\0", Py_TPFLAGS_LIST_SUBCLASS);
#[unsafe(no_mangle)]
pub static mut PyDict_Type: PyTypeObject = blank_type(b"dict\0", Py_TPFLAGS_DICT_SUBCLASS);
#[unsafe(no_mangle)]
pub static mut PySet_Type: PyTypeObject = blank_type(b"set\0", 0);
#[unsafe(no_mangle)]
pub static mut PyFrozenSet_Type: PyTypeObject = blank_type(b"frozenset\0", 0);
#[unsafe(no_mangle)]
pub static mut PyFunction_Type: PyTypeObject = blank_type(b"function\0", 0);
#[unsafe(no_mangle)]
pub static mut PyMethod_Type: PyTypeObject = blank_type(b"method\0", 0);
#[unsafe(no_mangle)]
pub static mut PyCFunction_Type: PyTypeObject =
    blank_type(b"builtin_function_or_method\0", 0);
#[unsafe(no_mangle)]
pub static mut PyModule_Type: PyTypeObject = blank_type(b"module\0", 0);
#[unsafe(no_mangle)]
pub static mut PyCapsule_Type: PyTypeObject = blank_type(b"PyCapsule\0", 0);
#[unsafe(no_mangle)]
pub static mut PySlice_Type: PyTypeObject = blank_type(b"slice\0", 0);
#[unsafe(no_mangle)]
pub static mut PyProperty_Type: PyTypeObject = blank_type(b"property\0", 0);
#[unsafe(no_mangle)]
pub static mut PyClassMethod_Type: PyTypeObject = blank_type(b"classmethod\0", 0);
#[unsafe(no_mangle)]
pub static mut PyStaticMethod_Type: PyTypeObject = blank_type(b"staticmethod\0", 0);
#[unsafe(no_mangle)]
pub static mut PySeqIter_Type: PyTypeObject = blank_type(b"iterator\0", 0);
#[unsafe(no_mangle)]
pub static mut PyComplex_Type: PyTypeObject = blank_type(b"complex\0", 0);
#[unsafe(no_mangle)]
pub static mut PyReversed_Type: PyTypeObject = blank_type(b"reversed\0", 0);

#[unsafe(no_mangle)]
pub static mut _Py_NoneStruct: PyObject = PyObject {
    ob_refcnt: isize::MAX / 2,
    ob_type: ptr::null_mut(),
    ob_mule: ptr::null_mut(),
};
#[unsafe(no_mangle)]
pub static mut _Py_TrueStruct: PyVarObject = PyVarObject {
    ob_base: PyObject {
        ob_refcnt: isize::MAX / 2,
        ob_type: ptr::null_mut(),
        ob_mule: ptr::null_mut(),
    },
    ob_size: 1,
};
#[unsafe(no_mangle)]
pub static mut _Py_FalseStruct: PyVarObject = PyVarObject {
    ob_base: PyObject {
        ob_refcnt: isize::MAX / 2,
        ob_type: ptr::null_mut(),
        ob_mule: ptr::null_mut(),
    },
    ob_size: 0,
};
#[unsafe(no_mangle)]
pub static mut _Py_NotImplementedStruct: PyObject = PyObject {
    ob_refcnt: isize::MAX / 2,
    ob_type: ptr::null_mut(),
    ob_mule: ptr::null_mut(),
};
#[unsafe(no_mangle)]
pub static mut _Py_EllipsisObject: PyObject = PyObject {
    ob_refcnt: isize::MAX / 2,
    ob_type: ptr::null_mut(),
    ob_mule: ptr::null_mut(),
};

#[unsafe(no_mangle)]
pub static mut PyExc_BaseException: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_Exception: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_TypeError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_ValueError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_AttributeError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_KeyError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_IndexError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_RuntimeError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_NotImplementedError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_StopIteration: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_ImportError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_ModuleNotFoundError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_MemoryError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_OverflowError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_ZeroDivisionError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_NameError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_UnboundLocalError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_OSError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_SystemError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_AssertionError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_LookupError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_ArithmeticError: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_DeprecationWarning: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_RuntimeWarning: *mut PyObject = ptr::null_mut();
#[unsafe(no_mangle)]
pub static mut PyExc_UserWarning: *mut PyObject = ptr::null_mut();

struct CapiState {
    initialized: bool,
    val_to_py: HashMap<usize, *mut PyObject>,
    c_instances: HashMap<usize, *mut PyObject>,
    utf8_cache: HashMap<usize, CString>,
    module_defs: HashMap<usize, (*mut PyModuleDef, *mut c_void)>,
    capsules: HashMap<usize, CapsuleData>,
    c_callable_meta: HashMap<usize, CCallableMeta>,
    type_modules: HashMap<usize, usize>,
}

struct CapsuleData {
    pointer: *mut c_void,
    name: *const c_char,
    context: *mut c_void,
    #[allow(dead_code)]
    destructor: Option<unsafe extern "C" fn(*mut PyObject)>,
}

#[derive(Clone)]
enum CCallableMeta {
    MethodDef {
        ml_meth: PyCFunction,
        ml_flags: c_int,
        module_self: *mut PyObject,
        #[allow(dead_code)]
        defining_class: *mut PyTypeObject,
    },
    TpNew {
        tp_new:
            unsafe extern "C" fn(*mut PyTypeObject, *mut PyObject, *mut PyObject) -> *mut PyObject,
        default_tp: *mut PyTypeObject,
    },
    TpInit {
        tp_init: unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> c_int,
    },
    TpGetattro {
        tp_getattro: unsafe extern "C" fn(*mut PyObject, *mut PyObject) -> *mut PyObject,
    },
    TpSetattro {
        tp_setattro: unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> c_int,
        is_del: bool,
    },
    TpCall {
        tp_call:
            unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> *mut PyObject,
    },
    TpDescrGet {
        tp_descr_get:
            unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> *mut PyObject,
    },
    TpDescrSet {
        tp_descr_set: unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> c_int,
        is_del: bool,
    },
    TpRepr {
        func: unsafe extern "C" fn(*mut PyObject) -> *mut PyObject,
    },
    TpHash {
        func: unsafe extern "C" fn(*mut PyObject) -> Py_hash_t,
    },
    TpIter {
        func: unsafe extern "C" fn(*mut PyObject) -> *mut PyObject,
    },
    TpIternext {
        func: unsafe extern "C" fn(*mut PyObject) -> *mut PyObject,
    },
    TpRichCompare {
        func: unsafe extern "C" fn(*mut PyObject, *mut PyObject, c_int) -> *mut PyObject,
        op: c_int,
    },
    UnaryOp {
        func: unsafe extern "C" fn(*mut PyObject) -> *mut PyObject,
    },
    InquiryBool {
        func: unsafe extern "C" fn(*mut PyObject) -> c_int,
    },
    LenOp {
        func: unsafe extern "C" fn(*mut PyObject) -> Py_ssize_t,
    },
    BinaryOp {
        func: unsafe extern "C" fn(*mut PyObject, *mut PyObject) -> *mut PyObject,
        swapped: bool,
    },
    TernaryPower {
        func: unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> *mut PyObject,
        swapped: bool,
    },
    MpAssSubscript {
        func: unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> c_int,
        is_del: bool,
    },
    SqContains {
        func: unsafe extern "C" fn(*mut PyObject, *mut PyObject) -> c_int,
    },
    SqRepeat {
        func: unsafe extern "C" fn(*mut PyObject, Py_ssize_t) -> *mut PyObject,
    },
    GetSetGetter {
        get: unsafe extern "C" fn(*mut PyObject, *mut c_void) -> *mut PyObject,
        closure: *mut c_void,
    },
    GetSetSetter {
        set: unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut c_void) -> c_int,
        closure: *mut c_void,
        is_del: bool,
    },
    MemberGetter {
        member: PyMemberDef,
    },
    MemberSetter {
        member: PyMemberDef,
        is_del: bool,
    },
}

unsafe impl Send for CapiState {}

static STATE: Mutex<Option<CapiState>> = Mutex::new(None);
static NEXT_CALLABLE_ID: AtomicUsize = AtomicUsize::new(1);

fn with_state<R>(f: impl FnOnce(&mut CapiState) -> R) -> R {
    let mut guard = STATE.lock().unwrap_or_else(|e| e.into_inner());
    let state = guard.get_or_insert_with(|| CapiState {
        initialized: false,
        val_to_py: HashMap::new(),
        c_instances: HashMap::new(),
        utf8_cache: HashMap::new(),
        module_defs: HashMap::new(),
        capsules: HashMap::new(),
        c_callable_meta: HashMap::new(),
        type_modules: HashMap::new(),
    });
    f(state)
}

unsafe extern "C" fn generic_tp_descr_get(
    descr: *mut PyObject,
    obj: *mut PyObject,
    owner: *mut PyObject,
) -> *mut PyObject {
    unsafe {
        let descr_val = from_py_borrowed(descr);
        let obj_val = if obj.is_null() {
            mule_none()
        } else {
            from_py_borrowed(obj)
        };
        let owner_val = if owner.is_null() {
            mule_none()
        } else {
            from_py_borrowed(owner)
        };
        let res = object::special(descr_val, "__get__", &[obj_val, owner_val]);
        if has_pending_exception() {
            return ptr::null_mut();
        }
        if res.is_null() {
            Py_IncRef(descr);
            return descr;
        }
        to_py_owned(res)
    }
}

unsafe extern "C" fn generic_tp_descr_set(
    descr: *mut PyObject,
    obj: *mut PyObject,
    value: *mut PyObject,
) -> c_int {
    unsafe {
        let descr_val = from_py_borrowed(descr);
        let obj_val = from_py_borrowed(obj);
        let res = if value.is_null() {
            object::special(descr_val, "__delete__", &[obj_val])
        } else {
            let val = from_py_borrowed(value);
            object::special(descr_val, "__set__", &[obj_val, val])
        };
        if res.is_null() || has_pending_exception() {
            return -1;
        }
        object::release(res);
        0
    }
}

unsafe extern "C" fn generic_tp_call(
    callable: *mut PyObject,
    args: *mut PyObject,
    kwargs: *mut PyObject,
) -> *mut PyObject {
    unsafe { PyObject_Call(callable, args, kwargs) }
}

fn ensure_initialized() {
    let already = with_state(|s| s.initialized);
    if already {
        return;
    }
    let globals = mule_protocol_globals();
    if globals.is_null() {
        return;
    }
    with_state(|s| s.initialized = true);
    unsafe {
        let init_type = |tp: *mut PyTypeObject, b: Builtin| {
            let cls = classes::builtin_type(b);
            (*tp).ob_base.ob_base.ob_type = &raw mut PyType_Type;
            (*tp).ob_base.ob_base.ob_mule = cls;
            (*tp).tp_base = &raw mut PyBaseObject_Type;
            if !cls.is_null() {
                with_state(|s| {
                    s.val_to_py.insert(cls as usize, tp as *mut PyObject);
                });
            }
        };
        init_type(&raw mut PyBaseObject_Type, Builtin::Object);
        PyBaseObject_Type.tp_base = ptr::null_mut();
        init_type(&raw mut PyType_Type, Builtin::Type);
        init_type(&raw mut PyLong_Type, Builtin::Int);
        init_type(&raw mut PyBool_Type, Builtin::Bool);
        PyBool_Type.tp_base = &raw mut PyLong_Type;
        init_type(&raw mut PyFloat_Type, Builtin::Float);
        init_type(&raw mut PyUnicode_Type, Builtin::Str);
        init_type(&raw mut PyBytes_Type, Builtin::Bytes);
        init_type(&raw mut PyTuple_Type, Builtin::Tuple);
        init_type(&raw mut PyList_Type, Builtin::List);
        init_type(&raw mut PyDict_Type, Builtin::Dict);
        init_type(&raw mut PyFunction_Type, Builtin::Function);
        PyFunction_Type.tp_descr_get = Some(generic_tp_descr_get);
        PyFunction_Type.tp_call = Some(generic_tp_call);
        init_type(&raw mut PyMethod_Type, Builtin::Method);
        PyMethod_Type.tp_call = Some(generic_tp_call);
        init_type(&raw mut PyCFunction_Type, Builtin::BuiltinFunction);
        PyCFunction_Type.tp_call = Some(generic_tp_call);
        init_type(&raw mut PyModule_Type, Builtin::Module);

        let bind_named_type =
            |tp: *mut PyTypeObject, name: &str, descr_get: bool, descr_set: bool| {
                let cls = protocol_lookup(name);
                (*tp).ob_base.ob_base.ob_type = &raw mut PyType_Type;
                (*tp).ob_base.ob_base.ob_mule = cls;
                (*tp).tp_base = &raw mut PyBaseObject_Type;
                if descr_get {
                    (*tp).tp_descr_get = Some(generic_tp_descr_get);
                }
                if descr_set {
                    (*tp).tp_descr_set = Some(generic_tp_descr_set);
                }
                if !cls.is_null() {
                    with_state(|s| {
                        s.val_to_py.insert(cls as usize, tp as *mut PyObject);
                    });
                }
            };
        bind_named_type(&raw mut PyByteArray_Type, "bytearray", false, false);
        bind_named_type(&raw mut PySet_Type, "set", false, false);
        bind_named_type(&raw mut PyFrozenSet_Type, "frozenset", false, false);
        bind_named_type(&raw mut PySlice_Type, "slice", false, false);
        bind_named_type(&raw mut PyProperty_Type, "property", true, true);
        bind_named_type(&raw mut PyClassMethod_Type, "classmethod", true, false);
        bind_named_type(&raw mut PyStaticMethod_Type, "staticmethod", true, false);
        bind_named_type(&raw mut PyComplex_Type, "complex", false, false);
        bind_named_type(&raw mut PyReversed_Type, "reversed", false, false);
        PyCapsule_Type.ob_base.ob_base.ob_type = &raw mut PyType_Type;
        PyCapsule_Type.tp_base = &raw mut PyBaseObject_Type;

        let none_val = mule_none();
        _Py_NoneStruct.ob_mule = none_val;
        _Py_NoneStruct.ob_type = py_type_for_class(classes::builtin_type(Builtin::NoneType));
        let true_val = mule_true();
        _Py_TrueStruct.ob_base.ob_mule = true_val;
        _Py_TrueStruct.ob_base.ob_type = &raw mut PyBool_Type;
        let false_val = mule_false();
        _Py_FalseStruct.ob_base.ob_mule = false_val;
        _Py_FalseStruct.ob_base.ob_type = &raw mut PyBool_Type;
        let ni_val = mule_not_implemented();
        _Py_NotImplementedStruct.ob_mule = ni_val;
        _Py_NotImplementedStruct.ob_type = py_type_for_class(classes::class_of_value(ni_val));
        let ell_val = mule_ellipsis();
        _Py_EllipsisObject.ob_mule = ell_val;
        _Py_EllipsisObject.ob_type = py_type_for_class(classes::class_of_value(ell_val));

        with_state(|s| {
            s.val_to_py
                .insert(none_val as usize, &raw mut _Py_NoneStruct);
            s.val_to_py.insert(
                true_val as usize,
                (&raw mut _Py_TrueStruct) as *mut PyObject,
            );
            s.val_to_py.insert(
                false_val as usize,
                (&raw mut _Py_FalseStruct) as *mut PyObject,
            );
            s.val_to_py
                .insert(ni_val as usize, &raw mut _Py_NotImplementedStruct);
            s.val_to_py
                .insert(ell_val as usize, &raw mut _Py_EllipsisObject);
        });

        let init_exc = |slot: *mut *mut PyObject, name: &str| {
            let v = protocol_lookup(name);
            if !v.is_null() {
                *slot = to_py_borrowed(v);
            }
        };
        init_exc(&raw mut PyExc_BaseException, "BaseException");
        init_exc(&raw mut PyExc_Exception, "Exception");
        init_exc(&raw mut PyExc_TypeError, "TypeError");
        init_exc(&raw mut PyExc_ValueError, "ValueError");
        init_exc(&raw mut PyExc_AttributeError, "AttributeError");
        init_exc(&raw mut PyExc_KeyError, "KeyError");
        init_exc(&raw mut PyExc_IndexError, "IndexError");
        init_exc(&raw mut PyExc_RuntimeError, "RuntimeError");
        init_exc(&raw mut PyExc_NotImplementedError, "NotImplementedError");
        init_exc(&raw mut PyExc_StopIteration, "StopIteration");
        init_exc(&raw mut PyExc_ImportError, "ImportError");
        init_exc(&raw mut PyExc_ModuleNotFoundError, "ModuleNotFoundError");
        init_exc(&raw mut PyExc_MemoryError, "MemoryError");
        init_exc(&raw mut PyExc_OverflowError, "OverflowError");
        init_exc(&raw mut PyExc_ZeroDivisionError, "ZeroDivisionError");
        init_exc(&raw mut PyExc_NameError, "NameError");
        init_exc(&raw mut PyExc_UnboundLocalError, "UnboundLocalError");
        init_exc(&raw mut PyExc_OSError, "OSError");
        init_exc(&raw mut PyExc_SystemError, "SystemError");
        init_exc(&raw mut PyExc_AssertionError, "AssertionError");
        init_exc(&raw mut PyExc_LookupError, "LookupError");
        init_exc(&raw mut PyExc_ArithmeticError, "ArithmeticError");
        init_exc(&raw mut PyExc_DeprecationWarning, "DeprecationWarning");
        init_exc(&raw mut PyExc_RuntimeWarning, "RuntimeWarning");
        init_exc(&raw mut PyExc_UserWarning, "UserWarning");
    }
}

fn py_type_for_class(cls: Value) -> *mut PyTypeObject {
    if cls.is_null() {
        return &raw mut PyBaseObject_Type;
    }
    if let Some(existing) = with_state(|s| s.val_to_py.get(&(cls as usize)).copied()) {
        return existing as *mut PyTypeObject;
    }
    unsafe {
        if cls == classes::builtin_type(Builtin::Object) {
            return &raw mut PyBaseObject_Type;
        }
        if cls == classes::builtin_type(Builtin::Type) {
            return &raw mut PyType_Type;
        }
        if cls == classes::builtin_type(Builtin::Int) {
            return &raw mut PyLong_Type;
        }
        if cls == classes::builtin_type(Builtin::Bool) {
            return &raw mut PyBool_Type;
        }
        if cls == classes::builtin_type(Builtin::Float) {
            return &raw mut PyFloat_Type;
        }
        if cls == classes::builtin_type(Builtin::Str) {
            return &raw mut PyUnicode_Type;
        }
        if cls == classes::builtin_type(Builtin::Bytes) {
            return &raw mut PyBytes_Type;
        }
        if cls == classes::builtin_type(Builtin::Tuple) {
            return &raw mut PyTuple_Type;
        }
        if cls == classes::builtin_type(Builtin::List) {
            return &raw mut PyList_Type;
        }
        if cls == classes::builtin_type(Builtin::Dict) {
            return &raw mut PyDict_Type;
        }
        if cls == classes::builtin_type(Builtin::Function) {
            return &raw mut PyFunction_Type;
        }
        if cls == classes::builtin_type(Builtin::Method) {
            return &raw mut PyMethod_Type;
        }
        if cls == classes::builtin_type(Builtin::BuiltinFunction) {
            return &raw mut PyCFunction_Type;
        }
        if cls == classes::builtin_type(Builtin::Module) {
            return &raw mut PyModule_Type;
        }

        let tp = calloc(1, std::mem::size_of::<PyTypeObject>()) as *mut PyTypeObject;
        (*tp).ob_base.ob_base.ob_refcnt = ref_count(cls).max(1) as Py_ssize_t;
        (*tp).ob_base.ob_base.ob_type = &raw mut PyType_Type;
        (*tp).ob_base.ob_base.ob_mule = cls;
        (*tp).tp_basicsize = std::mem::size_of::<PyObject>() as Py_ssize_t;
        (*tp).tp_getattro = Some(PyObject_GenericGetAttr);
        (*tp).tp_setattro = Some(PyObject_GenericSetAttr);
        (*tp).tp_alloc = Some(PyType_GenericAlloc);
        (*tp).tp_new = Some(PyType_GenericNew);
        (*tp).tp_free = Some(PyObject_GC_Del);
        let mut flags = Py_TPFLAGS_READY | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_HEAPTYPE;
        if classes::is_subclass(cls, classes::builtin_type(Builtin::Int)) {
            flags |= Py_TPFLAGS_LONG_SUBCLASS;
        }
        if classes::is_subclass(cls, classes::builtin_type(Builtin::List)) {
            flags |= Py_TPFLAGS_LIST_SUBCLASS;
        }
        if classes::is_subclass(cls, classes::builtin_type(Builtin::Tuple)) {
            flags |= Py_TPFLAGS_TUPLE_SUBCLASS;
        }
        if classes::is_subclass(cls, classes::builtin_type(Builtin::Bytes)) {
            flags |= Py_TPFLAGS_BYTES_SUBCLASS;
        }
        if classes::is_subclass(cls, classes::builtin_type(Builtin::Str)) {
            flags |= Py_TPFLAGS_UNICODE_SUBCLASS;
        }
        if classes::is_subclass(cls, classes::builtin_type(Builtin::Dict)) {
            flags |= Py_TPFLAGS_DICT_SUBCLASS;
        }
        let base_exc = protocol_lookup("BaseException");
        if !base_exc.is_null() && classes::is_subclass(cls, base_exc) {
            flags |= Py_TPFLAGS_BASE_EXC_SUBCLASS;
        }
        if classes::is_subclass(cls, classes::builtin_type(Builtin::Type)) {
            flags |= Py_TPFLAGS_TYPE_SUBCLASS;
        }
        (*tp).tp_flags = flags;
        (*tp).tp_call = Some(generic_tp_call);

        with_state(|s| {
            s.val_to_py.insert(cls as usize, tp as *mut PyObject);
        });

        let layout = classes::layout_of_class_pub(cls);
        if !layout.is_null() {
            let cname = CString::new((*layout).name.as_str()).unwrap_or_default();
            (*tp).tp_name = cname.into_raw();
            if let Some(&first_base) = (*layout).bases.first() {
                (*tp).tp_base = py_type_for_class(first_base);
            } else {
                (*tp).tp_base = &raw mut PyBaseObject_Type;
            }
            if !classes::special_bound_on_class(cls, "__get__").is_null() {
                (*tp).tp_descr_get = Some(generic_tp_descr_get);
            }
            if !classes::special_bound_on_class(cls, "__set__").is_null()
                || !classes::special_bound_on_class(cls, "__delete__").is_null()
            {
                (*tp).tp_descr_set = Some(generic_tp_descr_set);
            }
            for &ancestor in &(*layout).mro {
                let anc_tp = py_type_for_class(ancestor);
                if !anc_tp.is_null()
                    && ((*anc_tp).tp_basicsize > std::mem::size_of::<PyObject>() as Py_ssize_t
                        || (*anc_tp).tp_dealloc.is_some()
                        || (*anc_tp).tp_dictoffset != 0)
                {
                    (*tp).tp_basicsize = (*anc_tp).tp_basicsize;
                    (*tp).tp_itemsize = (*anc_tp).tp_itemsize;
                    (*tp).tp_dealloc = (*anc_tp).tp_dealloc;
                    (*tp).tp_clear = (*anc_tp).tp_clear;
                    (*tp).tp_traverse = (*anc_tp).tp_traverse;
                    (*tp).tp_dictoffset = (*anc_tp).tp_dictoffset;
                    (*tp).tp_weaklistoffset = (*anc_tp).tp_weaklistoffset;
                    break;
                }
            }
        }
        tp
    }
}

pub fn to_py_borrowed(val: Value) -> *mut PyObject {
    if val.is_null() {
        return ptr::null_mut();
    }
    ensure_initialized();
    if val == mule_none() {
        return &raw mut _Py_NoneStruct;
    }
    if val == mule_true() {
        return (&raw mut _Py_TrueStruct) as *mut PyObject;
    }
    if val == mule_false() {
        return (&raw mut _Py_FalseStruct) as *mut PyObject;
    }
    if val == mule_not_implemented() {
        return &raw mut _Py_NotImplementedStruct;
    }
    if let Some(existing) = with_state(|s| s.val_to_py.get(&(val as usize)).copied()) {
        unsafe {
            if !object::is_fixnum(val) {
                (*existing).ob_refcnt = ref_count(val).max(1) as Py_ssize_t;
            }
        }
        return existing;
    }
    unsafe {
        if !object::is_fixnum(val) && matches!(&(*val).data, Data::Class(_)) {
            return py_type_for_class(val) as *mut PyObject;
        }
        let cls = classes::class_of_value(val);
        let tp = py_type_for_class(cls);
        let is_var = cls == classes::builtin_type(Builtin::Tuple)
            || cls == classes::builtin_type(Builtin::List)
            || cls == classes::builtin_type(Builtin::Bytes);
        let alloc_size = if is_var {
            std::mem::size_of::<PyVarObject>()
        } else {
            std::mem::size_of::<PyObject>()
        };
        let op = calloc(1, alloc_size) as *mut PyObject;
        (*op).ob_refcnt = if object::is_fixnum(val) {
            isize::MAX / 4
        } else {
            ref_count(val).max(1) as Py_ssize_t
        };
        (*op).ob_type = tp;
        (*op).ob_mule = val;
        if is_var && !object::is_fixnum(val) {
            let sz = match &(*val).data {
                Data::Args(items, ..) => items.len() as Py_ssize_t,
                Data::List(items, ..) => items.len() as Py_ssize_t,
                Data::Bytes(items, ..) => items.len() as Py_ssize_t,
                _ => 0,
            };
            (*(op as *mut PyVarObject)).ob_size = sz;
        }
        with_state(|s| {
            s.val_to_py.insert(val as usize, op);
        });
        op
    }
}

pub fn to_py_owned(val: Value) -> *mut PyObject {
    if val.is_null() {
        return ptr::null_mut();
    }
    to_py_borrowed(val)
}

pub fn from_py_borrowed(op: *mut PyObject) -> Value {
    if op.is_null() {
        return ptr::null_mut();
    }
    ensure_initialized();
    unsafe { (*op).ob_mule }
}

pub fn from_py_newref(op: *mut PyObject) -> Value {
    if op.is_null() {
        return ptr::null_mut();
    }
    ensure_initialized();
    unsafe {
        let v = (*op).ob_mule;
        if !v.is_null() {
            object::retain(v);
        }
        v
    }
}

pub fn on_object_destroy(val: Value) {
    if val.is_null() || object::is_fixnum(val) {
        return;
    }
    let (py_opt, c_inst_opt, utf8_opt) = with_state(|s| {
        if !s.initialized {
            return (None, None, None);
        }
        (
            s.val_to_py.remove(&(val as usize)),
            s.c_instances.remove(&(val as usize)),
            s.utf8_cache.remove(&(val as usize)),
        )
    });
    drop(utf8_opt);
    unsafe {
        if let Some(c_op) = c_inst_opt {
            let tp = (*c_op).ob_type;
            if !tp.is_null() {
                if let Some(dealloc) = (*tp).tp_dealloc {
                    (*c_op).ob_refcnt = 1;
                    (*c_op).ob_mule = ptr::null_mut();
                    dealloc(c_op);
                    return;
                }
                if ((*tp).tp_flags & Py_TPFLAGS_HEAPTYPE) != 0 {
                    Py_DecRef(tp as *mut PyObject);
                }
            }
            free(c_op as *mut c_void);
        } else if let Some(op) = py_opt {
            let is_static = ptr::eq(op, &raw mut _Py_NoneStruct)
                || ptr::eq(op, (&raw mut _Py_TrueStruct) as *mut PyObject)
                || ptr::eq(op, (&raw mut _Py_FalseStruct) as *mut PyObject)
                || ptr::eq(op, &raw mut _Py_NotImplementedStruct)
                || ptr::eq(op, &raw mut _Py_EllipsisObject)
                || matches!(&(*val).data, Data::Class(_));
            if !is_static {
                free(op as *mut c_void);
            }
        }
    }
}

pub fn has_c_tp_dictoffset(mro: &[Value]) -> bool {
    for &cls in mro {
        if let Some(op) = with_state(|s| s.val_to_py.get(&(cls as usize)).copied()) {
            let tp = op as *mut PyTypeObject;
            unsafe {
                if (*tp).tp_dictoffset > 0 {
                    return true;
                }
            }
        }
    }
    false
}

unsafe fn c_instance_dict_slot(val: Value) -> *mut *mut PyObject {
    if val.is_null() || object::is_fixnum(val) {
        return ptr::null_mut();
    }
    let Some(c_op) = with_state(|s| s.c_instances.get(&(val as usize)).copied()) else {
        return ptr::null_mut();
    };
    unsafe {
        let tp = (*c_op).ob_type;
        if tp.is_null() || (*tp).tp_dictoffset <= 0 {
            return ptr::null_mut();
        }
        (c_op as *mut u8).add((*tp).tp_dictoffset as usize) as *mut *mut PyObject
    }
}

pub fn c_instance_has_dict_slot(val: Value) -> bool {
    unsafe { !c_instance_dict_slot(val).is_null() }
}

pub fn c_instance_dict(val: Value, create: bool) -> Value {
    unsafe {
        let slot = c_instance_dict_slot(val);
        if slot.is_null() {
            return ptr::null_mut();
        }
        if (*slot).is_null() {
            if !create {
                return ptr::null_mut();
            }
            let d = PyDict_New();
            if d.is_null() {
                return ptr::null_mut();
            }
            *slot = d;
        }
        from_py_borrowed(*slot)
    }
}

pub fn c_instance_set_dict(val: Value, new_dict: Value) -> bool {
    unsafe {
        let slot = c_instance_dict_slot(val);
        if slot.is_null() {
            return false;
        }
        let new_py = to_py_borrowed(new_dict);
        Py_IncRef(new_py);
        let old = *slot;
        *slot = new_py;
        if !old.is_null() {
            Py_DecRef(old);
        }
        true
    }
}

pub fn c_instance_dict_lookup(val: Value, name: &str) -> Option<Value> {
    let d = c_instance_dict(val, false);
    if d.is_null() {
        return None;
    }
    let key = mule_string(name.as_ptr(), name.len());
    let item = unsafe { PyDict_GetItemWithError(to_py_borrowed(d), to_py_borrowed(key)) };
    unsafe { object::release(key) };
    if item.is_null() {
        None
    } else {
        Some(from_py_newref(item))
    }
}

pub fn c_instance_dict_set(val: Value, name: &str, attr_val: Value) -> bool {
    let d = c_instance_dict(val, true);
    if d.is_null() {
        return false;
    }
    let key = mule_string(name.as_ptr(), name.len());
    let rc = unsafe {
        PyDict_SetItem(
            to_py_borrowed(d),
            to_py_borrowed(key),
            to_py_borrowed(attr_val),
        )
    };
    unsafe { object::release(key) };
    rc == 0
}

pub fn c_instance_dict_del(val: Value, name: &str) -> Option<bool> {
    if !c_instance_has_dict_slot(val) {
        return None;
    }
    let d = c_instance_dict(val, false);
    if d.is_null() {
        return Some(false);
    }
    let key = mule_string(name.as_ptr(), name.len());
    let rc = unsafe { PyDict_DelItem(to_py_borrowed(d), to_py_borrowed(key)) };
    unsafe { object::release(key) };
    if rc < 0 {
        unsafe { PyErr_Clear() };
        Some(false)
    } else {
        Some(true)
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn Py_IncRef(op: *mut PyObject) {
    if op.is_null() {
        return;
    }
    unsafe {
        let val = (*op).ob_mule;
        if !val.is_null() && !object::is_fixnum(val) {
            object::retain(val);
            (*op).ob_refcnt = ref_count(val).max(1) as Py_ssize_t;
        } else {
            (*op).ob_refcnt = (*op).ob_refcnt.saturating_add(1);
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn Py_DecRef(op: *mut PyObject) {
    if op.is_null() {
        return;
    }
    unsafe {
        let val = (*op).ob_mule;
        if !val.is_null() && !object::is_fixnum(val) {
            let prev = ref_count(val);
            object::release(val);
            if prev > 1 {
                (*op).ob_refcnt = (prev - 1) as Py_ssize_t;
            }
        } else {
            (*op).ob_refcnt = (*op).ob_refcnt.saturating_sub(1);
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyMem_Malloc(size: usize) -> *mut c_void {
    unsafe { malloc(size.max(1)) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyMem_Calloc(nelem: usize, elsize: usize) -> *mut c_void {
    unsafe { calloc(nelem.max(1), elsize.max(1)) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyMem_Realloc(ptr: *mut c_void, new_size: usize) -> *mut c_void {
    unsafe { realloc(ptr, new_size.max(1)) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyMem_Free(ptr: *mut c_void) {
    if !ptr.is_null() {
        unsafe { free(ptr) };
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_Malloc(size: usize) -> *mut c_void {
    unsafe { malloc(size.max(1)) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_Calloc(nelem: usize, elsize: usize) -> *mut c_void {
    unsafe { calloc(nelem.max(1), elsize.max(1)) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_Realloc(ptr: *mut c_void, new_size: usize) -> *mut c_void {
    unsafe { realloc(ptr, new_size.max(1)) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_Free(ptr: *mut c_void) {
    if !ptr.is_null() {
        unsafe { free(ptr) };
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_GC_Del(op: *mut c_void) {
    if !op.is_null() {
        unsafe { free(op) };
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_GC_Track(_op: *mut c_void) {}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_GC_UnTrack(_op: *mut c_void) {}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyType_GenericAlloc(
    tp: *mut PyTypeObject,
    nitems: Py_ssize_t,
) -> *mut PyObject {
    ensure_initialized();
    unsafe {
        let base_size = ((*tp).tp_basicsize as usize).max(std::mem::size_of::<PyObject>());
        let item_size = ((*tp).tp_itemsize as usize).saturating_mul(nitems.max(0) as usize);
        let total = base_size.saturating_add(item_size);
        let raw = calloc(1, total) as *mut PyObject;
        if raw.is_null() {
            return PyErr_NoMemory();
        }
        let cls = (*tp).ob_base.ob_base.ob_mule;
        let inst = crate::arguments::mule_allocate_instance(cls);
        if ((*tp).tp_flags & Py_TPFLAGS_HEAPTYPE) != 0 {
            Py_IncRef(tp as *mut PyObject);
        }
        (*raw).ob_refcnt = 1;
        (*raw).ob_type = tp;
        (*raw).ob_mule = inst;
        if (*tp).tp_itemsize > 0 {
            (*(raw as *mut PyVarObject)).ob_size = nitems;
        }
        with_state(|s| {
            s.val_to_py.insert(inst as usize, raw);
            s.c_instances.insert(inst as usize, raw);
        });
        raw
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyType_GenericNew(
    tp: *mut PyTypeObject,
    _args: *mut PyObject,
    _kwds: *mut PyObject,
) -> *mut PyObject {
    unsafe {
        if let Some(alloc) = (*tp).tp_alloc {
            alloc(tp, 0)
        } else {
            PyType_GenericAlloc(tp, 0)
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn _PyObject_New(tp: *mut PyTypeObject) -> *mut PyObject {
    unsafe { PyType_GenericAlloc(tp, 0) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn _PyObject_NewVar(
    tp: *mut PyTypeObject,
    nitems: Py_ssize_t,
) -> *mut PyVarObject {
    unsafe { PyType_GenericAlloc(tp, nitems) as *mut PyVarObject }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn _PyObject_GC_New(tp: *mut PyTypeObject) -> *mut PyObject {
    unsafe { PyType_GenericAlloc(tp, 0) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn _PyObject_GC_NewVar(
    tp: *mut PyTypeObject,
    nitems: Py_ssize_t,
) -> *mut PyVarObject {
    unsafe { PyType_GenericAlloc(tp, nitems) as *mut PyVarObject }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_SetString(exc: *mut PyObject, message: *const c_char) {
    let msg_str = if message.is_null() {
        ""
    } else {
        unsafe { CStr::from_ptr(message).to_str().unwrap_or("") }
    };
    let msg_val = mule_string(msg_str.as_ptr(), msg_str.len());
    unsafe {
        PyErr_SetObject(exc, to_py_borrowed(msg_val));
        object::release(msg_val);
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_SetObject(exc: *mut PyObject, value: *mut PyObject) {
    ensure_initialized();
    unsafe {
        let exc_val = from_py_borrowed(exc);
        let val = from_py_borrowed(value);
        if exc_val.is_null() {
            return;
        }
        let base_exc = protocol_lookup("BaseException");
        if !val.is_null() && !base_exc.is_null() && classes::is_instance(val, base_exc) {
            mule_raise_value(val);
            return;
        }
        let inst = if val.is_null() || val == mule_none() {
            object::call(exc_val, &[])
        } else if classes::class_of_value(val) == classes::builtin_type(Builtin::Tuple) {
            let items: Vec<Value> = match &(*val).data {
                Data::Args(v, ..) => v.to_vec(),
                _ => vec![val],
            };
            object::call(exc_val, &items)
        } else {
            object::call(exc_val, &[val])
        };
        if !inst.is_null() && !has_pending_exception() {
            mule_raise_value(inst);
            object::release(inst);
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_SetNone(exc: *mut PyObject) {
    unsafe { PyErr_SetObject(exc, ptr::null_mut()) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_NoMemory() -> *mut PyObject {
    ensure_initialized();
    unsafe { PyErr_SetNone(PyExc_MemoryError) };
    ptr::null_mut()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_BadArgument() -> c_int {
    ensure_initialized();
    unsafe {
        PyErr_SetString(
            PyExc_TypeError,
            b"bad argument type for built-in operation\0".as_ptr() as *const c_char,
        );
    }
    0
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_BadInternalCall() {
    ensure_initialized();
    unsafe {
        PyErr_SetString(
            PyExc_SystemError,
            b"bad argument to internal function\0".as_ptr() as *const c_char,
        );
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_Occurred() -> *mut PyObject {
    if !has_pending_exception() {
        return ptr::null_mut();
    }
    ensure_initialized();
    let kind = if crate::sequences::has_pending() {
        crate::sequences::pending_kind()
    } else {
        object::error_kind()
    };
    let cls = protocol_lookup(&kind);
    if !cls.is_null() {
        to_py_borrowed(cls)
    } else {
        unsafe { PyExc_Exception }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_Clear() {
    object::clear_error();
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_GetRaisedException() -> *mut PyObject {
    if !has_pending_exception() {
        return ptr::null_mut();
    }
    let exc = take_pending_exception();
    to_py_owned(exc)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_SetRaisedException(exc: *mut PyObject) {
    if exc.is_null() {
        unsafe { PyErr_Clear() };
        return;
    }
    let val = from_py_borrowed(exc);
    mule_raise_value(val);
    unsafe { Py_DecRef(exc) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_Fetch(
    ptype: *mut *mut PyObject,
    pvalue: *mut *mut PyObject,
    ptraceback: *mut *mut PyObject,
) {
    unsafe {
        if !ptype.is_null() {
            *ptype = ptr::null_mut();
        }
        if !pvalue.is_null() {
            *pvalue = ptr::null_mut();
        }
        if !ptraceback.is_null() {
            *ptraceback = ptr::null_mut();
        }
        if !has_pending_exception() {
            return;
        }
        let exc = take_pending_exception();
        if exc.is_null() {
            return;
        }
        let cls = classes::class_of_value(exc);
        if !ptype.is_null() {
            let tp_py = to_py_borrowed(cls);
            Py_IncRef(tp_py);
            *ptype = tp_py;
        }
        if !pvalue.is_null() {
            *pvalue = to_py_owned(exc);
        } else {
            object::release(exc);
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_Restore(
    r#type: *mut PyObject,
    value: *mut PyObject,
    traceback: *mut PyObject,
) {
    unsafe {
        PyErr_Clear();
        if !r#type.is_null() || !value.is_null() {
            let base_exc = protocol_lookup("BaseException");
            if !value.is_null()
                && !base_exc.is_null()
                && classes::is_instance(from_py_borrowed(value), base_exc)
            {
                mule_raise_value(from_py_borrowed(value));
            } else if !r#type.is_null() {
                PyErr_SetObject(r#type, value);
            }
        }
        if !r#type.is_null() {
            Py_DecRef(r#type);
        }
        if !value.is_null() {
            Py_DecRef(value);
        }
        if !traceback.is_null() {
            Py_DecRef(traceback);
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_GivenExceptionMatches(
    err: *mut PyObject,
    exc: *mut PyObject,
) -> c_int {
    if err.is_null() || exc.is_null() {
        return 0;
    }
    unsafe {
        let err_val = from_py_borrowed(err);
        let exc_val = from_py_borrowed(exc);
        let err_cls = if !object::is_fixnum(err_val) && matches!(&(*err_val).data, Data::Class(_)) {
            err_val
        } else {
            classes::class_of_value(err_val)
        };
        if classes::class_of_value(exc_val) == classes::builtin_type(Builtin::Tuple) {
            let items: Vec<Value> = match &(*exc_val).data {
                Data::Args(v, ..) => v.to_vec(),
                _ => Vec::new(),
            };
            for item in items {
                if PyErr_GivenExceptionMatches(to_py_borrowed(err_cls), to_py_borrowed(item)) != 0 {
                    return 1;
                }
            }
            0
        } else if !object::is_fixnum(exc_val) && matches!(&(*exc_val).data, Data::Class(_)) {
            c_int::from(classes::is_subclass(err_cls, exc_val))
        } else {
            0
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_ExceptionMatches(exc: *mut PyObject) -> c_int {
    let cur = unsafe { PyErr_Occurred() };
    if cur.is_null() {
        return 0;
    }
    unsafe { PyErr_GivenExceptionMatches(cur, exc) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_WarnEx(
    _category: *mut PyObject,
    _message: *const c_char,
    _stack_level: Py_ssize_t,
) -> c_int {
    0
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_WriteUnraisable(_obj: *mut PyObject) {
    unsafe { PyErr_Clear() };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyType_IsSubtype(a: *mut PyTypeObject, b: *mut PyTypeObject) -> c_int {
    if a.is_null() || b.is_null() {
        return 0;
    }
    if ptr::eq(a, b) {
        return 1;
    }
    unsafe {
        let cls_a = (*a).ob_base.ob_base.ob_mule;
        let cls_b = (*b).ob_base.ob_base.ob_mule;
        if !cls_a.is_null() && !cls_b.is_null() {
            return c_int::from(classes::is_subclass(cls_a, cls_b));
        }
        let mut cur = (*a).tp_base;
        while !cur.is_null() {
            if ptr::eq(cur, b) {
                return 1;
            }
            cur = (*cur).tp_base;
        }
        0
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyType_Ready(tp: *mut PyTypeObject) -> c_int {
    if tp.is_null() {
        return -1;
    }
    ensure_initialized();
    unsafe {
        if (*tp).ob_base.ob_base.ob_type.is_null() {
            (*tp).ob_base.ob_base.ob_type = &raw mut PyType_Type;
        }
        if (*tp).tp_base.is_null() && !ptr::eq(tp, &raw mut PyBaseObject_Type) {
            (*tp).tp_base = &raw mut PyBaseObject_Type;
        }
        if (*tp).tp_alloc.is_none() {
            (*tp).tp_alloc = Some(PyType_GenericAlloc);
        }
        if (*tp).tp_new.is_none() {
            (*tp).tp_new = (*(*tp).tp_base).tp_new;
        }
        if (*tp).tp_free.is_none() {
            (*tp).tp_free = Some(PyObject_GC_Del);
        }
        (*tp).tp_flags |= Py_TPFLAGS_READY;
    }
    0
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyType_GetModuleState(tp: *mut PyTypeObject) -> *mut c_void {
    let _ = tp;
    ptr::null_mut()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_IsInstance(
    inst: *mut PyObject,
    cls: *mut PyObject,
) -> c_int {
    let inst_val = from_py_borrowed(inst);
    let cls_val = from_py_borrowed(cls);
    if inst_val.is_null() || cls_val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    let fn_val = protocol_lookup("isinstance");
    let res = unsafe { object::call(fn_val, &[inst_val, cls_val]) };
    if res.is_null() || has_pending_exception() {
        return -1;
    }
    let b = is_truthy(res);
    unsafe { object::release(res) };
    c_int::from(b)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_IsSubclass(
    derived: *mut PyObject,
    cls: *mut PyObject,
) -> c_int {
    let derived_val = from_py_borrowed(derived);
    let cls_val = from_py_borrowed(cls);
    if derived_val.is_null() || cls_val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    let fn_val = protocol_lookup("issubclass");
    let res = unsafe { object::call(fn_val, &[derived_val, cls_val]) };
    if res.is_null() || has_pending_exception() {
        return -1;
    }
    let b = is_truthy(res);
    unsafe { object::release(res) };
    c_int::from(b)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyCallable_Check(o: *mut PyObject) -> c_int {
    let val = from_py_borrowed(o);
    if val.is_null() {
        return 0;
    }
    let callable_fn = protocol_lookup("callable");
    let res = unsafe { object::call(callable_fn, &[val]) };
    if res.is_null() || has_pending_exception() {
        unsafe { PyErr_Clear() };
        return 0;
    }
    let b = is_truthy(res);
    unsafe { object::release(res) };
    c_int::from(b)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_GetAttr(
    o: *mut PyObject,
    attr_name: *mut PyObject,
) -> *mut PyObject {
    let obj_val = from_py_borrowed(o);
    let name_val = from_py_borrowed(attr_name);
    if obj_val.is_null() || name_val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    if unsafe { classes::class_of_value(name_val) } != classes::builtin_type(Builtin::Str) {
        unsafe {
            PyErr_SetString(
                PyExc_TypeError,
                b"attribute name must be string\0".as_ptr() as *const c_char,
            );
        }
        return ptr::null_mut();
    }
    let name_str = string_value(name_val);
    let res = get_attr_str(obj_val, &name_str);
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_GetAttrString(
    o: *mut PyObject,
    attr_name: *const c_char,
) -> *mut PyObject {
    if attr_name.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let obj_val = from_py_borrowed(o);
    if obj_val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let res = unsafe { classes::mule_get_attr(obj_val, attr_name) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_GetOptionalAttr(
    o: *mut PyObject,
    attr_name: *mut PyObject,
    result: *mut *mut PyObject,
) -> c_int {
    let val = unsafe { PyObject_GetAttr(o, attr_name) };
    unsafe {
        if !val.is_null() {
            if !result.is_null() {
                *result = val;
            } else {
                Py_DecRef(val);
            }
            return 1;
        }
        if !result.is_null() {
            *result = ptr::null_mut();
        }
        if PyErr_ExceptionMatches(PyExc_AttributeError) != 0 {
            PyErr_Clear();
            return 0;
        }
        -1
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_GetOptionalAttrString(
    o: *mut PyObject,
    attr_name: *const c_char,
    result: *mut *mut PyObject,
) -> c_int {
    let s = unsafe { CStr::from_ptr(attr_name).to_bytes() };
    let name_val = mule_string(s.as_ptr(), s.len());
    let rc = unsafe { PyObject_GetOptionalAttr(o, to_py_borrowed(name_val), result) };
    unsafe { object::release(name_val) };
    rc
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_HasAttr(
    o: *mut PyObject,
    attr_name: *mut PyObject,
) -> c_int {
    let val = unsafe { PyObject_GetAttr(o, attr_name) };
    if !val.is_null() {
        unsafe { Py_DecRef(val) };
        1
    } else {
        unsafe { PyErr_Clear() };
        0
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_HasAttrString(
    o: *mut PyObject,
    attr_name: *const c_char,
) -> c_int {
    let val = unsafe { PyObject_GetAttrString(o, attr_name) };
    if !val.is_null() {
        unsafe { Py_DecRef(val) };
        1
    } else {
        unsafe { PyErr_Clear() };
        0
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_HasAttrWithError(
    o: *mut PyObject,
    attr_name: *mut PyObject,
) -> c_int {
    let mut res: *mut PyObject = ptr::null_mut();
    let rc = unsafe { PyObject_GetOptionalAttr(o, attr_name, &raw mut res) };
    if !res.is_null() {
        unsafe { Py_DecRef(res) };
    }
    rc
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_HasAttrStringWithError(
    o: *mut PyObject,
    attr_name: *const c_char,
) -> c_int {
    let mut res: *mut PyObject = ptr::null_mut();
    let rc = unsafe { PyObject_GetOptionalAttrString(o, attr_name, &raw mut res) };
    if !res.is_null() {
        unsafe { Py_DecRef(res) };
    }
    rc
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_SetAttr(
    o: *mut PyObject,
    attr_name: *mut PyObject,
    v: *mut PyObject,
) -> c_int {
    let obj_val = from_py_borrowed(o);
    let name_val = from_py_borrowed(attr_name);
    if obj_val.is_null() || name_val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    if unsafe { classes::class_of_value(name_val) } != classes::builtin_type(Builtin::Str) {
        unsafe {
            PyErr_SetString(
                PyExc_TypeError,
                b"attribute name must be string\0".as_ptr() as *const c_char,
            );
        }
        return -1;
    }
    let name_str = string_value(name_val);
    if v.is_null() {
        let res = del_attr_str(obj_val, &name_str);
        if res.is_null() || has_pending_exception() {
            return -1;
        }
        unsafe { object::release(res) };
        0
    } else {
        let val = from_py_borrowed(v);
        if set_attr_str(obj_val, &name_str, val) < 0 || has_pending_exception() {
            -1
        } else {
            0
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_SetAttrString(
    o: *mut PyObject,
    attr_name: *const c_char,
    v: *mut PyObject,
) -> c_int {
    if attr_name.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    let s = unsafe { CStr::from_ptr(attr_name).to_bytes() };
    let name_val = mule_string(s.as_ptr(), s.len());
    let rc = unsafe { PyObject_SetAttr(o, to_py_borrowed(name_val), v) };
    unsafe { object::release(name_val) };
    rc
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_DelAttr(
    o: *mut PyObject,
    attr_name: *mut PyObject,
) -> c_int {
    unsafe { PyObject_SetAttr(o, attr_name, ptr::null_mut()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_DelAttrString(
    o: *mut PyObject,
    attr_name: *const c_char,
) -> c_int {
    unsafe { PyObject_SetAttrString(o, attr_name, ptr::null_mut()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_GenericGetAttr(
    o: *mut PyObject,
    name: *mut PyObject,
) -> *mut PyObject {
    let obj_val = from_py_borrowed(o);
    let name_val = from_py_borrowed(name);
    if obj_val.is_null() || name_val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    if unsafe { classes::class_of_value(name_val) } != classes::builtin_type(Builtin::Str) {
        unsafe {
            PyErr_SetString(
                PyExc_TypeError,
                b"attribute name must be string\0".as_ptr() as *const c_char,
            );
        }
        return ptr::null_mut();
    }
    let name_str = string_value(name_val);
    let Ok(c_attr) = CString::new(name_str) else {
        return ptr::null_mut();
    };
    let res = unsafe { classes::generic_get_attribute_raw(obj_val, c_attr.as_ptr()) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_GenericSetAttr(
    o: *mut PyObject,
    name: *mut PyObject,
    value: *mut PyObject,
) -> c_int {
    let obj_val = from_py_borrowed(o);
    let name_val = from_py_borrowed(name);
    if obj_val.is_null() || name_val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    if unsafe { classes::class_of_value(name_val) } != classes::builtin_type(Builtin::Str) {
        unsafe {
            PyErr_SetString(
                PyExc_TypeError,
                b"attribute name must be string\0".as_ptr() as *const c_char,
            );
        }
        return -1;
    }
    let name_str = string_value(name_val);
    if value.is_null() {
        let res = unsafe { classes::delete_attribute_with_pub(obj_val, &name_str, false) };
        if res.is_null() || has_pending_exception() {
            return -1;
        }
        unsafe { object::release(res) };
        0
    } else {
        let Ok(c_attr) = CString::new(name_str) else {
            return -1;
        };
        let val = from_py_borrowed(value);
        if unsafe { classes::generic_set_attribute_raw(obj_val, c_attr.as_ptr(), val) } < 0
            || has_pending_exception()
        {
            -1
        } else {
            0
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_GenericGetDict(
    o: *mut PyObject,
    _context: *mut c_void,
) -> *mut PyObject {
    let val = from_py_borrowed(o);
    let d = c_instance_dict(val, true);
    if !d.is_null() {
        unsafe { object::retain(d) };
        return to_py_owned(d);
    }
    unsafe { PyObject_GetAttrString(o, b"__dict__\0".as_ptr() as *const c_char) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_GenericSetDict(
    o: *mut PyObject,
    value: *mut PyObject,
    _context: *mut c_void,
) -> c_int {
    if value.is_null() {
        unsafe {
            PyErr_SetString(
                PyExc_TypeError,
                b"cannot delete __dict__\0".as_ptr() as *const c_char,
            );
        }
        return -1;
    }
    let d_val = from_py_borrowed(value);
    if unsafe { !classes::is_instance(d_val, classes::builtin_type(Builtin::Dict)) } {
        unsafe {
            PyErr_SetString(
                PyExc_TypeError,
                b"__dict__ must be set to a dictionary\0".as_ptr() as *const c_char,
            );
        }
        return -1;
    }
    let obj_val = from_py_borrowed(o);
    if c_instance_set_dict(obj_val, d_val) {
        return 0;
    }
    unsafe { PyObject_SetAttrString(o, b"__dict__\0".as_ptr() as *const c_char, value) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_Call(
    callable: *mut PyObject,
    args: *mut PyObject,
    kwargs: *mut PyObject,
) -> *mut PyObject {
    let cal_val = from_py_borrowed(callable);
    if cal_val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let pos_vec: Vec<Value> = if args.is_null() {
        Vec::new()
    } else {
        let args_val = from_py_borrowed(args);
        unsafe {
            match &(*args_val).data {
                Data::Args(items, ..) => items.to_vec(),
                _ => {
                    PyErr_SetString(
                        PyExc_TypeError,
                        b"argument list must be a tuple\0".as_ptr() as *const c_char,
                    );
                    return ptr::null_mut();
                }
            }
        }
    };
    let res = if kwargs.is_null() {
        unsafe { object::call(cal_val, &pos_vec) }
    } else {
        unsafe {
            let kw_val = from_py_borrowed(kwargs);
            let keys_res = object::special(kw_val, "keys", &[]);
            if keys_res.is_null() || has_pending_exception() {
                return ptr::null_mut();
            }
            let keys_list = object::call(classes::builtin_type(Builtin::List), &[keys_res]);
            object::release(keys_res);
            if keys_list.is_null() || has_pending_exception() {
                return ptr::null_mut();
            }
            let key_items = match &(*keys_list).data {
                Data::List(v, ..) => v.to_vec(),
                _ => Vec::new(),
            };
            let mut kw_names: Vec<String> = Vec::with_capacity(key_items.len());
            let mut kw_vals: Vec<Value> = Vec::with_capacity(key_items.len());
            for k in &key_items {
                if classes::class_of_value(*k) != classes::builtin_type(Builtin::Str) {
                    object::release(keys_list);
                    PyErr_SetString(
                        PyExc_TypeError,
                        b"keywords must be strings\0".as_ptr() as *const c_char,
                    );
                    return ptr::null_mut();
                }
                let v = object::special(kw_val, "__getitem__", &[*k]);
                if v.is_null() || has_pending_exception() {
                    object::release(keys_list);
                    for kv in kw_vals {
                        object::release(kv);
                    }
                    return ptr::null_mut();
                }
                kw_names.push(string_value(*k));
                kw_vals.push(v);
            }
            object::release(keys_list);
            let kw_refs: Vec<(&str, Value)> = kw_names
                .iter()
                .zip(kw_vals.iter())
                .map(|(n, &v)| (n.as_str(), v))
                .collect();
            let r = call_with_keywords(cal_val, &pos_vec, &kw_refs);
            for kv in kw_vals {
                object::release(kv);
            }
            r
        }
    };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_CallNoArgs(callable: *mut PyObject) -> *mut PyObject {
    let cal_val = from_py_borrowed(callable);
    if cal_val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let res = unsafe { object::call(cal_val, &[]) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_CallOneArg(
    callable: *mut PyObject,
    arg: *mut PyObject,
) -> *mut PyObject {
    let cal_val = from_py_borrowed(callable);
    let arg_val = from_py_borrowed(arg);
    if cal_val.is_null() || arg_val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let res = unsafe { object::call(cal_val, &[arg_val]) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_CallObject(
    callable: *mut PyObject,
    args: *mut PyObject,
) -> *mut PyObject {
    unsafe { PyObject_Call(callable, args, ptr::null_mut()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_Vectorcall(
    callable: *mut PyObject,
    args: *const *mut PyObject,
    nargsf: usize,
    kwnames: *mut PyObject,
) -> *mut PyObject {
    let cal_val = from_py_borrowed(callable);
    let nargs = nargsf & !(1usize << 63);
    let mut pos_vals = Vec::with_capacity(nargs);
    unsafe {
        for i in 0..nargs {
            pos_vals.push(from_py_borrowed(*args.add(i)));
        }
        if kwnames.is_null() {
            let res = object::call(cal_val, &pos_vals);
            if res.is_null() || has_pending_exception() {
                return ptr::null_mut();
            }
            return to_py_owned(res);
        }
        let kw_tup = from_py_borrowed(kwnames);
        let names: Vec<Value> = match &(*kw_tup).data {
            Data::Args(v, ..) => v.to_vec(),
            _ => Vec::new(),
        };
        let mut kw_strs = Vec::with_capacity(names.len());
        let mut kw_vals = Vec::with_capacity(names.len());
        for (idx, &k) in names.iter().enumerate() {
            kw_strs.push(string_value(k));
            kw_vals.push(from_py_borrowed(*args.add(nargs + idx)));
        }
        let kw_refs: Vec<(&str, Value)> = kw_strs
            .iter()
            .zip(kw_vals.iter())
            .map(|(s, &v)| (s.as_str(), v))
            .collect();
        let res = call_with_keywords(cal_val, &pos_vals, &kw_refs);
        if res.is_null() || has_pending_exception() {
            return ptr::null_mut();
        }
        to_py_owned(res)
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_RichCompare(
    v: *mut PyObject,
    w: *mut PyObject,
    op: c_int,
) -> *mut PyObject {
    let a = from_py_borrowed(v);
    let b = from_py_borrowed(w);
    if a.is_null() || b.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let mule_op = match op {
        Py_EQ => 0,
        Py_NE => 1,
        Py_LT => 2,
        Py_LE => 3,
        Py_GT => 4,
        Py_GE => 5,
        _ => {
            unsafe { PyErr_BadInternalCall() };
            return ptr::null_mut();
        }
    };
    let res = unsafe { object::compare(mule_op, a, b) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_RichCompareBool(
    v: *mut PyObject,
    w: *mut PyObject,
    op: c_int,
) -> c_int {
    if ptr::eq(v, w) {
        if op == Py_EQ {
            return 1;
        }
        if op == Py_NE {
            return 0;
        }
    }
    let res = unsafe { PyObject_RichCompare(v, w, op) };
    if res.is_null() {
        return -1;
    }
    let b = unsafe { PyObject_IsTrue(res) };
    unsafe { Py_DecRef(res) };
    b
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_IsTrue(v: *mut PyObject) -> c_int {
    let val = from_py_borrowed(v);
    if val.is_null() {
        return -1;
    }
    let Some(b) = (unsafe { object::truth(val) }) else {
        return -1;
    };
    c_int::from(b)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_Not(v: *mut PyObject) -> c_int {
    let r = unsafe { PyObject_IsTrue(v) };
    if r < 0 {
        r
    } else if r == 0 {
        1
    } else {
        0
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_Repr(v: *mut PyObject) -> *mut PyObject {
    let val = from_py_borrowed(v);
    if val.is_null() {
        return unsafe { PyUnicode_FromString(b"<NULL>\0".as_ptr() as *const c_char) };
    }
    let res = unsafe { object::mule_render_text(val, 1) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_Str(v: *mut PyObject) -> *mut PyObject {
    let val = from_py_borrowed(v);
    if val.is_null() {
        return unsafe { PyUnicode_FromString(b"<NULL>\0".as_ptr() as *const c_char) };
    }
    let res = unsafe { object::mule_render_text(val, 0) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_Bytes(v: *mut PyObject) -> *mut PyObject {
    let val = from_py_borrowed(v);
    if val.is_null() {
        return unsafe { PyBytes_FromStringAndSize(ptr::null(), 0) };
    }
    let res = unsafe { object::call(classes::builtin_type(Builtin::Bytes), &[val]) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_Hash(v: *mut PyObject) -> Py_hash_t {
    let val = from_py_borrowed(v);
    if val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    let hash_fn = protocol_lookup("hash");
    let res = unsafe { object::call(hash_fn, &[val]) };
    if res.is_null() || has_pending_exception() {
        return -1;
    }
    let mut h = int_value(res) as Py_hash_t;
    unsafe { object::release(res) };
    if h == -1 {
        h = -2;
    }
    h
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_Dir(o: *mut PyObject) -> *mut PyObject {
    let val = from_py_borrowed(o);
    let dir_fn = protocol_lookup("dir");
    let res = unsafe {
        if val.is_null() {
            object::call(dir_fn, &[])
        } else {
            object::call(dir_fn, &[val])
        }
    };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_Length(o: *mut PyObject) -> Py_ssize_t {
    let val = from_py_borrowed(o);
    if val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    let res = unsafe { crate::builtins::mule_fast_len(val) };
    if res.is_null() || has_pending_exception() {
        return -1;
    }
    let n = int_value(res) as Py_ssize_t;
    unsafe { object::release(res) };
    n
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_Size(o: *mut PyObject) -> Py_ssize_t {
    unsafe { PyObject_Length(o) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_GetItem(
    o: *mut PyObject,
    key: *mut PyObject,
) -> *mut PyObject {
    let obj_val = from_py_borrowed(o);
    let key_val = from_py_borrowed(key);
    if obj_val.is_null() || key_val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let mut res = unsafe { crate::sequences::mule_fast_getitem(obj_val, key_val) };
    if res.is_null() && !has_pending_exception() {
        let getitem_fn = protocol_lookup("getitem");
        res = unsafe { object::call(getitem_fn, &[obj_val, key_val]) };
    }
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_SetItem(
    o: *mut PyObject,
    key: *mut PyObject,
    v: *mut PyObject,
) -> c_int {
    let obj_val = from_py_borrowed(o);
    let key_val = from_py_borrowed(key);
    if obj_val.is_null() || key_val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    unsafe {
        if v.is_null() {
            let del_fn = protocol_lookup("delitem");
            let res = object::call(del_fn, &[obj_val, key_val]);
            if res.is_null() || has_pending_exception() {
                return -1;
            }
            object::release(res);
            0
        } else {
            let val_v = from_py_borrowed(v);
            let rc = crate::sequences::mule_fast_setitem(obj_val, key_val, val_v);
            if rc > 0 {
                return 0;
            }
            if has_pending_exception() {
                return -1;
            }
            let set_fn = protocol_lookup("setitem");
            let res = object::call(set_fn, &[obj_val, key_val, val_v]);
            if res.is_null() || has_pending_exception() {
                return -1;
            }
            object::release(res);
            0
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_DelItem(
    o: *mut PyObject,
    key: *mut PyObject,
) -> c_int {
    unsafe { PyObject_SetItem(o, key, ptr::null_mut()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_GetIter(o: *mut PyObject) -> *mut PyObject {
    let val = from_py_borrowed(o);
    if val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let mut res = unsafe { crate::sequences::mule_fast_iterate(val) };
    if res.is_null() && !has_pending_exception() {
        let iter_fn = protocol_lookup("iterate");
        res = unsafe { object::call(iter_fn, &[val]) };
    }
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyIter_Next(iter: *mut PyObject) -> *mut PyObject {
    let val = from_py_borrowed(iter);
    if val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let mut out: Value = ptr::null_mut();
    let status = unsafe { crate::sequences::mule_fast_next_item(val, &raw mut out) };
    if status == 1 && !out.is_null() {
        return to_py_owned(out);
    }
    if status == 0 {
        return ptr::null_mut();
    }
    let next_fn = protocol_lookup("next_item");
    let res = unsafe { object::call(next_fn, &[val]) };
    if res.is_null() || has_pending_exception() {
        unsafe {
            if PyErr_ExceptionMatches(PyExc_StopIteration) != 0 {
                PyErr_Clear();
            }
        }
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyIter_Check(o: *mut PyObject) -> c_int {
    unsafe { PyObject_HasAttrString(o, b"__next__\0".as_ptr() as *const c_char) }
}

fn wrap_binop(v: *mut PyObject, w: *mut PyObject, op: i32, inplace: bool) -> *mut PyObject {
    let a = from_py_borrowed(v);
    let b = from_py_borrowed(w);
    if a.is_null() || b.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let res = unsafe { object::binary(op, a, b, inplace) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

fn wrap_unop(v: *mut PyObject, fn_name: &str) -> *mut PyObject {
    let a = from_py_borrowed(v);
    if a.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let f = protocol_lookup(fn_name);
    let res = unsafe { object::call(f, &[a]) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_Check(o: *mut PyObject) -> c_int {
    let val = from_py_borrowed(o);
    if val.is_null() {
        return 0;
    }
    let cls = unsafe { classes::class_of_value(val) };
    if cls == classes::builtin_type(Builtin::Int)
        || cls == classes::builtin_type(Builtin::Bool)
        || cls == classes::builtin_type(Builtin::Float)
    {
        return 1;
    }
    unsafe {
        if PyObject_HasAttrString(o, b"__index__\0".as_ptr() as *const c_char) != 0
            || PyObject_HasAttrString(o, b"__int__\0".as_ptr() as *const c_char) != 0
            || PyObject_HasAttrString(o, b"__float__\0".as_ptr() as *const c_char) != 0
        {
            1
        } else {
            0
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyIndex_Check(o: *mut PyObject) -> c_int {
    let val = from_py_borrowed(o);
    if val.is_null() {
        return 0;
    }
    let cls = unsafe { classes::class_of_value(val) };
    if cls == classes::builtin_type(Builtin::Int) || cls == classes::builtin_type(Builtin::Bool) {
        return 1;
    }
    unsafe { PyObject_HasAttrString(o, b"__index__\0".as_ptr() as *const c_char) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_Add(v: *mut PyObject, w: *mut PyObject) -> *mut PyObject {
    wrap_binop(v, w, 0, false)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_Subtract(v: *mut PyObject, w: *mut PyObject) -> *mut PyObject {
    wrap_binop(v, w, 1, false)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_Multiply(v: *mut PyObject, w: *mut PyObject) -> *mut PyObject {
    wrap_binop(v, w, 2, false)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_TrueDivide(v: *mut PyObject, w: *mut PyObject) -> *mut PyObject {
    wrap_binop(v, w, 3, false)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_FloorDivide(
    v: *mut PyObject,
    w: *mut PyObject,
) -> *mut PyObject {
    wrap_binop(v, w, 4, false)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_Remainder(v: *mut PyObject, w: *mut PyObject) -> *mut PyObject {
    wrap_binop(v, w, 5, false)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_MatrixMultiply(
    v: *mut PyObject,
    w: *mut PyObject,
) -> *mut PyObject {
    wrap_binop(v, w, 6, false)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_Lshift(v: *mut PyObject, w: *mut PyObject) -> *mut PyObject {
    wrap_binop(v, w, 8, false)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_Rshift(v: *mut PyObject, w: *mut PyObject) -> *mut PyObject {
    wrap_binop(v, w, 9, false)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_And(v: *mut PyObject, w: *mut PyObject) -> *mut PyObject {
    wrap_binop(v, w, 10, false)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_Xor(v: *mut PyObject, w: *mut PyObject) -> *mut PyObject {
    wrap_binop(v, w, 11, false)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_Or(v: *mut PyObject, w: *mut PyObject) -> *mut PyObject {
    wrap_binop(v, w, 12, false)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_InPlaceAdd(v: *mut PyObject, w: *mut PyObject) -> *mut PyObject {
    wrap_binop(v, w, 0, true)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_InPlaceSubtract(
    v: *mut PyObject,
    w: *mut PyObject,
) -> *mut PyObject {
    wrap_binop(v, w, 1, true)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_InPlaceMultiply(
    v: *mut PyObject,
    w: *mut PyObject,
) -> *mut PyObject {
    wrap_binop(v, w, 2, true)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_InPlaceTrueDivide(
    v: *mut PyObject,
    w: *mut PyObject,
) -> *mut PyObject {
    wrap_binop(v, w, 3, true)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_InPlaceFloorDivide(
    v: *mut PyObject,
    w: *mut PyObject,
) -> *mut PyObject {
    wrap_binop(v, w, 4, true)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_InPlaceRemainder(
    v: *mut PyObject,
    w: *mut PyObject,
) -> *mut PyObject {
    wrap_binop(v, w, 5, true)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_InPlaceMatrixMultiply(
    v: *mut PyObject,
    w: *mut PyObject,
) -> *mut PyObject {
    wrap_binop(v, w, 6, true)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_InPlaceLshift(
    v: *mut PyObject,
    w: *mut PyObject,
) -> *mut PyObject {
    wrap_binop(v, w, 8, true)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_InPlaceRshift(
    v: *mut PyObject,
    w: *mut PyObject,
) -> *mut PyObject {
    wrap_binop(v, w, 9, true)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_InPlaceAnd(v: *mut PyObject, w: *mut PyObject) -> *mut PyObject {
    wrap_binop(v, w, 10, true)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_InPlaceXor(v: *mut PyObject, w: *mut PyObject) -> *mut PyObject {
    wrap_binop(v, w, 11, true)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_InPlaceOr(v: *mut PyObject, w: *mut PyObject) -> *mut PyObject {
    wrap_binop(v, w, 12, true)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_Divmod(v: *mut PyObject, w: *mut PyObject) -> *mut PyObject {
    let a = from_py_borrowed(v);
    let b = from_py_borrowed(w);
    if a.is_null() || b.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let dm = protocol_lookup("divmod");
    let res = unsafe { object::call(dm, &[a, b]) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_Power(
    v: *mut PyObject,
    w: *mut PyObject,
    z: *mut PyObject,
) -> *mut PyObject {
    let a = from_py_borrowed(v);
    let b = from_py_borrowed(w);
    let c = from_py_borrowed(z);
    if a.is_null() || b.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let res = unsafe {
        if c.is_null() || c == mule_none() {
            object::binary(7, a, b, false)
        } else {
            let pow_fn = protocol_lookup("pow");
            object::call(pow_fn, &[a, b, c])
        }
    };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_InPlacePower(
    v: *mut PyObject,
    w: *mut PyObject,
    z: *mut PyObject,
) -> *mut PyObject {
    let a = from_py_borrowed(v);
    let b = from_py_borrowed(w);
    let c = from_py_borrowed(z);
    if a.is_null() || b.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let res = unsafe {
        if c.is_null() || c == mule_none() {
            object::binary(7, a, b, true)
        } else {
            return PyNumber_Power(v, w, z);
        }
    };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_Negative(v: *mut PyObject) -> *mut PyObject {
    wrap_unop(v, "negative")
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_Positive(v: *mut PyObject) -> *mut PyObject {
    wrap_unop(v, "positive")
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_Absolute(v: *mut PyObject) -> *mut PyObject {
    wrap_unop(v, "abs")
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_Invert(v: *mut PyObject) -> *mut PyObject {
    wrap_unop(v, "invert")
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_Long(o: *mut PyObject) -> *mut PyObject {
    let val = from_py_borrowed(o);
    if val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let res = unsafe { object::call(classes::builtin_type(Builtin::Int), &[val]) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_Float(o: *mut PyObject) -> *mut PyObject {
    let val = from_py_borrowed(o);
    if val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let res = unsafe { object::call(classes::builtin_type(Builtin::Float), &[val]) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_Index(o: *mut PyObject) -> *mut PyObject {
    wrap_unop(o, "integer_index")
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyNumber_AsSsize_t(
    o: *mut PyObject,
    exc: *mut PyObject,
) -> Py_ssize_t {
    let idx = unsafe { PyNumber_Index(o) };
    if idx.is_null() {
        return -1;
    }
    let _ = exc;
    let v = unsafe { PyLong_AsSsize_t(idx) };
    unsafe { Py_DecRef(idx) };
    v
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySequence_Check(o: *mut PyObject) -> c_int {
    let val = from_py_borrowed(o);
    if val.is_null() || unsafe { classes::is_instance(val, classes::builtin_type(Builtin::Dict)) } {
        return 0;
    }
    unsafe { PyObject_HasAttrString(o, b"__getitem__\0".as_ptr() as *const c_char) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySequence_Size(o: *mut PyObject) -> Py_ssize_t {
    unsafe { PyObject_Length(o) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySequence_Length(o: *mut PyObject) -> Py_ssize_t {
    unsafe { PyObject_Length(o) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySequence_Concat(
    o1: *mut PyObject,
    o2: *mut PyObject,
) -> *mut PyObject {
    unsafe { PyNumber_Add(o1, o2) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySequence_Repeat(
    o: *mut PyObject,
    count: Py_ssize_t,
) -> *mut PyObject {
    let n = mule_int(count as i64);
    let res = unsafe { PyNumber_Multiply(o, to_py_borrowed(n)) };
    unsafe { object::release(n) };
    res
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySequence_InPlaceConcat(
    o1: *mut PyObject,
    o2: *mut PyObject,
) -> *mut PyObject {
    unsafe { PyNumber_InPlaceAdd(o1, o2) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySequence_InPlaceRepeat(
    o: *mut PyObject,
    count: Py_ssize_t,
) -> *mut PyObject {
    let n = mule_int(count as i64);
    let res = unsafe { PyNumber_InPlaceMultiply(o, to_py_borrowed(n)) };
    unsafe { object::release(n) };
    res
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySequence_GetItem(
    o: *mut PyObject,
    i: Py_ssize_t,
) -> *mut PyObject {
    let idx = mule_int(i as i64);
    let res = unsafe { PyObject_GetItem(o, to_py_borrowed(idx)) };
    unsafe { object::release(idx) };
    res
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySequence_SetItem(
    o: *mut PyObject,
    i: Py_ssize_t,
    v: *mut PyObject,
) -> c_int {
    let idx = mule_int(i as i64);
    let rc = unsafe { PyObject_SetItem(o, to_py_borrowed(idx), v) };
    unsafe { object::release(idx) };
    rc
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySequence_DelItem(o: *mut PyObject, i: Py_ssize_t) -> c_int {
    unsafe { PySequence_SetItem(o, i, ptr::null_mut()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySequence_Tuple(o: *mut PyObject) -> *mut PyObject {
    let val = from_py_borrowed(o);
    if val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let res = unsafe { object::call(classes::builtin_type(Builtin::Tuple), &[val]) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySequence_List(o: *mut PyObject) -> *mut PyObject {
    let val = from_py_borrowed(o);
    if val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let res = unsafe { object::call(classes::builtin_type(Builtin::List), &[val]) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySequence_Fast(
    o: *mut PyObject,
    m: *const c_char,
) -> *mut PyObject {
    let val = from_py_borrowed(o);
    if val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let cls = unsafe { classes::class_of_value(val) };
    if cls == classes::builtin_type(Builtin::List) || cls == classes::builtin_type(Builtin::Tuple) {
        unsafe { object::retain(val) };
        return to_py_owned(val);
    }
    let res = unsafe { PySequence_List(o) };
    if res.is_null() && !m.is_null() {
        unsafe {
            if PyErr_ExceptionMatches(PyExc_TypeError) != 0 {
                PyErr_SetString(PyExc_TypeError, m);
            }
        }
    }
    res
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySequence_Contains(
    seq: *mut PyObject,
    ob: *mut PyObject,
) -> c_int {
    let s = from_py_borrowed(seq);
    let item = from_py_borrowed(ob);
    if s.is_null() || item.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    let res = unsafe { object::compare(8, item, s) };
    if res.is_null() || has_pending_exception() {
        return -1;
    }
    let b = is_truthy(res);
    unsafe { object::release(res) };
    c_int::from(b)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyMapping_Check(o: *mut PyObject) -> c_int {
    unsafe { PyObject_HasAttrString(o, b"__getitem__\0".as_ptr() as *const c_char) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyMapping_Size(o: *mut PyObject) -> Py_ssize_t {
    unsafe { PyObject_Length(o) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyMapping_Length(o: *mut PyObject) -> Py_ssize_t {
    unsafe { PyObject_Length(o) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyMapping_HasKeyString(
    o: *mut PyObject,
    key: *const c_char,
) -> c_int {
    let v = unsafe { PyMapping_GetItemString(o, key) };
    if !v.is_null() {
        unsafe { Py_DecRef(v) };
        1
    } else {
        unsafe { PyErr_Clear() };
        0
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyMapping_HasKey(
    o: *mut PyObject,
    key: *mut PyObject,
) -> c_int {
    let v = unsafe { PyObject_GetItem(o, key) };
    if !v.is_null() {
        unsafe { Py_DecRef(v) };
        1
    } else {
        unsafe { PyErr_Clear() };
        0
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyMapping_Keys(o: *mut PyObject) -> *mut PyObject {
    let val = from_py_borrowed(o);
    let keys = unsafe { object::special(val, "keys", &[]) };
    if keys.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    let list = unsafe { object::call(classes::builtin_type(Builtin::List), &[keys]) };
    unsafe { object::release(keys) };
    if list.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(list)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyMapping_Values(o: *mut PyObject) -> *mut PyObject {
    let val = from_py_borrowed(o);
    let vals = unsafe { object::special(val, "values", &[]) };
    if vals.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    let list = unsafe { object::call(classes::builtin_type(Builtin::List), &[vals]) };
    unsafe { object::release(vals) };
    if list.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(list)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyMapping_Items(o: *mut PyObject) -> *mut PyObject {
    let val = from_py_borrowed(o);
    let items = unsafe { object::special(val, "items", &[]) };
    if items.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    let list = unsafe { object::call(classes::builtin_type(Builtin::List), &[items]) };
    unsafe { object::release(items) };
    if list.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(list)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyMapping_GetItemString(
    o: *mut PyObject,
    key: *const c_char,
) -> *mut PyObject {
    let s = unsafe { CStr::from_ptr(key).to_bytes() };
    let k = mule_string(s.as_ptr(), s.len());
    let res = unsafe { PyObject_GetItem(o, to_py_borrowed(k)) };
    unsafe { object::release(k) };
    res
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyMapping_SetItemString(
    o: *mut PyObject,
    key: *const c_char,
    value: *mut PyObject,
) -> c_int {
    let s = unsafe { CStr::from_ptr(key).to_bytes() };
    let k = mule_string(s.as_ptr(), s.len());
    let rc = unsafe { PyObject_SetItem(o, to_py_borrowed(k), value) };
    unsafe { object::release(k) };
    rc
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyUnicode_FromString(u: *const c_char) -> *mut PyObject {
    if u.is_null() {
        return to_py_owned(mule_string(ptr::null(), 0));
    }
    let bytes = unsafe { CStr::from_ptr(u).to_bytes() };
    to_py_owned(mule_string(bytes.as_ptr(), bytes.len()))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyUnicode_FromStringAndSize(
    u: *const c_char,
    size: Py_ssize_t,
) -> *mut PyObject {
    if size < 0 {
        unsafe {
            PyErr_SetString(
                PyExc_SystemError,
                b"Negative size passed to PyUnicode_FromStringAndSize\0".as_ptr() as *const c_char,
            );
        }
        return ptr::null_mut();
    }
    if u.is_null() || size == 0 {
        return to_py_owned(mule_string(ptr::null(), 0));
    }
    to_py_owned(mule_string(u as *const u8, size as usize))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyUnicode_AsUTF8AndSize(
    unicode: *mut PyObject,
    size: *mut Py_ssize_t,
) -> *const c_char {
    let val = from_py_borrowed(unicode);
    if val.is_null() || unsafe { !classes::is_instance(val, classes::builtin_type(Builtin::Str)) } {
        unsafe { PyErr_BadArgument() };
        return ptr::null();
    }
    let s = string_value(val);
    let len = s.len();
    unsafe {
        if !size.is_null() {
            *size = len as Py_ssize_t;
        }
    }
    with_state(|st| {
        if !st.utf8_cache.contains_key(&(val as usize)) {
            let mut bytes = s.into_bytes();
            bytes.push(0);
            let cstr = unsafe { CString::from_vec_unchecked(bytes) };
            st.utf8_cache.insert(val as usize, cstr);
        }
        st.utf8_cache.get(&(val as usize)).unwrap().as_ptr()
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyUnicode_AsUTF8(unicode: *mut PyObject) -> *const c_char {
    unsafe { PyUnicode_AsUTF8AndSize(unicode, ptr::null_mut()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyUnicode_AsUTF8String(unicode: *mut PyObject) -> *mut PyObject {
    let mut sz: Py_ssize_t = 0;
    let ptr = unsafe { PyUnicode_AsUTF8AndSize(unicode, &raw mut sz) };
    if ptr.is_null() {
        return ptr::null_mut();
    }
    unsafe { PyBytes_FromStringAndSize(ptr, sz) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyUnicode_GetLength(unicode: *mut PyObject) -> Py_ssize_t {
    let val = from_py_borrowed(unicode);
    if val.is_null() || unsafe { !classes::is_instance(val, classes::builtin_type(Builtin::Str)) } {
        unsafe { PyErr_BadArgument() };
        return -1;
    }
    string_value(val).chars().count() as Py_ssize_t
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyUnicode_Concat(
    left: *mut PyObject,
    right: *mut PyObject,
) -> *mut PyObject {
    unsafe { PyNumber_Add(left, right) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyUnicode_CompareWithASCIIString(
    uni: *mut PyObject,
    string: *const c_char,
) -> c_int {
    let val = from_py_borrowed(uni);
    if val.is_null() || string.is_null() {
        return -1;
    }
    let s1 = string_value(val);
    let s2 = unsafe { CStr::from_ptr(string).to_str().unwrap_or("") };
    if s1.as_str() < s2 {
        -1
    } else if s1.as_str() > s2 {
        1
    } else {
        0
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyUnicode_InternInPlace(_p: *mut *mut PyObject) {}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyUnicode_InternFromString(u: *const c_char) -> *mut PyObject {
    unsafe { PyUnicode_FromString(u) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyLong_FromLong(v: c_long) -> *mut PyObject {
    to_py_owned(mule_int(v as i64))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyLong_FromUnsignedLong(v: c_ulong) -> *mut PyObject {
    to_py_owned(unsafe { crate::numeric::integer_from_usize(v as usize) })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyLong_FromLongLong(v: c_longlong) -> *mut PyObject {
    to_py_owned(mule_int(v as i64))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyLong_FromUnsignedLongLong(v: c_ulonglong) -> *mut PyObject {
    unsafe { PyLong_FromUnsignedLong(v as c_ulong) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyLong_FromSsize_t(v: Py_ssize_t) -> *mut PyObject {
    to_py_owned(mule_int(v as i64))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyLong_FromSize_t(v: usize) -> *mut PyObject {
    unsafe { PyLong_FromUnsignedLong(v as c_ulong) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyLong_AsLong(obj: *mut PyObject) -> c_long {
    let mut ovf = 0;
    unsafe { PyLong_AsLongAndOverflow(obj, &raw mut ovf) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyLong_AsLongAndOverflow(
    obj: *mut PyObject,
    overflow: *mut c_int,
) -> c_long {
    unsafe {
        if !overflow.is_null() {
            *overflow = 0;
        }
    }
    let val = from_py_borrowed(obj);
    if val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    if unsafe { classes::is_instance(val, classes::builtin_type(Builtin::Int)) } {
        return int_value(val) as c_long;
    }
    let iv = unsafe { PyNumber_Index(obj) };
    if iv.is_null() {
        return -1;
    }
    let n = int_value(from_py_borrowed(iv)) as c_long;
    unsafe { Py_DecRef(iv) };
    n
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyLong_AsLongLong(obj: *mut PyObject) -> c_longlong {
    unsafe { PyLong_AsLong(obj) as c_longlong }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyLong_AsSsize_t(obj: *mut PyObject) -> Py_ssize_t {
    unsafe { PyLong_AsLong(obj) as Py_ssize_t }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyLong_AsSize_t(obj: *mut PyObject) -> usize {
    let v = unsafe { PyLong_AsLong(obj) };
    if v < 0 && !unsafe { PyErr_Occurred() }.is_null() {
        return usize::MAX;
    }
    if v < 0 {
        unsafe {
            PyErr_SetString(
                PyExc_OverflowError,
                b"can't convert negative value to size_t\0".as_ptr() as *const c_char,
            );
        }
        return usize::MAX;
    }
    v as usize
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyLong_AsUnsignedLong(obj: *mut PyObject) -> c_ulong {
    unsafe { PyLong_AsSize_t(obj) as c_ulong }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyLong_AsUnsignedLongLong(obj: *mut PyObject) -> c_ulonglong {
    unsafe { PyLong_AsSize_t(obj) as c_ulonglong }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyBool_FromLong(v: c_long) -> *mut PyObject {
    ensure_initialized();
    if v != 0 {
        (&raw mut _Py_TrueStruct) as *mut PyObject
    } else {
        (&raw mut _Py_FalseStruct) as *mut PyObject
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyFloat_FromDouble(v: c_double) -> *mut PyObject {
    to_py_owned(mule_float(v))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyFloat_AsDouble(obj: *mut PyObject) -> c_double {
    let val = from_py_borrowed(obj);
    if val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return -1.0;
    }
    if let Some(d) = unsafe { crate::numeric::number_double(val) } {
        return d;
    }
    let fv = unsafe { PyNumber_Float(obj) };
    if fv.is_null() {
        return -1.0;
    }
    let d = unsafe { crate::numeric::number_double(from_py_borrowed(fv)).unwrap_or(0.0) };
    unsafe { Py_DecRef(fv) };
    d
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyBytes_FromStringAndSize(
    v: *const c_char,
    len: Py_ssize_t,
) -> *mut PyObject {
    if len < 0 {
        unsafe {
            PyErr_SetString(
                PyExc_SystemError,
                b"Negative size passed to PyBytes_FromStringAndSize\0".as_ptr() as *const c_char,
            );
        }
        return ptr::null_mut();
    }
    if v.is_null() {
        let zeros = vec![0u8; len as usize];
        return to_py_owned(mule_bytes(zeros.as_ptr(), zeros.len()));
    }
    to_py_owned(mule_bytes(v as *const u8, len as usize))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyBytes_FromString(v: *const c_char) -> *mut PyObject {
    let b = unsafe { CStr::from_ptr(v).to_bytes() };
    to_py_owned(mule_bytes(b.as_ptr(), b.len()))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyBytes_AsString(o: *mut PyObject) -> *mut c_char {
    let val = from_py_borrowed(o);
    if val.is_null() || object::is_fixnum(val) {
        unsafe { PyErr_BadArgument() };
        return ptr::null_mut();
    }
    unsafe {
        match &(*val).data {
            Data::Bytes(b, ..) => b.as_ptr() as *mut c_char,
            _ => {
                PyErr_BadArgument();
                ptr::null_mut()
            }
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyBytes_Size(o: *mut PyObject) -> Py_ssize_t {
    let val = from_py_borrowed(o);
    if val.is_null() || object::is_fixnum(val) {
        unsafe { PyErr_BadArgument() };
        return -1;
    }
    unsafe {
        match &(*val).data {
            Data::Bytes(b, ..) => b.len() as Py_ssize_t,
            _ => {
                PyErr_BadArgument();
                -1
            }
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyBytes_AsStringAndSize(
    obj: *mut PyObject,
    s: *mut *mut c_char,
    len: *mut Py_ssize_t,
) -> c_int {
    let p = unsafe { PyBytes_AsString(obj) };
    if p.is_null() {
        return -1;
    }
    let n = unsafe { PyBytes_Size(obj) };
    unsafe {
        if !s.is_null() {
            *s = p;
        }
        if !len.is_null() {
            *len = n;
        }
    }
    0
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyTuple_New(size: Py_ssize_t) -> *mut PyObject {
    if size < 0 {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let items: Vec<Value> = (0..size as usize).map(|_| mule_none()).collect();
    to_py_owned(mule_tuple(items.as_ptr(), items.len()))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyTuple_Size(op: *mut PyObject) -> Py_ssize_t {
    let val = from_py_borrowed(op);
    if val.is_null() || object::is_fixnum(val) {
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    unsafe {
        match &(*val).data {
            Data::Args(items, ..) => items.len() as Py_ssize_t,
            _ => PyObject_Length(op),
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyTuple_GetItem(
    op: *mut PyObject,
    i: Py_ssize_t,
) -> *mut PyObject {
    let val = from_py_borrowed(op);
    if val.is_null() || object::is_fixnum(val) {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    unsafe {
        if let Data::Args(items, ..) = &(*val).data {
            if i < 0 || (i as usize) >= items.len() {
                PyErr_SetString(
                    PyExc_IndexError,
                    b"tuple index out of range\0".as_ptr() as *const c_char,
                );
                return ptr::null_mut();
            }
            return to_py_borrowed(items[i as usize]);
        }
        PyErr_BadInternalCall();
        ptr::null_mut()
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyTuple_SetItem(
    op: *mut PyObject,
    i: Py_ssize_t,
    newitem: *mut PyObject,
) -> c_int {
    let val = from_py_borrowed(op);
    if val.is_null() || object::is_fixnum(val) {
        if !newitem.is_null() {
            unsafe { Py_DecRef(newitem) };
        }
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    unsafe {
        if let Data::Args(items, ..) = &mut (*val).data {
            if i < 0 || (i as usize) >= items.len() {
                if !newitem.is_null() {
                    Py_DecRef(newitem);
                }
                PyErr_SetString(
                    PyExc_IndexError,
                    b"tuple assignment index out of range\0".as_ptr() as *const c_char,
                );
                return -1;
            }
            let old = items[i as usize];
            items[i as usize] = if newitem.is_null() {
                let n = mule_none();
                object::retain(n);
                n
            } else {
                from_py_borrowed(newitem)
            };
            if !old.is_null() {
                object::release(old);
            }
            return 0;
        }
        if !newitem.is_null() {
            Py_DecRef(newitem);
        }
        PyErr_BadInternalCall();
        -1
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyTuple_GetSlice(
    op: *mut PyObject,
    low: Py_ssize_t,
    high: Py_ssize_t,
) -> *mut PyObject {
    let val = from_py_borrowed(op);
    if val.is_null() || object::is_fixnum(val) {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    unsafe {
        if let Data::Args(items, ..) = &(*val).data {
            let len = items.len() as Py_ssize_t;
            let l = low.clamp(0, len) as usize;
            let h = high.clamp(0, len).max(l as Py_ssize_t) as usize;
            let slice = &items[l..h];
            return to_py_owned(mule_tuple(slice.as_ptr(), slice.len()));
        }
        PyErr_BadInternalCall();
        ptr::null_mut()
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyList_New(size: Py_ssize_t) -> *mut PyObject {
    if size < 0 {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let items: Vec<Value> = (0..size as usize).map(|_| mule_none()).collect();
    to_py_owned(mule_list(items.as_ptr(), items.len()))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyList_Size(op: *mut PyObject) -> Py_ssize_t {
    let val = from_py_borrowed(op);
    if val.is_null() || object::is_fixnum(val) {
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    unsafe {
        match &(*val).data {
            Data::List(items, ..) => items.len() as Py_ssize_t,
            _ => PyObject_Length(op),
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyList_GetItem(
    op: *mut PyObject,
    i: Py_ssize_t,
) -> *mut PyObject {
    let val = from_py_borrowed(op);
    if val.is_null() || object::is_fixnum(val) {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    unsafe {
        if let Data::List(items, ..) = &(*val).data {
            if i < 0 || (i as usize) >= items.len() {
                PyErr_SetString(
                    PyExc_IndexError,
                    b"list index out of range\0".as_ptr() as *const c_char,
                );
                return ptr::null_mut();
            }
            return to_py_borrowed(items[i as usize]);
        }
        PyErr_BadInternalCall();
        ptr::null_mut()
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyList_SetItem(
    op: *mut PyObject,
    i: Py_ssize_t,
    newitem: *mut PyObject,
) -> c_int {
    let val = from_py_borrowed(op);
    if val.is_null() || object::is_fixnum(val) {
        if !newitem.is_null() {
            unsafe { Py_DecRef(newitem) };
        }
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    unsafe {
        if let Data::List(items, ..) = &mut (*val).data {
            if i < 0 || (i as usize) >= items.len() {
                if !newitem.is_null() {
                    Py_DecRef(newitem);
                }
                PyErr_SetString(
                    PyExc_IndexError,
                    b"list assignment index out of range\0".as_ptr() as *const c_char,
                );
                return -1;
            }
            let old = items[i as usize];
            items[i as usize] = if newitem.is_null() {
                let n = mule_none();
                object::retain(n);
                n
            } else {
                from_py_borrowed(newitem)
            };
            if !old.is_null() {
                object::release(old);
            }
            return 0;
        }
        if !newitem.is_null() {
            Py_DecRef(newitem);
        }
        PyErr_BadInternalCall();
        -1
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyList_Append(
    op: *mut PyObject,
    newitem: *mut PyObject,
) -> c_int {
    let val = from_py_borrowed(op);
    let item = from_py_borrowed(newitem);
    if val.is_null() || item.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    mule_list_push(val, item);
    if has_pending_exception() {
        -1
    } else {
        0
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyList_Insert(
    op: *mut PyObject,
    where_idx: Py_ssize_t,
    newitem: *mut PyObject,
) -> c_int {
    let val = from_py_borrowed(op);
    let item = from_py_borrowed(newitem);
    if val.is_null() || item.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    let idx = mule_int(where_idx as i64);
    let res = unsafe { object::special(val, "insert", &[idx, item]) };
    unsafe { object::release(idx) };
    if res.is_null() || has_pending_exception() {
        return -1;
    }
    unsafe { object::release(res) };
    0
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyList_GetSlice(
    op: *mut PyObject,
    low: Py_ssize_t,
    high: Py_ssize_t,
) -> *mut PyObject {
    let val = from_py_borrowed(op);
    if val.is_null() || object::is_fixnum(val) {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    unsafe {
        if let Data::List(items, ..) = &(*val).data {
            let len = items.len() as Py_ssize_t;
            let l = low.clamp(0, len) as usize;
            let h = high.clamp(0, len).max(l as Py_ssize_t) as usize;
            let slice = &items[l..h];
            return to_py_owned(mule_list(slice.as_ptr(), slice.len()));
        }
        PyErr_BadInternalCall();
        ptr::null_mut()
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyList_AsTuple(op: *mut PyObject) -> *mut PyObject {
    unsafe { PySequence_Tuple(op) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyList_Sort(op: *mut PyObject) -> c_int {
    let val = from_py_borrowed(op);
    let res = unsafe { object::special(val, "sort", &[]) };
    if res.is_null() || has_pending_exception() {
        return -1;
    }
    unsafe { object::release(res) };
    0
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyList_Reverse(op: *mut PyObject) -> c_int {
    let val = from_py_borrowed(op);
    let res = unsafe { object::special(val, "reverse", &[]) };
    if res.is_null() || has_pending_exception() {
        return -1;
    }
    unsafe { object::release(res) };
    0
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyDict_New() -> *mut PyObject {
    let d = unsafe { object::call(classes::builtin_type(Builtin::Dict), &[]) };
    if d.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(d)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyDict_Size(mp: *mut PyObject) -> Py_ssize_t {
    unsafe { PyObject_Length(mp) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyDict_GetItemWithError(
    mp: *mut PyObject,
    key: *mut PyObject,
) -> *mut PyObject {
    let d = from_py_borrowed(mp);
    let k = from_py_borrowed(key);
    if d.is_null() || k.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let res = unsafe { object::special(d, "__getitem__", &[k]) };
    if res.is_null() || has_pending_exception() {
        unsafe {
            if PyErr_ExceptionMatches(PyExc_KeyError) != 0 {
                PyErr_Clear();
            }
        }
        return ptr::null_mut();
    }
    let py = to_py_borrowed(res);
    unsafe { object::release(res) };
    py
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyDict_GetItem(
    mp: *mut PyObject,
    key: *mut PyObject,
) -> *mut PyObject {
    let saved = object::save_error();
    let res = unsafe { PyDict_GetItemWithError(mp, key) };
    object::clear_error();
    object::restore_error(saved);
    res
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyDict_GetItemString(
    mp: *mut PyObject,
    key: *const c_char,
) -> *mut PyObject {
    let s = unsafe { CStr::from_ptr(key).to_bytes() };
    let k = mule_string(s.as_ptr(), s.len());
    let res = unsafe { PyDict_GetItem(mp, to_py_borrowed(k)) };
    unsafe { object::release(k) };
    res
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyDict_SetItem(
    mp: *mut PyObject,
    key: *mut PyObject,
    item: *mut PyObject,
) -> c_int {
    let d = from_py_borrowed(mp);
    let k = from_py_borrowed(key);
    let v = from_py_borrowed(item);
    if d.is_null() || k.is_null() || v.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    let res = unsafe { object::special(d, "__setitem__", &[k, v]) };
    if res.is_null() || has_pending_exception() {
        return -1;
    }
    unsafe { object::release(res) };
    0
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyDict_SetItemString(
    mp: *mut PyObject,
    key: *const c_char,
    item: *mut PyObject,
) -> c_int {
    let s = unsafe { CStr::from_ptr(key).to_bytes() };
    let k = mule_string(s.as_ptr(), s.len());
    let rc = unsafe { PyDict_SetItem(mp, to_py_borrowed(k), item) };
    unsafe { object::release(k) };
    rc
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyDict_DelItem(
    mp: *mut PyObject,
    key: *mut PyObject,
) -> c_int {
    let d = from_py_borrowed(mp);
    let k = from_py_borrowed(key);
    if d.is_null() || k.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    let res = unsafe { object::special(d, "__delitem__", &[k]) };
    if res.is_null() || has_pending_exception() {
        return -1;
    }
    unsafe { object::release(res) };
    0
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyDict_DelItemString(
    mp: *mut PyObject,
    key: *const c_char,
) -> c_int {
    let s = unsafe { CStr::from_ptr(key).to_bytes() };
    let k = mule_string(s.as_ptr(), s.len());
    let rc = unsafe { PyDict_DelItem(mp, to_py_borrowed(k)) };
    unsafe { object::release(k) };
    rc
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyDict_Contains(
    mp: *mut PyObject,
    key: *mut PyObject,
) -> c_int {
    let d = from_py_borrowed(mp);
    let k = from_py_borrowed(key);
    if d.is_null() || k.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return -1;
    }
    let res = unsafe { object::special(d, "__contains__", &[k]) };
    if res.is_null() || has_pending_exception() {
        return -1;
    }
    let b = is_truthy(res);
    unsafe { object::release(res) };
    c_int::from(b)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyDict_Copy(mp: *mut PyObject) -> *mut PyObject {
    let d = from_py_borrowed(mp);
    let res = unsafe { object::special(d, "copy", &[]) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyDict_Clear(mp: *mut PyObject) {
    let d = from_py_borrowed(mp);
    let res = unsafe { object::special(d, "clear", &[]) };
    if !res.is_null() {
        unsafe { object::release(res) };
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyDict_Update(
    mp: *mut PyObject,
    other: *mut PyObject,
) -> c_int {
    let d = from_py_borrowed(mp);
    let o = from_py_borrowed(other);
    let res = unsafe { object::special(d, "update", &[o]) };
    if res.is_null() || has_pending_exception() {
        return -1;
    }
    unsafe { object::release(res) };
    0
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyDict_Keys(mp: *mut PyObject) -> *mut PyObject {
    unsafe { PyMapping_Keys(mp) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyDict_Values(mp: *mut PyObject) -> *mut PyObject {
    unsafe { PyMapping_Values(mp) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyDict_Items(mp: *mut PyObject) -> *mut PyObject {
    unsafe { PyMapping_Items(mp) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyDict_Next(
    mp: *mut PyObject,
    ppos: *mut Py_ssize_t,
    pkey: *mut *mut PyObject,
    pvalue: *mut *mut PyObject,
) -> c_int {
    if mp.is_null() || ppos.is_null() {
        return 0;
    }
    unsafe {
        let d = from_py_borrowed(mp);
        let keys_view = object::special(d, "keys", &[]);
        if keys_view.is_null() || has_pending_exception() {
            PyErr_Clear();
            return 0;
        }
        let keys_list = object::call(classes::builtin_type(Builtin::List), &[keys_view]);
        object::release(keys_view);
        if keys_list.is_null() || has_pending_exception() {
            PyErr_Clear();
            return 0;
        }
        let pos = *ppos;
        let items = match &(*keys_list).data {
            Data::List(v, ..) => v.to_vec(),
            _ => Vec::new(),
        };
        object::release(keys_list);
        if pos < 0 || (pos as usize) >= items.len() {
            return 0;
        }
        *ppos = pos + 1;
        let k = items[pos as usize];
        let v = object::special(d, "__getitem__", &[k]);
        if v.is_null() || has_pending_exception() {
            PyErr_Clear();
            return 0;
        }
        if !pkey.is_null() {
            *pkey = to_py_borrowed(k);
        }
        if !pvalue.is_null() {
            *pvalue = to_py_borrowed(v);
        }
        object::release(v);
        1
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySet_New(iterable: *mut PyObject) -> *mut PyObject {
    let cls = protocol_lookup("set");
    let res = unsafe {
        if iterable.is_null() {
            object::call(cls, &[])
        } else {
            object::call(cls, &[from_py_borrowed(iterable)])
        }
    };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyFrozenSet_New(iterable: *mut PyObject) -> *mut PyObject {
    let cls = protocol_lookup("frozenset");
    let res = unsafe {
        if iterable.is_null() {
            object::call(cls, &[])
        } else {
            object::call(cls, &[from_py_borrowed(iterable)])
        }
    };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySet_Size(anyset: *mut PyObject) -> Py_ssize_t {
    unsafe { PyObject_Length(anyset) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySet_Contains(
    anyset: *mut PyObject,
    key: *mut PyObject,
) -> c_int {
    unsafe { PySequence_Contains(anyset, key) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySet_Add(
    set: *mut PyObject,
    key: *mut PyObject,
) -> c_int {
    let s = from_py_borrowed(set);
    let k = from_py_borrowed(key);
    let res = unsafe { object::special(s, "add", &[k]) };
    if res.is_null() || has_pending_exception() {
        return -1;
    }
    unsafe { object::release(res) };
    0
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySet_Discard(
    set: *mut PyObject,
    key: *mut PyObject,
) -> c_int {
    let c = unsafe { PySet_Contains(set, key) };
    if c < 0 {
        return -1;
    }
    let s = from_py_borrowed(set);
    let k = from_py_borrowed(key);
    let res = unsafe { object::special(s, "discard", &[k]) };
    if res.is_null() || has_pending_exception() {
        return -1;
    }
    unsafe { object::release(res) };
    c
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySet_Pop(set: *mut PyObject) -> *mut PyObject {
    let s = from_py_borrowed(set);
    let res = unsafe { object::special(s, "pop", &[]) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySet_Clear(set: *mut PyObject) -> c_int {
    let s = from_py_borrowed(set);
    let res = unsafe { object::special(s, "clear", &[]) };
    if res.is_null() || has_pending_exception() {
        return -1;
    }
    unsafe { object::release(res) };
    0
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySlice_New(
    start: *mut PyObject,
    stop: *mut PyObject,
    step: *mut PyObject,
) -> *mut PyObject {
    let a = if start.is_null() {
        mule_none()
    } else {
        from_py_borrowed(start)
    };
    let b = if stop.is_null() {
        mule_none()
    } else {
        from_py_borrowed(stop)
    };
    let c = if step.is_null() {
        mule_none()
    } else {
        from_py_borrowed(step)
    };
    let slice_fn = protocol_lookup("make_slice");
    let res = unsafe { object::call(slice_fn, &[a, b, c]) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyMethod_New(
    func: *mut PyObject,
    self_obj: *mut PyObject,
) -> *mut PyObject {
    let f = from_py_borrowed(func);
    let s = from_py_borrowed(self_obj);
    if f.is_null() || s.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let m = unsafe {
        object::new_value(Data::BoundMethod {
            receiver: object::retain(s),
            method: object::retain(f),
        })
    };
    to_py_owned(m)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyMethod_Function(im: *mut PyObject) -> *mut PyObject {
    let val = from_py_borrowed(im);
    let f = get_attr_str(val, "__func__");
    if f.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    let py = to_py_borrowed(f);
    unsafe { object::release(f) };
    py
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyMethod_Self(im: *mut PyObject) -> *mut PyObject {
    let val = from_py_borrowed(im);
    let s = get_attr_str(val, "__self__");
    if s.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    let py = to_py_borrowed(s);
    unsafe { object::release(s) };
    py
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyInstanceMethod_New(func: *mut PyObject) -> *mut PyObject {
    unsafe { Py_IncRef(func) };
    func
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyCapsule_New(
    pointer: *mut c_void,
    name: *const c_char,
    destructor: Option<unsafe extern "C" fn(*mut PyObject)>,
) -> *mut PyObject {
    ensure_initialized();
    if pointer.is_null() {
        unsafe {
            PyErr_SetString(
                PyExc_ValueError,
                b"PyCapsule_New called with null pointer\0".as_ptr() as *const c_char,
            );
        }
        return ptr::null_mut();
    }
    let val = unsafe { object::new_value(Data::None) };
    let op = unsafe { calloc(1, std::mem::size_of::<PyObject>()) as *mut PyObject };
    unsafe {
        (*op).ob_refcnt = 1;
        (*op).ob_type = &raw mut PyCapsule_Type;
        (*op).ob_mule = val;
    }
    with_state(|s| {
        s.val_to_py.insert(val as usize, op);
        s.capsules.insert(
            op as usize,
            CapsuleData {
                pointer,
                name,
                context: ptr::null_mut(),
                destructor,
            },
        );
    });
    op
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyCapsule_GetPointer(
    capsule: *mut PyObject,
    _name: *const c_char,
) -> *mut c_void {
    with_state(|s| {
        s.capsules
            .get(&(capsule as usize))
            .map(|c| c.pointer)
            .unwrap_or(ptr::null_mut())
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyCapsule_GetName(capsule: *mut PyObject) -> *const c_char {
    with_state(|s| {
        s.capsules
            .get(&(capsule as usize))
            .map(|c| c.name)
            .unwrap_or(ptr::null())
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyCapsule_GetContext(capsule: *mut PyObject) -> *mut c_void {
    with_state(|s| {
        s.capsules
            .get(&(capsule as usize))
            .map(|c| c.context)
            .unwrap_or(ptr::null_mut())
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyCapsule_SetContext(
    capsule: *mut PyObject,
    context: *mut c_void,
) -> c_int {
    with_state(|s| {
        if let Some(c) = s.capsules.get_mut(&(capsule as usize)) {
            c.context = context;
            0
        } else {
            -1
        }
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyCapsule_Import(
    name: *const c_char,
    _no_block: c_int,
) -> *mut c_void {
    if name.is_null() {
        return ptr::null_mut();
    }
    let full = unsafe { CStr::from_ptr(name).to_str().unwrap_or("") };
    let Some((mod_name, attr_name)) = full.rsplit_once('.') else {
        return ptr::null_mut();
    };
    let c_mod = CString::new(mod_name).unwrap_or_default();
    let m = unsafe { PyImport_ImportModule(c_mod.as_ptr()) };
    if m.is_null() {
        return ptr::null_mut();
    }
    let c_attr = CString::new(attr_name).unwrap_or_default();
    let cap = unsafe { PyObject_GetAttrString(m, c_attr.as_ptr()) };
    unsafe { Py_DecRef(m) };
    if cap.is_null() {
        return ptr::null_mut();
    }
    let ptr = unsafe { PyCapsule_GetPointer(cap, name) };
    unsafe { Py_DecRef(cap) };
    ptr
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyImport_ImportModule(name: *const c_char) -> *mut PyObject {
    let u = unsafe { PyUnicode_FromString(name) };
    let res = unsafe { PyImport_Import(u) };
    unsafe { Py_DecRef(u) };
    res
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyImport_Import(name: *mut PyObject) -> *mut PyObject {
    let val = from_py_borrowed(name);
    if val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let s = string_value(val);
    let Ok(c_name) = CString::new(s) else {
        unsafe {
            PyErr_SetString(
                PyExc_ValueError,
                b"embedded null byte\0".as_ptr() as *const c_char,
            );
        }
        return ptr::null_mut();
    };
    let res = unsafe { crate::builtins::mule_import_missing(c_name.as_ptr()) };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyImport_GetModuleDict() -> *mut PyObject {
    let Some(sys) = (unsafe {
        crate::builtins::find_loaded_module("sys")
            .ok()
            .flatten()
            .or_else(|| crate::imports::import_or_compile_module("sys"))
    }) else {
        return ptr::null_mut();
    };
    let mods = get_attr_str(sys, "modules");
    unsafe { object::release(sys) };
    if mods.is_null() {
        return ptr::null_mut();
    }
    let py = to_py_borrowed(mods);
    unsafe { object::release(mods) };
    py
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySys_GetObject(name: *const c_char) -> *mut PyObject {
    if name.is_null() {
        return ptr::null_mut();
    }
    let Some(sys) = (unsafe {
        crate::builtins::find_loaded_module("sys")
            .ok()
            .flatten()
            .or_else(|| crate::imports::import_or_compile_module("sys"))
    }) else {
        return ptr::null_mut();
    };
    let attr = unsafe { classes::mule_get_attr(sys, name) };
    unsafe { object::release(sys) };
    if attr.is_null() || has_pending_exception() {
        unsafe { PyErr_Clear() };
        return ptr::null_mut();
    }
    let py = to_py_borrowed(attr);
    unsafe { object::release(attr) };
    py
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PySys_SetObject(
    name: *const c_char,
    v: *mut PyObject,
) -> c_int {
    let Some(sys) = (unsafe {
        crate::builtins::find_loaded_module("sys")
            .ok()
            .flatten()
            .or_else(|| crate::imports::import_or_compile_module("sys"))
    }) else {
        return -1;
    };
    let rc = unsafe { PyObject_SetAttrString(to_py_borrowed(sys), name, v) };
    unsafe { object::release(sys) };
    rc
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyEval_GetBuiltins() -> *mut PyObject {
    let b = unsafe { crate::builtins::mule_builtins_module() };
    if b.is_null() {
        return ptr::null_mut();
    }
    let py = unsafe { PyModule_GetDict(to_py_borrowed(b)) };
    unsafe { object::release(b) };
    py
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyEval_GetGlobals() -> *mut PyObject {
    let g = mule_protocol_globals();
    if g.is_null() {
        return ptr::null_mut();
    }
    unsafe { PyModule_GetDict(to_py_borrowed(g)) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyEval_SaveThread() -> *mut c_void {
    1usize as *mut c_void
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyEval_RestoreThread(_tstate: *mut c_void) {}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyGILState_Ensure() -> c_int {
    0
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyGILState_Release(_state: c_int) {}


// ===========================================================================
// Callable / Method / Type / Module Construction
// ===========================================================================

type RawCFunction = unsafe extern "C" fn(*mut PyObject, *mut PyObject) -> *mut PyObject;
type RawCFunctionKw =
    unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> *mut PyObject;
type RawGetter = unsafe extern "C" fn(*mut PyObject, *mut c_void) -> *mut PyObject;
type RawSetter = unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut c_void) -> c_int;
type RawInitproc = unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> c_int;
type RawNewfunc =
    unsafe extern "C" fn(*mut PyTypeObject, *mut PyObject, *mut PyObject) -> *mut PyObject;
type RawTernary =
    unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> *mut PyObject;
type RawRepr = unsafe extern "C" fn(*mut PyObject) -> *mut PyObject;
type RawHash = unsafe extern "C" fn(*mut PyObject) -> Py_hash_t;
type RawGetattro = unsafe extern "C" fn(*mut PyObject, *mut PyObject) -> *mut PyObject;
type RawSetattro = unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> c_int;
type RawDescrGet =
    unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> *mut PyObject;
type RawDescrSet = unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> c_int;
type RawRichcmp = unsafe extern "C" fn(*mut PyObject, *mut PyObject, c_int) -> *mut PyObject;
type RawUnary = unsafe extern "C" fn(*mut PyObject) -> *mut PyObject;
type RawBinary = unsafe extern "C" fn(*mut PyObject, *mut PyObject) -> *mut PyObject;
type RawInquiry = unsafe extern "C" fn(*mut PyObject) -> c_int;
type RawLen = unsafe extern "C" fn(*mut PyObject) -> Py_ssize_t;
type RawSsizearg = unsafe extern "C" fn(*mut PyObject, Py_ssize_t) -> *mut PyObject;
type RawSsizeObjArg = unsafe extern "C" fn(*mut PyObject, Py_ssize_t, *mut PyObject) -> c_int;
type RawObjObjProc = unsafe extern "C" fn(*mut PyObject, *mut PyObject) -> c_int;
type RawObjObjArgProc =
    unsafe extern "C" fn(*mut PyObject, *mut PyObject, *mut PyObject) -> c_int;

#[derive(Clone)]
enum CCallableKind {
    Method {
        func: RawCFunction,
        flags: c_int,
        self_py: *mut PyObject,
        is_type_method: bool,
    },
    Getter(RawGetter),
    Setter(RawSetter),
    MemberGet {
        member_type: c_int,
        offset: Py_ssize_t,
    },
    MemberSet {
        member_type: c_int,
        offset: Py_ssize_t,
        readonly: c_int,
    },
    SlotInit(RawInitproc),
    SlotNew(RawNewfunc),
    SlotCall(RawTernary),
    SlotRepr(RawRepr),
    SlotHash(RawHash),
    SlotGetattro(RawGetattro),
    SlotSetattro(RawSetattro),
    SlotDescrGet(RawDescrGet),
    SlotDescrSet(RawDescrSet),
    SlotRichcmp(RawRichcmp, c_int),
    SlotUnary(RawUnary),
    SlotBinary(RawBinary),
    SlotBinaryRev(RawBinary),
    SlotTernary(RawTernary),
    SlotTernaryRev(RawTernary),
    SlotInquiryBool(RawInquiry),
    SlotLen(RawLen),
    SlotSsizearg(RawSsizearg),
    SlotSsizeObjArg(RawSsizeObjArg),
    SlotObjObjProc(RawObjObjProc),
    SlotObjObjArgProc(RawObjObjArgProc),
}

#[derive(Clone)]
struct CCallableInfo {
    kind: CCallableKind,
    #[allow(dead_code)]
    name: &'static str,
    #[allow(dead_code)]
    doc: *const c_char,
    closure: *mut c_void,
}

unsafe impl Send for CCallableInfo {}
unsafe impl Sync for CCallableInfo {}

static CALLABLES: Mutex<Vec<CCallableInfo>> = Mutex::new(Vec::new());

const T_OBJECT_EX: c_int = Py_T_OBJECT_EX;
const T_OBJECT: c_int = _Py_T_OBJECT;
const T_INT: c_int = Py_T_INT;
const T_LONG: c_int = Py_T_LONG;
const T_PYSSIZET: c_int = Py_T_PYSSIZET;
const T_BOOL: c_int = Py_T_BOOL;
const READONLY: c_int = Py_READONLY;
const PYTHON_API_VERSION: c_int = 1013;
const RTLD_GLOBAL: c_int = 0x100;

#[inline]
unsafe fn py_none() -> *mut PyObject {
    &raw mut _Py_NoneStruct
}

#[inline]
fn make_str_from_rust(s: &str) -> Value {
    mule_string(s.as_ptr(), s.len())
}

#[inline]
fn make_tuple_from_slice(items: &[Value]) -> Value {
    mule_tuple(items.as_ptr(), items.len())
}

#[inline]
unsafe fn is_tuple_value(v: Value) -> bool {
    !v.is_null() && !object::is_fixnum(v) && unsafe { matches!(&(*v).data, Data::Args(..)) }
}

#[inline]
unsafe fn tuple_len(v: Value) -> usize {
    if !v.is_null() && !object::is_fixnum(v) {
        if let Data::Args(items, ..) = unsafe { &(*v).data } {
            return items.len();
        }
    }
    0
}

#[inline]
unsafe fn tuple_item(v: Value, i: usize) -> Value {
    if !v.is_null() && !object::is_fixnum(v) {
        if let Data::Args(items, ..) = unsafe { &(*v).data } {
            return items.get(i).copied().unwrap_or(ptr::null_mut());
        }
    }
    ptr::null_mut()
}

#[inline]
unsafe fn set_class_attr(cls: Value, name: &str, val: Value) {
    let _ = unsafe { classes::set_c_class_attribute(cls, name, val) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_ASCII(v: *mut PyObject) -> *mut PyObject {
    unsafe { PyObject_Repr(v) }
}

unsafe extern "C" fn c_callable_entry(_globals: Value, view: Value) -> Value {
    unsafe {
        let defaults_val = if !view.is_null() && !object::is_fixnum(view) {
            match &(*view).data {
                Data::ArgumentView { defaults, .. }
                | Data::ReceiverArgs { defaults, .. }
                | Data::KeywordArgs { defaults, .. } => *defaults,
                _ => ptr::null_mut(),
            }
        } else {
            ptr::null_mut()
        };
        let callable_id = if object::is_fixnum(defaults_val) {
            object::fixnum(defaults_val) as usize
        } else {
            0
        };

        let info = {
            let guard = CALLABLES.lock().unwrap_or_else(|e| e.into_inner());
            guard.get(callable_id).cloned()
        };
        let Some(info) = info else {
            return ptr::null_mut();
        };

        let argc = object::argument_count(view).unwrap_or(0);
        let mut flat_pos = Vec::with_capacity(argc);
        for i in 0..argc {
            if let Some(arg) = object::argument(view, i) {
                flat_pos.push(arg);
            }
        }

        let kw_count = crate::arguments::keyword_count(view);
        let mut kw_py: *mut PyObject = ptr::null_mut();
        if kw_count > 0 && !view.is_null() && !object::is_fixnum(view) {
            if let Data::KeywordArgs {
                names,
                keyword_values,
                keyword_count,
                ..
            } = &(*view).data
            {
                kw_py = PyDict_New();
                for j in 0..*keyword_count {
                    PyDict_SetItemString(
                        kw_py,
                        *names.add(j),
                        to_py_borrowed(*keyword_values.add(j)),
                    );
                }
            }
        }

        let res = match info.kind {
            CCallableKind::Method {
                func,
                flags,
                self_py,
                is_type_method,
            } => {
                let (actual_self, args_slice) = if !self_py.is_null() {
                    (self_py, &flat_pos[..])
                } else if (flags & METH_STATIC) != 0 {
                    (ptr::null_mut(), &flat_pos[..])
                } else if is_type_method {
                    if flat_pos.is_empty() {
                        PyErr_SetString(
                            PyExc_TypeError,
                            c"descriptor needs an argument".as_ptr(),
                        );
                        (ptr::null_mut(), &flat_pos[..])
                    } else {
                        (to_py_borrowed(flat_pos[0]), &flat_pos[1..])
                    }
                } else {
                    (ptr::null_mut(), &flat_pos[..])
                };

                if !PyErr_Occurred().is_null() {
                    ptr::null_mut()
                } else {
                    let call_flags =
                        flags & (METH_VARARGS | METH_KEYWORDS | METH_NOARGS | METH_O | METH_FASTCALL);
                    if (call_flags & METH_FASTCALL) != 0 && (call_flags & METH_KEYWORDS) == 0 {
                        let py_args: Vec<*mut PyObject> =
                            args_slice.iter().map(|&v| to_py_borrowed(v)).collect();
                        let f: unsafe extern "C" fn(
                            *mut PyObject,
                            *const *mut PyObject,
                            Py_ssize_t,
                        ) -> *mut PyObject = std::mem::transmute(func);
                        f(actual_self, py_args.as_ptr(), py_args.len() as Py_ssize_t)
                    } else if (call_flags & METH_KEYWORDS) != 0 {
                        let tup = make_py_tuple_from_values(args_slice);
                        let f: RawCFunctionKw = std::mem::transmute(func);
                        let r = f(actual_self, tup, kw_py);
                        Py_DecRef(tup);
                        r
                    } else if (call_flags & METH_NOARGS) != 0 {
                        if !args_slice.is_empty() || !kw_py.is_null() {
                            PyErr_SetString(
                                PyExc_TypeError,
                                c"function takes no arguments".as_ptr(),
                            );
                            ptr::null_mut()
                        } else {
                            func(actual_self, ptr::null_mut())
                        }
                    } else if (call_flags & METH_O) != 0 {
                        if args_slice.len() != 1 || !kw_py.is_null() {
                            PyErr_SetString(
                                PyExc_TypeError,
                                c"function takes exactly one argument".as_ptr(),
                            );
                            ptr::null_mut()
                        } else {
                            func(actual_self, to_py_borrowed(args_slice[0]))
                        }
                    } else if !kw_py.is_null() {
                        PyErr_SetString(
                            PyExc_TypeError,
                            c"function takes no keyword arguments".as_ptr(),
                        );
                        ptr::null_mut()
                    } else {
                        let tup = make_py_tuple_from_values(args_slice);
                        let r = func(actual_self, tup);
                        Py_DecRef(tup);
                        r
                    }
                }
            }
            CCallableKind::Getter(getter) => {
                if flat_pos.is_empty() {
                    ptr::null_mut()
                } else {
                    getter(to_py_borrowed(flat_pos[0]), info.closure)
                }
            }
            CCallableKind::Setter(setter) => {
                if flat_pos.len() == 1 {
                    let rc = setter(to_py_borrowed(flat_pos[0]), ptr::null_mut(), info.closure);
                    if rc < 0 {
                        ptr::null_mut()
                    } else {
                        let n = py_none();
                        Py_IncRef(n);
                        n
                    }
                } else if flat_pos.len() >= 2 {
                    let rc = setter(
                        to_py_borrowed(flat_pos[0]),
                        to_py_borrowed(flat_pos[1]),
                        info.closure,
                    );
                    if rc < 0 {
                        ptr::null_mut()
                    } else {
                        let n = py_none();
                        Py_IncRef(n);
                        n
                    }
                } else {
                    ptr::null_mut()
                }
            }
            CCallableKind::MemberGet {
                member_type,
                offset,
            } => {
                if flat_pos.is_empty() {
                    ptr::null_mut()
                } else {
                    let self_py = to_py_borrowed(flat_pos[0]);
                    let base = (self_py as *const u8).add(offset as usize);
                    match member_type {
                        T_OBJECT_EX => {
                            let p = *(base as *const *mut PyObject);
                            if p.is_null() {
                                PyErr_SetString(
                                    PyExc_AttributeError,
                                    c"attribute is not set".as_ptr(),
                                );
                                ptr::null_mut()
                            } else {
                                Py_IncRef(p);
                                p
                            }
                        }
                        T_OBJECT => {
                            let p = *(base as *const *mut PyObject);
                            if p.is_null() {
                                let n = py_none();
                                Py_IncRef(n);
                                n
                            } else {
                                Py_IncRef(p);
                                p
                            }
                        }
                        T_INT => PyLong_FromLong(*(base as *const c_int) as c_long),
                        T_LONG => PyLong_FromLong(*(base as *const c_long)),
                        T_PYSSIZET => PyLong_FromSsize_t(*(base as *const Py_ssize_t)),
                        T_BOOL => PyBool_FromLong(*(base as *const c_char) as c_long),
                        _ => {
                            let n = py_none();
                            Py_IncRef(n);
                            n
                        }
                    }
                }
            }
            CCallableKind::MemberSet {
                member_type,
                offset,
                readonly,
            } => {
                if readonly != 0 {
                    PyErr_SetString(PyExc_AttributeError, c"readonly attribute".as_ptr());
                    ptr::null_mut()
                } else if flat_pos.is_empty() {
                    ptr::null_mut()
                } else {
                    let self_py = to_py_borrowed(flat_pos[0]);
                    let val_py = if flat_pos.len() >= 2 {
                        to_py_borrowed(flat_pos[1])
                    } else {
                        ptr::null_mut()
                    };
                    let base = (self_py as *mut u8).add(offset as usize);
                    match member_type {
                        T_OBJECT_EX | T_OBJECT => {
                            let slot = base as *mut *mut PyObject;
                            let old = *slot;
                            if !val_py.is_null() {
                                Py_IncRef(val_py);
                            }
                            *slot = val_py;
                            if !old.is_null() {
                                Py_DecRef(old);
                            }
                            let n = py_none();
                            Py_IncRef(n);
                            n
                        }
                        T_BOOL => {
                            if val_py.is_null() {
                                PyErr_SetString(
                                    PyExc_TypeError,
                                    c"cannot delete boolean attribute".as_ptr(),
                                );
                                ptr::null_mut()
                            } else {
                                let b = PyObject_IsTrue(val_py);
                                if b < 0 {
                                    ptr::null_mut()
                                } else {
                                    *(base as *mut c_char) = if b != 0 { 1 } else { 0 };
                                    let n = py_none();
                                    Py_IncRef(n);
                                    n
                                }
                            }
                        }
                        T_INT => {
                            let v = PyLong_AsLong(val_py);
                            if v == -1 && !PyErr_Occurred().is_null() {
                                ptr::null_mut()
                            } else {
                                *(base as *mut c_int) = v as c_int;
                                let n = py_none();
                                Py_IncRef(n);
                                n
                            }
                        }
                        T_LONG => {
                            let v = PyLong_AsLong(val_py);
                            if v == -1 && !PyErr_Occurred().is_null() {
                                ptr::null_mut()
                            } else {
                                *(base as *mut c_long) = v;
                                let n = py_none();
                                Py_IncRef(n);
                                n
                            }
                        }
                        T_PYSSIZET => {
                            let v = PyLong_AsSsize_t(val_py);
                            if v == -1 && !PyErr_Occurred().is_null() {
                                ptr::null_mut()
                            } else {
                                *(base as *mut Py_ssize_t) = v;
                                let n = py_none();
                                Py_IncRef(n);
                                n
                            }
                        }
                        _ => {
                            let n = py_none();
                            Py_IncRef(n);
                            n
                        }
                    }
                }
            }
            CCallableKind::SlotInit(initproc) => {
                if flat_pos.is_empty() {
                    ptr::null_mut()
                } else {
                    let self_py = to_py_borrowed(flat_pos[0]);
                    let tup = make_py_tuple_from_values(&flat_pos[1..]);
                    let rc = initproc(self_py, tup, kw_py);
                    Py_DecRef(tup);
                    if rc < 0 {
                        ptr::null_mut()
                    } else {
                        let n = py_none();
                        Py_IncRef(n);
                        n
                    }
                }
            }
            CCallableKind::SlotNew(newfunc) => {
                if flat_pos.is_empty() {
                    ptr::null_mut()
                } else {
                    let cls_py = to_py_borrowed(flat_pos[0]) as *mut PyTypeObject;
                    let tup = make_py_tuple_from_values(&flat_pos[1..]);
                    let r = newfunc(cls_py, tup, kw_py);
                    Py_DecRef(tup);
                    r
                }
            }
            CCallableKind::SlotCall(callfunc) => {
                if flat_pos.is_empty() {
                    ptr::null_mut()
                } else {
                    let self_py = to_py_borrowed(flat_pos[0]);
                    let tup = make_py_tuple_from_values(&flat_pos[1..]);
                    let r = callfunc(self_py, tup, kw_py);
                    Py_DecRef(tup);
                    r
                }
            }
            CCallableKind::SlotRepr(reprfunc) => {
                if flat_pos.is_empty() {
                    ptr::null_mut()
                } else {
                    reprfunc(to_py_borrowed(flat_pos[0]))
                }
            }
            CCallableKind::SlotHash(hashfunc) => {
                if flat_pos.is_empty() {
                    ptr::null_mut()
                } else {
                    let h = hashfunc(to_py_borrowed(flat_pos[0]));
                    if h == -1 && !PyErr_Occurred().is_null() {
                        ptr::null_mut()
                    } else {
                        PyLong_FromSsize_t(h)
                    }
                }
            }
            CCallableKind::SlotGetattro(getattrofunc) => {
                if flat_pos.len() < 2 {
                    ptr::null_mut()
                } else {
                    getattrofunc(to_py_borrowed(flat_pos[0]), to_py_borrowed(flat_pos[1]))
                }
            }
            CCallableKind::SlotSetattro(setattrofunc) => {
                if flat_pos.len() == 2 {
                    let rc = setattrofunc(
                        to_py_borrowed(flat_pos[0]),
                        to_py_borrowed(flat_pos[1]),
                        ptr::null_mut(),
                    );
                    if rc < 0 {
                        ptr::null_mut()
                    } else {
                        let n = py_none();
                        Py_IncRef(n);
                        n
                    }
                } else if flat_pos.len() >= 3 {
                    let rc = setattrofunc(
                        to_py_borrowed(flat_pos[0]),
                        to_py_borrowed(flat_pos[1]),
                        to_py_borrowed(flat_pos[2]),
                    );
                    if rc < 0 {
                        ptr::null_mut()
                    } else {
                        let n = py_none();
                        Py_IncRef(n);
                        n
                    }
                } else {
                    ptr::null_mut()
                }
            }
            CCallableKind::SlotDescrGet(descrgetfunc) => {
                if flat_pos.is_empty() {
                    ptr::null_mut()
                } else {
                    let self_py = to_py_borrowed(flat_pos[0]);
                    let obj_py = if flat_pos.len() >= 2 && flat_pos[1] != mule_none() {
                        to_py_borrowed(flat_pos[1])
                    } else {
                        ptr::null_mut()
                    };
                    let owner_py = if flat_pos.len() >= 3 && flat_pos[2] != mule_none() {
                        to_py_borrowed(flat_pos[2])
                    } else {
                        ptr::null_mut()
                    };
                    descrgetfunc(self_py, obj_py, owner_py)
                }
            }
            CCallableKind::SlotDescrSet(descrsetfunc) => {
                if flat_pos.len() == 2 {
                    let rc = descrsetfunc(
                        to_py_borrowed(flat_pos[0]),
                        to_py_borrowed(flat_pos[1]),
                        ptr::null_mut(),
                    );
                    if rc < 0 {
                        ptr::null_mut()
                    } else {
                        let n = py_none();
                        Py_IncRef(n);
                        n
                    }
                } else if flat_pos.len() >= 3 {
                    let rc = descrsetfunc(
                        to_py_borrowed(flat_pos[0]),
                        to_py_borrowed(flat_pos[1]),
                        to_py_borrowed(flat_pos[2]),
                    );
                    if rc < 0 {
                        ptr::null_mut()
                    } else {
                        let n = py_none();
                        Py_IncRef(n);
                        n
                    }
                } else {
                    ptr::null_mut()
                }
            }
            CCallableKind::SlotRichcmp(richcmpfunc, op) => {
                if flat_pos.len() < 2 {
                    ptr::null_mut()
                } else {
                    richcmpfunc(to_py_borrowed(flat_pos[0]), to_py_borrowed(flat_pos[1]), op)
                }
            }
            CCallableKind::SlotUnary(unaryfunc) => {
                if flat_pos.is_empty() {
                    ptr::null_mut()
                } else {
                    unaryfunc(to_py_borrowed(flat_pos[0]))
                }
            }
            CCallableKind::SlotBinary(binaryfunc) => {
                if flat_pos.len() < 2 {
                    ptr::null_mut()
                } else {
                    binaryfunc(to_py_borrowed(flat_pos[0]), to_py_borrowed(flat_pos[1]))
                }
            }
            CCallableKind::SlotBinaryRev(binaryfunc) => {
                if flat_pos.len() < 2 {
                    ptr::null_mut()
                } else {
                    binaryfunc(to_py_borrowed(flat_pos[1]), to_py_borrowed(flat_pos[0]))
                }
            }
            CCallableKind::SlotTernary(ternaryfunc) => {
                if flat_pos.len() < 2 {
                    ptr::null_mut()
                } else {
                    let z = if flat_pos.len() >= 3 {
                        to_py_borrowed(flat_pos[2])
                    } else {
                        py_none()
                    };
                    ternaryfunc(to_py_borrowed(flat_pos[0]), to_py_borrowed(flat_pos[1]), z)
                }
            }
            CCallableKind::SlotTernaryRev(ternaryfunc) => {
                if flat_pos.len() < 2 {
                    ptr::null_mut()
                } else {
                    let z = if flat_pos.len() >= 3 {
                        to_py_borrowed(flat_pos[2])
                    } else {
                        py_none()
                    };
                    ternaryfunc(to_py_borrowed(flat_pos[1]), to_py_borrowed(flat_pos[0]), z)
                }
            }
            CCallableKind::SlotInquiryBool(inquiry) => {
                if flat_pos.is_empty() {
                    ptr::null_mut()
                } else {
                    let r = inquiry(to_py_borrowed(flat_pos[0]));
                    if r < 0 {
                        ptr::null_mut()
                    } else {
                        PyBool_FromLong(r as c_long)
                    }
                }
            }
            CCallableKind::SlotLen(lenfunc) => {
                if flat_pos.is_empty() {
                    ptr::null_mut()
                } else {
                    let r = lenfunc(to_py_borrowed(flat_pos[0]));
                    if r < 0 {
                        ptr::null_mut()
                    } else {
                        PyLong_FromSsize_t(r)
                    }
                }
            }
            CCallableKind::SlotSsizearg(ssizeargfunc) => {
                if flat_pos.len() < 2 {
                    ptr::null_mut()
                } else {
                    let idx = PyLong_AsSsize_t(to_py_borrowed(flat_pos[1]));
                    if idx == -1 && !PyErr_Occurred().is_null() {
                        ptr::null_mut()
                    } else {
                        ssizeargfunc(to_py_borrowed(flat_pos[0]), idx)
                    }
                }
            }
            CCallableKind::SlotSsizeObjArg(ssizeobjargproc) => {
                if flat_pos.len() < 2 {
                    ptr::null_mut()
                } else {
                    let idx = PyLong_AsSsize_t(to_py_borrowed(flat_pos[1]));
                    if idx == -1 && !PyErr_Occurred().is_null() {
                        ptr::null_mut()
                    } else {
                        let val_py = if flat_pos.len() >= 3 {
                            to_py_borrowed(flat_pos[2])
                        } else {
                            ptr::null_mut()
                        };
                        let rc = ssizeobjargproc(to_py_borrowed(flat_pos[0]), idx, val_py);
                        if rc < 0 {
                            ptr::null_mut()
                        } else {
                            let n = py_none();
                            Py_IncRef(n);
                            n
                        }
                    }
                }
            }
            CCallableKind::SlotObjObjProc(objobjproc) => {
                if flat_pos.len() < 2 {
                    ptr::null_mut()
                } else {
                    let rc = objobjproc(to_py_borrowed(flat_pos[0]), to_py_borrowed(flat_pos[1]));
                    if rc < 0 {
                        ptr::null_mut()
                    } else {
                        PyBool_FromLong(rc as c_long)
                    }
                }
            }
            CCallableKind::SlotObjObjArgProc(objobjargproc) => {
                if flat_pos.len() < 2 {
                    ptr::null_mut()
                } else {
                    let val_py = if flat_pos.len() >= 3 {
                        to_py_borrowed(flat_pos[2])
                    } else {
                        ptr::null_mut()
                    };
                    let rc = objobjargproc(
                        to_py_borrowed(flat_pos[0]),
                        to_py_borrowed(flat_pos[1]),
                        val_py,
                    );
                    if rc < 0 {
                        ptr::null_mut()
                    } else {
                        let n = py_none();
                        Py_IncRef(n);
                        n
                    }
                }
            }
        };

        if !kw_py.is_null() {
            Py_DecRef(kw_py);
        }

        if res.is_null() {
            ptr::null_mut()
        } else {
            from_py_borrowed(res)
        }
    }
}

unsafe fn make_py_tuple_from_values(vals: &[Value]) -> *mut PyObject {
    unsafe {
        let tup = PyTuple_New(vals.len() as Py_ssize_t);
        for (i, &v) in vals.iter().enumerate() {
            let py_v = to_py_borrowed(v);
            Py_IncRef(py_v);
            PyTuple_SetItem(tup, i as Py_ssize_t, py_v);
        }
        tup
    }
}

unsafe fn make_c_function(name: &'static str, info: CCallableInfo) -> Value {
    unsafe {
        let id = {
            let mut guard = CALLABLES.lock().unwrap_or_else(|e| e.into_inner());
            let id = guard.len();
            guard.push(info);
            id
        };
        let func_val = object::function(ptr::null_mut(), c_callable_entry, name);
        if let Data::Function {
            defaults,
            keyword_entry,
            ..
        } = &mut (*func_val).data
        {
            *defaults = object::small(id as i64);
            *keyword_entry = Some(c_callable_entry);
        }
        func_val
    }
}

unsafe fn build_method_def_value(
    def: &PyMethodDef,
    self_py: *mut PyObject,
    is_type_method: bool,
) -> Value {
    unsafe {
        let name_str = if def.ml_name.is_null() {
            "method"
        } else {
            CStr::from_ptr(def.ml_name).to_str().unwrap_or("method")
        };
        let leaked_name: &'static str = Box::leak(name_str.to_owned().into_boxed_str());
        let Some(func) = def.ml_meth else {
            return mule_none();
        };
        let fn_val = make_c_function(
            leaked_name,
            CCallableInfo {
                kind: CCallableKind::Method {
                    func,
                    flags: def.ml_flags,
                    self_py,
                    is_type_method,
                },
                name: leaked_name,
                doc: def.ml_doc,
                closure: ptr::null_mut(),
            },
        );
        if is_type_method && (def.ml_flags & METH_STATIC) != 0 {
            let sm_cls = protocol_lookup("staticmethod");
            if !sm_cls.is_null() {
                let sm = object::call(sm_cls, &[fn_val]);
                object::release(fn_val);
                return sm;
            }
        }
        if is_type_method && (def.ml_flags & METH_CLASS) != 0 {
            let cm_cls = protocol_lookup("classmethod");
            if !cm_cls.is_null() {
                let cm = object::call(cm_cls, &[fn_val]);
                object::release(fn_val);
                return cm;
            }
        }
        fn_val
    }
}

// ===========================================================================
// Type Construction (PyType_FromModuleAndSpec / PyType_FromSpecWithBases)
// ===========================================================================

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyType_FromModuleAndSpec(
    module: *mut PyObject,
    spec: *mut PyType_Spec,
    bases: *mut PyObject,
) -> *mut PyObject {
    unsafe {
        if spec.is_null() {
            return ptr::null_mut();
        }
        let full_name = if (*spec).name.is_null() {
            "CType"
        } else {
            CStr::from_ptr((*spec).name).to_str().unwrap_or("CType")
        };
        let (mod_from_name, short_name) = match full_name.rsplit_once('.') {
            Some((m, s)) => (Some(m), s),
            None => (None, full_name),
        };
        let leaked_short: &'static str = Box::leak(short_name.to_owned().into_boxed_str());

        let bases_tup = if bases.is_null() {
            let obj_cls = classes::builtin_type(Builtin::Object);
            make_tuple_from_slice(&[obj_cls])
        } else {
            let b_val = from_py_borrowed(bases);
            if is_tuple_value(b_val) {
                object::retain(b_val)
            } else {
                make_tuple_from_slice(&[b_val])
            }
        };

        let ns = object::namespace();
        let c_short_name = CString::new(leaked_short).unwrap();
        let cls_val = classes::mule_class(
            c_short_name.as_ptr(),
            bases_tup,
            0,
            ptr::null(),
            ns,
            mule_none(),
        );
        object::release(bases_tup);
        object::release(ns);
        if cls_val.is_null() {
            return ptr::null_mut();
        }

        if let Some(m_str) = mod_from_name {
            let m_val = make_str_from_rust(m_str);
            set_class_attr(cls_val, "__module__", m_val);
            object::release(m_val);
        } else if !module.is_null() {
            let mod_val = from_py_borrowed(module);
            let mod_name = get_attr_str(mod_val, "__name__");
            if !mod_name.is_null() {
                set_class_attr(cls_val, "__module__", mod_name);
                object::release(mod_name);
            }
        }

        let py_type = py_type_for_class(cls_val);
        object::retain(cls_val);
        (*py_type).tp_name = (*spec).name;
        (*py_type).tp_basicsize = (*spec).basicsize as Py_ssize_t;
        (*py_type).tp_itemsize = (*spec).itemsize as Py_ssize_t;
        (*py_type).tp_flags =
            ((*spec).flags as c_ulong) | Py_TPFLAGS_HEAPTYPE | Py_TPFLAGS_READY;
        if !module.is_null() {
            with_state(|s| {
                s.type_modules.insert(py_type as usize, module as usize);
            });
        }

        let bases_len = if !bases.is_null() {
            let b_val = from_py_borrowed(bases);
            if is_tuple_value(b_val) {
                tuple_len(b_val)
            } else {
                1
            }
        } else {
            0
        };
        if bases_len > 0 {
            let b_val = from_py_borrowed(bases);
            let first_base = if is_tuple_value(b_val) {
                tuple_item(b_val, 0)
            } else {
                b_val
            };
            if !first_base.is_null() {
                let base_py_type = py_type_for_class(first_base);
                if !base_py_type.is_null() {
                    (*py_type).tp_base = base_py_type;
                    if (*py_type).tp_basicsize == 0 {
                        (*py_type).tp_basicsize = (*base_py_type).tp_basicsize;
                    }
                    (*py_type).tp_new = (*base_py_type).tp_new;
                    if (*py_type).tp_init.is_none() {
                        (*py_type).tp_init = (*base_py_type).tp_init;
                    }
                    if (*py_type).tp_dealloc.is_none() {
                        (*py_type).tp_dealloc = (*base_py_type).tp_dealloc;
                    }
                    if (*py_type).tp_getattro.is_none() {
                        (*py_type).tp_getattro = (*base_py_type).tp_getattro;
                    }
                    if (*py_type).tp_setattro.is_none() {
                        (*py_type).tp_setattro = (*base_py_type).tp_setattro;
                    }
                    if (*py_type).tp_descr_get.is_none() {
                        (*py_type).tp_descr_get = (*base_py_type).tp_descr_get;
                    }
                    if (*py_type).tp_descr_set.is_none() {
                        (*py_type).tp_descr_set = (*base_py_type).tp_descr_set;
                    }
                    if (*py_type).tp_iter.is_none() {
                        (*py_type).tp_iter = (*base_py_type).tp_iter;
                    }
                    if (*py_type).tp_iternext.is_none() {
                        (*py_type).tp_iternext = (*base_py_type).tp_iternext;
                    }
                    if (*py_type).tp_repr.is_none() {
                        (*py_type).tp_repr = (*base_py_type).tp_repr;
                    }
                    if (*py_type).tp_str.is_none() {
                        (*py_type).tp_str = (*base_py_type).tp_str;
                    }
                    if (*py_type).tp_hash.is_none() {
                        (*py_type).tp_hash = (*base_py_type).tp_hash;
                    }
                    if (*py_type).tp_richcompare.is_none() {
                        (*py_type).tp_richcompare = (*base_py_type).tp_richcompare;
                    }
                    if (*py_type).tp_clear.is_none() {
                        (*py_type).tp_clear = (*base_py_type).tp_clear;
                    }
                    if (*py_type).tp_traverse.is_none() {
                        (*py_type).tp_traverse = (*base_py_type).tp_traverse;
                    }
                    if (*py_type).tp_weaklistoffset == 0 {
                        (*py_type).tp_weaklistoffset = (*base_py_type).tp_weaklistoffset;
                    }
                    if (*py_type).tp_dictoffset == 0 {
                        (*py_type).tp_dictoffset = (*base_py_type).tp_dictoffset;
                    }
                }
            }
        }

        let mut dictoffset: Py_ssize_t = (*py_type).tp_dictoffset;
        let mut slot_ptr = (*spec).slots;
        while !slot_ptr.is_null() && (*slot_ptr).slot != 0 {
            let slot_id = (*slot_ptr).slot;
            let pfunc = (*slot_ptr).pfunc;
            match slot_id {
                Py_tp_doc => {
                    if !pfunc.is_null() {
                        let doc_str = CStr::from_ptr(pfunc as *const c_char)
                            .to_str()
                            .unwrap_or("");
                        let doc_val = make_str_from_rust(doc_str);
                        set_class_attr(cls_val, "__doc__", doc_val);
                        object::release(doc_val);
                    }
                }
                Py_tp_methods => {
                    let mut m_ptr = pfunc as *const PyMethodDef;
                    while !m_ptr.is_null() && !(*m_ptr).ml_name.is_null() {
                        let m_name = CStr::from_ptr((*m_ptr).ml_name).to_str().unwrap_or("");
                        let fn_val = build_method_def_value(&*m_ptr, ptr::null_mut(), true);
                        set_class_attr(cls_val, m_name, fn_val);
                        object::release(fn_val);
                        m_ptr = m_ptr.add(1);
                    }
                }
                Py_tp_members => {
                    let mut mem_ptr = pfunc as *const PyMemberDef;
                    while !mem_ptr.is_null() && !(*mem_ptr).name.is_null() {
                        let mem_name = CStr::from_ptr((*mem_ptr).name).to_str().unwrap_or("");
                        if mem_name == "__dictoffset__" {
                            dictoffset = (*mem_ptr).offset;
                            (*py_type).tp_dictoffset = dictoffset;
                        } else if mem_name != "__weaklistoffset__" {
                            let leaked_mem: &'static str =
                                Box::leak(mem_name.to_owned().into_boxed_str());
                            let fget = make_c_function(
                                leaked_mem,
                                CCallableInfo {
                                    kind: CCallableKind::MemberGet {
                                        member_type: (*mem_ptr).r#type,
                                        offset: (*mem_ptr).offset,
                                    },
                                    name: leaked_mem,
                                    doc: (*mem_ptr).doc,
                                    closure: ptr::null_mut(),
                                },
                            );
                            let fset = make_c_function(
                                leaked_mem,
                                CCallableInfo {
                                    kind: CCallableKind::MemberSet {
                                        member_type: (*mem_ptr).r#type,
                                        offset: (*mem_ptr).offset,
                                        readonly: (*mem_ptr).flags & READONLY,
                                    },
                                    name: leaked_mem,
                                    doc: (*mem_ptr).doc,
                                    closure: ptr::null_mut(),
                                },
                            );
                            let prop_cls = protocol_lookup("property");
                            let prop_val = object::call(prop_cls, &[fget, fset, fset]);
                            object::release(fget);
                            object::release(fset);
                            set_class_attr(cls_val, mem_name, prop_val);
                            object::release(prop_val);
                        }
                        mem_ptr = mem_ptr.add(1);
                    }
                }
                Py_tp_getset => {
                    let mut gs_ptr = pfunc as *const PyGetSetDef;
                    while !gs_ptr.is_null() && !(*gs_ptr).name.is_null() {
                        let gs_name = CStr::from_ptr((*gs_ptr).name).to_str().unwrap_or("");
                        let leaked_gs: &'static str =
                            Box::leak(gs_name.to_owned().into_boxed_str());
                        let fget = if let Some(getter) = (*gs_ptr).get {
                            make_c_function(
                                leaked_gs,
                                CCallableInfo {
                                    kind: CCallableKind::Getter(getter),
                                    name: leaked_gs,
                                    doc: (*gs_ptr).doc,
                                    closure: (*gs_ptr).closure,
                                },
                            )
                        } else {
                            mule_none()
                        };
                        let fset = if let Some(setter) = (*gs_ptr).set {
                            make_c_function(
                                leaked_gs,
                                CCallableInfo {
                                    kind: CCallableKind::Setter(setter),
                                    name: leaked_gs,
                                    doc: (*gs_ptr).doc,
                                    closure: (*gs_ptr).closure,
                                },
                            )
                        } else {
                            mule_none()
                        };
                        let prop_cls = protocol_lookup("property");
                        let prop_val = object::call(prop_cls, &[fget, fset, fset]);
                        object::release(fget);
                        object::release(fset);
                        set_class_attr(cls_val, gs_name, prop_val);
                        object::release(prop_val);
                        gs_ptr = gs_ptr.add(1);
                    }
                }
                Py_tp_new => {
                    if !pfunc.is_null() {
                        let f: RawNewfunc = std::mem::transmute(pfunc);
                        (*py_type).tp_new = Some(f);
                        let fn_val = make_c_function(
                            "__new__",
                            CCallableInfo {
                                kind: CCallableKind::SlotNew(f),
                                name: "__new__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__new__", fn_val);
                        object::release(fn_val);
                    }
                }
                Py_tp_init => {
                    if !pfunc.is_null() {
                        let f: RawInitproc = std::mem::transmute(pfunc);
                        (*py_type).tp_init = Some(f);
                        let fn_val = make_c_function(
                            "__init__",
                            CCallableInfo {
                                kind: CCallableKind::SlotInit(f),
                                name: "__init__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__init__", fn_val);
                        object::release(fn_val);
                    }
                }
                Py_tp_dealloc => {
                    if !pfunc.is_null() {
                        let f: unsafe extern "C" fn(*mut PyObject) = std::mem::transmute(pfunc);
                        (*py_type).tp_dealloc = Some(f);
                    }
                }
                Py_tp_traverse => {
                    if !pfunc.is_null() {
                        let f: unsafe extern "C" fn(*mut PyObject, visitproc, *mut c_void) -> c_int =
                            std::mem::transmute(pfunc);
                        (*py_type).tp_traverse = Some(f);
                    }
                }
                Py_tp_clear => {
                    if !pfunc.is_null() {
                        let f: RawInquiry = std::mem::transmute(pfunc);
                        (*py_type).tp_clear = Some(f);
                    }
                }
                Py_tp_call => {
                    if !pfunc.is_null() {
                        let f: RawTernary = std::mem::transmute(pfunc);
                        (*py_type).tp_call = Some(f);
                        let fn_val = make_c_function(
                            "__call__",
                            CCallableInfo {
                                kind: CCallableKind::SlotCall(f),
                                name: "__call__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__call__", fn_val);
                        object::release(fn_val);
                    }
                }
                Py_tp_repr => {
                    if !pfunc.is_null() {
                        let f: RawRepr = std::mem::transmute(pfunc);
                        (*py_type).tp_repr = Some(f);
                        let fn_val = make_c_function(
                            "__repr__",
                            CCallableInfo {
                                kind: CCallableKind::SlotRepr(f),
                                name: "__repr__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__repr__", fn_val);
                        object::release(fn_val);
                    }
                }
                Py_tp_str => {
                    if !pfunc.is_null() {
                        let f: RawRepr = std::mem::transmute(pfunc);
                        (*py_type).tp_str = Some(f);
                        let fn_val = make_c_function(
                            "__str__",
                            CCallableInfo {
                                kind: CCallableKind::SlotRepr(f),
                                name: "__str__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__str__", fn_val);
                        object::release(fn_val);
                    }
                }
                Py_tp_hash => {
                    if !pfunc.is_null() {
                        let f: RawHash = std::mem::transmute(pfunc);
                        (*py_type).tp_hash = Some(f);
                        let fn_val = make_c_function(
                            "__hash__",
                            CCallableInfo {
                                kind: CCallableKind::SlotHash(f),
                                name: "__hash__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__hash__", fn_val);
                        object::release(fn_val);
                    }
                }
                Py_tp_getattro => {
                    if !pfunc.is_null() {
                        let f: RawGetattro = std::mem::transmute(pfunc);
                        (*py_type).tp_getattro = Some(f);
                        let fn_val = make_c_function(
                            "__getattribute__",
                            CCallableInfo {
                                kind: CCallableKind::SlotGetattro(f),
                                name: "__getattribute__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__getattribute__", fn_val);
                        object::release(fn_val);
                    }
                }
                Py_tp_setattro => {
                    if !pfunc.is_null() {
                        let f: RawSetattro = std::mem::transmute(pfunc);
                        (*py_type).tp_setattro = Some(f);
                        let fn_set = make_c_function(
                            "__setattr__",
                            CCallableInfo {
                                kind: CCallableKind::SlotSetattro(f),
                                name: "__setattr__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__setattr__", fn_set);
                        object::release(fn_set);
                        let fn_del = make_c_function(
                            "__delattr__",
                            CCallableInfo {
                                kind: CCallableKind::SlotSetattro(f),
                                name: "__delattr__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__delattr__", fn_del);
                        object::release(fn_del);
                    }
                }
                Py_tp_descr_get => {
                    if !pfunc.is_null() {
                        let f: RawDescrGet = std::mem::transmute(pfunc);
                        (*py_type).tp_descr_get = Some(f);
                        let fn_val = make_c_function(
                            "__get__",
                            CCallableInfo {
                                kind: CCallableKind::SlotDescrGet(f),
                                name: "__get__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__get__", fn_val);
                        object::release(fn_val);
                    }
                }
                Py_tp_descr_set => {
                    if !pfunc.is_null() {
                        let f: RawDescrSet = std::mem::transmute(pfunc);
                        (*py_type).tp_descr_set = Some(f);
                        let fn_set = make_c_function(
                            "__set__",
                            CCallableInfo {
                                kind: CCallableKind::SlotDescrSet(f),
                                name: "__set__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__set__", fn_set);
                        object::release(fn_set);
                        let fn_del = make_c_function(
                            "__delete__",
                            CCallableInfo {
                                kind: CCallableKind::SlotDescrSet(f),
                                name: "__delete__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__delete__", fn_del);
                        object::release(fn_del);
                    }
                }
                Py_tp_richcompare => {
                    if !pfunc.is_null() {
                        let f: RawRichcmp = std::mem::transmute(pfunc);
                        (*py_type).tp_richcompare = Some(f);
                        for &(dunder, op) in &[
                            ("__lt__", Py_LT),
                            ("__le__", Py_LE),
                            ("__eq__", Py_EQ),
                            ("__ne__", Py_NE),
                            ("__gt__", Py_GT),
                            ("__ge__", Py_GE),
                        ] {
                            let fn_val = make_c_function(
                                dunder,
                                CCallableInfo {
                                    kind: CCallableKind::SlotRichcmp(f, op),
                                    name: dunder,
                                    doc: ptr::null(),
                                    closure: ptr::null_mut(),
                                },
                            );
                            set_class_attr(cls_val, dunder, fn_val);
                            object::release(fn_val);
                        }
                    }
                }
                Py_tp_iter => {
                    if !pfunc.is_null() {
                        let f: RawUnary = std::mem::transmute(pfunc);
                        (*py_type).tp_iter = Some(f);
                        let fn_val = make_c_function(
                            "__iter__",
                            CCallableInfo {
                                kind: CCallableKind::SlotUnary(f),
                                name: "__iter__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__iter__", fn_val);
                        object::release(fn_val);
                    }
                }
                Py_tp_iternext => {
                    if !pfunc.is_null() {
                        let f: RawUnary = std::mem::transmute(pfunc);
                        (*py_type).tp_iternext = Some(f);
                        let fn_val = make_c_function(
                            "__next__",
                            CCallableInfo {
                                kind: CCallableKind::SlotUnary(f),
                                name: "__next__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__next__", fn_val);
                        object::release(fn_val);
                    }
                }
                Py_nb_bool => {
                    if !pfunc.is_null() {
                        let f: RawInquiry = std::mem::transmute(pfunc);
                        let fn_val = make_c_function(
                            "__bool__",
                            CCallableInfo {
                                kind: CCallableKind::SlotInquiryBool(f),
                                name: "__bool__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__bool__", fn_val);
                        object::release(fn_val);
                    }
                }
                Py_nb_int => bind_unary_slot(cls_val, "__int__", pfunc),
                Py_nb_float => bind_unary_slot(cls_val, "__float__", pfunc),
                Py_nb_index => bind_unary_slot(cls_val, "__index__", pfunc),
                Py_nb_negative => bind_unary_slot(cls_val, "__neg__", pfunc),
                Py_nb_positive => bind_unary_slot(cls_val, "__pos__", pfunc),
                Py_nb_absolute => bind_unary_slot(cls_val, "__abs__", pfunc),
                Py_nb_invert => bind_unary_slot(cls_val, "__invert__", pfunc),
                Py_nb_add => bind_binary_slot_pair(cls_val, "__add__", "__radd__", pfunc),
                Py_nb_subtract => bind_binary_slot_pair(cls_val, "__sub__", "__rsub__", pfunc),
                Py_nb_multiply => bind_binary_slot_pair(cls_val, "__mul__", "__rmul__", pfunc),
                Py_nb_remainder => bind_binary_slot_pair(cls_val, "__mod__", "__rmod__", pfunc),
                Py_nb_divmod => bind_binary_slot_pair(cls_val, "__divmod__", "__rdivmod__", pfunc),
                Py_nb_lshift => bind_binary_slot_pair(cls_val, "__lshift__", "__rlshift__", pfunc),
                Py_nb_rshift => bind_binary_slot_pair(cls_val, "__rshift__", "__rrshift__", pfunc),
                Py_nb_and => bind_binary_slot_pair(cls_val, "__and__", "__rand__", pfunc),
                Py_nb_xor => bind_binary_slot_pair(cls_val, "__xor__", "__rxor__", pfunc),
                Py_nb_or => bind_binary_slot_pair(cls_val, "__or__", "__ror__", pfunc),
                Py_nb_floor_divide => {
                    bind_binary_slot_pair(cls_val, "__floordiv__", "__rfloordiv__", pfunc)
                }
                Py_nb_true_divide => {
                    bind_binary_slot_pair(cls_val, "__truediv__", "__rtruediv__", pfunc)
                }
                Py_nb_matrix_multiply => {
                    bind_binary_slot_pair(cls_val, "__matmul__", "__rmatmul__", pfunc)
                }
                Py_nb_inplace_add => bind_binary_slot(cls_val, "__iadd__", pfunc),
                Py_nb_inplace_subtract => bind_binary_slot(cls_val, "__isub__", pfunc),
                Py_nb_inplace_multiply => bind_binary_slot(cls_val, "__imul__", pfunc),
                Py_nb_inplace_remainder => bind_binary_slot(cls_val, "__imod__", pfunc),
                Py_nb_inplace_lshift => bind_binary_slot(cls_val, "__ilshift__", pfunc),
                Py_nb_inplace_rshift => bind_binary_slot(cls_val, "__irshift__", pfunc),
                Py_nb_inplace_and => bind_binary_slot(cls_val, "__iand__", pfunc),
                Py_nb_inplace_xor => bind_binary_slot(cls_val, "__ixor__", pfunc),
                Py_nb_inplace_or => bind_binary_slot(cls_val, "__ior__", pfunc),
                Py_nb_inplace_floor_divide => bind_binary_slot(cls_val, "__ifloordiv__", pfunc),
                Py_nb_inplace_true_divide => bind_binary_slot(cls_val, "__itruediv__", pfunc),
                Py_nb_inplace_matrix_multiply => bind_binary_slot(cls_val, "__imatmul__", pfunc),
                Py_nb_power => {
                    if !pfunc.is_null() {
                        let f: RawTernary = std::mem::transmute(pfunc);
                        let fn_val = make_c_function(
                            "__pow__",
                            CCallableInfo {
                                kind: CCallableKind::SlotTernary(f),
                                name: "__pow__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__pow__", fn_val);
                        object::release(fn_val);
                        let rfn_val = make_c_function(
                            "__rpow__",
                            CCallableInfo {
                                kind: CCallableKind::SlotTernaryRev(f),
                                name: "__rpow__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__rpow__", rfn_val);
                        object::release(rfn_val);
                    }
                }
                Py_nb_inplace_power => {
                    if !pfunc.is_null() {
                        let f: RawTernary = std::mem::transmute(pfunc);
                        let fn_val = make_c_function(
                            "__ipow__",
                            CCallableInfo {
                                kind: CCallableKind::SlotTernary(f),
                                name: "__ipow__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__ipow__", fn_val);
                        object::release(fn_val);
                    }
                }
                Py_sq_length | Py_mp_length => {
                    if !pfunc.is_null() {
                        let f: RawLen = std::mem::transmute(pfunc);
                        let fn_val = make_c_function(
                            "__len__",
                            CCallableInfo {
                                kind: CCallableKind::SlotLen(f),
                                name: "__len__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__len__", fn_val);
                        object::release(fn_val);
                    }
                }
                Py_sq_concat => bind_binary_slot(cls_val, "__add__", pfunc),
                Py_sq_repeat => {
                    if !pfunc.is_null() {
                        let f: RawSsizearg = std::mem::transmute(pfunc);
                        let fn_val = make_c_function(
                            "__mul__",
                            CCallableInfo {
                                kind: CCallableKind::SlotSsizearg(f),
                                name: "__mul__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__mul__", fn_val);
                        object::release(fn_val);
                    }
                }
                Py_sq_item => {
                    if !pfunc.is_null() {
                        let f: RawSsizearg = std::mem::transmute(pfunc);
                        let fn_val = make_c_function(
                            "__getitem__",
                            CCallableInfo {
                                kind: CCallableKind::SlotSsizearg(f),
                                name: "__getitem__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__getitem__", fn_val);
                        object::release(fn_val);
                    }
                }
                Py_sq_ass_item => {
                    if !pfunc.is_null() {
                        let f: RawSsizeObjArg = std::mem::transmute(pfunc);
                        let fn_set = make_c_function(
                            "__setitem__",
                            CCallableInfo {
                                kind: CCallableKind::SlotSsizeObjArg(f),
                                name: "__setitem__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__setitem__", fn_set);
                        object::release(fn_set);
                        let fn_del = make_c_function(
                            "__delitem__",
                            CCallableInfo {
                                kind: CCallableKind::SlotSsizeObjArg(f),
                                name: "__delitem__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__delitem__", fn_del);
                        object::release(fn_del);
                    }
                }
                Py_sq_contains => {
                    if !pfunc.is_null() {
                        let f: RawObjObjProc = std::mem::transmute(pfunc);
                        let fn_val = make_c_function(
                            "__contains__",
                            CCallableInfo {
                                kind: CCallableKind::SlotObjObjProc(f),
                                name: "__contains__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__contains__", fn_val);
                        object::release(fn_val);
                    }
                }
                Py_mp_subscript => bind_binary_slot(cls_val, "__getitem__", pfunc),
                Py_mp_ass_subscript => {
                    if !pfunc.is_null() {
                        let f: RawObjObjArgProc = std::mem::transmute(pfunc);
                        let fn_set = make_c_function(
                            "__setitem__",
                            CCallableInfo {
                                kind: CCallableKind::SlotObjObjArgProc(f),
                                name: "__setitem__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__setitem__", fn_set);
                        object::release(fn_set);
                        let fn_del = make_c_function(
                            "__delitem__",
                            CCallableInfo {
                                kind: CCallableKind::SlotObjObjArgProc(f),
                                name: "__delitem__",
                                doc: ptr::null(),
                                closure: ptr::null_mut(),
                            },
                        );
                        set_class_attr(cls_val, "__delitem__", fn_del);
                        object::release(fn_del);
                    }
                }
                _ => {}
            }
            slot_ptr = slot_ptr.add(1);
        }

        if let Some(f) = (*py_type).tp_new {
            let fn_val = make_c_function(
                "__new__",
                CCallableInfo {
                    kind: CCallableKind::SlotNew(f),
                    name: "__new__",
                    doc: ptr::null(),
                    closure: ptr::null_mut(),
                },
            );
            set_class_attr(cls_val, "__new__", fn_val);
            object::release(fn_val);
        }

        sync_type_mro_and_dict(py_type);
        py_type as *mut PyObject
    }
}

unsafe fn bind_unary_slot(cls_val: Value, name: &'static str, pfunc: *mut c_void) {
    if pfunc.is_null() {
        return;
    }
    unsafe {
        let f: RawUnary = std::mem::transmute(pfunc);
        let fn_val = make_c_function(
            name,
            CCallableInfo {
                kind: CCallableKind::SlotUnary(f),
                name,
                doc: ptr::null(),
                closure: ptr::null_mut(),
            },
        );
        set_class_attr(cls_val, name, fn_val);
        object::release(fn_val);
    }
}

unsafe fn bind_binary_slot(cls_val: Value, name: &'static str, pfunc: *mut c_void) {
    if pfunc.is_null() {
        return;
    }
    unsafe {
        let f: RawBinary = std::mem::transmute(pfunc);
        let fn_val = make_c_function(
            name,
            CCallableInfo {
                kind: CCallableKind::SlotBinary(f),
                name,
                doc: ptr::null(),
                closure: ptr::null_mut(),
            },
        );
        set_class_attr(cls_val, name, fn_val);
        object::release(fn_val);
    }
}

unsafe fn bind_binary_slot_pair(
    cls_val: Value,
    name: &'static str,
    rname: &'static str,
    pfunc: *mut c_void,
) {
    if pfunc.is_null() {
        return;
    }
    unsafe {
        let f: RawBinary = std::mem::transmute(pfunc);
        let fn_val = make_c_function(
            name,
            CCallableInfo {
                kind: CCallableKind::SlotBinary(f),
                name,
                doc: ptr::null(),
                closure: ptr::null_mut(),
            },
        );
        set_class_attr(cls_val, name, fn_val);
        object::release(fn_val);
        let rfn_val = make_c_function(
            rname,
            CCallableInfo {
                kind: CCallableKind::SlotBinaryRev(f),
                name,
                doc: ptr::null(),
                closure: ptr::null_mut(),
            },
        );
        set_class_attr(cls_val, rname, rfn_val);
        object::release(rfn_val);
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyType_FromSpecWithBases(
    spec: *mut PyType_Spec,
    bases: *mut PyObject,
) -> *mut PyObject {
    unsafe { PyType_FromModuleAndSpec(ptr::null_mut(), spec, bases) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyType_FromSpec(spec: *mut PyType_Spec) -> *mut PyObject {
    unsafe { PyType_FromModuleAndSpec(ptr::null_mut(), spec, ptr::null_mut()) }
}

// ===========================================================================
// Module Creation & Multi-phase Init
// ===========================================================================

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyModule_NewObject(name: *mut PyObject) -> *mut PyObject {
    unsafe {
        let name_val = from_py_borrowed(name);
        let m = object::namespace();
        let none = mule_none();
        set_attr_str(m, "__name__", name_val);
        set_attr_str(m, "__doc__", none);
        set_attr_str(m, "__package__", none);
        set_attr_str(m, "__loader__", none);
        set_attr_str(m, "__spec__", none);
        to_py_owned(m)
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyModule_New(name: *const c_char) -> *mut PyObject {
    unsafe {
        let s = PyUnicode_FromString(name);
        let m = PyModule_NewObject(s);
        Py_DecRef(s);
        m
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyModule_Create2(
    def: *mut PyModuleDef,
    _module_api_version: c_int,
) -> *mut PyObject {
    unsafe {
        if def.is_null() {
            return ptr::null_mut();
        }
        let name_ptr = if (*def).m_name.is_null() {
            c"_ext".as_ptr()
        } else {
            (*def).m_name
        };
        let m = PyModule_New(name_ptr);
        if m.is_null() {
            return ptr::null_mut();
        }
        let m_val = from_py_borrowed(m);
        if !(*def).m_doc.is_null() {
            let doc_str = CStr::from_ptr((*def).m_doc).to_str().unwrap_or("");
            let doc_val = make_str_from_rust(doc_str);
            set_attr_str(m_val, "__doc__", doc_val);
            object::release(doc_val);
        }
        if !(*def).m_methods.is_null() {
            PyModule_AddFunctions(m, (*def).m_methods);
        }
        let state_ptr = if (*def).m_size > 0 {
            calloc(1, (*def).m_size as usize)
        } else {
            ptr::null_mut()
        };
        with_state(|s| {
            s.module_defs.insert(m_val as usize, (def, state_ptr));
        });
        m
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyModuleDef_Init(def: *mut PyModuleDef) -> *mut PyObject {
    unsafe {
        if def.is_null() {
            return ptr::null_mut();
        }
        if (*def).m_base.ob_base.ob_type.is_null() {
            (*def).m_base.ob_base.ob_refcnt = 1;
            (*def).m_base.ob_base.ob_type = &raw mut PyType_Type;
        }
        with_state(|s| {
            s.module_defs.insert(def as usize, (def, ptr::null_mut()));
        });
        def as *mut PyObject
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyModule_FromDefAndSpec2(
    def: *mut PyModuleDef,
    spec: *mut PyObject,
    _module_api_version: c_int,
) -> *mut PyObject {
    unsafe {
        if def.is_null() {
            return ptr::null_mut();
        }
        let mut create_fn: Option<
            unsafe extern "C" fn(*mut PyObject, *mut PyModuleDef) -> *mut PyObject,
        > = None;
        let mut slot_ptr = (*def).m_slots;
        while !slot_ptr.is_null() && (*slot_ptr).slot != 0 {
            if (*slot_ptr).slot == Py_mod_create && !(*slot_ptr).value.is_null() {
                create_fn = Some(std::mem::transmute((*slot_ptr).value));
            }
            slot_ptr = slot_ptr.add(1);
        }

        let m = if let Some(cf) = create_fn {
            cf(spec, def)
        } else {
            let name_obj = if !spec.is_null() {
                PyObject_GetAttrString(spec, c"name".as_ptr())
            } else {
                ptr::null_mut()
            };
            if !name_obj.is_null() {
                let r = PyModule_NewObject(name_obj);
                Py_DecRef(name_obj);
                r
            } else {
                let name_ptr = if (*def).m_name.is_null() {
                    c"_ext".as_ptr()
                } else {
                    (*def).m_name
                };
                PyModule_New(name_ptr)
            }
        };
        if m.is_null() {
            return ptr::null_mut();
        }
        let m_val = from_py_borrowed(m);
        if !(*def).m_doc.is_null() {
            let doc_str = CStr::from_ptr((*def).m_doc).to_str().unwrap_or("");
            let doc_val = make_str_from_rust(doc_str);
            set_attr_str(m_val, "__doc__", doc_val);
            object::release(doc_val);
        }
        if !(*def).m_methods.is_null() {
            PyModule_AddFunctions(m, (*def).m_methods);
        }
        let state_ptr = if (*def).m_size > 0 {
            calloc(1, (*def).m_size as usize)
        } else {
            ptr::null_mut()
        };
        with_state(|s| {
            s.module_defs.insert(m_val as usize, (def, state_ptr));
        });
        m
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyModule_ExecDef(
    module: *mut PyObject,
    def: *mut PyModuleDef,
) -> c_int {
    unsafe {
        if module.is_null() || def.is_null() {
            return -1;
        }
        let mut slot_ptr = (*def).m_slots;
        while !slot_ptr.is_null() && (*slot_ptr).slot != 0 {
            if (*slot_ptr).slot == Py_mod_exec && !(*slot_ptr).value.is_null() {
                let exec_fn: unsafe extern "C" fn(*mut PyObject) -> c_int =
                    std::mem::transmute((*slot_ptr).value);
                let rc = exec_fn(module);
                if rc != 0 {
                    return rc;
                }
            }
            slot_ptr = slot_ptr.add(1);
        }
        0
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyModule_GetState(module: *mut PyObject) -> *mut c_void {
    let m_val = from_py_borrowed(module);
    with_state(|s| {
        s.module_defs
            .get(&(m_val as usize))
            .map(|&(_, st)| st)
            .unwrap_or(ptr::null_mut())
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyModule_GetDef(module: *mut PyObject) -> *mut PyModuleDef {
    let m_val = from_py_borrowed(module);
    with_state(|s| {
        s.module_defs
            .get(&(m_val as usize))
            .map(|&(d, _)| d)
            .unwrap_or(ptr::null_mut())
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyModule_GetDict(module: *mut PyObject) -> *mut PyObject {
    unsafe {
        let m_val = from_py_borrowed(module);
        if m_val.is_null() {
            return ptr::null_mut();
        }
        let gdict_fn = protocol_lookup("globals_dict");
        if gdict_fn.is_null() {
            return ptr::null_mut();
        }
        let d_val = object::call(gdict_fn, &[m_val]);
        if d_val.is_null() {
            return ptr::null_mut();
        }
        let py = to_py_borrowed(d_val);
        object::release(d_val);
        py
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyModule_GetNameObject(module: *mut PyObject) -> *mut PyObject {
    unsafe { PyObject_GetAttrString(module, c"__name__".as_ptr()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyModule_GetName(module: *mut PyObject) -> *const c_char {
    unsafe {
        let n = PyModule_GetNameObject(module);
        if n.is_null() {
            return ptr::null();
        }
        let s = PyUnicode_AsUTF8(n);
        Py_DecRef(n);
        s
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyModule_AddObjectRef(
    module: *mut PyObject,
    name: *const c_char,
    value: *mut PyObject,
) -> c_int {
    unsafe {
        if module.is_null() || name.is_null() || value.is_null() {
            PyErr_SetString(PyExc_TypeError, c"PyModule_AddObjectRef: NULL arg".as_ptr());
            return -1;
        }
        let m_val = from_py_borrowed(module);
        let v_val = from_py_borrowed(value);
        let name_str = CStr::from_ptr(name).to_str().unwrap_or("");
        set_attr_str(m_val, name_str, v_val);
        if !PyErr_Occurred().is_null() {
            -1
        } else {
            0
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyModule_AddObject(
    module: *mut PyObject,
    name: *const c_char,
    value: *mut PyObject,
) -> c_int {
    unsafe {
        let rc = PyModule_AddObjectRef(module, name, value);
        if rc == 0 {
            Py_DecRef(value);
        }
        rc
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyModule_Add(
    module: *mut PyObject,
    name: *const c_char,
    value: *mut PyObject,
) -> c_int {
    unsafe {
        if value.is_null() {
            return -1;
        }
        let rc = PyModule_AddObjectRef(module, name, value);
        Py_DecRef(value);
        rc
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyModule_AddIntConstant(
    module: *mut PyObject,
    name: *const c_char,
    value: c_long,
) -> c_int {
    unsafe {
        let o = PyLong_FromLong(value);
        PyModule_Add(module, name, o)
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyModule_AddStringConstant(
    module: *mut PyObject,
    name: *const c_char,
    value: *const c_char,
) -> c_int {
    unsafe {
        let o = PyUnicode_FromString(value);
        PyModule_Add(module, name, o)
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyModule_AddType(
    module: *mut PyObject,
    type_: *mut PyTypeObject,
) -> c_int {
    unsafe {
        if type_.is_null() || (*type_).tp_name.is_null() {
            return -1;
        }
        let full = CStr::from_ptr((*type_).tp_name).to_str().unwrap_or("");
        let short = full.rsplit('.').next().unwrap_or(full);
        let c_short = CString::new(short).unwrap();
        PyModule_AddObjectRef(module, c_short.as_ptr(), type_ as *mut PyObject)
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyModule_AddFunctions(
    module: *mut PyObject,
    functions: *mut PyMethodDef,
) -> c_int {
    unsafe {
        let m_val = from_py_borrowed(module);
        let mut ptr = functions;
        while !ptr.is_null() && !(*ptr).ml_name.is_null() {
            let name_str = CStr::from_ptr((*ptr).ml_name).to_str().unwrap_or("");
            let fn_val = build_method_def_value(&*ptr, module, false);
            set_attr_str(m_val, name_str, fn_val);
            object::release(fn_val);
            ptr = ptr.add(1);
        }
        0
    }
}

// ===========================================================================
// SysV x86_64 Variadic Support (PyArg_ParseTuple, Py_BuildValue, etc.)
// ===========================================================================

#[repr(C)]
pub struct VaListImpl {
    pub gp_offset: u32,
    pub fp_offset: u32,
    pub overflow_arg_area: *mut u8,
    pub reg_save_area: *mut u8,
}

impl VaListImpl {
    unsafe fn next_gp<T: Copy>(&mut self) -> T {
        unsafe {
            if self.gp_offset <= 40 {
                let p = self.reg_save_area.add(self.gp_offset as usize) as *const T;
                self.gp_offset += 8;
                *p
            } else {
                let p = self.overflow_arg_area as *const T;
                self.overflow_arg_area = self.overflow_arg_area.add(8);
                *p
            }
        }
    }

    unsafe fn next_fp64(&mut self) -> f64 {
        unsafe {
            if self.fp_offset <= 160 {
                let p = self.reg_save_area.add(self.fp_offset as usize) as *const f64;
                self.fp_offset += 16;
                *p
            } else {
                let p = self.overflow_arg_area as *const f64;
                self.overflow_arg_area = self.overflow_arg_area.add(8);
                *p
            }
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_FormatV(
    exception: *mut PyObject,
    format: *const c_char,
    vargs: *mut VaListImpl,
) -> *mut PyObject {
    unsafe {
        let s = PyUnicode_FromFormatV(format, vargs);
        if !s.is_null() {
            PyErr_SetObject(exception, s);
            Py_DecRef(s);
        }
        ptr::null_mut()
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyUnicode_FromFormatV(
    format: *const c_char,
    vargs: *mut VaListImpl,
) -> *mut PyObject {
    unsafe {
        if format.is_null() {
            return PyUnicode_FromString(c"".as_ptr());
        }
        let fmt_bytes = CStr::from_ptr(format).to_bytes();
        let mut out = String::new();
        let mut i = 0;
        while i < fmt_bytes.len() {
            if fmt_bytes[i] != b'%' {
                out.push(fmt_bytes[i] as char);
                i += 1;
                continue;
            }
            i += 1;
            if i >= fmt_bytes.len() {
                break;
            }
            if fmt_bytes[i] == b'%' {
                out.push('%');
                i += 1;
                continue;
            }
            let mut is_long = false;
            let mut is_size = false;
            while i < fmt_bytes.len()
                && (fmt_bytes[i].is_ascii_digit() || fmt_bytes[i] == b'.' || fmt_bytes[i] == b'-')
            {
                i += 1;
            }
            if i < fmt_bytes.len() && fmt_bytes[i] == b'l' {
                is_long = true;
                i += 1;
                if i < fmt_bytes.len() && fmt_bytes[i] == b'l' {
                    i += 1;
                }
            } else if i < fmt_bytes.len() && fmt_bytes[i] == b'z' {
                is_size = true;
                i += 1;
            }
            if i >= fmt_bytes.len() {
                break;
            }
            let spec = fmt_bytes[i];
            i += 1;
            match spec {
                b's' => {
                    let s_ptr: *const c_char = (*vargs).next_gp();
                    if s_ptr.is_null() {
                        out.push_str("(null)");
                    } else {
                        out.push_str(&CStr::from_ptr(s_ptr).to_string_lossy());
                    }
                }
                b'd' | b'i' => {
                    if is_long || is_size {
                        let v: i64 = (*vargs).next_gp();
                        out.push_str(&v.to_string());
                    } else {
                        let v: i32 = (*vargs).next_gp();
                        out.push_str(&v.to_string());
                    }
                }
                b'u' => {
                    if is_long || is_size {
                        let v: u64 = (*vargs).next_gp();
                        out.push_str(&v.to_string());
                    } else {
                        let v: u32 = (*vargs).next_gp();
                        out.push_str(&v.to_string());
                    }
                }
                b'x' | b'p' => {
                    let v: usize = (*vargs).next_gp();
                    out.push_str(&format!("{:x}", v));
                }
                b'c' => {
                    let v: u32 = (*vargs).next_gp();
                    if let Some(ch) = char::from_u32(v) {
                        out.push(ch);
                    }
                }
                b'S' | b'U' => {
                    let obj: *mut PyObject = (*vargs).next_gp();
                    if !obj.is_null() {
                        let s_obj = PyObject_Str(obj);
                        if !s_obj.is_null() {
                            let c_s = PyUnicode_AsUTF8(s_obj);
                            if !c_s.is_null() {
                                out.push_str(&CStr::from_ptr(c_s).to_string_lossy());
                            }
                            Py_DecRef(s_obj);
                        }
                    }
                }
                b'R' => {
                    let obj: *mut PyObject = (*vargs).next_gp();
                    if !obj.is_null() {
                        let r_obj = PyObject_Repr(obj);
                        if !r_obj.is_null() {
                            let c_s = PyUnicode_AsUTF8(r_obj);
                            if !c_s.is_null() {
                                out.push_str(&CStr::from_ptr(c_s).to_string_lossy());
                            }
                            Py_DecRef(r_obj);
                        }
                    }
                }
                b'A' => {
                    let obj: *mut PyObject = (*vargs).next_gp();
                    if !obj.is_null() {
                        let r_obj = PyObject_ASCII(obj);
                        if !r_obj.is_null() {
                            let c_s = PyUnicode_AsUTF8(r_obj);
                            if !c_s.is_null() {
                                out.push_str(&CStr::from_ptr(c_s).to_string_lossy());
                            }
                            Py_DecRef(r_obj);
                        }
                    }
                }
                _ => {}
            }
        }
        to_py_owned(make_str_from_rust(&out))
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_VaParseTupleAndKeywords(
    args: *mut PyObject,
    kwargs: *mut PyObject,
    format: *const c_char,
    kwlist: *mut *mut c_char,
    vargs: *mut VaListImpl,
) -> c_int {
    unsafe {
        if format.is_null() {
            return 1;
        }
        let fmt_str = CStr::from_ptr(format).to_str().unwrap_or("");
        let fmt_part = fmt_str
            .split(':')
            .next()
            .unwrap_or("")
            .split(';')
            .next()
            .unwrap_or("");

        let pos_len = if args.is_null() {
            0
        } else {
            PyTuple_Size(args) as usize
        };
        let mut pos_vals = Vec::with_capacity(pos_len);
        for idx in 0..pos_len {
            pos_vals.push(PyTuple_GetItem(args, idx as Py_ssize_t));
        }

        let mut kw_names: Vec<&str> = Vec::new();
        if !kwlist.is_null() {
            let mut p = kwlist;
            while !(*p).is_null() {
                kw_names.push(CStr::from_ptr(*p).to_str().unwrap_or(""));
                p = p.add(1);
            }
        }

        let bytes = fmt_part.as_bytes();
        let mut i = 0;
        let mut arg_idx = 0usize;
        let mut optional = false;
        let mut kw_only = false;

        while i < bytes.len() {
            let ch = bytes[i];
            i += 1;
            match ch {
                b'|' => {
                    optional = true;
                    continue;
                }
                b'$' => {
                    kw_only = true;
                    continue;
                }
                b'(' | b')' | b' ' | b'\t' => continue,
                _ => {}
            }

            let mut val_py: *mut PyObject = ptr::null_mut();
            if !kw_only && arg_idx < pos_vals.len() {
                val_py = pos_vals[arg_idx];
            } else if !kwargs.is_null() && arg_idx < kw_names.len() && !kw_names[arg_idx].is_empty()
            {
                let c_k = CString::new(kw_names[arg_idx]).unwrap();
                val_py = PyDict_GetItemString(kwargs, c_k.as_ptr());
            }
            arg_idx += 1;

            match ch {
                b'O' => {
                    if i < bytes.len() && bytes[i] == b'!' {
                        i += 1;
                        let expected_type: *mut PyTypeObject = (*vargs).next_gp();
                        let out_ptr: *mut *mut PyObject = (*vargs).next_gp();
                        if !val_py.is_null() {
                            if PyType_IsSubtype((*val_py).ob_type, expected_type) == 0 {
                                PyErr_SetString(
                                    PyExc_TypeError,
                                    c"argument has wrong type".as_ptr(),
                                );
                                return 0;
                            }
                            *out_ptr = val_py;
                        } else if !optional {
                            PyErr_SetString(
                                PyExc_TypeError,
                                c"missing required argument".as_ptr(),
                            );
                            return 0;
                        }
                    } else if i < bytes.len() && bytes[i] == b'&' {
                        i += 1;
                        let conv: unsafe extern "C" fn(*mut PyObject, *mut c_void) -> c_int =
                            (*vargs).next_gp();
                        let out_ptr: *mut c_void = (*vargs).next_gp();
                        if !val_py.is_null() {
                            if conv(val_py, out_ptr) == 0 {
                                return 0;
                            }
                        } else if !optional {
                            PyErr_SetString(
                                PyExc_TypeError,
                                c"missing required argument".as_ptr(),
                            );
                            return 0;
                        }
                    } else {
                        let out_ptr: *mut *mut PyObject = (*vargs).next_gp();
                        if !val_py.is_null() {
                            *out_ptr = val_py;
                        } else if !optional {
                            PyErr_SetString(
                                PyExc_TypeError,
                                c"missing required argument".as_ptr(),
                            );
                            return 0;
                        }
                    }
                }
                b'U' | b'S' | b'Y' => {
                    let out_ptr: *mut *mut PyObject = (*vargs).next_gp();
                    if !val_py.is_null() {
                        *out_ptr = val_py;
                    } else if !optional {
                        PyErr_SetString(PyExc_TypeError, c"missing required argument".as_ptr());
                        return 0;
                    }
                }
                b's' | b'z' => {
                    let has_hash = if i < bytes.len() && bytes[i] == b'#' {
                        i += 1;
                        true
                    } else if i < bytes.len() && bytes[i] == b'*' {
                        i += 1;
                        false
                    } else {
                        false
                    };
                    let out_ptr: *mut *const c_char = (*vargs).next_gp();
                    let len_ptr: *mut Py_ssize_t = if has_hash {
                        (*vargs).next_gp()
                    } else {
                        ptr::null_mut()
                    };
                    if !val_py.is_null() {
                        if ch == b'z' && val_py == py_none() {
                            *out_ptr = ptr::null();
                            if !len_ptr.is_null() {
                                *len_ptr = 0;
                            }
                        } else {
                            let mut sz: Py_ssize_t = 0;
                            let s = PyUnicode_AsUTF8AndSize(val_py, &mut sz);
                            if s.is_null() {
                                return 0;
                            }
                            *out_ptr = s;
                            if !len_ptr.is_null() {
                                *len_ptr = sz;
                            }
                        }
                    } else if !optional {
                        PyErr_SetString(PyExc_TypeError, c"missing required argument".as_ptr());
                        return 0;
                    }
                }
                b'i' => {
                    let out_ptr: *mut c_int = (*vargs).next_gp();
                    if !val_py.is_null() {
                        let v = PyLong_AsLong(val_py);
                        if v == -1 && !PyErr_Occurred().is_null() {
                            return 0;
                        }
                        *out_ptr = v as c_int;
                    } else if !optional {
                        PyErr_SetString(PyExc_TypeError, c"missing required argument".as_ptr());
                        return 0;
                    }
                }
                b'l' => {
                    let out_ptr: *mut c_long = (*vargs).next_gp();
                    if !val_py.is_null() {
                        let v = PyLong_AsLong(val_py);
                        if v == -1 && !PyErr_Occurred().is_null() {
                            return 0;
                        }
                        *out_ptr = v;
                    } else if !optional {
                        PyErr_SetString(PyExc_TypeError, c"missing required argument".as_ptr());
                        return 0;
                    }
                }
                b'L' => {
                    let out_ptr: *mut c_longlong = (*vargs).next_gp();
                    if !val_py.is_null() {
                        let v = PyLong_AsLongLong(val_py);
                        if v == -1 && !PyErr_Occurred().is_null() {
                            return 0;
                        }
                        *out_ptr = v;
                    } else if !optional {
                        PyErr_SetString(PyExc_TypeError, c"missing required argument".as_ptr());
                        return 0;
                    }
                }
                b'n' => {
                    let out_ptr: *mut Py_ssize_t = (*vargs).next_gp();
                    if !val_py.is_null() {
                        let v = PyLong_AsSsize_t(val_py);
                        if v == -1 && !PyErr_Occurred().is_null() {
                            return 0;
                        }
                        *out_ptr = v;
                    } else if !optional {
                        PyErr_SetString(PyExc_TypeError, c"missing required argument".as_ptr());
                        return 0;
                    }
                }
                b'p' => {
                    let out_ptr: *mut c_int = (*vargs).next_gp();
                    if !val_py.is_null() {
                        let b = PyObject_IsTrue(val_py);
                        if b < 0 {
                            return 0;
                        }
                        *out_ptr = b;
                    } else if !optional {
                        PyErr_SetString(PyExc_TypeError, c"missing required argument".as_ptr());
                        return 0;
                    }
                }
                b'd' => {
                    let out_ptr: *mut c_double = (*vargs).next_gp();
                    if !val_py.is_null() {
                        let d = PyFloat_AsDouble(val_py);
                        if d == -1.0 && !PyErr_Occurred().is_null() {
                            return 0;
                        }
                        *out_ptr = d;
                    } else if !optional {
                        PyErr_SetString(PyExc_TypeError, c"missing required argument".as_ptr());
                        return 0;
                    }
                }
                _ => {}
            }
        }
        1
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn Py_VaBuildValue(
    format: *const c_char,
    vargs: *mut VaListImpl,
) -> *mut PyObject {
    unsafe {
        if format.is_null() {
            let n = py_none();
            Py_IncRef(n);
            return n;
        }
        let bytes = CStr::from_ptr(format).to_bytes();
        let mut i = 0;
        let items = build_value_items(bytes, &mut i, vargs, 0);
        if items.is_empty() {
            let n = py_none();
            Py_IncRef(n);
            n
        } else if items.len() == 1 {
            items[0]
        } else {
            let tup = PyTuple_New(items.len() as Py_ssize_t);
            for (idx, &it) in items.iter().enumerate() {
                PyTuple_SetItem(tup, idx as Py_ssize_t, it);
            }
            tup
        }
    }
}

unsafe fn build_value_items(
    bytes: &[u8],
    i: &mut usize,
    vargs: *mut VaListImpl,
    end_char: u8,
) -> Vec<*mut PyObject> {
    unsafe {
        let mut items = Vec::new();
        while *i < bytes.len() {
            let ch = bytes[*i];
            *i += 1;
            if ch == end_char {
                break;
            }
            match ch {
                b' ' | b'\t' | b',' | b':' => continue,
                b'(' => {
                    let sub = build_value_items(bytes, i, vargs, b')');
                    let tup = PyTuple_New(sub.len() as Py_ssize_t);
                    for (idx, &it) in sub.iter().enumerate() {
                        PyTuple_SetItem(tup, idx as Py_ssize_t, it);
                    }
                    items.push(tup);
                }
                b'[' => {
                    let sub = build_value_items(bytes, i, vargs, b']');
                    let lst = PyList_New(sub.len() as Py_ssize_t);
                    for (idx, &it) in sub.iter().enumerate() {
                        PyList_SetItem(lst, idx as Py_ssize_t, it);
                    }
                    items.push(lst);
                }
                b'{' => {
                    let sub = build_value_items(bytes, i, vargs, b'}');
                    let d = PyDict_New();
                    let mut j = 0;
                    while j + 1 < sub.len() {
                        PyDict_SetItem(d, sub[j], sub[j + 1]);
                        Py_DecRef(sub[j]);
                        Py_DecRef(sub[j + 1]);
                        j += 2;
                    }
                    items.push(d);
                }
                b's' | b'z' | b'U' => {
                    let has_hash = if *i < bytes.len() && bytes[*i] == b'#' {
                        *i += 1;
                        true
                    } else {
                        false
                    };
                    let s_ptr: *const c_char = (*vargs).next_gp();
                    let sz: Py_ssize_t = if has_hash { (*vargs).next_gp() } else { -1 };
                    if s_ptr.is_null() {
                        let n = py_none();
                        Py_IncRef(n);
                        items.push(n);
                    } else if has_hash {
                        items.push(PyUnicode_FromStringAndSize(s_ptr, sz));
                    } else {
                        items.push(PyUnicode_FromString(s_ptr));
                    }
                }
                b'y' => {
                    let has_hash = if *i < bytes.len() && bytes[*i] == b'#' {
                        *i += 1;
                        true
                    } else {
                        false
                    };
                    let s_ptr: *const c_char = (*vargs).next_gp();
                    let sz: Py_ssize_t = if has_hash { (*vargs).next_gp() } else { -1 };
                    if s_ptr.is_null() {
                        let n = py_none();
                        Py_IncRef(n);
                        items.push(n);
                    } else if has_hash {
                        items.push(PyBytes_FromStringAndSize(s_ptr, sz));
                    } else {
                        items.push(PyBytes_FromString(s_ptr));
                    }
                }
                b'i' | b'b' | b'h' => {
                    let v: c_int = (*vargs).next_gp();
                    items.push(PyLong_FromLong(v as c_long));
                }
                b'I' | b'B' | b'H' => {
                    let v: c_uint = (*vargs).next_gp();
                    items.push(PyLong_FromUnsignedLong(v as c_ulong));
                }
                b'l' => {
                    let v: c_long = (*vargs).next_gp();
                    items.push(PyLong_FromLong(v));
                }
                b'k' => {
                    let v: c_ulong = (*vargs).next_gp();
                    items.push(PyLong_FromUnsignedLong(v));
                }
                b'L' => {
                    let v: c_longlong = (*vargs).next_gp();
                    items.push(PyLong_FromLongLong(v));
                }
                b'K' => {
                    let v: c_ulonglong = (*vargs).next_gp();
                    items.push(PyLong_FromUnsignedLongLong(v));
                }
                b'n' => {
                    let v: Py_ssize_t = (*vargs).next_gp();
                    items.push(PyLong_FromSsize_t(v));
                }
                b'd' | b'f' => {
                    let v = (*vargs).next_fp64();
                    items.push(PyFloat_FromDouble(v));
                }
                b'O' | b'S' => {
                    if *i < bytes.len() && bytes[*i] == b'&' {
                        *i += 1;
                        let conv: unsafe extern "C" fn(*mut c_void) -> *mut PyObject =
                            (*vargs).next_gp();
                        let arg: *mut c_void = (*vargs).next_gp();
                        items.push(conv(arg));
                    } else {
                        let o: *mut PyObject = (*vargs).next_gp();
                        if !o.is_null() {
                            Py_IncRef(o);
                        }
                        items.push(o);
                    }
                }
                b'N' => {
                    let o: *mut PyObject = (*vargs).next_gp();
                    items.push(o);
                }
                _ => {}
            }
        }
        items
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_VaPack(n: Py_ssize_t, vargs: *mut VaListImpl) -> *mut PyObject {
    unsafe {
        let tup = PyTuple_New(n);
        for idx in 0..n {
            let o: *mut PyObject = (*vargs).next_gp();
            if !o.is_null() {
                Py_IncRef(o);
            }
            PyTuple_SetItem(tup, idx, o);
        }
        tup
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_VaCallFunctionObjArgs(
    callable: *mut PyObject,
    vargs: *mut VaListImpl,
) -> *mut PyObject {
    unsafe {
        let mut args = Vec::new();
        loop {
            let o: *mut PyObject = (*vargs).next_gp();
            if o.is_null() {
                break;
            }
            Py_IncRef(o);
            args.push(o);
        }
        let tup = PyTuple_New(args.len() as Py_ssize_t);
        for (idx, &o) in args.iter().enumerate() {
            PyTuple_SetItem(tup, idx as Py_ssize_t, o);
        }
        let res = PyObject_Call(callable, tup, ptr::null_mut());
        Py_DecRef(tup);
        res
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_VaCallMethodObjArgs(
    obj: *mut PyObject,
    name: *mut PyObject,
    vargs: *mut VaListImpl,
) -> *mut PyObject {
    unsafe {
        let meth = PyObject_GetAttr(obj, name);
        if meth.is_null() {
            return ptr::null_mut();
        }
        let res = mule_VaCallFunctionObjArgs(meth, vargs);
        Py_DecRef(meth);
        res
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_VaCallFunction(
    callable: *mut PyObject,
    format: *const c_char,
    vargs: *mut VaListImpl,
) -> *mut PyObject {
    unsafe {
        let args = if format.is_null() || *format == 0 {
            PyTuple_New(0)
        } else {
            let v = Py_VaBuildValue(format, vargs);
            if v.is_null() {
                return ptr::null_mut();
            }
            if PyType_IsSubtype((*v).ob_type, &raw mut PyTuple_Type) != 0 {
                v
            } else {
                let t = PyTuple_New(1);
                PyTuple_SetItem(t, 0, v);
                t
            }
        };
        let res = PyObject_Call(callable, args, ptr::null_mut());
        Py_DecRef(args);
        res
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_VaCallMethod(
    obj: *mut PyObject,
    name: *const c_char,
    format: *const c_char,
    vargs: *mut VaListImpl,
) -> *mut PyObject {
    unsafe {
        let meth = PyObject_GetAttrString(obj, name);
        if meth.is_null() {
            return ptr::null_mut();
        }
        let res = mule_VaCallFunction(meth, format, vargs);
        Py_DecRef(meth);
        res
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_VaSNPrintf(
    str_buf: *mut c_char,
    size: usize,
    format: *const c_char,
    vargs: *mut VaListImpl,
) -> c_int {
    unsafe {
        if str_buf.is_null() || size == 0 {
            return 0;
        }
        let s_obj = PyUnicode_FromFormatV(format, vargs);
        if s_obj.is_null() {
            *str_buf = 0;
            return -1;
        }
        let mut sz: Py_ssize_t = 0;
        let utf8 = PyUnicode_AsUTF8AndSize(s_obj, &mut sz);
        let copy_len = (sz as usize).min(size - 1);
        if !utf8.is_null() && copy_len > 0 {
            ptr::copy_nonoverlapping(utf8, str_buf, copy_len);
        }
        *str_buf.add(copy_len) = 0;
        Py_DecRef(s_obj);
        sz as c_int
    }
}

core::arch::global_asm!(
    r#"
.macro MULE_VA_TRAMPOLINE name, target, gp_offset, rdi_Setup=""
.globl \name
.type \name, @function
\name:
    sub rsp, 216
    mov [rsp + 48], rdi
    mov [rsp + 56], rsi
    mov [rsp + 64], rdx
    mov [rsp + 72], rcx
    mov [rsp + 80], r8
    mov [rsp + 88], r9
    test al, al
    je 1f
    movaps [rsp + 96], xmm0
    movaps [rsp + 112], xmm1
    movaps [rsp + 128], xmm2
    movaps [rsp + 144], xmm3
    movaps [rsp + 160], xmm4
    movaps [rsp + 176], xmm5
    movaps [rsp + 192], xmm6
    movaps [rsp + 208], xmm7
1:
    mov dword ptr [rsp], \gp_offset
    mov dword ptr [rsp + 4], 48
    lea rax, [rsp + 224]
    mov [rsp + 8], rax
    lea rax, [rsp + 48]
    mov [rsp + 16], rax
    \rdi_Setup
    call \target
    add rsp, 216
    ret
.endm

MULE_VA_TRAMPOLINE PyUnicode_FromFormat, PyUnicode_FromFormatV, 8, "mov rsi, rsp"
MULE_VA_TRAMPOLINE PyErr_Format, PyErr_FormatV, 16, "mov rdx, rsp"
MULE_VA_TRAMPOLINE _PyErr_FormatFromCause, PyErr_FormatV, 16, "mov rdx, rsp"
MULE_VA_TRAMPOLINE Py_BuildValue, Py_VaBuildValue, 8, "mov rsi, rsp"
MULE_VA_TRAMPOLINE _Py_BuildValue_SizeT, Py_VaBuildValue, 8, "mov rsi, rsp"
MULE_VA_TRAMPOLINE PyTuple_Pack, mule_VaPack, 8, "mov rsi, rsp"
MULE_VA_TRAMPOLINE PyObject_CallFunctionObjArgs, mule_VaCallFunctionObjArgs, 8, "mov rsi, rsp"
MULE_VA_TRAMPOLINE PyObject_CallMethodObjArgs, mule_VaCallMethodObjArgs, 16, "mov rdx, rsp"
MULE_VA_TRAMPOLINE PyObject_CallFunction, mule_VaCallFunction, 16, "mov rdx, rsp"
MULE_VA_TRAMPOLINE _PyObject_CallFunction_SizeT, mule_VaCallFunction, 16, "mov rdx, rsp"
MULE_VA_TRAMPOLINE PyObject_CallMethod, mule_VaCallMethod, 24, "mov rcx, rsp"
MULE_VA_TRAMPOLINE _PyObject_CallMethod_SizeT, mule_VaCallMethod, 24, "mov rcx, rsp"
MULE_VA_TRAMPOLINE PyOS_snprintf, mule_VaSNPrintf, 24, "mov rcx, rsp"

.globl PyArg_ParseTuple
.type PyArg_ParseTuple, @function
PyArg_ParseTuple:
    sub rsp, 216
    mov [rsp + 48], rdi
    mov [rsp + 56], rsi
    mov [rsp + 64], rdx
    mov [rsp + 72], rcx
    mov [rsp + 80], r8
    mov [rsp + 88], r9
    mov dword ptr [rsp], 16
    mov dword ptr [rsp + 4], 48
    lea rax, [rsp + 224]
    mov [rsp + 8], rax
    lea rax, [rsp + 48]
    mov [rsp + 16], rax
    mov rdx, rsi
    xor rsi, rsi
    xor rcx, rcx
    mov r8, rsp
    call mule_VaParseTupleAndKeywords
    add rsp, 216
    ret

.globl _PyArg_ParseTuple_SizeT
.type _PyArg_ParseTuple_SizeT, @function
_PyArg_ParseTuple_SizeT:
    jmp PyArg_ParseTuple

.globl PyArg_UnpackTuple
.type PyArg_UnpackTuple, @function
PyArg_UnpackTuple:
    sub rsp, 216
    mov [rsp + 48], rdi
    mov [rsp + 56], rsi
    mov [rsp + 64], rdx
    mov [rsp + 72], rcx
    mov [rsp + 80], r8
    mov [rsp + 88], r9
    mov dword ptr [rsp], 32
    mov dword ptr [rsp + 4], 48
    lea rax, [rsp + 224]
    mov [rsp + 8], rax
    lea rax, [rsp + 48]
    mov [rsp + 16], rax
    mov r8, rsp
    call mule_VaUnpackTuple
    add rsp, 216
    ret

.globl PyArg_ParseTupleAndKeywords
.type PyArg_ParseTupleAndKeywords, @function
PyArg_ParseTupleAndKeywords:
    sub rsp, 216
    mov [rsp + 48], rdi
    mov [rsp + 56], rsi
    mov [rsp + 64], rdx
    mov [rsp + 72], rcx
    mov [rsp + 80], r8
    mov [rsp + 88], r9
    mov dword ptr [rsp], 32
    mov dword ptr [rsp + 4], 48
    lea rax, [rsp + 224]
    mov [rsp + 8], rax
    lea rax, [rsp + 48]
    mov [rsp + 16], rax
    mov r8, rsp
    call mule_VaParseTupleAndKeywords
    add rsp, 216
    ret

.globl _PyArg_ParseTupleAndKeywords_SizeT
.type _PyArg_ParseTupleAndKeywords_SizeT, @function
_PyArg_ParseTupleAndKeywords_SizeT:
    jmp PyArg_ParseTupleAndKeywords
"#
);

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_VaUnpackTuple(
    args: *mut PyObject,
    _name: *const c_char,
    min: Py_ssize_t,
    max: Py_ssize_t,
    vargs: *mut VaListImpl,
) -> c_int {
    unsafe {
        let len = if args.is_null() { 0 } else { PyTuple_Size(args) };
        if len < min || len > max {
            PyErr_SetString(PyExc_TypeError, c"unpacked tuple has wrong length".as_ptr());
            return 0;
        }
        for idx in 0..max {
            let out_ptr: *mut *mut PyObject = (*vargs).next_gp();
            if idx < len && !out_ptr.is_null() {
                *out_ptr = PyTuple_GetItem(args, idx);
            }
        }
        1
    }
}

// ===========================================================================
// C Extension Loader Entry Point (dlopen + PyInit_<shortname>)
// ===========================================================================

pub(crate) fn load_c_extension(module_name: &str, so_path: &Path) -> Result<Value, String> {
    ensure_initialized();
    unsafe {
        let short_name = module_name.rsplit('.').next().unwrap_or(module_name);
        let init_sym_name = format!("PyInit_{short_name}");
        let c_path = CString::new(so_path.to_string_lossy().as_bytes())
            .map_err(|_| "invalid extension path".to_string())?;
        let c_sym = CString::new(init_sym_name.as_bytes())
            .map_err(|_| "invalid symbol name".to_string())?;

        let handle = dlopen(c_path.as_ptr(), RTLD_NOW | RTLD_GLOBAL);
        if handle.is_null() {
            let err = dlerror();
            let msg = if err.is_null() {
                "dlopen failed".to_string()
            } else {
                CStr::from_ptr(err as *const c_char)
                    .to_string_lossy()
                    .into_owned()
            };
            return Err(msg);
        }

        let sym = dlsym(handle, c_sym.as_ptr());
        if sym.is_null() {
            return Err(format!(
                "dynamic module does not define module export function ({init_sym_name})"
            ));
        }

        let init_fn: unsafe extern "C" fn() -> *mut PyObject = std::mem::transmute(sym);
        let res_py = init_fn();
        if res_py.is_null() {
            return Err(format!("initialization of {short_name} failed"));
        }

        let is_mod_def = with_state(|s| s.module_defs.contains_key(&(res_py as usize)));
        let mod_py = if is_mod_def {
            let def = res_py as *mut PyModuleDef;
            let m = PyModule_FromDefAndSpec2(def, ptr::null_mut(), PYTHON_API_VERSION);
            if m.is_null() {
                return Err(format!("module creation of {short_name} failed"));
            }
            let full_name_val = make_str_from_rust(module_name);
            set_attr_str(from_py_borrowed(m), "__name__", full_name_val);
            object::release(full_name_val);
            if PyModule_ExecDef(m, def) != 0 {
                Py_DecRef(m);
                return Err(format!("execution of {short_name} failed"));
            }
            m
        } else {
            let full_name_val = make_str_from_rust(module_name);
            set_attr_str(from_py_borrowed(res_py), "__name__", full_name_val);
            object::release(full_name_val);
            res_py
        };

        let mod_val = from_py_borrowed(mod_py);
        let _ = crate::builtins::record_loaded_module(module_name, mod_val);
        Ok(object::retain(mod_val))
    }
}

unsafe fn sync_single_type_dict(cls: Value, tp: *mut PyTypeObject) {
    unsafe {
        if cls.is_null() || tp.is_null() {
            return;
        }
        let layout = classes::layout_of_class_pub(cls);
        if layout.is_null() {
            return;
        }
        if (*tp).tp_dict.is_null() {
            (*tp).tp_dict = PyDict_New();
        }
        let d = (*tp).tp_dict;
        if d.is_null() {
            return;
        }
        for (k, v) in &(*layout).attributes {
            let k_val = make_str_from_rust(k);
            let _ = PyDict_SetItem(d, to_py_borrowed(k_val), to_py_borrowed(*v));
            object::release(k_val);
        }
    }
}

unsafe fn sync_type_mro_and_dict(tp: *mut PyTypeObject) {
    if tp.is_null() {
        return;
    }
    unsafe {
        let cls = (*tp).ob_base.ob_base.ob_mule;
        if cls.is_null() || object::is_fixnum(cls) {
            return;
        }
        let epoch = classes::mule_attribute_epoch.load(Ordering::Relaxed) as c_uint;
        if (*tp).tp_version_tag == epoch && !(*tp).tp_mro.is_null() && !(*tp).tp_dict.is_null() {
            return;
        }
        (*tp).tp_version_tag = epoch;
        let layout = classes::layout_of_class_pub(cls);
        if layout.is_null() {
            return;
        }
        sync_single_type_dict(cls, tp);
        let mro_len = 1 + (*layout).mro.len();
        let mro_tup = PyTuple_New(mro_len as Py_ssize_t);
        Py_IncRef(tp as *mut PyObject);
        PyTuple_SetItem(mro_tup, 0, tp as *mut PyObject);
        for (idx, &anc_cls) in (*layout).mro.iter().enumerate() {
            let anc_tp = py_type_for_class(anc_cls);
            sync_single_type_dict(anc_cls, anc_tp);
            Py_IncRef(anc_tp as *mut PyObject);
            PyTuple_SetItem(mro_tup, (idx + 1) as Py_ssize_t, anc_tp as *mut PyObject);
        }
        let old_mro = (*tp).tp_mro;
        (*tp).tp_mro = mro_tup;
        if !old_mro.is_null() {
            Py_DecRef(old_mro);
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn Py_TYPE(op: *mut PyObject) -> *mut PyTypeObject {
    if op.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let tp = (*op).ob_type;
        sync_type_mro_and_dict(tp);
        tp
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn Py_REFCNT(op: *mut PyObject) -> Py_ssize_t {
    if op.is_null() {
        0
    } else {
        unsafe { (*op).ob_refcnt }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn Py_SIZE(op: *mut PyObject) -> Py_ssize_t {
    if op.is_null() {
        0
    } else {
        unsafe { (*(op as *const PyVarObject)).ob_size }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_Type(o: *mut PyObject) -> *mut PyObject {
    if o.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    unsafe {
        let tp = Py_TYPE(o) as *mut PyObject;
        Py_IncRef(tp);
        tp
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_Format(
    obj: *mut PyObject,
    format_spec: *mut PyObject,
) -> *mut PyObject {
    let o_val = from_py_borrowed(obj);
    if o_val.is_null() {
        unsafe { PyErr_BadInternalCall() };
        return ptr::null_mut();
    }
    let fmt_fn = protocol_lookup("format");
    let res = unsafe {
        if format_spec.is_null() {
            object::call(fmt_fn, &[o_val])
        } else {
            object::call(fmt_fn, &[o_val, from_py_borrowed(format_spec)])
        }
    };
    if res.is_null() || has_pending_exception() {
        return ptr::null_mut();
    }
    to_py_owned(res)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyObject_ClearWeakRefs(_op: *mut PyObject) {}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyType_GetModule(tp: *mut PyTypeObject) -> *mut PyObject {
    if tp.is_null() {
        return ptr::null_mut();
    }
    let found = with_state(|s| s.type_modules.get(&(tp as usize)).copied());
    if let Some(m_addr) = found {
        m_addr as *mut PyObject
    } else {
        unsafe {
            PyErr_SetString(
                PyExc_TypeError,
                c"PyType_GetModule: Type has no associated module".as_ptr(),
            );
        }
        ptr::null_mut()
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyType_GetModuleByDef(
    tp: *mut PyTypeObject,
    def: *mut PyModuleDef,
) -> *mut PyObject {
    if tp.is_null() || def.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        sync_type_mro_and_dict(tp);
        let mro = (*tp).tp_mro;
        if !mro.is_null() {
            let n = PyTuple_Size(mro);
            for i in 0..n {
                let t = PyTuple_GetItem(mro, i) as *mut PyTypeObject;
                let found = with_state(|s| s.type_modules.get(&(t as usize)).copied());
                if let Some(m_addr) = found {
                    let m = m_addr as *mut PyObject;
                    if PyModule_GetDef(m) == def {
                        return m;
                    }
                }
            }
        }
        PyErr_SetString(
            PyExc_TypeError,
            c"PyType_GetModuleByDef: No superclass has the given module".as_ptr(),
        );
        ptr::null_mut()
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyType_GetSlot(
    tp: *mut PyTypeObject,
    slot: c_int,
) -> *mut c_void {
    if tp.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        match slot {
            Py_tp_dealloc => (*tp).tp_dealloc.map(|f| f as *mut c_void).unwrap_or(ptr::null_mut()),
            Py_tp_repr => (*tp).tp_repr.map(|f| f as *mut c_void).unwrap_or(ptr::null_mut()),
            Py_tp_call => (*tp).tp_call.map(|f| f as *mut c_void).unwrap_or(ptr::null_mut()),
            Py_tp_str => (*tp).tp_str.map(|f| f as *mut c_void).unwrap_or(ptr::null_mut()),
            Py_tp_getattro => (*tp).tp_getattro.map(|f| f as *mut c_void).unwrap_or(ptr::null_mut()),
            Py_tp_setattro => (*tp).tp_setattro.map(|f| f as *mut c_void).unwrap_or(ptr::null_mut()),
            Py_tp_descr_get => (*tp).tp_descr_get.map(|f| f as *mut c_void).unwrap_or(ptr::null_mut()),
            Py_tp_descr_set => (*tp).tp_descr_set.map(|f| f as *mut c_void).unwrap_or(ptr::null_mut()),
            Py_tp_init => (*tp).tp_init.map(|f| f as *mut c_void).unwrap_or(ptr::null_mut()),
            Py_tp_new => (*tp).tp_new.map(|f| f as *mut c_void).unwrap_or(ptr::null_mut()),
            _ => ptr::null_mut(),
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyType_GetName(tp: *mut PyTypeObject) -> *mut PyObject {
    unsafe { PyObject_GetAttrString(tp as *mut PyObject, c"__name__".as_ptr()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyType_GetQualName(tp: *mut PyTypeObject) -> *mut PyObject {
    unsafe { PyObject_GetAttrString(tp as *mut PyObject, c"__qualname__".as_ptr()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn Py_GenericAlias(
    origin: *mut PyObject,
    args: *mut PyObject,
) -> *mut PyObject {
    unsafe {
        let types_mod = PyImport_ImportModule(c"types".as_ptr());
        if !types_mod.is_null() {
            let ga = PyObject_GetAttrString(types_mod, c"GenericAlias".as_ptr());
            Py_DecRef(types_mod);
            if !ga.is_null() {
                let tup = PyTuple_New(2);
                Py_IncRef(origin);
                Py_IncRef(args);
                PyTuple_SetItem(tup, 0, origin);
                PyTuple_SetItem(tup, 1, args);
                let res = PyObject_Call(ga, tup, ptr::null_mut());
                Py_DecRef(tup);
                Py_DecRef(ga);
                if !res.is_null() {
                    return res;
                }
                PyErr_Clear();
            } else {
                PyErr_Clear();
            }
        } else {
            PyErr_Clear();
        }
        Py_IncRef(origin);
        origin
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyLong_FromVoidPtr(p: *mut c_void) -> *mut PyObject {
    unsafe { PyLong_FromSize_t(p as usize) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyDict_GetItemRef(
    mp: *mut PyObject,
    key: *mut PyObject,
    result: *mut *mut PyObject,
) -> c_int {
    unsafe {
        let item = PyDict_GetItemWithError(mp, key);
        if !item.is_null() {
            Py_IncRef(item);
            if !result.is_null() {
                *result = item;
            }
            1
        } else if !PyErr_Occurred().is_null() {
            if !result.is_null() {
                *result = ptr::null_mut();
            }
            -1
        } else {
            if !result.is_null() {
                *result = ptr::null_mut();
            }
            0
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyUnicode_Compare(
    left: *mut PyObject,
    right: *mut PyObject,
) -> c_int {
    let a = string_value(from_py_borrowed(left));
    let b = string_value(from_py_borrowed(right));
    match a.cmp(&b) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyUnicode_EqualToUTF8(
    unicode: *mut PyObject,
    str_ptr: *const c_char,
) -> c_int {
    unsafe { c_int::from(PyUnicode_CompareWithASCIIString(unicode, str_ptr) == 0) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyUnicode_Tailmatch(
    str_obj: *mut PyObject,
    substr: *mut PyObject,
    start: Py_ssize_t,
    end: Py_ssize_t,
    direction: c_int,
) -> c_int {
    let s = string_value(from_py_borrowed(str_obj));
    let sub = string_value(from_py_borrowed(substr));
    let chars: Vec<char> = s.chars().collect();
    let len = chars.len() as isize;
    let mut st = if start < 0 { len + start } else { start };
    let mut en = if end < 0 { len + end } else { end };
    st = st.clamp(0, len);
    en = en.clamp(0, len);
    if st > en {
        return 0;
    }
    let slice_str: String = chars[st as usize..en as usize].iter().collect();
    let matched = if direction < 0 {
        slice_str.starts_with(&sub)
    } else {
        slice_str.ends_with(&sub)
    };
    c_int::from(matched)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_NormalizeException(
    exc: *mut *mut PyObject,
    val: *mut *mut PyObject,
    _tb: *mut *mut PyObject,
) {
    if exc.is_null() || val.is_null() || unsafe { (*exc).is_null() } {
        return;
    }
    unsafe {
        let exc_obj = *exc;
        let val_obj = *val;
        if !val_obj.is_null() && PyObject_IsInstance(val_obj, exc_obj) > 0 {
            return;
        }
        let inst = if val_obj.is_null() || val_obj == py_none() {
            PyObject_CallNoArgs(exc_obj)
        } else if PyType_IsSubtype((*val_obj).ob_type, &raw mut PyTuple_Type) != 0 {
            PyObject_Call(exc_obj, val_obj, ptr::null_mut())
        } else {
            PyObject_CallOneArg(exc_obj, val_obj)
        };
        if !inst.is_null() {
            if !val_obj.is_null() {
                Py_DecRef(val_obj);
            }
            *val = inst;
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyErr_NewException(
    name: *const c_char,
    base: *mut PyObject,
    dict: *mut PyObject,
) -> *mut PyObject {
    unsafe {
        if name.is_null() {
            return ptr::null_mut();
        }
        let full = CStr::from_ptr(name).to_str().unwrap_or("Error");
        let (mod_opt, short_name) = match full.rsplit_once('.') {
            Some((m, s)) => (Some(m), s),
            None => (None, full),
        };
        let base_cls = if base.is_null() {
            from_py_borrowed(PyExc_Exception)
        } else {
            from_py_borrowed(base)
        };
        let bases_tup = if is_tuple_value(base_cls) {
            object::retain(base_cls)
        } else {
            make_tuple_from_slice(&[base_cls])
        };
        let ns = object::namespace();
        let c_short = CString::new(short_name).unwrap_or_default();
        let cls_val = classes::mule_class(
            c_short.as_ptr(),
            bases_tup,
            0,
            ptr::null(),
            ns,
            mule_none(),
        );
        object::release(bases_tup);
        object::release(ns);
        if cls_val.is_null() {
            return ptr::null_mut();
        }
        if let Some(m_str) = mod_opt {
            let m_val = make_str_from_rust(m_str);
            set_class_attr(cls_val, "__module__", m_val);
            object::release(m_val);
        }
        if !dict.is_null() {
            let mut pos: Py_ssize_t = 0;
            let mut k: *mut PyObject = ptr::null_mut();
            let mut v: *mut PyObject = ptr::null_mut();
            while PyDict_Next(dict, &raw mut pos, &raw mut k, &raw mut v) != 0 {
                let k_str = string_value(from_py_borrowed(k));
                set_class_attr(cls_val, &k_str, from_py_borrowed(v));
            }
        }
        to_py_owned(cls_val)
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn PyUnstable_Module_SetGIL(
    _module: *mut PyObject,
    _gil: *mut c_void,
) -> c_int {
    0
}

#[inline(never)]
pub(crate) fn ensure_linked() {
    let ptrs: &[*const c_void] = &[
        Py_TYPE as *const c_void,
        Py_REFCNT as *const c_void,
        Py_SIZE as *const c_void,
        Py_IncRef as *const c_void,
        Py_DecRef as *const c_void,
        PyObject_GetAttr as *const c_void,
        PyObject_GenericGetAttr as *const c_void,
        PyObject_GenericSetAttr as *const c_void,
        PyType_FromModuleAndSpec as *const c_void,
        PyType_GetModule as *const c_void,
        PyType_GetModuleByDef as *const c_void,
        PyModule_Create2 as *const c_void,
        PyModuleDef_Init as *const c_void,
        PyDict_GetItemRef as *const c_void,
        PyUnicode_Compare as *const c_void,
        PyUnicode_Tailmatch as *const c_void,
        PyObject_Format as *const c_void,
        PyObject_ClearWeakRefs as *const c_void,
        Py_GenericAlias as *const c_void,
    ];
    std::hint::black_box(ptrs);
}

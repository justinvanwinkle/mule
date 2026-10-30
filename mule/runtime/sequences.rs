//! Physical storage and pending-exception primitives for compiled Mule protocols.
use crate::numeric;
use crate::object::{self, Data, Value};
use std::cell::{Cell, RefCell};
use std::ptr::null_mut;

thread_local! {
    static GLOBALS: Cell<Value> = const { Cell::new(null_mut()) };
    static STORAGE_ITERATOR: Cell<Value> = const { Cell::new(null_mut()) };
    static RANGE_ITERATOR: Cell<Value> = const { Cell::new(null_mut()) };
    static BYTEARRAY_CLASS: Cell<Value> = const { Cell::new(null_mut()) };
    static GENERATOR_CLASS: Cell<Value> = const { Cell::new(null_mut()) };
    static PENDING: Cell<Value> = const { Cell::new(null_mut()) };
    static ACTIVE: RefCell<Vec<Value>> = const { RefCell::new(Vec::new()) };
}
unsafe fn physical_byte(value: Value) -> Option<u8> {
    if object::is_fixnum(value) {
        if let Ok(byte) = u8::try_from(unsafe { object::fixnum(value) }) {
            return Some(byte);
        }
    }
    object::error("ValueError", "byte must be in range(0, 256)");
    None
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_byte_storage_new() -> Value {
    unsafe { object::new_untracked_value(Data::ByteStorage(crate::byte_buffer::ByteBuffer::new())) }
}

/// Cold allocation only. The caller owns validation and export policy.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_byte_storage_reserve(storage: Value, additional: i64) -> i32 {
    unsafe {
        if storage.is_null() || object::is_fixnum(storage) {
            object::error("TypeError", "compact byte storage is required");
            return -1;
        }
        let Data::ByteStorage(buffer) = &mut (*storage).data else {
            object::error("TypeError", "compact byte storage is required");
            return -1;
        };
        if additional < 0 || !buffer.reserve(additional as usize) {
            object::error("MemoryError", "byte storage allocation failed");
            return -1;
        }
        1
    }
}

#[repr(C)]
pub struct ByteView {
    pub data: *const u8,
    pub length: usize,
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_bytes_storage(value: Value, out: *mut ByteView) -> i32 {
    unsafe {
        if value.is_null() || object::is_fixnum(value) || out.is_null() {
            object::error("TypeError", "bytes storage is required");
            return -1;
        }
        let Data::Bytes(bytes, ..) = &(*value).data else {
            object::error("TypeError", "bytes storage is required");
            return -1;
        };
        out.write(ByteView {
            data: bytes.as_ptr(),
            length: bytes.len(),
        });
        1
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_cursor_storage(value: Value, out: *mut ByteView) -> i32 {
    unsafe {
        if value.is_null() || object::is_fixnum(value) || out.is_null() {
            object::error("TypeError", "text storage is required");
            return -1;
        }
        let Data::Text(text, ..) = &(*value).data else {
            object::error("TypeError", "text storage is required");
            return -1;
        };
        let (data, length) = text.raw_storage();
        out.write(ByteView {
            data,
            length: length | (usize::from(text.kind() != 1) << 62),
        });
        1
    }
}
#[inline(always)]
fn replace_pending(new_value: Value) -> Value {
    object::set_has_pending_exception(!new_value.is_null());
    PENDING.with(|value| value.replace(new_value))
}
#[inline(always)]
pub fn has_pending() -> bool {
    PENDING.with(|value| !value.get().is_null())
}
/// Remove and return the pending exception, if any, for later restoration.
pub fn take_pending() -> Value {
    replace_pending(null_mut())
}
/// Clear the pending exception when it is an instance of the protocol
/// class named `kind` or a subclass, reporting whether it was cleared.
pub unsafe fn clear_pending_instance_of(kind: &str) -> bool {
    unsafe {
        let pending = PENDING.with(Cell::get);
        let class = object::lookup(mule_protocol_globals(), kind);
        if pending.is_null() || class.is_null() {
            return false;
        }
        let raised = mule_raised_class(pending);
        let matched = crate::classes::is_subclass(raised, class);
        object::release(raised);
        if matched {
            clear_pending();
            crate::traceback::clear();
        }
        matched
    }
}
/// Reinstate an exception taken with `take_pending`, replacing any current one.
pub fn restore_pending(saved: Value) {
    unsafe { object::release(replace_pending(saved)) };
}
pub fn clear_pending() {
    unsafe { object::release(replace_pending(null_mut())) };
}
pub fn pending_kind() -> String {
    PENDING.with(|value| unsafe {
        let class = mule_raised_class(value.get());
        let name = if !class.is_null() && !object::is_fixnum(class) {
            match &(*class).data {
                Data::Class(layout) => layout.name.clone(),
                _ => "Exception".into(),
            }
        } else {
            "Exception".into()
        };
        object::release(class);
        name
    })
}
pub fn pending_message() -> String {
    PENDING.with(|pending| unsafe {
        let value = pending.get();
        // Formatting runs compiled user code, which may catch errors or fail.
        // Preserve the complete original error, including its traceback.
        let saved = object::save_error();
        let result = object::special(value, "__str__", &[]);
        let message = if result.is_null() {
            "<exception str() failed>".into()
        } else {
            object::formatted_value(result, false, false).map_or_else(String::new, |bytes| {
                String::from_utf8_lossy(&bytes).into_owned()
            })
        };
        object::release(result);
        object::clear_error();
        object::restore_error(saved);
        message
    })
}
pub unsafe fn initialize(native: *mut std::ffi::c_void) -> bool {
    unsafe {
        if native.is_null() {
            return true;
        }
        let globals = object::namespace();
        GLOBALS.with(|value| value.set(globals));
        object::bind(globals, "__builtins__", crate::builtins::namespace());
        // The protocols define the builtins, so their classes report that module.
        let name = object::new_value(Data::Text(crate::text::Text::from_rust_str("builtins"), std::ptr::null_mut(), std::ptr::null_mut()));
        object::bind(globals, "__name__", name);
        object::release(name);
        let function = object::function(
            globals,
            std::mem::transmute::<*mut std::ffi::c_void, object::Native>(native),
            "sequence protocols",
        );
        let result = object::call(function, &[]);
        object::release(function);
        let success = !result.is_null();
        object::release(result);
        success
    }
}
pub unsafe fn finish() {
    unsafe {
        clear_pending();
        ACTIVE.with_borrow_mut(|values| {
            for value in values.drain(..) {
                object::release(value);
            }
        });
        crate::classes::mule_set_attribute_lookup(null_mut());
        STORAGE_ITERATOR.with(|value| value.set(null_mut()));
        RANGE_ITERATOR.with(|value| value.set(null_mut()));
        BYTEARRAY_CLASS.with(|value| value.set(null_mut()));
        GENERATOR_CLASS.with(|value| value.set(null_mut()));
        GLOBALS.with(|value| object::release(value.replace(null_mut())));
    }
}
#[unsafe(no_mangle)]
pub extern "C" fn mule_protocol_globals() -> Value {
    GLOBALS.with(Cell::get)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_install_storage(
    list: Value,
    tuple: Value,
    text: Value,
    bytes: Value,
    dict: Value,
) -> Value {
    unsafe {
        use crate::classes::Builtin;
        for (kind, class) in [
            (Builtin::List, list),
            (Builtin::Tuple, tuple),
            (Builtin::Str, text),
            (Builtin::Bytes, bytes),
            (Builtin::Dict, dict),
        ] {
            crate::classes::register_builtin(kind, class);
        }
        object::none_value()
    }
}
/// Borrow the method of a builtin value's class, found through its bases.
pub unsafe fn binding(value: Value, name: &str) -> Value {
    unsafe {
        if value.is_null() {
            return null_mut();
        }
        let class = crate::classes::class_of_value(value);
        if class.is_null() {
            return null_mut();
        }
        let Data::Class(layout) = &(*class).data else {
            unreachable!()
        };
        layout.special(name)
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_install_builtin(name: Value, value: Value) -> Value {
    unsafe {
        // Builtin classes report the builtins module, wherever they are defined.
        if !value.is_null() && !object::is_fixnum(value) {
            if matches!(&(*value).data, Data::Class(_)) {
                let module = object::new_value(Data::Text(crate::text::Text::from_rust_str("builtins"), std::ptr::null_mut(), std::ptr::null_mut()));
                let status = crate::classes::mule_set_attr(value, c"__module__".as_ptr(), module);
                object::release(module);
                if status < 0 {
                    return null_mut();
                }
            } else if let Data::Function { kind, .. } = &mut (*value).data {
                *kind = object::FunctionKind::Builtin;
            }
        }
        if !name.is_null() && !object::is_fixnum(name) {
            if let Data::Text(name, ..) = &(*name).data {
                if let Some(name) = name.to_rust_string() {
                    if object::bind(crate::builtins::namespace(), &name, value) {
                        return object::none_value();
                    }
                }
            }
        }
        object::error("SystemError", "invalid builtin binding");
        null_mut()
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_integer_storage(value: Value) -> Value {
    unsafe {
        if object::is_fixnum(value) {
            return value;
        }
        if !value.is_null() {
            match &(*value).data {
                Data::BigInt(integer, ty, ..) => {
                    return if ty.is_null() {
                        object::retain(value)
                    } else {
                        crate::numeric::integer_import(integer)
                    };
                }
                Data::Bool(value) => return object::small(i64::from(*value)),
                _ => (),
            }
        }
        let name = object::type_name(value).unwrap_or_else(|| "object".into());
        object::error(
            "TypeError",
            &format!("'{name}' object cannot be interpreted as an integer"),
        );
        null_mut()
    }
}
unsafe fn index(value: Value) -> Option<usize> {
    unsafe {
        let number = if object::is_fixnum(value) {
            Some(object::fixnum(value))
        } else if !value.is_null() {
            match &(*value).data {
                Data::BigInt(number, ..) => number.to_i64(),
                _ => None,
            }
        } else {
            None
        };
        let result = number.and_then(|value| usize::try_from(value).ok());
        if result.is_none() {
            object::error("IndexError", "storage index out of range");
        }
        result
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_tuple_shape_matches(value: Value, arity: usize) -> i32 {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::Args(values, class, ..) = &(*value).data {
                return i32::from(class.is_null() && values.len() == arity);
            }
        }
        0
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_list_shape_matches(value: Value, arity: usize) -> i32 {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::List(values, class, ..) = &(*value).data {
                return i32::from(class.is_null() && values.len() == arity);
            }
        }
        0
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_exact_list(value: Value) -> i32 {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::List(_, class, ..) = &(*value).data {
                return i32::from(class.is_null());
            }
        }
        0
    }
}

/// Exact builtin text excludes subclass overrides from specialized entries.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_exact_text(value: Value) -> i32 {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::Text(_, class, ..) = &(*value).data {
                return i32::from(class.is_null());
            }
        }
        0
    }
}

/// Compiler-proved physical access: a live builtin tuple, an in-range index,
/// and initialized non-NULL elements. The returned element is owned.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_tuple_slot(value: Value, index: usize) -> Value {
    unsafe {
        let Data::Args(values, ..) = &(*value).data else {
            std::hint::unreachable_unchecked();
        };
        object::retain(*values.as_ptr().add(index))
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_storage_length(value: Value) -> Value {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            let length = match &(*value).data {
                Data::List(values, ..) | Data::Args(values, ..) => values.len(),
                Data::Bytes(values, ..) => values.len(),
                Data::ByteStorage(values) => values.length,
                Data::Text(..) => return object::mule_text_length(value),
                Data::Instance { .. } =>
                {
                    if let Some(storage) = crate::buffers::byte_storage(value) {
                        let Data::ByteStorage(bytes) = &(*storage).data else { unreachable!() };
                        return numeric::integer_from_usize(bytes.length);
                    }
                    object::error("TypeError", "sequence storage is required");
                    return null_mut();
                }
                _ => {
                    object::error("TypeError", "sequence storage is required");
                    return null_mut();
                }
            };
            return numeric::integer_from_usize(length);
        }
        object::error("TypeError", "sequence storage is required");
        null_mut()
    }
}
/// The physical write barrier after publishing a compiler-emitted slot store.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_storage_track(container: Value, replacement: Value) {
    unsafe { object::track_if_needed(container, replacement) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_storage_read(value: Value, position: Value) -> Value {
    unsafe {
        let Some(index) = index(position) else {
            return null_mut();
        };
        if !value.is_null() && !object::is_fixnum(value) {
            match &(*value).data {
                Data::List(values, ..) | Data::Args(values, ..) => {
                    if let Some(value) = values.get(index) {
                        return object::retain(*value);
                    }
                }
                Data::Bytes(values, ..) => {
                    if let Some(value) = values.get(index) {
                        return object::small(i64::from(*value));
                    }
                }
                Data::ByteStorage(values) => {
                    if let Some(value) = values.as_slice().get(index) {
                        return object::small(i64::from(*value));
                    }
                }
                Data::Instance { .. } => {
                    if let Some(storage) = crate::buffers::byte_storage(value) {
                        let Data::ByteStorage(bytes) = &(*storage).data else { unreachable!() };
                        if let Some(byte) = bytes.as_slice().get(index) {
                            return object::small(i64::from(*byte));
                        }
                    } else {
                        object::error("TypeError", "sequence storage is required");
                        return null_mut();
                    }
                }
                Data::Text(..) => return object::mule_text_character(value, position),
                _ => {
                    object::error("TypeError", "sequence storage is required");
                    return null_mut();
                }
            }
        }
        object::error("IndexError", "storage index out of range");
        null_mut()
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_storage_write(
    value: Value,
    position: Value,
    replacement: Value,
) -> Value {
    unsafe {
        let Some(index) = index(position) else {
            return null_mut();
        };
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::ByteStorage(values) = &mut (*value).data {
                if index >= values.length {
                    object::error("IndexError", "storage index out of range");
                    return null_mut();
                }
                let Some(byte) = physical_byte(replacement) else {
                    return null_mut();
                };
                values.as_mut_slice()[index] = byte;
                return object::none_value();
            }
            if let Data::List(values, ..) = &mut (*value).data {
                if let Some(slot) = values.get_mut(index) {
                    let old = *slot;
                    *slot = object::retain(replacement);
                    object::track_if_needed(value, replacement);
                    object::release(old);
                    return object::none_value();
                }
                object::error("IndexError", "storage index out of range");
                return null_mut();
            }
        }
        object::error("TypeError", "mutable list storage is required");
        null_mut()
    }
}
#[cold]
#[inline(never)]
unsafe fn list_index_error() -> Value {
    object::error("IndexError", "list index out of range");
    null_mut()
}

/// Direct `list[index]` when `sequence` is a proven exact builtin `list` and
/// `index` is a proven tagged fixnum.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_list_getitem(sequence: Value, index: Value) -> Value {
    unsafe {
        if !object::is_fixnum(index) || sequence.is_null() || object::is_fixnum(sequence) {
            return list_index_error();
        }
        let Data::List(values, ty, ..) = &(*sequence).data else {
            return list_index_error();
        };
        if !ty.is_null() {
            return list_index_error();
        }
        let raw = object::fixnum(index);
        let len = values.len() as i64;
        let pos = if raw < 0 { raw + len } else { raw };
        if (pos as u64) < (len as u64) {
            let elem = *values.get_unchecked(pos as usize);
            if !object::is_fixnum(elem) {
                object::retain(elem);
            }
            return elem;
        }
        list_index_error()
    }
}

#[cold]
#[inline(never)]
unsafe fn sequence_index_error() -> Value {
    object::error("IndexError", "sequence index out of range");
    null_mut()
}

/// Direct `bytes[index]` when `sequence` is a proven exact builtin `bytes` and
/// `index` is a proven tagged fixnum.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_bytes_getitem(sequence: Value, index: Value) -> Value {
    unsafe {
        if !object::is_fixnum(index) || sequence.is_null() || object::is_fixnum(sequence) {
            return sequence_index_error();
        }
        let Data::Bytes(values, ty, ..) = &(*sequence).data else {
            return sequence_index_error();
        };
        if !ty.is_null() {
            return sequence_index_error();
        }
        let raw = object::fixnum(index);
        let len = values.len() as i64;
        let pos = if raw < 0 { raw + len } else { raw };
        if (pos as u64) < (len as u64) {
            return object::small(i64::from(*values.get_unchecked(pos as usize)));
        }
        sequence_index_error()
    }
}

/// Direct `bytearray[index]` when `array` is a proven exact builtin `bytearray`
/// and `index` is a proven tagged fixnum.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_bytearray_getitem_fast(array: Value, index: Value) -> Value {
    unsafe {
        if !object::is_fixnum(index) {
            return sequence_index_error();
        }
        let Some(storage) = crate::buffers::byte_storage(array) else {
            return sequence_index_error();
        };
        let Data::ByteStorage(bytes) = &(*storage).data else {
            unreachable!()
        };
        let raw = object::fixnum(index);
        let pos = if raw < 0 { raw + bytes.length as i64 } else { raw };
        if (pos as u64) < bytes.length as u64 {
            return object::small(i64::from(*bytes.data.add(pos as usize)));
        }
        sequence_index_error()
    }
}

/// Fast path for `bytearray[index] = item` when `array` is an exact `bytearray`,
/// `index` is an in-bounds fixnum, and `item` is a fixnum in `0..=255`.
/// Returns 1 when handled or 0 when the general `setitem` protocol is required.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_bytearray_setitem_fast(
    array: Value,
    index: Value,
    item: Value,
) -> i32 {
    unsafe {
        if !object::is_fixnum(index) || !object::is_fixnum(item) {
            return 0;
        }
        let byte = object::fixnum(item);
        if !(0..=255).contains(&byte) {
            return 0;
        }
        let Some(storage) = crate::buffers::byte_storage(array) else {
            return 0;
        };
        let Data::ByteStorage(bytes) = &mut (*storage).data else {
            unreachable!()
        };
        let raw = object::fixnum(index);
        let pos = if raw < 0 { raw + bytes.length as i64 } else { raw };
        if (pos as u64) < bytes.length as u64 {
            bytes.data.add(pos as usize).write(byte as u8);
            return 1;
        }
        0
    }
}

/// Fast path for `bytearray.append(item)` on a proven `bytearray` receiver.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_bytearray_append_fast(array: Value, item: Value) -> Value {
    unsafe {
        let function = object::lookup(mule_protocol_globals(), "bytearray_append");
        if function.is_null() {
            return null_mut();
        }
        object::call(function, &[array, item])
    }
}

/// Borrowed `list[index]` when `sequence` is a proven exact builtin `list`
/// kept alive by the caller across the consumer, and `index` is a proven
/// tagged fixnum.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_list_getitem_borrow(sequence: Value, index: Value) -> Value {
    unsafe {
        if !object::is_fixnum(index) || sequence.is_null() || object::is_fixnum(sequence) {
            return list_index_error();
        }
        let Data::List(values, ty, ..) = &(*sequence).data else {
            return list_index_error();
        };
        if !ty.is_null() {
            return list_index_error();
        }
        let raw = object::fixnum(index);
        let len = values.len() as i64;
        let pos = if raw < 0 { raw + len } else { raw };
        if (pos as u64) < (len as u64) {
            return *values.get_unchecked(pos as usize);
        }
        list_index_error()
    }
}

#[inline(never)]
unsafe fn fast_getitem_non_list(sequence: Value, raw: i64) -> Value {
    unsafe {
        match &(*sequence).data {
            Data::Args(values, ty, ..) if ty.is_null() => {
                let len = values.len() as i64;
                let pos = if raw < 0 { raw + len } else { raw };
                if (pos as u64) < (len as u64) {
                    let elem = *values.get_unchecked(pos as usize);
                    if !object::is_fixnum(elem) {
                        object::retain(elem);
                    }
                    return elem;
                }
                null_mut()
            }
            Data::Bytes(values, ty, ..) if ty.is_null() => {
                let len = values.len() as i64;
                let pos = if raw < 0 { raw + len } else { raw };
                if (pos as u64) < (len as u64) {
                    return object::small(i64::from(*values.get_unchecked(pos as usize)));
                }
                null_mut()
            }
            Data::Text(text, ty, ..) if ty.is_null() => {
                let len = text.len() as i64;
                let pos = if raw < 0 { raw + len } else { raw };
                if (pos as u64) < (len as u64) {
                    return object::text_character_at(text, pos as usize);
                }
                null_mut()
            }
            _ => null_mut(),
        }
    }
}

/// Fast path for `sequence[index]` when `sequence` is an exact builtin
/// `list`, `tuple`, `bytes`, or `str` (`ty.is_null()`) and `index` is an
/// in-bounds fixnum (non-negative or negative). Returns NULL without setting
/// an error when the operands need the general compiled `getitem` protocol.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_fast_getitem(sequence: Value, index: Value) -> Value {
    unsafe {
        if !object::is_fixnum(index) || sequence.is_null() || object::is_fixnum(sequence) {
            return null_mut();
        }
        let raw = object::fixnum(index);
        if let Data::List(values, ty, ..) = &(*sequence).data {
            if ty.is_null() {
                let len = values.len() as i64;
                let pos = if raw < 0 { raw + len } else { raw };
                if (pos as u64) < (len as u64) {
                    let elem = *values.get_unchecked(pos as usize);
                    if !object::is_fixnum(elem) {
                        object::retain(elem);
                    }
                    return elem;
                }
            }
            return null_mut();
        }
        fast_getitem_non_list(sequence, raw)
    }
}

unsafe fn slice_bound(value: Value, default: i64, len: i64) -> Option<usize> {
    unsafe {
        if value.is_null() {
            return None;
        }
        if object::is_fixnum(value) {
            let raw = object::fixnum(value);
            let adjusted = if raw < 0 { raw + len } else { raw };
            return Some(adjusted.clamp(0, len) as usize);
        }
        match &(*value).data {
            Data::None => Some(default as usize),
            Data::Bool(flag) => {
                let raw = i64::from(*flag);
                Some(raw.clamp(0, len) as usize)
            }
            Data::BigInt(bigint, ty, ..) if ty.is_null() => {
                Some(if bigint.sign() < 0 { 0 } else { len as usize })
            }
            _ => None,
        }
    }
}

/// Fast path for `sequence[start:stop:step]` when `sequence` is an exact
/// builtin `str`, `bytes`, `list`, or `tuple`, `step` is `None` or `1`, and
/// `start`/`stop` are `None` or fixnums. Avoids allocating a `slice` object.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_fast_getslice(
    sequence: Value,
    start: Value,
    stop: Value,
    step: Value,
) -> Value {
    unsafe {
        if sequence.is_null() || object::is_fixnum(sequence) || step.is_null() {
            return null_mut();
        }
        let unit_step = if object::is_fixnum(step) {
            object::fixnum(step) == 1
        } else {
            matches!(&(*step).data, Data::None)
        };
        if !unit_step {
            return null_mut();
        }
        match &(*sequence).data {
            Data::Text(text, ty, ..) if ty.is_null() => {
                let len = text.len() as i64;
                let (Some(s), Some(e)) = (slice_bound(start, 0, len), slice_bound(stop, len, len))
                else {
                    return null_mut();
                };
                if s == 0 && e >= text.len() {
                    return object::retain(sequence);
                }
                object::text_slice_range(text, s, e)
            }
            Data::Bytes(values, ty, ..) if ty.is_null() => {
                let len = values.len() as i64;
                let (Some(s), Some(e)) = (slice_bound(start, 0, len), slice_bound(stop, len, len))
                else {
                    return null_mut();
                };
                let slice = if s < e { &values[s..e] } else { &[] };
                let mut copied = Vec::new();
                if copied.try_reserve_exact(slice.len()).is_err() {
                    object::error("MemoryError", "bytes allocation failed");
                    return null_mut();
                }
                copied.extend_from_slice(slice);
                object::new_value(Data::Bytes(copied, null_mut(), null_mut()))
            }
            Data::List(values, ty, ..) if ty.is_null() => {
                let len = values.len() as i64;
                let (Some(s), Some(e)) = (slice_bound(start, 0, len), slice_bound(stop, len, len))
                else {
                    return null_mut();
                };
                let slice = if s < e { &values[s..e] } else { &[] };
                object::mule_sequence_list(slice.len() as isize, slice.as_ptr())
            }
            Data::Args(values, ty, ..) if ty.is_null() => {
                let len = values.len() as i64;
                let (Some(s), Some(e)) = (slice_bound(start, 0, len), slice_bound(stop, len, len))
                else {
                    return null_mut();
                };
                let slice = if s < e { &values[s..e] } else { &[] };
                object::mule_sequence_tuple(slice.len() as isize, slice.as_ptr())
            }
            _ => null_mut(),
        }
    }
}

/// Fast path for `sequence[start:stop] == target` (or `!=` when `invert != 0`)
/// when `sequence` and `target` are exact builtin `str` objects (`ty.is_null()`)
/// and `start`/`stop` are `None` or fixnums. Compares the slice in-place
/// without allocating a substring; falls back to `mule_fast_getslice` +
/// `mule_compare` if any precondition is not met, or returns NULL when the
/// general compiled `getslice` protocol is required.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_slice_eq(
    sequence: Value,
    start: Value,
    stop: Value,
    target: Value,
    invert: i32,
) -> Value {
    unsafe {
        if !sequence.is_null()
            && !object::is_fixnum(sequence)
            && !target.is_null()
            && !object::is_fixnum(target)
        {
            if let (Data::Text(text, ty, ..), Data::Text(other, other_ty, ..)) =
                (&(*sequence).data, &(*target).data)
            {
                if ty.is_null() && other_ty.is_null() {
                    let len = text.len() as i64;
                    if let (Some(s), Some(e)) =
                        (slice_bound(start, 0, len), slice_bound(stop, len, len))
                    {
                        let matched = text.slice_eq(s, e, other);
                        return object::bool_value(if invert != 0 { !matched } else { matched });
                    }
                }
            }
        }
        let slice = mule_fast_getslice(sequence, start, stop, object::none_value());
        if slice.is_null() {
            return null_mut();
        }
        let op = if invert != 0 { 1 } else { 0 };
        let result = object::compare(op, slice, target);
        object::release(slice);
        result
    }
}

/// Fast path for `left[start1:stop1] == right[start2:stop2]` (or `!=` when
/// `invert != 0`) when both `left` and `right` are exact builtin `str` objects
/// (`ty.is_null()`) and all slice bounds are `None` or fixnums. Compares both
/// slices in-place without allocating either substring.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_slice2_eq(
    left: Value,
    start1: Value,
    stop1: Value,
    right: Value,
    start2: Value,
    stop2: Value,
    invert: i32,
) -> Value {
    unsafe {
        if !left.is_null()
            && !object::is_fixnum(left)
            && !right.is_null()
            && !object::is_fixnum(right)
        {
            if let (Data::Text(text1, ty1, ..), Data::Text(text2, ty2, ..)) =
                (&(*left).data, &(*right).data)
            {
                if ty1.is_null() && ty2.is_null() {
                    let len1 = text1.len() as i64;
                    let len2 = text2.len() as i64;
                    if let (Some(s1), Some(e1), Some(s2), Some(e2)) = (
                        slice_bound(start1, 0, len1),
                        slice_bound(stop1, len1, len1),
                        slice_bound(start2, 0, len2),
                        slice_bound(stop2, len2, len2),
                    ) {
                        let matched = text1.slice_range_eq(s1, e1, text2, s2, e2);
                        return object::bool_value(if invert != 0 { !matched } else { matched });
                    }
                }
            }
        }
        let slice1 = mule_fast_getslice(left, start1, stop1, object::none_value());
        if slice1.is_null() {
            return null_mut();
        }
        let slice2 = mule_fast_getslice(right, start2, stop2, object::none_value());
        if slice2.is_null() {
            object::release(slice1);
            return null_mut();
        }
        let op = if invert != 0 { 1 } else { 0 };
        let result = object::compare(op, slice1, slice2);
        object::release(slice1);
        object::release(slice2);
        result
    }
}
#[inline(never)]
unsafe fn setitem_retain_and_release(item: Value, old: Value) -> i32 {
    unsafe {
        if !object::is_fixnum(item) {
            object::retain(item);
        }
        if !object::is_fixnum(old) {
            object::release(old);
        }
        1
    }
}
/// Fast path for `sequence[index] = item` when `sequence` is an exact builtin
/// `list` (`ty.is_null()`) and `index` is an in-bounds fixnum. Returns 1 when
/// handled (retaining `item` and releasing the old element), or 0 when the
/// operands need the general compiled `setitem` protocol.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_fast_setitem(sequence: Value, index: Value, item: Value) -> i32 {
    unsafe {
        if !object::is_fixnum(index) || sequence.is_null() || object::is_fixnum(sequence) {
            return 0;
        }
        let raw = object::fixnum(index);
        if let Data::List(values, ty, ..) = &mut (*sequence).data {
            if ty.is_null() {
                let len = values.len() as i64;
                let pos = if raw < 0 { raw + len } else { raw };
                if pos >= 0 && pos < len {
                    let slot = values.get_unchecked_mut(pos as usize);
                    let old = *slot;
                    *slot = item;
                    if object::is_fixnum(item) && object::is_fixnum(old) {
                        return 1;
                    }
                    return setitem_retain_and_release(item, old);
                }
            }
        }
        0
    }
}
/// Fast path for `sequence[index] = item` that transfers ownership of `item`
/// when `sequence` is an exact builtin `list` (`ty.is_null()`) and `index` is
/// an in-bounds fixnum. When returning 0, `item` remains owned by the caller.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_fast_setitem_move(sequence: Value, index: Value, item: Value) -> i32 {
    unsafe {
        if !object::is_fixnum(index) || sequence.is_null() || object::is_fixnum(sequence) {
            return 0;
        }
        let raw = object::fixnum(index);
        if let Data::List(values, ty, ..) = &mut (*sequence).data {
            if ty.is_null() {
                let len = values.len() as i64;
                let pos = if raw < 0 { raw + len } else { raw };
                if pos >= 0 && pos < len {
                    let slot = values.get_unchecked_mut(pos as usize);
                    let old = *slot;
                    *slot = item;
                    if !object::is_fixnum(old) {
                        object::release(old);
                    }
                    return 1;
                }
            }
        }
        0
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_storage_append(value: Value, element: Value) -> Value {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::ByteStorage(values) = &mut (*value).data {
                let Some(byte) = physical_byte(element) else {
                    return null_mut();
                };
                if !values.push(byte) {
                    object::error("MemoryError", "byte storage allocation failed");
                    return null_mut();
                }
                return object::none_value();
            }
            if let Data::List(values, ..) = &mut (*value).data {
                if values.try_reserve(1).is_err() {
                    object::error("MemoryError", "list allocation failed");
                    return null_mut();
                }
                values.push(object::retain(element));
                object::track_if_needed(value, element);
                return object::none_value();
            }
        }
        object::error("TypeError", "mutable list storage is required");
        null_mut()
    }
}
/// Append `element` to a mutable list, consuming the caller's owned `element` reference.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_storage_append_move(value: Value, element: Value) -> Value {
    unsafe {
        if !value.is_null()
            && !object::is_fixnum(value)
            && matches!(&(*value).data, Data::ByteStorage(_))
        {
            let result = mule_storage_append(value, element);
            object::release(element);
            return result;
        }
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::List(values, ..) = &mut (*value).data {
                if values.try_reserve(1).is_err() {
                    object::release(element);
                    object::error("MemoryError", "list allocation failed");
                    return null_mut();
                }
                values.push(element);
                object::track_if_needed(value, element);
                return object::none_value();
            }
        }
        object::release(element);
        object::error("TypeError", "mutable list storage is required");
        null_mut()
    }
}
/// Fast path for `str.find(sub[, start[, end]])` and `str.rfind(sub[, start[, end]])`
/// when `receiver` and `sub` are proven strings and `start`/`end` are fixnums or `None`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_find(
    receiver: Value,
    sub: Value,
    start: Value,
    end: Value,
    reverse: i32,
) -> Value {
    unsafe {
        if !receiver.is_null()
            && !object::is_fixnum(receiver)
            && !sub.is_null()
            && !object::is_fixnum(sub)
        {
            if let (Data::Text(text, ty, ..), Data::Text(sub_text, sub_ty, ..)) =
                (&(*receiver).data, &(*sub).data)
            {
                if ty.is_null() && sub_ty.is_null() {
                    let len = text.len() as i64;
                    let s = if start.is_null() {
                        Some(0usize)
                    } else if object::is_fixnum(start) {
                        let raw = object::fixnum(start);
                        let adj = if raw < 0 {
                            (raw + len).max(0)
                        } else if raw > len {
                            len + 1
                        } else {
                            raw
                        };
                        Some(adj as usize)
                    } else if matches!(&(*start).data, Data::None) {
                        Some(0usize)
                    } else {
                        None
                    };
                    let e = if end.is_null() {
                        Some(text.len())
                    } else if object::is_fixnum(end) {
                        let raw = object::fixnum(end);
                        let adj = if raw > len {
                            len
                        } else if raw < 0 {
                            (raw + len).max(0)
                        } else {
                            raw
                        };
                        Some(adj as usize)
                    } else if matches!(&(*end).data, Data::None) {
                        Some(text.len())
                    } else {
                        None
                    };
                    if let (Some(s), Some(e)) = (s, e) {
                        let found = if reverse != 0 {
                            text.rfind(sub_text, s, e)
                        } else {
                            text.find(sub_text, s, e)
                        };
                        return object::small(found.map_or(-1, |pos| pos as i64));
                    }
                }
            }
        }
        let mut bound = null_mut();
        let attr = if reverse != 0 {
            c"rfind".as_ptr()
        } else {
            c"find".as_ptr()
        };
        let method = crate::classes::mule_resolve_method(receiver, attr, null_mut(), &mut bound);
        if method.is_null() {
            return null_mut();
        }
        let s_arg = if start.is_null() {
            object::none_value()
        } else {
            start
        };
        let e_arg = if end.is_null() {
            object::none_value()
        } else {
            end
        };
        let args = [sub, s_arg, e_arg];
        let result = crate::classes::mule_call_resolved(method, bound, 3, args.as_ptr());
        object::release(bound);
        object::release(method);
        result
    }
}

#[inline]
unsafe fn exact_text_slice(
    value: Value,
    text: &crate::text::Text,
    start: usize,
    stop: usize,
) -> Value {
    unsafe {
        if start == 0 && stop >= text.len() {
            object::retain(value)
        } else {
            object::text_slice_range(text, start, stop)
        }
    }
}

/// Fast path for exact `str.partition(sep)` / `str.rpartition(sep)` unpacking directly
/// into `out[0..3]`. Returns 1 on success and 0 on exception.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_partition_into(
    receiver: Value,
    sep: Value,
    reverse: i32,
    out: *mut Value,
) -> i32 {
    unsafe {
        if !receiver.is_null() && !object::is_fixnum(receiver) {
            if let Data::Text(text, ty, ..) = &(*receiver).data
                && ty.is_null()
                && !sep.is_null()
                && !object::is_fixnum(sep)
            {
                if let Data::Text(sep_text, sep_ty, ..) = &(*sep).data
                    && sep_ty.is_null()
                {
                    if let (Some(haystack), Some(needle)) = (text.as_latin1(), sep_text.as_latin1()) {
                        let sep_len = needle.len();
                        if sep_len == 0 {
                            object::error("ValueError", "empty separator");
                            return 0;
                        }
                        let len = haystack.len();
                        let found = if sep_len > len {
                            None
                        } else if sep_len == 1 {
                            let b0 = needle[0];
                            if reverse != 0 {
                                haystack.iter().rposition(|&b| b == b0)
                            } else {
                                haystack.iter().position(|&b| b == b0)
                            }
                        } else if reverse != 0 {
                            text.rfind(sep_text, 0, len)
                        } else {
                            text.find(sep_text, 0, len)
                        };
                        if let Some(pos) = found {
                            let before = if pos == len {
                                object::retain(receiver)
                            } else {
                                object::latin1_slice_value(&haystack[..pos])
                            };
                            if before.is_null() {
                                return 0;
                            }
                            let after_start = pos + sep_len;
                            let after = if after_start == 0 {
                                object::retain(receiver)
                            } else {
                                object::latin1_slice_value(&haystack[after_start..])
                            };
                            if after.is_null() {
                                object::release(before);
                                return 0;
                            }
                            *out.add(0) = before;
                            *out.add(1) = object::retain(sep);
                            *out.add(2) = after;
                            return 1;
                        }
                        let empty = object::empty_text();
                        if reverse != 0 {
                            *out.add(0) = empty;
                            *out.add(1) = empty;
                            *out.add(2) = object::retain(receiver);
                        } else {
                            *out.add(0) = object::retain(receiver);
                            *out.add(1) = empty;
                            *out.add(2) = empty;
                        }
                        return 1;
                    }
                    if sep_text.is_empty() {
                        object::error("ValueError", "empty separator");
                        return 0;
                    }
                    let len = text.len();
                    let found = if reverse != 0 {
                        text.rfind(sep_text, 0, len)
                    } else {
                        text.find(sep_text, 0, len)
                    };
                    if let Some(pos) = found {
                        let before = exact_text_slice(receiver, text, 0, pos);
                        if before.is_null() {
                            return 0;
                        }
                        let after = exact_text_slice(receiver, text, pos + sep_text.len(), len);
                        if after.is_null() {
                            object::release(before);
                            return 0;
                        }
                        *out.add(0) = before;
                        *out.add(1) = object::retain(sep);
                        *out.add(2) = after;
                        return 1;
                    }
                    let empty = object::empty_text();
                    if reverse != 0 {
                        *out.add(0) = empty;
                        *out.add(1) = empty;
                        *out.add(2) = object::retain(receiver);
                    } else {
                        *out.add(0) = object::retain(receiver);
                        *out.add(1) = empty;
                        *out.add(2) = empty;
                    }
                    return 1;
                }
            }
        }
        let tuple = mule_text_partition(receiver, sep, reverse);
        if tuple.is_null() {
            return 0;
        }
        if !object::is_fixnum(tuple)
            && let Data::Args(values, ty, ..) = &(*tuple).data
            && ty.is_null()
            && values.len() == 3
        {
            *out.add(0) = object::retain(values[0]);
            *out.add(1) = object::retain(values[1]);
            *out.add(2) = object::retain(values[2]);
            object::release(tuple);
            return 1;
        }
        let unpack_fn = object::lookup(mule_protocol_globals(), "unpack");
        let unpacked = if unpack_fn.is_null() {
            null_mut()
        } else {
            object::call(unpack_fn, &[tuple, object::small(3)])
        };
        object::release(tuple);
        if unpacked.is_null() {
            return 0;
        }
        for idx in 0..3usize {
            let item = mule_storage_read(unpacked, object::small(idx as i64));
            if item.is_null() {
                for prev in 0..idx {
                    object::release(*out.add(prev));
                }
                object::release(unpacked);
                return 0;
            }
            *out.add(idx) = item;
        }
        object::release(unpacked);
        1
    }
}

/// Fast path for `str == str` / `str != str` when both operands are proven `str`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_eq(lhs: Value, rhs: Value) -> i32 {
    if lhs == rhs {
        return 1;
    }
    unsafe {
        let (Data::Text(left, ..), Data::Text(right, ..)) = (&(*lhs).data, &(*rhs).data) else {
            return 0;
        };
        i32::from(left == right)
    }
}

/// Fast path for `str.partition(sep)` / `str.rpartition(sep)` returning a 3-tuple.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_partition(
    receiver: Value,
    sep: Value,
    reverse: i32,
) -> Value {
    unsafe {
        if !receiver.is_null()
            && !object::is_fixnum(receiver)
            && matches!(&(*receiver).data, Data::Text(_, ty, ..) if ty.is_null())
            && !sep.is_null()
            && !object::is_fixnum(sep)
            && matches!(&(*sep).data, Data::Text(_, sep_ty, ..) if sep_ty.is_null())
        {
            let mut slots = [null_mut(); 3];
            if mule_text_partition_into(receiver, sep, reverse, slots.as_mut_ptr()) == 0 {
                return null_mut();
            }
            return object::mule_sequence_tuple_owned(3, slots.as_ptr());
        }
        let mut bound = null_mut();
        let attr = if reverse != 0 {
            c"rpartition".as_ptr()
        } else {
            c"partition".as_ptr()
        };
        let method = crate::classes::mule_resolve_method(receiver, attr, null_mut(), &mut bound);
        if method.is_null() {
            return null_mut();
        }
        let args = [sep];
        let result = crate::classes::mule_call_resolved(method, bound, 1, args.as_ptr());
        object::release(bound);
        object::release(method);
        result
    }
}

/// Fast path for `str.split(sep, maxsplit)` when `receiver` and `sep` are exact `str`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_split(
    receiver: Value,
    sep: Value,
    maxsplit: Value,
) -> Value {
    unsafe {
        if !receiver.is_null() && !object::is_fixnum(receiver) {
            if let Data::Text(text, ty, ..) = &(*receiver).data
                && ty.is_null()
                && !sep.is_null()
                && !object::is_fixnum(sep)
            {
                if let Data::Text(sep_text, sep_ty, ..) = &(*sep).data
                    && sep_ty.is_null()
                {
                    let max_val = if maxsplit.is_null() {
                        Some(-1i64)
                    } else if object::is_fixnum(maxsplit) {
                        Some(object::fixnum(maxsplit))
                    } else {
                        match &(*maxsplit).data {
                            Data::Bool(flag) => Some(i64::from(*flag)),
                            Data::BigInt(number, int_ty, ..) if int_ty.is_null() => {
                                let Some(limit) = number.to_i64() else {
                                    object::error("OverflowError", "Python int too large to convert to C ssize_t");
                                    return null_mut();
                                };
                                Some(limit)
                            }
                            _ => None,
                        }
                    };
                    if let Some(mut remaining) = max_val {
                        if let (Some(haystack), Some(needle)) = (text.as_latin1(), sep_text.as_latin1()) {
                            let sep_len = needle.len();
                            if sep_len == 0 {
                                object::error("ValueError", "empty separator");
                                return null_mut();
                            }
                            let len = haystack.len();
                            let mut parts = Vec::with_capacity(8);
                            let mut start = 0usize;
                            if sep_len == 1 {
                                let b0 = needle[0];
                                while remaining != 0 {
                                    let Some(rel) = haystack[start..].iter().position(|&b| b == b0) else {
                                        break;
                                    };
                                    let pos = start + rel;
                                    let piece = if start == 0 && pos == len {
                                        object::retain(receiver)
                                    } else {
                                        object::latin1_slice_value(&haystack[start..pos])
                                    };
                                    if piece.is_null() {
                                        for item in parts {
                                            object::release(item);
                                        }
                                        return null_mut();
                                    }
                                    parts.push(piece);
                                    start = pos + 1;
                                    if remaining > 0 {
                                        remaining -= 1;
                                    }
                                }
                            } else {
                                while remaining != 0 {
                                    let Some(pos) = text.find(sep_text, start, len) else {
                                        break;
                                    };
                                    let piece = if start == 0 && pos == len {
                                        object::retain(receiver)
                                    } else {
                                        object::latin1_slice_value(&haystack[start..pos])
                                    };
                                    if piece.is_null() {
                                        for item in parts {
                                            object::release(item);
                                        }
                                        return null_mut();
                                    }
                                    parts.push(piece);
                                    start = pos + sep_len;
                                    if remaining > 0 {
                                        remaining -= 1;
                                    }
                                }
                            }
                            let tail = if start == 0 {
                                object::retain(receiver)
                            } else {
                                object::latin1_slice_value(&haystack[start..])
                            };
                            if tail.is_null() {
                                for item in parts {
                                    object::release(item);
                                }
                                return null_mut();
                            }
                            parts.push(tail);
                            return object::new_untracked_value(Data::List(parts.into(), null_mut(), null_mut()));
                        }
                        if sep_text.is_empty() {
                            object::error("ValueError", "empty separator");
                            return null_mut();
                        }
                        let len = text.len();
                        let sep_len = sep_text.len();
                        let mut parts = Vec::with_capacity(8);
                        let mut start = 0usize;
                        while remaining != 0 {
                            let Some(pos) = text.find(sep_text, start, len) else {
                                 break;
                            };
                            let piece = exact_text_slice(receiver, text, start, pos);
                            if piece.is_null() {
                                for item in parts {
                                    object::release(item);
                                }
                                return null_mut();
                            }
                            parts.push(piece);
                            start = pos + sep_len;
                            if remaining > 0 {
                                remaining -= 1;
                            }
                        }
                        let tail = exact_text_slice(receiver, text, start, len);
                        if tail.is_null() {
                            for item in parts {
                                object::release(item);
                            }
                            return null_mut();
                        }
                        parts.push(tail);
                        return object::new_untracked_value(Data::List(parts.into(), null_mut(), null_mut()));
                    }
                }
            }
        }
        let mut bound = null_mut();
        let method =
            crate::classes::mule_resolve_method(receiver, c"split".as_ptr(), null_mut(), &mut bound);
        if method.is_null() {
            return null_mut();
        }
        let result = if maxsplit.is_null() {
            let args = [sep];
            crate::classes::mule_call_resolved(method, bound, 1, args.as_ptr())
        } else {
            let args = [sep, maxsplit];
            crate::classes::mule_call_resolved(method, bound, 2, args.as_ptr())
        };
        object::release(bound);
        object::release(method);
        result
    }
}

/// Fast path for `str.startswith` (`is_end == 0`) and `str.endswith` (`is_end != 0`).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_affix(
    receiver: Value,
    affix: Value,
    start: Value,
    end: Value,
    is_end: i32,
) -> Value {
    unsafe {
        if !receiver.is_null() && !object::is_fixnum(receiver) {
            if let Data::Text(text, ty, ..) = &(*receiver).data
                && ty.is_null()
                && !affix.is_null()
                && !object::is_fixnum(affix)
            {
                let len = text.len() as i64;
                let s = if start.is_null() {
                    Some(0usize)
                } else if object::is_fixnum(start) {
                    let raw = object::fixnum(start);
                    let adj = if raw < 0 {
                        (raw + len).max(0)
                    } else if raw > len {
                        len + 1
                    } else {
                        raw
                    };
                    Some(adj as usize)
                } else if matches!(&(*start).data, Data::None) {
                    Some(0usize)
                } else {
                    None
                };
                let e = if end.is_null() {
                    Some(text.len())
                } else if object::is_fixnum(end) {
                    let raw = object::fixnum(end);
                    let adj = if raw > len {
                        len
                    } else if raw < 0 {
                        (raw + len).max(0)
                    } else {
                        raw
                    };
                    Some(adj as usize)
                } else if matches!(&(*end).data, Data::None) {
                    Some(text.len())
                } else {
                    None
                };
                if let (Some(s), Some(e)) = (s, e) {
                    match &(*affix).data {
                        Data::Text(sub_text, ..) => {
                            let size = sub_text.len();
                            let matched = if e < s || e - s < size {
                                false
                            } else if is_end != 0 {
                                text.slice_eq(e - size, e, sub_text)
                            } else {
                                text.slice_eq(s, s + size, sub_text)
                            };
                            return object::bool_value(matched);
                        }
                        Data::Args(items, ..) => {
                            let mut all_text = true;
                            for &item in items {
                                if item.is_null() || object::is_fixnum(item) {
                                    all_text = false;
                                    break;
                                }
                                let Data::Text(sub_text, ..) = &(*item).data else {
                                    all_text = false;
                                    break;
                                };
                                let size = sub_text.len();
                                let matched = if e < s || e - s < size {
                                    false
                                } else if is_end != 0 {
                                    text.slice_eq(e - size, e, sub_text)
                                } else {
                                    text.slice_eq(s, s + size, sub_text)
                                };
                                if matched {
                                    return object::bool_value(true);
                                }
                            }
                            if all_text {
                                return object::bool_value(false);
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
        let mut bound = null_mut();
        let attr = if is_end != 0 {
            c"endswith".as_ptr()
        } else {
            c"startswith".as_ptr()
        };
        let method = crate::classes::mule_resolve_method(receiver, attr, null_mut(), &mut bound);
        if method.is_null() {
            return null_mut();
        }
        let s_arg = if start.is_null() {
            object::none_value()
        } else {
            start
        };
        let e_arg = if end.is_null() {
            object::none_value()
        } else {
            end
        };
        let args = [affix, s_arg, e_arg];
        let result = crate::classes::mule_call_resolved(method, bound, 3, args.as_ptr());
        object::release(bound);
        object::release(method);
        result
    }
}
/// Insert an element before a position no greater than the list's length.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_storage_insert(
    value: Value,
    position: Value,
    element: Value,
) -> Value {
    unsafe {
        let Some(index) = index(position) else {
            return null_mut();
        };
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::List(values, ..) = &mut (*value).data {
                if index > values.len() {
                    object::error("IndexError", "storage index out of range");
                    return null_mut();
                }
                if values.try_reserve(1).is_err() {
                    object::error("MemoryError", "list allocation failed");
                    return null_mut();
                }
                values.insert(index, object::retain(element));
                object::track_if_needed(value, element);
                return object::none_value();
            }
        }
        object::error("TypeError", "mutable list storage is required");
        null_mut()
    }
}
/// Remove the elements in positions [start, stop) of a list.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_storage_delete(value: Value, start: Value, stop: Value) -> Value {
    unsafe {
        let (Some(start), Some(stop)) = (index(start), index(stop)) else {
            return null_mut();
        };
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::ByteStorage(values) = &mut (*value).data {
                if start > stop || stop > values.length {
                    object::error("IndexError", "storage index out of range");
                    return null_mut();
                }
                values.delete(start, stop);
                return object::none_value();
            }
            if let Data::List(values, ..) = &mut (*value).data {
                if start > stop || stop > values.len() {
                    object::error("IndexError", "storage index out of range");
                    return null_mut();
                }
                if start == stop {
                    return object::none_value();
                }
                if stop - start == 1 {
                    let removed = if stop == values.len() {
                        values.pop().unwrap()
                    } else {
                        values.remove(start)
                    };
                    object::release(removed);
                    return object::none_value();
                }
                // Release after removal: a destructor may observe the list.
                let removed = values.remove_range(start..stop);
                for element in removed {
                    object::release(element);
                }
                return object::none_value();
            }
        }
        object::error("TypeError", "mutable list storage is required");
        null_mut()
    }
}
/// Pop and return the last element of a mutable list, transferring ownership.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_storage_pop(value: Value) -> Value {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::List(values, ..) = &mut (*value).data {
                if let Some(element) = values.pop() {
                    return element;
                }
                object::error("IndexError", "pop from empty list");
                return null_mut();
            }
        }
        object::error("TypeError", "mutable list storage is required");
        null_mut()
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_tuple_storage(value: Value) -> Value {
    unsafe {
        object::bool_value(
            !value.is_null()
                && !object::is_fixnum(value)
                && matches!(&(*value).data, Data::Args(..)),
        )
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_storage_tuple(value: Value) -> Value {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            match &(*value).data {
                Data::List(values, ..) => {
                    return object::mule_sequence_tuple(values.len() as isize, values.as_ptr());
                }
                Data::Args(..) => return object::retain(value),
                _ => (),
            }
        }
        object::error("TypeError", "element storage is required");
        null_mut()
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_storage_compatible(left: Value, right: Value) -> Value {
    unsafe {
        let result = !left.is_null()
            && !right.is_null()
            && !object::is_fixnum(left)
            && !object::is_fixnum(right)
            && matches!(
                (&(*left).data, &(*right).data),
                (Data::List(..), Data::List(..)) | (Data::Args(..), Data::Args(..))
            );
        object::bool_value(result)
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_class_identity(value: Value) -> Value {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) && matches!(&(*value).data, Data::Class(_))
        {
            return object::retain(value);
        }
        object::none_value()
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_class_parent(value: Value) -> Value {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::Class(layout) = &(*value).data {
                return if layout.base.is_null() {
                    object::none_value()
                } else {
                    object::retain(layout.base)
                };
            }
        }
        object::error("SystemError", "class storage is required");
        null_mut()
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_class_is_subclass(derived: Value, class: Value) -> Value {
    unsafe { object::bool_value(crate::classes::is_subclass(derived, class)) }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_raised_class(value: Value) -> Value {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            match &(*value).data {
                Data::Class(_) => return object::retain(value),
                Data::Instance { ty, .. } | Data::NativeInstance { ty, .. } => {
                    return object::retain(*ty);
                }
                _ => (),
            }
        }
        object::none_value()
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_exception_targets(value: Value) -> Value {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) && matches!(&(*value).data, Data::Args(..))
        {
            return object::retain(value);
        }
        object::mule_sequence_tuple(1, &value)
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_invalid_raise() -> Value {
    object::error("TypeError", "exceptions must derive from BaseException");
    null_mut()
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_raise_pending(value: Value) -> Value {
    unsafe {
        let instance = if !value.is_null()
            && !object::is_fixnum(value)
            && matches!(&(*value).data, Data::Class(_))
        {
            object::call(value, &[])
        } else {
            object::retain(value)
        };
        if !instance.is_null() {
            crate::traceback::resume(instance);
            object::release(replace_pending(instance));
        }
        null_mut()
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_catch_exception() -> Value {
    unsafe {
        // The handler's exception carries the traceback it was raised along.
        let traceback = crate::traceback::take();
        let exception = caught_exception();
        if crate::signals::has_pending_signals() {
            crate::threads::request_safepoint();
        }
        if exception.is_null() || traceback.is_null() {
            object::release(traceback);
            return exception;
        }
        let status = crate::classes::set_exception_traceback(exception, traceback);
        object::release(traceback);
        if status < 0 {
            object::release(exception);
            return null_mut();
        }
        exception
    }
}
unsafe fn caught_exception() -> Value {
    unsafe {
        let exception = replace_pending(null_mut());
        if !exception.is_null() {
            return exception;
        }
        let Some((kind, message)) = object::take_native_error() else {
            object::error("SystemError", "missing pending exception");
            return null_mut();
        };
        let class = object::lookup(mule_protocol_globals(), &kind);
        if class.is_null() {
            object::error(&kind, &message);
            return null_mut();
        }
        let message = object::new_value(Data::Text(crate::text::Text::from_rust_str(&message), std::ptr::null_mut(), std::ptr::null_mut()));
        let result = object::call(class, &[message]);
        object::release(message);
        result
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_enter_handler(value: Value) -> i32 {
    unsafe {
        ACTIVE.with_borrow_mut(|values| {
            if values.try_reserve(1).is_err() {
                object::error("MemoryError", "exception state allocation failed");
                return -1;
            }
            values.push(object::retain(value));
            0
        })
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_leave_handler() {
    ACTIVE.with_borrow_mut(|values| {
        if let Some(value) = values.pop() {
            unsafe {
                object::release(value);
            }
        }
    });
}

/// Handlers inside a suspended coroutine belong to that coroutine, rather
/// than the thread's resumer. Caller handlers remain underneath while active.
pub fn enter_coroutine_handlers(handlers: &mut Vec<Value>) -> usize {
    ACTIVE.with_borrow_mut(|active| {
        let boundary = active.len();
        if !handlers.is_empty() {
            active.append(handlers);
        }
        boundary
    })
}

pub fn leave_coroutine_handlers(boundary: usize, out: &mut Vec<Value>) {
    ACTIVE.with_borrow_mut(|active| {
        if active.len() > boundary {
            out.extend(active.drain(boundary..));
        }
    });
}
/// The exception being handled by the innermost active handler, or None.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_active_exception() -> Value {
    unsafe {
        ACTIVE.with_borrow(|values| match values.last() {
            Some(value) => object::retain(*value),
            None => object::none_value(),
        })
    }
}
/// Instantiate an exception class with no arguments; an instance is returned as is.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_exception_instance(value: Value) -> Value {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) && matches!(&(*value).data, Data::Class(_))
        {
            object::call(value, &[])
        } else {
            object::retain(value)
        }
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_reraise() -> i32 {
    unsafe {
        ACTIVE.with_borrow(|values| {
            if let Some(value) = values.last() {
                crate::traceback::resume(*value);
                object::release(replace_pending(object::retain(*value)));
            } else {
                object::error("RuntimeError", "no active exception to reraise");
            }
        });
        -1
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_check_rest_args(args: Value, minimum: i32) -> i32 {
    unsafe {
        if crate::arguments::keyword_count(args) != 0 {
            object::error("TypeError", "unexpected keyword argument");
            return -1;
        }
        if let Some(count) = object::argument_count(args) {
            if minimum >= 0 && count >= minimum as usize {
                return 0;
            }
        }
        object::error("TypeError", "not enough positional arguments");
        -1
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_rest_args(args: Value, start: i32) -> Value {
    unsafe {
        let Some(count) = object::argument_count(args) else {
            return null_mut();
        };
        if start < 0 || start as usize > count {
            object::error("SystemError", "invalid rest argument offset");
            return null_mut();
        }
        let len = count - start as usize;
        if len == 0 {
            return object::empty_tuple();
        }
        let Some(mut values) = crate::value_buffer::ValueBuffer::try_with_capacity_exact(len) else {
            object::error("MemoryError", "argument allocation failed");
            return null_mut();
        };
        let dst = values.as_mut_ptr();
        for (offset, index) in (start as usize..count).enumerate() {
            let Some(value) = object::argument(args, index) else {
                values.set_len(offset);
                for &v in values.as_slice() {
                    object::release(v);
                }
                return null_mut();
            };
            *dst.add(offset) = object::retain(value);
        }
        values.set_len(len);
        object::new_value(Data::Args(values, std::ptr::null_mut(), std::ptr::null_mut()))
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_run_protocols(
    native: *mut std::ffi::c_void,
    protocols: *mut std::ffi::c_void,
) -> i32 {
    unsafe { crate::builtins::run_protocols(native, protocols) }
}

#[cold]
#[inline(never)]
unsafe fn resolve_range_iterator() -> Value {
    unsafe {
        let resolved = object::lookup(mule_protocol_globals(), "RangeIterator");
        RANGE_ITERATOR.with(|slot| slot.set(resolved));
        resolved
    }
}

#[inline(always)]
unsafe fn cached_range_iterator() -> Value {
    let cached = RANGE_ITERATOR.with(Cell::get);
    if !cached.is_null() {
        return cached;
    }
    unsafe { resolve_range_iterator() }
}

/// Borrow the raw scalar fields of a freshly constructed private range iterator.
/// The generated loop supplies the callable-identity guard and owns the count.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_range_storage(iterator: Value, state: *mut i64) -> i32 {
    unsafe {
        if iterator.is_null() || object::is_fixnum(iterator) || state.is_null() {
            return 0;
        }
        let Data::Instance { ty, fields, .. } = &(*iterator).data else {
            return 0;
        };
        if *ty != cached_range_iterator() || fields.len() < 3 {
            return 0;
        }
        let c = *fields.get_unchecked(0);
        let stop = *fields.get_unchecked(1);
        let step = *fields.get_unchecked(2);
        if !object::is_fixnum(c) || !object::is_fixnum(stop) || !object::is_fixnum(step) {
            return 0;
        }
        let step_val = object::fixnum(step);
        if step_val == 0 {
            return 0;
        }
        *state = object::fixnum(c);
        *state.add(1) = object::fixnum(stop);
        *state.add(2) = step_val;
        1
    }
}

#[cold]
#[inline(never)]
unsafe fn resolve_bytearray_class() -> Value {
    unsafe {
        let resolved = object::lookup(mule_protocol_globals(), "bytearray");
        BYTEARRAY_CLASS.with(|slot| slot.set(resolved));
        resolved
    }
}

#[inline(always)]
unsafe fn cached_bytearray_class() -> Value {
    let cached = BYTEARRAY_CLASS.with(Cell::get);
    if !cached.is_null() {
        return cached;
    }
    unsafe { resolve_bytearray_class() }
}

#[cold]
#[inline(never)]
unsafe fn resolve_generator_class() -> Value {
    unsafe {
        let resolved = object::lookup(mule_protocol_globals(), "_generator");
        GENERATOR_CLASS.with(|slot| slot.set(resolved));
        resolved
    }
}

#[inline(always)]
unsafe fn cached_generator_class() -> Value {
    let cached = GENERATOR_CLASS.with(Cell::get);
    if !cached.is_null() {
        return cached;
    }
    unsafe { resolve_generator_class() }
}

#[cold]
#[inline(never)]
pub(crate) unsafe fn convert_stop_iteration_to_runtime_error() {
    unsafe {
        let pending = PENDING.with(Cell::get);
        let stop_class = object::lookup(mule_protocol_globals(), "StopIteration");
        if pending.is_null() || stop_class.is_null() {
            return;
        }
        let raised = mule_raised_class(pending);
        let is_stop = crate::classes::is_subclass(raised, stop_class);
        object::release(raised);
        if !is_stop {
            return;
        }
        let error = mule_catch_exception();
        let runtime_class = object::lookup(mule_protocol_globals(), "RuntimeError");
        if !error.is_null() && !runtime_class.is_null() {
            let msg = object::new_value(Data::Text(
                crate::text::Text::from_rust_str("generator raised StopIteration"),
                null_mut(),
                null_mut(),
            ));
            let runtime_err = object::call(runtime_class, &[msg]);
            object::release(msg);
            if !runtime_err.is_null() {
                let _ = crate::classes::mule_set_attr(runtime_err, c"_cause".as_ptr(), error);
                let _ = crate::classes::mule_set_attr(runtime_err, c"_context".as_ptr(), error);
                let _ = crate::classes::mule_set_attr(
                    runtime_err,
                    c"_suppress_context".as_ptr(),
                    object::bool_value(true),
                );
                let _ = mule_raise_pending(runtime_err);
                object::release(runtime_err);
            }
        }
        object::release(error);
    }
}

#[cold]
#[inline(never)]
pub(crate) unsafe fn raise_stop_iteration_value(value: Value) {
    unsafe {
        let stop_class = object::lookup(mule_protocol_globals(), "StopIteration");
        if !stop_class.is_null() {
            let stop_inst = if value == object::none_value() {
                object::call(stop_class, &[])
            } else {
                object::call(stop_class, &[value])
            };
            if !stop_inst.is_null() {
                let _ = mule_raise_pending(stop_inst);
                object::release(stop_inst);
            }
        } else {
            object::error("StopIteration", "");
        }
        object::release(value);
    }
}

/// Advance an exact builtin `generator` instance for `for`-loop iteration,
/// avoiding `StopIteration` construction when the generator returns normally.
/// When the generator body itself raises `StopIteration`, PEP 479 converts it
/// to `RuntimeError` with `from error` cause chaining.
#[inline]
unsafe fn advance_generator_fast(handle: Value, out: *mut Value) -> i32 {
    unsafe {
        let status = crate::coroutine::step_coroutine_none(handle, out);
        if status == 1 {
            return 1;
        }
        if status == 0 {
            let returned = *out;
            *out = null_mut();
            object::release(returned);
            return 0;
        }
        if crate::coroutine::is_coroutine_finished(handle) {
            convert_stop_iteration_to_runtime_error();
        }
        -1
    }
}

#[cold]
#[inline(never)]
unsafe fn resolve_storage_iterator() -> Value {
    unsafe {
        let resolved = object::lookup(mule_protocol_globals(), "StorageIterator");
        STORAGE_ITERATOR.with(|slot| slot.set(resolved));
        resolved
    }
}

#[inline(always)]
unsafe fn cached_storage_iterator() -> Value {
    let cached = STORAGE_ITERATOR.with(Cell::get);
    if !cached.is_null() {
        return cached;
    }
    unsafe { resolve_storage_iterator() }
}

/// Fast path for `for`-loop advancement on a `StorageIterator` (`list`/`tuple`).
/// Returns `1` and writes the retained element to `*out` when `iterator` is an
/// unexhausted `StorageIterator` with an in-bounds fixnum index; returns `0`
/// and marks `exhausted = True` when `index >= len`; returns `-1` without
/// setting an error when `iterator` needs the general `next_item` protocol.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_fast_next_item(iterator: Value, out: *mut Value) -> i32 {
    unsafe {
        if iterator.is_null() || object::is_fixnum(iterator) || out.is_null() {
            return -1;
        }
        match &mut (*iterator).data {
            Data::SeqIter { sequence, index } => {
                let seq = *sequence;
                if seq.is_null() || object::is_fixnum(seq) {
                    return -1;
                }
                if let Some(storage) = crate::buffers::byte_storage(seq) {
                    let Data::ByteStorage(bytes) = &(*storage).data else { unreachable!() };
                    let pos = *index;
                    if pos == usize::MAX { return 0; }
                    if pos >= bytes.length {
                        *index = usize::MAX;
                        return 0;
                    }
                    *index = pos + 1;
                    *out = object::small(i64::from(*bytes.data.add(pos)));
                    return 1;
                }
                match &(*seq).data {
                    Data::List(values, ..) | Data::Args(values, ..) => {
                        let pos = *index;
                        if pos >= values.len() {
                            return 0;
                        }
                        *index = pos + 1;
                        *out = object::retain(*values.get_unchecked(pos));
                        1
                    }
                    Data::Bytes(bytes, ..) => {
                        let pos = *index;
                        if pos >= bytes.len() {
                            return 0;
                        }
                        *index = pos + 1;
                        *out = object::small(i64::from(*bytes.get_unchecked(pos)));
                        1
                    }
                    _ => -1,
                }
            }
            Data::Instance { ty, fields, .. } => {
                let storage_iterator = cached_storage_iterator();
                if !storage_iterator.is_null() && *ty == storage_iterator && fields.len() >= 3 {
                    let sequence = fields[0];
                    let index = fields[1];
                    let exhausted = fields[2];
                    if exhausted != object::bool_value(false)
                        || !object::is_fixnum(index)
                        || sequence.is_null()
                        || object::is_fixnum(sequence)
                    {
                        return -1;
                    }
                    let pos = object::fixnum(index);
                    if pos < 0 {
                        return -1;
                    }
                    if let Some(storage) = crate::buffers::byte_storage(sequence) {
                        let Data::ByteStorage(bytes) = &(*storage).data else { unreachable!() };
                        if pos as usize >= bytes.length {
                            fields[2] = object::bool_value(true);
                            return 0;
                        }
                        fields[1] = object::small(pos + 1);
                        *out = object::small(i64::from(*bytes.data.add(pos as usize)));
                        return 1;
                    }
                    let values = match &(*sequence).data {
                        Data::List(values, ..) | Data::Args(values, ..) => values,
                        _ => return -1,
                    };
                    let len = values.len() as i64;
                    if pos >= len {
                        let old = fields[2];
                        fields[2] = object::bool_value(true);
                        object::release(old);
                        return 0;
                    }
                    let Some(next_pos) = pos.checked_add(1) else {
                        return -1;
                    };
                    if !(numeric::MIN..=numeric::MAX).contains(&next_pos) {
                        return -1;
                    }
                    fields[1] = object::small(next_pos);
                    *out = object::retain(*values.get_unchecked(pos as usize));
                    return 1;
                }
                if !fields.is_empty() && *ty == cached_generator_class() {
                    let handle = fields[0];
                    return advance_generator_fast(handle, out);
                }
                -1
            }
            _ => -1,
        }
    }
}

/// Fast path for `iterate(value)` on exact builtin `list`, `tuple`, `bytes`, `bytearray`, and `generator` objects.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_fast_iterate(value: Value) -> Value {
    unsafe {
        if value.is_null() || object::is_fixnum(value) {
            return null_mut();
        }
        let target = match &(*value).data {
            Data::SeqIter { .. } => return object::retain(value),
            Data::List(_, ty, ..) | Data::Args(_, ty, ..) | Data::Bytes(_, ty, ..)
                if ty.is_null() =>
            {
                value
            }
            Data::Instance { ty, fields, .. } => {
                if *ty == cached_generator_class() {
                    return object::retain(value);
                }
                if !fields.is_empty() && *ty == cached_bytearray_class() {
                    let inner = fields[0];
                    if inner.is_null()
                        || object::is_fixnum(inner)
                        || !matches!(&(*inner).data, Data::ByteStorage(_))
                    {
                        return null_mut();
                    }
                    value
                } else {
                    return null_mut();
                }
            }
            _ => return null_mut(),
        };
        object::new_untracked_value(Data::SeqIter {
            sequence: object::retain(target),
            index: 0,
        })
    }
}

/// Fast path for `iterate(value)` that consumes the caller's owned `value` reference
/// when `value` is an exact builtin `list`, `tuple`, `bytes`, `bytearray`, `generator`, or `SeqIter`.
/// Leaves `value` unconsumed when returning NULL.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_fast_iterate_move(value: Value) -> Value {
    unsafe {
        if value.is_null() || object::is_fixnum(value) {
            return null_mut();
        }
        match &(*value).data {
            Data::SeqIter { .. } => return value,
            Data::List(_, ty, ..) | Data::Args(_, ty, ..) | Data::Bytes(_, ty, ..)
                if ty.is_null() => {}
            Data::Instance { ty, fields, .. } => {
                if *ty == cached_generator_class() {
                    return value;
                }
                if !fields.is_empty() && *ty == cached_bytearray_class() {
                    let inner = fields[0];
                    if inner.is_null()
                        || object::is_fixnum(inner)
                        || !matches!(&(*inner).data, Data::ByteStorage(_))
                    {
                        return null_mut();
                    }
                    return object::new_untracked_value(Data::SeqIter {
                        sequence: value,
                        index: 0,
                    });
                }
                return null_mut();
            }
            _ => return null_mut(),
        }
        object::new_untracked_value(Data::SeqIter {
            sequence: value,
            index: 0,
        })
    }
}

/// Fast path for unpacking an exact builtin `tuple` or `list` of length `arity`
/// directly into `out[0..arity]`. Consumes `value` and returns `1` on a shape
/// match; leaves `value` unconsumed and returns `0` when the general `unpack`
/// protocol is required.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_fast_unpack(value: Value, arity: isize, out: *mut Value) -> i32 {
    unsafe {
        if value.is_null() || object::is_fixnum(value) || arity < 0 || out.is_null() {
            return 0;
        }
        let count = arity as usize;
        let matched = match &(*value).data {
            Data::Args(values, ty, ..) | Data::List(values, ty, ..) => {
                ty.is_null() && values.len() == count
            }
            _ => false,
        };
        if !matched {
            return 0;
        }
        if (*value).references.load(std::sync::atomic::Ordering::Relaxed) == 1 {
            let (Data::Args(values, ..) | Data::List(values, ..)) = &mut (*value).data else {
                unreachable!();
            };
            std::ptr::copy_nonoverlapping(values.as_ptr(), out, count);
            values.set_len(0);
            object::release(value);
            return 1;
        }
        let (Data::Args(values, ..) | Data::List(values, ..)) = &(*value).data else {
            unreachable!();
        };
        for i in 0..count {
            let elem = *values.get_unchecked(i);
            if !object::is_fixnum(elem) {
                object::retain(elem);
            }
            *out.add(i) = elem;
        }
        object::release(value);
        1
    }
}

/// Fast path for `for a, b, ... in seq:` on a `SeqIter` or `StorageIterator`
/// whose next item is an exact `tuple` or `list` of length `arity`: unpacks the
/// item's elements directly into `unpack_out[0..arity]` without retaining/releasing
/// the item container, stores `null` into `*out` as a sentinel, and returns `1`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_fast_next_unpack(
    iterator: Value,
    arity: isize,
    unpack_out: *mut Value,
    out: *mut Value,
) -> i32 {
    unsafe {
        if iterator.is_null()
            || object::is_fixnum(iterator)
            || arity < 0
            || unpack_out.is_null()
            || out.is_null()
        {
            return -1;
        }
        let count = arity as usize;
        match &mut (*iterator).data {
            Data::SeqIter { sequence, index } => {
                let seq = *sequence;
                if seq.is_null() || object::is_fixnum(seq) {
                    return -1;
                }
                let values = match &(*seq).data {
                    Data::List(values, ..) | Data::Args(values, ..) => values,
                    _ => return -1,
                };
                let pos = *index;
                if pos >= values.len() {
                    return 0;
                }
                *index = pos + 1;
                let item = *values.get_unchecked(pos);
                if !item.is_null() && !object::is_fixnum(item) {
                    match &(*item).data {
                        Data::Args(tuple_vals, item_ty, ..)
                        | Data::List(tuple_vals, item_ty, ..)
                            if item_ty.is_null() && tuple_vals.len() == count =>
                        {
                            for i in 0..count {
                                let elem = *tuple_vals.get_unchecked(i);
                                if !object::is_fixnum(elem) {
                                    object::retain(elem);
                                }
                                *unpack_out.add(i) = elem;
                            }
                            *out = null_mut();
                            return 1;
                        }
                        _ => (),
                    }
                }
                if !object::is_fixnum(item) {
                    object::retain(item);
                }
                *out = item;
                1
            }
            Data::Instance { ty, fields, .. } => {
                if !fields.is_empty() && *ty == cached_generator_class() {
                    let status = advance_generator_fast(fields[0], out);
                    if status <= 0 {
                        return status;
                    }
                    let item = *out;
                    if !item.is_null() && !object::is_fixnum(item) {
                        match &(*item).data {
                            Data::Args(tuple_vals, item_ty, ..)
                            | Data::List(tuple_vals, item_ty, ..)
                                if item_ty.is_null() && tuple_vals.len() == count =>
                            {
                                for i in 0..count {
                                    let elem = *tuple_vals.get_unchecked(i);
                                    if !object::is_fixnum(elem) {
                                        object::retain(elem);
                                    }
                                    *unpack_out.add(i) = elem;
                                }
                                *out = null_mut();
                                object::release(item);
                                return 1;
                            }
                            _ => (),
                        }
                    }
                    return 1;
                }
                let storage_iterator = cached_storage_iterator();
                if storage_iterator.is_null() || *ty != storage_iterator || fields.len() < 3 {
                    return -1;
                }
                let sequence = fields[0];
                let index = fields[1];
                let exhausted = fields[2];
                if exhausted != object::bool_value(false)
                    || !object::is_fixnum(index)
                    || sequence.is_null()
                    || object::is_fixnum(sequence)
                {
                    return -1;
                }
                let pos = object::fixnum(index);
                if pos < 0 {
                    return -1;
                }
                let values = match &(*sequence).data {
                    Data::List(values, ..) | Data::Args(values, ..) => values,
                    _ => return -1,
                };
                let len = values.len() as i64;
                if pos >= len {
                    let old = fields[2];
                    fields[2] = object::bool_value(true);
                    object::release(old);
                    return 0;
                }
                let Some(next_pos) = pos.checked_add(1) else {
                    return -1;
                };
                if !(numeric::MIN..=numeric::MAX).contains(&next_pos) {
                    return -1;
                }
                let item = *values.get_unchecked(pos as usize);
                if !item.is_null() && !object::is_fixnum(item) {
                    match &(*item).data {
                        Data::Args(tuple_vals, item_ty, ..)
                        | Data::List(tuple_vals, item_ty, ..)
                            if item_ty.is_null() && tuple_vals.len() == count =>
                        {
                            fields[1] = object::small(next_pos);
                            for i in 0..count {
                                let elem = *tuple_vals.get_unchecked(i);
                                if !object::is_fixnum(elem) {
                                    object::retain(elem);
                                }
                                *unpack_out.add(i) = elem;
                            }
                            *out = null_mut();
                            return 1;
                        }
                        _ => (),
                    }
                }
                fields[1] = object::small(next_pos);
                if !object::is_fixnum(item) {
                    object::retain(item);
                }
                *out = item;
                1
            }
            _ => -1,
        }
    }
}

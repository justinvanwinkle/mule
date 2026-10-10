//! Physical storage and pending-exception primitives for compiled Mule protocols.
use crate::numeric;
use crate::object::{self, Data, Value};
use std::cell::{Cell, RefCell};
use std::ffi::c_char;
use std::ptr::null_mut;

thread_local! {
    static GLOBALS: Cell<Value> = const { Cell::new(null_mut()) };
    static STORAGE_CLASSES: RefCell<[Value; 4]> = const { RefCell::new([null_mut(); 4]) };
    static PENDING: Cell<Value> = const { Cell::new(null_mut()) };
    static ACTIVE: RefCell<Vec<Value>> = const { RefCell::new(Vec::new()) };
}
pub fn has_pending() -> bool {
    PENDING.with(|value| !value.get().is_null())
}
pub fn clear_pending() {
    PENDING.with(|value| unsafe { object::release(value.replace(null_mut())) });
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
        let value = pending.replace(null_mut());
        let result = object::special(value, "__str__", &[]);
        let message = if result.is_null() {
            String::new()
        } else {
            object::formatted_value(result, false, false).map_or_else(String::new, |bytes| {
                String::from_utf8_lossy(&bytes).into_owned()
            })
        };
        object::release(result);
        pending.set(value);
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
        STORAGE_CLASSES.with_borrow_mut(|classes| {
            for class in classes.iter_mut() {
                object::release(*class);
                *class = null_mut();
            }
        });
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
) -> Value {
    unsafe {
        STORAGE_CLASSES.with_borrow_mut(|classes| {
            for (slot, value) in classes.iter_mut().zip([list, tuple, text, bytes]) {
                object::release(*slot);
                *slot = object::retain(value);
            }
        });
        object::none_value()
    }
}
/// Borrow the ordinary compiled method table associated with physical storage.
pub unsafe fn binding(value: Value, name: &str) -> Value {
    unsafe {
        if value.is_null() || object::is_fixnum(value) {
            return null_mut();
        }
        let index = match &(*value).data {
            Data::List(_) => 0,
            Data::Args(_) => 1,
            Data::Text(_) => 2,
            Data::Bytes(_) => 3,
            _ => return null_mut(),
        };
        STORAGE_CLASSES.with_borrow(|classes| {
            let class = classes[index];
            if class.is_null() {
                return null_mut();
            }
            let Data::Class(layout) = &(*class).data else {
                unreachable!()
            };
            layout.method(name)
        })
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_install_builtin(name: Value, value: Value) -> Value {
    unsafe {
        if !name.is_null() && !object::is_fixnum(name) {
            if let Data::Text(bytes) = &(*name).data {
                if let Ok(name) = std::str::from_utf8(bytes) {
                    if object::bind(crate::builtins::namespace(), name, value) {
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
                Data::BigInt(_) => return object::retain(value),
                Data::Bool(value) => return object::small(i64::from(*value)),
                _ => (),
            }
        }
        object::error("TypeError", "an integer index is required");
        null_mut()
    }
}
unsafe fn index(value: Value) -> Option<usize> {
    unsafe {
        let number = if object::is_fixnum(value) {
            Some(object::fixnum(value))
        } else if !value.is_null() {
            match &(*value).data {
                Data::BigInt(number) => number.to_i64(),
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
pub unsafe extern "C" fn mule_storage_length(value: Value) -> Value {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            let length = match &(*value).data {
                Data::List(values) | Data::Args(values) => values.len(),
                Data::Bytes(values) => values.len(),
                Data::Text(_) => return object::mule_text_length(value),
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
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_storage_read(value: Value, position: Value) -> Value {
    unsafe {
        let Some(index) = index(position) else {
            return null_mut();
        };
        if !value.is_null() && !object::is_fixnum(value) {
            match &(*value).data {
                Data::List(values) | Data::Args(values) => {
                    if let Some(value) = values.get(index) {
                        return object::retain(*value);
                    }
                }
                Data::Bytes(values) => {
                    if let Some(value) = values.get(index) {
                        return object::small(i64::from(*value));
                    }
                }
                Data::Text(_) => return object::mule_text_character(value, position),
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
            if let Data::List(values) = &mut (*value).data {
                if let Some(slot) = values.get_mut(index) {
                    let old = *slot;
                    *slot = object::retain(replacement);
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
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_storage_append(value: Value, element: Value) -> Value {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::List(values) = &mut (*value).data {
                if values.try_reserve(1).is_err() {
                    object::error("MemoryError", "list allocation failed");
                    return null_mut();
                }
                values.push(object::retain(element));
                return object::none_value();
            }
        }
        object::error("TypeError", "mutable list storage is required");
        null_mut()
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_storage_tuple(value: Value) -> Value {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            match &(*value).data {
                Data::List(values) => {
                    return object::mule_sequence_tuple(values.len() as isize, values.as_ptr());
                }
                Data::Args(_) => return object::retain(value),
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
                (Data::List(_), Data::List(_)) | (Data::Args(_), Data::Args(_))
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
        if !value.is_null() && !object::is_fixnum(value) && matches!(&(*value).data, Data::Args(_))
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
            PENDING.with(|slot| object::release(slot.replace(instance)));
        }
        null_mut()
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_catch_exception() -> Value {
    unsafe {
        let exception = PENDING.with(|value| value.replace(null_mut()));
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
        let message = object::new_value(Data::Text(message.into_bytes()));
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
                PENDING.with(|slot| object::release(slot.replace(object::retain(*value))));
            } else {
                object::error("RuntimeError", "no active exception to reraise");
            }
        });
        -1
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_clear_binding(namespace: Value, name: *const c_char) -> i32 {
    unsafe {
        if namespace.is_null() || object::is_fixnum(namespace) || name.is_null() {
            return -1;
        }
        let Ok(name) = std::ffi::CStr::from_ptr(name).to_str() else {
            return -1;
        };
        if let Data::Namespace(bindings) = &mut (*namespace).data {
            if let Some(index) = bindings.iter().position(|binding| binding.0 == name) {
                let (_, value) = bindings.remove(index);
                object::release(value);
            }
            return 0;
        }
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
        let mut values = Vec::new();
        if values.try_reserve_exact(count - start as usize).is_err() {
            object::error("MemoryError", "argument allocation failed");
            return null_mut();
        }
        for index in start as usize..count {
            let Some(value) = object::argument(args, index) else {
                return null_mut();
            };
            values.push(object::retain(value));
        }
        object::new_value(Data::Args(values))
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_run_protocols(
    native: *mut std::ffi::c_void,
    protocols: *mut std::ffi::c_void,
) -> i32 {
    unsafe { crate::builtins::run_protocols(native, protocols) }
}

/// Borrow the raw scalar fields of a freshly constructed private range iterator.
/// The generated loop supplies the callable-identity guard and owns the count.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_range_storage(iterator: Value, state: *mut i64) -> i32 {
    unsafe {
        if iterator.is_null() || object::is_fixnum(iterator) || state.is_null() {
            return 0;
        }
        let Data::Instance { ty, fields } = &(*iterator).data else {
            return 0;
        };
        if *ty != object::lookup(mule_protocol_globals(), "RangeIterator") {
            return 0;
        }
        let Data::Class(layout) = &(**ty).data else {
            return 0;
        };
        let mut words = [0; 3];
        for (slot, name) in words.iter_mut().zip(["current", "stop", "step"]) {
            let Some(index) = layout.fields.iter().position(|field| field == name) else {
                return 0;
            };
            let value = fields[index];
            if !object::is_fixnum(value) {
                return 0;
            }
            *slot = object::fixnum(value);
        }
        if words[2] == 0 {
            return 0;
        }
        std::ptr::copy_nonoverlapping(words.as_ptr(), state, 3);
        1
    }
}

//! Physical storage and pending-exception primitives for compiled Mule protocols.
use crate::numeric;
use crate::object::{self, Data, Value};
use std::cell::{Cell, RefCell};
use std::ptr::null_mut;

thread_local! {
    static GLOBALS: Cell<Value> = const { Cell::new(null_mut()) };
    static PENDING: Cell<Value> = const { Cell::new(null_mut()) };
    static ACTIVE: RefCell<Vec<Value>> = const { RefCell::new(Vec::new()) };
}
pub fn has_pending() -> bool {
    PENDING.with(|value| !value.get().is_null())
}
/// Remove and return the pending exception, if any, for later restoration.
pub fn take_pending() -> Value {
    PENDING.with(|value| value.replace(null_mut()))
}
/// Reinstate an exception taken with `take_pending`, replacing any current one.
pub fn restore_pending(saved: Value) {
    PENDING.with(|value| unsafe { object::release(value.replace(saved)) });
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
        crate::traceback::clear_code_cache();
        clear_pending();
        ACTIVE.with_borrow_mut(|values| {
            for value in values.drain(..) {
                object::release(value);
            }
        });
        crate::classes::mule_set_attribute_lookup(null_mut());
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
        use crate::classes::Builtin;
        for (kind, class) in [
            (Builtin::List, list),
            (Builtin::Tuple, tuple),
            (Builtin::Str, text),
            (Builtin::Bytes, bytes),
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
        if !value.is_null() && !object::is_fixnum(value) && matches!(&(*value).data, Data::Class(_)) {
            let module = object::new_value(Data::Text(crate::text::Text::from_rust_str("builtins"), std::ptr::null_mut(), std::ptr::null_mut()));
            let status = crate::classes::mule_set_attr(value, c"__module__".as_ptr(), module);
            object::release(module);
            if status < 0 {
                return null_mut();
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
                Data::BigInt(..) => return object::retain(value),
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
            if let Data::Args(values, ..) = &(*value).data {
                return i32::from(values.len() == arity);
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
                Data::Text(..) => return object::mule_text_length(value),
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
            if let Data::List(values, ..) = &mut (*value).data {
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
            if let Data::List(values, ..) = &mut (*value).data {
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
            if let Data::List(values, ..) = &mut (*value).data {
                if start > stop || stop > values.len() {
                    object::error("IndexError", "storage index out of range");
                    return null_mut();
                }
                // Release after removal: a destructor may observe the list.
                let removed: Vec<Value> = values.drain(start..stop).collect();
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
            PENDING.with(|slot| object::release(slot.replace(instance)));
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
        if exception.is_null() || traceback.is_null() {
            object::release(traceback);
            return exception;
        }
        let status = crate::classes::mule_set_attr(exception, c"__traceback__".as_ptr(), traceback);
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
                PENDING.with(|slot| object::release(slot.replace(object::retain(*value))));
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

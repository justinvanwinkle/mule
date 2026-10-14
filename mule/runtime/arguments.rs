//! Borrowed native call buffers and physical storage for compiled Python binders.

use crate::object::{self, Data, Native, Object, Value};
use std::cell::Cell;
use std::ffi::{c_char, c_void};

pub type Constructor = unsafe extern "C" fn(Value, Value) -> Value;
thread_local! {
    static CONSTRUCTOR: Cell<Option<Constructor>> = const { Cell::new(None) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_set_argument_constructor(entry: *mut c_void) {
    CONSTRUCTOR.with(|current| {
        current.set(if entry.is_null() {
            None
        } else {
            Some(unsafe { std::mem::transmute::<*mut c_void, Constructor>(entry) })
        })
    });
}

pub unsafe fn keyword_count(view: Value) -> usize {
    if view.is_null() || object::is_fixnum(view) {
        return 0;
    }
    match unsafe { &(*view).data } {
        Data::KeywordArgs { keyword_count, .. } => *keyword_count,
        _ => 0,
    }
}

unsafe fn defaults(view: Value) -> Value {
    match unsafe { &(*view).data } {
        Data::ArgumentView { defaults, .. }
        | Data::ReceiverArgs { defaults, .. }
        | Data::KeywordArgs { defaults, .. } => *defaults,
        _ => std::ptr::null_mut(),
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_binding_plain(view: Value, count: i32, rest: i32) -> i32 {
    unsafe {
        if keyword_count(view) != 0 {
            return 0;
        }
        object::argument_count(view).map_or(0, |actual| {
            i32::from(if rest == 0 {
                actual == count as usize
            } else {
                actual >= count as usize
            })
        })
    }
}

unsafe fn invoke(entry: Native, globals: Value, view: Value) -> Value {
    if object::mule_enter_frame() < 0 {
        return std::ptr::null_mut();
    }
    let result = unsafe { entry(globals, view) };
    object::mule_leave_frame();
    if result.is_null() && !object::has_error() {
        object::error("SystemError", "native entry returned NULL without an error");
    }
    result
}

/// Physical callable invocation; parameter names and binding policy are handled
/// by generated compiled entries. Views borrow all buffers for this call only.
unsafe fn call_buffers(
    callable: Value,
    receiver: Value,
    values: &[Value],
    names: *const *const c_char,
    keyword_values: &[Value],
) -> Value {
    unsafe {
        if object::has_error() {
            return std::ptr::null_mut();
        }
        if callable.is_null() || object::is_fixnum(callable) {
            object::error("TypeError", "object is not callable");
            return std::ptr::null_mut();
        }
        match &(*callable).data {
            Data::BoundMethod {
                receiver: bound,
                method,
            } => {
                if !receiver.is_null() {
                    // Nested bindings retain both receiver prefixes in order.
                    let mut prefixed = Vec::with_capacity(values.len() + 1);
                    prefixed.push(receiver);
                    prefixed.extend_from_slice(values);
                    return call_buffers(*method, *bound, &prefixed, names, keyword_values);
                }
                return call_buffers(*method, *bound, values, names, keyword_values);
            }
            // An unbound global read without a check (safety zero).
            Data::Undefined => {
                object::error("NameError", "name is not defined");
                return std::ptr::null_mut();
            }
            Data::Instance { .. } | Data::NativeInstance { .. } => {
                let name = c"__call__";
                let method = object::mule_lookup_special(callable, name.as_ptr());
                if method == object::none_value() {
                    object::error("TypeError", "object is not callable");
                    return std::ptr::null_mut();
                }
                let result =
                    call_buffers(method, receiver, values, names, keyword_values);
                object::release(method);
                return result;
            }
            _ => (),
        }
        let (captured, closure) = match &(*callable).data {
            Data::Function {
                defaults, closure, ..
            } => (*defaults, *closure),
            _ => (std::ptr::null_mut(), std::ptr::null_mut()),
        };
        let mut view = Object {
            references: usize::MAX,
            data: Data::KeywordArgs {
                receiver,
                values: values.as_ptr(),
                count: values.len(),
                names,
                keyword_values: keyword_values.as_ptr(),
                keyword_count: keyword_values.len(),
                defaults: captured,
                closure,
            },
        };
        match &(*callable).data {
            Data::Function {
                entry,
                globals,
                keyword_entry,
                ..
            } => {
                if !keyword_values.is_empty() && keyword_entry.is_none() {
                    object::error("TypeError", "callable has no keyword entry");
                    return std::ptr::null_mut();
                }
                invoke(keyword_entry.unwrap_or(*entry), *globals, &mut view)
            }
            Data::Class(_) if !crate::classes::metaclass_call(callable).is_null() => {
                let call = crate::classes::metaclass_call(callable);
                call_buffers(call, callable, values, names, keyword_values)
            }
            Data::Class(_) => CONSTRUCTOR.with(|current| match current.get() {
                Some(entry) => entry(callable, &mut view),
                None => {
                    object::error("SystemError", "compiled constructor is unavailable");
                    std::ptr::null_mut()
                }
            }),
            _ => {
                object::error("TypeError", "object does not accept keyword arguments");
                std::ptr::null_mut()
            }
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_call_keywords(
    callable: Value,
    receiver: Value,
    count: isize,
    values: *const Value,
    keyword_count: isize,
    names: *const *const c_char,
    keyword_values: *const Value,
) -> Value {
    unsafe {
        if count < 0
            || keyword_count < 0
            || (count != 0 && values.is_null())
            || (keyword_count != 0 && (names.is_null() || keyword_values.is_null()))
        {
            object::error("SystemError", "invalid native call buffers");
            return std::ptr::null_mut();
        }
        let positional = if count == 0 {
            &[]
        } else {
            std::slice::from_raw_parts(values, count as usize)
        };
        let keywords = if keyword_count == 0 {
            &[]
        } else {
            std::slice::from_raw_parts(keyword_values, keyword_count as usize)
        };
        call_buffers(callable, receiver, positional, names, keywords)
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_construct_arguments(
    class: Value,
    count: isize,
    values: *const Value,
) -> Value {
    unsafe {
        mule_call_keywords(
            class,
            std::ptr::null_mut(),
            count,
            values,
            0,
            std::ptr::null(),
            std::ptr::null(),
        )
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_forward_call(callable: Value, view: Value) -> Value {
    unsafe {
        match &(*view).data {
            Data::KeywordArgs {
                receiver,
                values,
                count,
                names,
                keyword_values,
                keyword_count,
                ..
            } if receiver.is_null() => mule_call_keywords(
                callable,
                std::ptr::null_mut(),
                *count as isize,
                *values,
                *keyword_count as isize,
                *names,
                *keyword_values,
            ),
            Data::ArgumentView { values, count, .. } => mule_call_keywords(
                callable,
                std::ptr::null_mut(),
                *count as isize,
                *values,
                0,
                std::ptr::null(),
                std::ptr::null(),
            ),
            _ => {
                object::error("SystemError", "invalid constructor argument view");
                std::ptr::null_mut()
            }
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_allocate_instance(class: Value) -> Value {
    unsafe {
        let Data::Class(layout) = &(*class).data else {
            object::error("SystemError", "invalid instance layout");
            return std::ptr::null_mut();
        };
        if layout.native.is_some() {
            return crate::native_storage::mule_native_new(class);
        }
        let mut fields = Vec::new();
        if fields.try_reserve_exact(layout.fields.len()).is_err() {
            object::error("MemoryError", "instance slot allocation failed");
            return std::ptr::null_mut();
        }
        fields.resize(layout.fields.len(), std::ptr::null_mut());
        object::new_value(Data::Instance {
            ty: object::retain(class),
            fields: fields.into_boxed_slice(),
            attributes: std::ptr::null_mut(),
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_function_defaults(
    globals: Value,
    native: *mut c_void,
    name: *const c_char,
    captured: Value,
) -> Value {
    unsafe {
        let value = object::mule_function(globals, native, name);
        if !value.is_null() {
            let Data::Function {
                defaults,
                entry,
                keyword_entry,
                ..
            } = &mut (*value).data
            else {
                unreachable!()
            };
            *defaults = object::retain(captured);
            *keyword_entry = Some(*entry);
        }
        value
    }
}

/// Borrow a captured default after the caller has guarded this callable's entry.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_function_default(callable: Value, offset: i32) -> Value {
    unsafe {
        let Data::Function { defaults, .. } = &(*callable).data else {
            unreachable!()
        };
        object::argument(*defaults, offset as usize).unwrap_or(std::ptr::null_mut())
    }
}

// This private immortal object is an identity sentinel, never a user value.
thread_local! {
    static MISSING: Object = const { Object { references: usize::MAX, data: Data::None } };
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_binding_missing() -> Value {
    MISSING.with(|value| std::ptr::from_ref(value).cast_mut())
}

unsafe fn index(value: Value) -> Option<usize> {
    if !object::is_fixnum(value) {
        object::error("SystemError", "invalid native buffer index");
        return None;
    }
    usize::try_from(unsafe { object::fixnum(value) }).ok()
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_binding_count(view: Value) -> Value {
    unsafe {
        object::argument_count(view)
            .map_or(std::ptr::null_mut(), |count| object::small(count as i64))
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_binding_keyword_count(view: Value) -> Value {
    object::small(unsafe { keyword_count(view) } as i64)
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_binding_pos(view: Value, offset: Value) -> Value {
    unsafe {
        index(offset)
            .and_then(|offset| object::argument(view, offset))
            .map_or(std::ptr::null_mut(), |value| object::retain(value))
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_binding_rest(view: Value, offset: Value) -> Value {
    unsafe {
        let Some(count) = object::argument_count(view) else {
            return std::ptr::null_mut();
        };
        index(offset).map_or(std::ptr::null_mut(), |offset| {
            crate::sequences::mule_rest_args(view, offset.min(count) as i32)
        })
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_binding_default(view: Value, offset: Value) -> Value {
    unsafe { object::mule_arg(defaults(view), object::fixnum(offset) as i32) }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_binding_keyword_name(view: Value, offset: Value) -> Value {
    unsafe {
        let Some(offset) = index(offset) else {
            return std::ptr::null_mut();
        };
        let Data::KeywordArgs {
            names,
            keyword_count,
            ..
        } = &(*view).data
        else {
            object::error("SystemError", "invalid keyword view");
            return std::ptr::null_mut();
        };
        if offset >= *keyword_count {
            object::error("SystemError", "keyword index out of range");
            return std::ptr::null_mut();
        }
        let Some(name) = object::name_from_c(*names.add(offset)) else {
            return std::ptr::null_mut();
        };
        object::new_value(Data::Text(crate::text::Text::from_rust_str(name), std::ptr::null_mut(), std::ptr::null_mut()))
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_binding_keyword_value(view: Value, offset: Value) -> Value {
    unsafe {
        let Some(offset) = index(offset) else {
            return std::ptr::null_mut();
        };
        let Data::KeywordArgs {
            keyword_values,
            keyword_count,
            ..
        } = &(*view).data
        else {
            object::error("SystemError", "invalid keyword view");
            return std::ptr::null_mut();
        };
        if offset >= *keyword_count {
            object::error("SystemError", "keyword index out of range");
            return std::ptr::null_mut();
        }
        object::retain(*keyword_values.add(offset))
    }
}

/// `call_with(callable, positional, names, values)`: call with a tuple of
/// positional arguments and keyword names and values, as compiled call
/// expansion (`f(*args, **kwargs)`) assembles them.
pub(crate) unsafe extern "C" fn call_with(_: Value, args: Value) -> Value {
    unsafe {
        with_call_buffers(args, |class, positional, names, values| {
            mule_call_keywords(class, std::ptr::null_mut(), positional.len() as isize, positional.as_ptr(), values.len() as isize, names.as_ptr(), values.as_ptr())
        })
    }
}

/// Invoke the compiled default constructor without redispatching a metaclass.
pub(crate) unsafe extern "C" fn default_construct(_: Value, args: Value) -> Value {
    unsafe {
        with_call_buffers(args, |class, positional, names, values| {
            let mut view = Object {
                references: usize::MAX,
                data: Data::KeywordArgs {
                    receiver: std::ptr::null_mut(),
                    values: positional.as_ptr(), count: positional.len(),
                    names: names.as_ptr(), keyword_values: values.as_ptr(),
                    keyword_count: values.len(), defaults: std::ptr::null_mut(),
                    closure: std::ptr::null_mut(),
                },
            };
            CONSTRUCTOR.with(|current| match current.get() {
                Some(entry) => entry(class, &mut view),
                None => {
                    object::error("SystemError", "compiled constructor is unavailable");
                    std::ptr::null_mut()
                }
            })
        })
    }
}

unsafe fn with_call_buffers(args: Value, invoke: impl FnOnce(Value, &[Value], &[*const c_char], &[Value]) -> Value) -> Value {
    unsafe {
        if object::has_error() {
            return std::ptr::null_mut();
        }
        if object::argument_count(args) != Some(4) || keyword_count(args) != 0 {
            object::error("TypeError", "call_with expects 4 positional arguments");
            return std::ptr::null_mut();
        }
        let operand = |index| object::argument(args, index).unwrap_or(std::ptr::null_mut());
        let (callable, positional, names, values) = (operand(0), operand(1), operand(2), operand(3));
        let tuple = |value: Value| -> Option<&Vec<Value>> {
            if value.is_null() || object::is_fixnum(value) {
                return None;
            }
            match &(*value).data {
                Data::Args(values, ..) => Some(values),
                _ => None,
            }
        };
        let (Some(positional), Some(names), Some(values)) =
            (tuple(positional), tuple(names), tuple(values))
        else {
            object::error("TypeError", "call_with requires tuples");
            return std::ptr::null_mut();
        };
        let mut owned_names = Vec::with_capacity(names.len());
        for name in names {
            let text = if object::is_fixnum(*name) {
                None
            } else if let Data::Text(text, ..) = &(**name).data {
                text.to_rust_string()
            } else {
                None
            };
            let Some(text) = text.and_then(|text| std::ffi::CString::new(text).ok()) else {
                object::error("TypeError", "keywords must be strings");
                return std::ptr::null_mut();
            };
            owned_names.push(text);
        }
        let pointers: Vec<*const c_char> = owned_names.iter().map(|name| name.as_ptr()).collect();
        invoke(callable, positional, &pointers, values)
    }
}

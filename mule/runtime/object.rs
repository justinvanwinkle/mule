//! Mule values and the native-code ABI. Heap ownership is explicit at this boundary;
//! generated code retains each live value and releases it on normal and error exits.
use crate::{builtins, gmp::BigInt, numeric};
use std::cell::{Cell, RefCell};
use std::ffi::{CStr, c_char, c_void};

pub type Value = *mut Object;
pub type Native = unsafe extern "C" fn(Value, Value) -> Value;
pub struct Object {
    pub references: usize,
    pub data: Data,
}
pub enum Data {
    None,
    Bool(bool),
    BigInt(BigInt),
    Float(f64),
    Text(Vec<u8>),
    Bytes(Vec<u8>),
    Function {
        entry: Native,
        globals: Value,
        name: String,
        defaults: Value,
        keyword_entry: Option<Native>,
    },
    Args(Vec<Value>),
    ArgumentView {
        values: *const Value,
        count: usize,
        defaults: Value,
    },
    ReceiverArgs {
        receiver: Value,
        values: *const Value,
        count: usize,
        defaults: Value,
    },
    KeywordArgs {
        receiver: Value,
        values: *const Value,
        count: usize,
        names: *const *const c_char,
        keyword_values: *const Value,
        keyword_count: usize,
        defaults: Value,
    },
    List(Vec<Value>),
    Namespace(Vec<(String, Value)>),
    Type {
        name: String,
        attributes: Value,
    },
    Class(Box<crate::classes::ClassLayout>),
    Instance {
        ty: Value,
        fields: Box<[Value]>,
    },
    NativeInstance {
        ty: Value,
        payload: Box<[i64]>,
    },
    BoundMethod {
        receiver: Value,
        method: Value,
    },
    StaticMethod(Value),
}
thread_local! {
    static ERROR: RefCell<Option<(String, String)>> = const { RefCell::new(None) };
    static DEPTH: Cell<usize> = const { Cell::new(0) };
}
pub fn error(kind: &str, message: &str) {
    if crate::sequences::has_pending() {
        return;
    }
    ERROR.with_borrow_mut(|current| {
        if current.is_none() {
            *current = Some((kind.into(), message.into()));
        }
    });
}
pub fn has_error() -> bool {
    ERROR.with_borrow(|current| current.is_some()) || crate::sequences::has_pending()
}
pub fn clear_error() {
    ERROR.with_borrow_mut(|current| *current = None);
    crate::sequences::clear_pending();
}
pub fn take_native_error() -> Option<(String, String)> {
    ERROR.with_borrow_mut(Option::take)
}
pub fn error_kind() -> String {
    if crate::sequences::has_pending() {
        return crate::sequences::pending_kind();
    }
    ERROR.with_borrow(|current| {
        current
            .as_ref()
            .map_or_else(String::new, |value| value.0.clone())
    })
}
pub fn error_message() -> String {
    if crate::sequences::has_pending() {
        return crate::sequences::pending_message();
    }
    ERROR.with_borrow(|current| {
        current
            .as_ref()
            .map_or_else(String::new, |value| value.1.clone())
    })
}
pub fn is_fixnum(value: Value) -> bool {
    value as usize & 1 != 0
}
pub fn small(value: i64) -> Value {
    (((value as u64) << 1) | 1) as Value
}
/// Only tagged values may be decoded; heap values must never be dereferenced as tags.
pub unsafe fn fixnum(value: Value) -> i64 {
    (value as isize >> 1) as i64
}
pub unsafe fn new_value(data: Data) -> Value {
    Box::into_raw(Box::new(Object {
        references: 1,
        data,
    }))
}
pub unsafe fn retain(value: Value) -> Value {
    unsafe {
        if !value.is_null() && !is_fixnum(value) && (*value).references != usize::MAX {
            (*value).references += 1;
        }
        value
    }
}
pub unsafe fn release(value: Value) {
    unsafe {
        if value.is_null() || is_fixnum(value) || (*value).references == usize::MAX {
            return;
        }
        (*value).references -= 1;
        if (*value).references != 0 {
            return;
        }
        // Box owns strings/vectors/GMP values. Their contained raw value references
        // are released before the Rust fields are dropped.
        let object = Box::from_raw(value);
        match &object.data {
            Data::Args(values) | Data::List(values) => {
                for value in values {
                    release(*value);
                }
            }
            Data::Namespace(bindings) => {
                for (_, value) in bindings {
                    release(*value);
                }
            }
            Data::Type { attributes, .. } => release(*attributes),
            Data::Class(layout) => {
                release(layout.base);
                for (_, method) in &layout.methods {
                    release(*method);
                }
            }
            Data::Instance { ty, fields } => {
                release(*ty);
                for field in fields {
                    release(*field);
                }
            }
            Data::NativeInstance { ty, .. } => release(*ty),
            Data::BoundMethod { receiver, method } => {
                release(*receiver);
                release(*method);
            }
            Data::StaticMethod(method) => release(*method),
            Data::Function { defaults, .. } => release(*defaults),
            _ => (), // Function globals are borrowed from the executing module.
        }
    }
}
/// This private wrapper is used only for frozen None/Bool objects. These variants
/// own no pointers or mutable storage, and their immortal reference count is
/// never modified by retain/release. No other Object variants may be stored here.
#[repr(transparent)]
struct ImmortalObject(Object);
unsafe impl Sync for ImmortalObject {}

#[unsafe(export_name = "mule.none")]
static NONE: ImmortalObject = ImmortalObject(Object {
    references: usize::MAX,
    data: Data::None,
});
#[unsafe(export_name = "mule.false")]
static FALSE: ImmortalObject = ImmortalObject(Object {
    references: usize::MAX,
    data: Data::Bool(false),
});
#[unsafe(export_name = "mule.true")]
static TRUE: ImmortalObject = ImmortalObject(Object {
    references: usize::MAX,
    data: Data::Bool(true),
});
pub unsafe fn none_value() -> Value {
    std::ptr::addr_of!(NONE.0).cast_mut()
}
pub unsafe fn bool_value(value: bool) -> Value {
    if value {
        std::ptr::addr_of!(TRUE.0).cast_mut()
    } else {
        std::ptr::addr_of!(FALSE.0).cast_mut()
    }
}
pub unsafe fn namespace() -> Value {
    unsafe { new_value(Data::Namespace(Vec::new())) }
}
pub unsafe fn list() -> Value {
    unsafe { new_value(Data::List(Vec::new())) }
}
pub unsafe fn list_append(list: Value, value: Value) -> bool {
    unsafe {
        if list.is_null() || is_fixnum(list) || !matches!(&(*list).data, Data::List(_)) {
            error("TypeError", "list append requires a list");
            return false;
        }
        match &mut (*list).data {
            Data::List(values) => {
                values.push(retain(value));
                true
            }
            _ => {
                error("TypeError", "list append requires a list");
                false
            }
        }
    }
}
/// Lookup borrows a binding; callers retain it before callbacks or mutation.
pub unsafe fn lookup(namespace: Value, name: &str) -> Value {
    unsafe {
        if !namespace.is_null() && !is_fixnum(namespace) {
            if let Data::Namespace(bindings) = &(*namespace).data {
                return bindings
                    .iter()
                    .find(|(key, _)| key == name)
                    .map_or(std::ptr::null_mut(), |(_, value)| *value);
            }
        }
        error("TypeError", "expected a Mule namespace");
        std::ptr::null_mut()
    }
}
pub unsafe fn bind(namespace: Value, name: &str, value: Value) -> bool {
    unsafe {
        if !namespace.is_null()
            && !is_fixnum(namespace)
            && !value.is_null()
            && matches!(&(*namespace).data, Data::Namespace(_))
        {
            if let Data::Namespace(bindings) = &mut (*namespace).data {
                if let Some((_, previous)) = bindings.iter_mut().find(|(key, _)| key == name) {
                    let old = *previous;
                    *previous = retain(value);
                    release(old);
                } else {
                    bindings.push((name.into(), retain(value)));
                }
                return true;
            }
        }
        error("TypeError", "invalid namespace binding");
        false
    }
}
pub unsafe fn function(globals: Value, entry: Native, name: &str) -> Value {
    unsafe {
        new_value(Data::Function {
            entry,
            globals,
            name: name.into(),
            defaults: std::ptr::null_mut(),
            keyword_entry: None,
        })
    }
}
pub unsafe fn argument(args: Value, index: usize) -> Option<Value> {
    unsafe {
        if !args.is_null() && !is_fixnum(args) {
            if let Data::Args(values) = &(*args).data {
                return values.get(index).copied();
            }
            match &(*args).data {
                Data::ArgumentView { values, count, .. } if index < *count => {
                    return Some(*values.add(index));
                }
                Data::ReceiverArgs { receiver, .. } if index == 0 => return Some(*receiver),
                Data::ReceiverArgs { values, count, .. } if index <= *count => {
                    return Some(*values.add(index - 1));
                }
                Data::KeywordArgs {
                    receiver,
                    values,
                    count,
                    ..
                } => {
                    let offset = usize::from(!receiver.is_null());
                    if offset == 1 && index == 0 {
                        return Some(*receiver);
                    }
                    if index >= offset && index - offset < *count {
                        return Some(*values.add(index - offset));
                    }
                }
                _ => (),
            }
        }
        error("SystemError", "native arguments must be an argument view");
        None
    }
}
pub unsafe fn argument_count(args: Value) -> Option<usize> {
    unsafe {
        if !args.is_null() && !is_fixnum(args) {
            if let Data::Args(values) = &(*args).data {
                return Some(values.len());
            }
            match &(*args).data {
                Data::ArgumentView { count, .. } => return Some(*count),
                Data::KeywordArgs {
                    receiver, count, ..
                } => return Some(*count + usize::from(!receiver.is_null())),
                Data::ReceiverArgs { count, .. } => return Some(*count + 1),
                _ => (),
            }
        }
        error("SystemError", "native arguments must be an argument view");
        None
    }
}
pub unsafe fn call(callable: Value, args: &[Value]) -> Value {
    unsafe {
        if has_error() {
            return std::ptr::null_mut();
        }
        if callable.is_null() || is_fixnum(callable) {
            error("TypeError", "object is not callable");
            return std::ptr::null_mut();
        }
        match &(*callable).data {
            Data::Type { .. } => {
                if !args.is_empty() {
                    error("TypeError", "instance constructor takes no arguments");
                    return std::ptr::null_mut();
                }
                return new_value(Data::Instance {
                    ty: retain(callable),
                    fields: Box::new([]),
                });
            }
            Data::Class(_) => return crate::classes::construct(callable, args),
            Data::BoundMethod { receiver, method } => {
                return call_with_receiver(*method, *receiver, args);
            }
            Data::StaticMethod(method) => return call(*method, args),
            Data::Instance { .. } | Data::NativeInstance { .. } => {
                let result = special(callable, "__call__", args);
                if result.is_null() && !has_error() {
                    error("TypeError", "object is not callable");
                }
                return result;
            }
            Data::Function { .. } => (),
            _ => {
                error("TypeError", "object is not callable");
                return std::ptr::null_mut();
            }
        }
        let (entry, globals, defaults) = match &(*callable).data {
            Data::Function {
                entry,
                globals,
                defaults,
                ..
            } => (*entry, *globals, *defaults),
            _ => unreachable!(),
        };
        if mule_enter_frame() < 0 {
            return std::ptr::null_mut();
        }
        // This stack object borrows the argument slice without allocating a vector.
        // Native callbacks may retain individual arguments, never the argument view.
        let mut view = Object {
            references: usize::MAX,
            data: Data::ArgumentView {
                values: args.as_ptr(),
                count: args.len(),
                defaults,
            },
        };
        let result = entry(globals, &mut view);
        mule_leave_frame();
        if result.is_null() && !has_error() {
            error(
                "SystemError",
                "native function returned NULL without an error",
            );
        }
        result
    }
}
/// Invoke a native method with a borrowed receiver-prefixed argument view.
/// The common call path allocates neither an argument vector nor a bound method.
pub unsafe fn call_with_receiver(method: Value, receiver: Value, args: &[Value]) -> Value {
    unsafe {
        if has_error() {
            return std::ptr::null_mut();
        }
        if !method.is_null() && !is_fixnum(method) {
            if let Data::Function {
                entry,
                globals,
                defaults,
                ..
            } = &(*method).data
            {
                if mule_enter_frame() < 0 {
                    return std::ptr::null_mut();
                }
                let mut view = Object {
                    references: usize::MAX,
                    data: Data::ReceiverArgs {
                        receiver,
                        values: args.as_ptr(),
                        count: args.len(),
                        defaults: *defaults,
                    },
                };
                let result = entry(*globals, &mut view);
                mule_leave_frame();
                if result.is_null() && !has_error() {
                    error(
                        "SystemError",
                        "native method returned NULL without an error",
                    );
                }
                return result;
            }
        }
        error("TypeError", "method is not a native function");
        std::ptr::null_mut()
    }
}
pub unsafe fn special(value: Value, name: &str, args: &[Value]) -> Value {
    unsafe {
        let method = crate::sequences::binding(value, name);
        if !method.is_null() {
            return call_with_receiver(method, value, args);
        }
        if value.is_null() || is_fixnum(value) {
            return std::ptr::null_mut();
        }
        let ty = match &(*value).data {
            Data::Instance { ty, .. } | Data::NativeInstance { ty, .. } => *ty,
            _ => return std::ptr::null_mut(),
        };
        let attributes = match &(*ty).data {
            Data::Type { attributes, .. } => *attributes,
            Data::Class(_) => return crate::classes::special_method(value, name, args),
            _ => unreachable!(),
        };
        let method = lookup(attributes, name);
        if method.is_null() {
            return method;
        }
        retain(method);
        let result = if is_fixnum(method) {
            call(method, args)
        } else {
            match &(*method).data {
                Data::StaticMethod(method) => call(*method, args),
                _ => call_with_receiver(method, value, args),
            }
        };
        release(method);
        result
    }
}
pub unsafe fn truth(value: Value) -> Option<bool> {
    unsafe {
        if has_error() {
            return None;
        }
        if is_fixnum(value) {
            return Some(fixnum(value) != 0);
        }
        if value.is_null() {
            error("SystemError", "NULL truth operand");
            return None;
        }
        Some(match &(*value).data {
            Data::None => false,
            Data::Bool(value) => *value,
            Data::BigInt(integer) => integer.sign() != 0,
            Data::Float(value) => *value != 0.0,
            Data::Text(value) | Data::Bytes(value) => !value.is_empty(),
            Data::Args(value) | Data::List(value) => !value.is_empty(),
            Data::Namespace(bindings) => !bindings.is_empty(),
            Data::Instance { .. } | Data::NativeInstance { .. } => {
                let result = special(value, "__bool__", &[]);
                if result.is_null() {
                    if has_error() {
                        return None;
                    }
                    let length = special(value, "__len__", &[]);
                    if length.is_null() {
                        return if has_error() { None } else { Some(true) };
                    }
                    return length_result(length).map(|length| length != 0);
                }
                let boolean = if !is_fixnum(result) {
                    match &(*result).data {
                        Data::Bool(value) => Some(*value),
                        _ => None,
                    }
                } else {
                    None
                };
                release(result);
                if boolean.is_none() {
                    error("TypeError", "__bool__ should return bool");
                }
                return boolean;
            }
            _ => true,
        })
    }
}
/// Validate and consume the owned result of the length protocol.
pub unsafe fn length_result(result: Value) -> Option<usize> {
    unsafe {
        if is_fixnum(result) {
            let length = fixnum(result);
            if length < 0 {
                error("ValueError", "__len__ must return a nonnegative integer");
                return None;
            }
            return Some(length as usize);
        }
        if !result.is_null() {
            if let Data::Bool(value) = &(*result).data {
                let length = usize::from(*value);
                release(result);
                return Some(length);
            }
        }
        let integer = numeric::integer_export(result);
        release(result);
        let integer = integer?;
        if integer.sign() < 0 {
            error("ValueError", "__len__ must return a nonnegative integer");
            return None;
        }
        let length = integer
            .to_u64()
            .and_then(|length| usize::try_from(length).ok());
        match length {
            Some(length) if length <= isize::MAX as usize => Some(length),
            _ => {
                error("OverflowError", "__len__ result is too large");
                None
            }
        }
    }
}
pub unsafe fn binary(op: i32, left: Value, right: Value, inplace: bool) -> Value {
    unsafe {
        if has_error() {
            return std::ptr::null_mut();
        }
        if !left.is_null() && !right.is_null() && !is_fixnum(left) && !is_fixnum(right) {
            match (&(*left).data, &(*right).data) {
                (Data::Text(a), Data::Text(b)) | (Data::Bytes(a), Data::Bytes(b)) if op == 0 => {
                    let mut result = a.clone();
                    result.extend_from_slice(b);
                    return new_value(if matches!(&(*left).data, Data::Text(_)) {
                        Data::Text(result)
                    } else {
                        Data::Bytes(result)
                    });
                }
                (Data::List(a), Data::List(b)) if op == 0 => {
                    // Snapshot borrowed values before mutating either aliased list.
                    let left_values = a.clone();
                    let right_values = b.clone();
                    let result = if inplace { retain(left) } else { list() };
                    if !inplace {
                        for value in left_values {
                            list_append(result, value);
                        }
                    }
                    for value in right_values {
                        list_append(result, value);
                    }
                    return result;
                }
                _ => (),
            }
        }
        if op == 2 {
            let (string, count) = if !left.is_null()
                && !is_fixnum(left)
                && matches!(&(*left).data, Data::Text(_) | Data::Bytes(_))
                && numeric::is_integer(right)
            {
                (left, right)
            } else if !right.is_null()
                && !is_fixnum(right)
                && matches!(&(*right).data, Data::Text(_) | Data::Bytes(_))
                && numeric::is_integer(left)
            {
                (right, left)
            } else {
                return numeric::binary(op, left, right);
            };
            let Some(count) = numeric::integer_export(count) else {
                return std::ptr::null_mut();
            };
            let data = match &(*string).data {
                Data::Text(data) | Data::Bytes(data) => data,
                _ => unreachable!(),
            };
            let result = if count.sign() <= 0 || data.is_empty() {
                Vec::new()
            } else {
                let Some(count) = count.to_u64().and_then(|value| usize::try_from(value).ok())
                else {
                    error("OverflowError", "repeated string too large");
                    return std::ptr::null_mut();
                };
                let Some(length) = data.len().checked_mul(count) else {
                    error("OverflowError", "repeated string too large");
                    return std::ptr::null_mut();
                };
                let mut result = Vec::new();
                if result.try_reserve_exact(length).is_err() {
                    error("MemoryError", "repeated string allocation failed");
                    return std::ptr::null_mut();
                }
                for _ in 0..count {
                    result.extend_from_slice(data);
                }
                result
            };
            return new_value(if matches!(&(*string).data, Data::Text(_)) {
                Data::Text(result)
            } else {
                Data::Bytes(result)
            });
        }
        numeric::binary(op, left, right)
    }
}
unsafe fn contains(container: Value, item: Value) -> Option<bool> {
    unsafe {
        if !container.is_null() && !is_fixnum(container) {
            match &(*container).data {
                Data::Text(haystack) | Data::Bytes(haystack) => {
                    if matches!(&(*container).data, Data::Bytes(_)) && numeric::is_integer(item) {
                        let integer = numeric::integer_export(item)?;
                        let Some(byte) =
                            integer.to_u64().and_then(|value| u8::try_from(value).ok())
                        else {
                            error("ValueError", "byte must be in range(0, 256)");
                            return None;
                        };
                        return Some(haystack.contains(&byte));
                    }
                    if !item.is_null() && !is_fixnum(item) {
                        let needle = match (&(*container).data, &(*item).data) {
                            (Data::Text(_), Data::Text(data))
                            | (Data::Bytes(_), Data::Bytes(data)) => Some(data),
                            _ => None,
                        };
                        if let Some(needle) = needle {
                            return Some(
                                needle.is_empty()
                                    || haystack
                                        .windows(needle.len())
                                        .any(|window| window == needle),
                            );
                        }
                    }
                    error("TypeError", "incompatible string containment operand");
                    return None;
                }
                Data::Args(values) | Data::List(values) => {
                    // Retain the snapshot across user equality/truth callbacks.
                    let values: Vec<_> = values.iter().map(|value| retain(*value)).collect();
                    let mut result = Some(false);
                    for value in &values {
                        if *value == item {
                            result = Some(true);
                            break;
                        }
                        let equal = compare(0, item, *value);
                        if equal.is_null() {
                            result = None;
                            break;
                        }
                        let matched = truth(equal);
                        release(equal);
                        if matched != Some(false) {
                            result = matched;
                            break;
                        }
                    }
                    for value in values {
                        release(value);
                    }
                    return result;
                }
                _ => (),
            }
        }
        error("TypeError", "object does not support containment");
        None
    }
}
pub unsafe fn compare(op: i32, left: Value, right: Value) -> Value {
    unsafe {
        if has_error() {
            return std::ptr::null_mut();
        }
        if op == 6 || op == 7 {
            return bool_value((left == right) == (op == 6));
        }
        if op == 8 || op == 9 {
            return contains(right, left)
                .map_or(std::ptr::null_mut(), |found| bool_value(found == (op == 8)));
        }
        let methods = ["__eq__", "__ne__", "__lt__", "__le__", "__gt__", "__ge__"];
        let Some(method) = usize::try_from(op).ok().and_then(|op| methods.get(op)) else {
            error("SystemError", "unknown comparison operation");
            return std::ptr::null_mut();
        };
        let result = special(left, method, &[right]);
        if !result.is_null() || has_error() {
            return result;
        }
        let numeric_value = |value: Value| {
            numeric::is_integer(value)
                || (!value.is_null()
                    && !is_fixnum(value)
                    && matches!(&(*value).data, Data::Float(_)))
        };
        let ordering = if numeric_value(left) && numeric_value(right) {
            numeric::compare(left, right)
        } else if !left.is_null() && !right.is_null() && !is_fixnum(left) && !is_fixnum(right) {
            match (&(*left).data, &(*right).data) {
                (Data::Text(a), Data::Text(b)) | (Data::Bytes(a), Data::Bytes(b)) => {
                    Some(match a.cmp(b) {
                        std::cmp::Ordering::Less => -1,
                        std::cmp::Ordering::Equal => 0,
                        std::cmp::Ordering::Greater => 1,
                    })
                }
                _ => None,
            }
        } else {
            None
        };
        if let Some(ordering) = ordering {
            return bool_value(if ordering == 2 {
                op == 1
            } else {
                match op {
                    0 => ordering == 0,
                    1 => ordering != 0,
                    2 => ordering < 0,
                    3 => ordering <= 0,
                    4 => ordering > 0,
                    _ => ordering >= 0,
                }
            });
        }
        if has_error() {
            return std::ptr::null_mut();
        }
        if op < 2 {
            return bool_value((left == right) == (op == 0));
        }
        error("TypeError", "incompatible comparison operands");
        std::ptr::null_mut()
    }
}

pub fn decode_text(data: &[u8]) -> Option<Vec<u32>> {
    let mut result = Vec::new();
    let mut offset = 0;
    while offset < data.len() {
        let first = data[offset];
        offset += 1;
        if first < 128 {
            result.push(u32::from(first));
            continue;
        }
        let (count, mut value, minimum) = match first {
            0xc2..=0xdf => (1, u32::from(first & 31), 0x80),
            0xe0..=0xef => (2, u32::from(first & 15), 0x800),
            0xf0..=0xf4 => (3, u32::from(first & 7), 0x10000),
            _ => {
                error("UnicodeDecodeError", "invalid UTF-8 text");
                return None;
            }
        };
        if data.len() - offset < count {
            error("UnicodeDecodeError", "incomplete UTF-8 text");
            return None;
        }
        for _ in 0..count {
            let byte = data[offset];
            offset += 1;
            if byte & 0xc0 != 0x80 {
                error("UnicodeDecodeError", "invalid UTF-8 text");
                return None;
            }
            value = (value << 6) | u32::from(byte & 63);
        }
        if value < minimum || value > 0x10ffff {
            error("UnicodeDecodeError", "invalid UTF-8 text");
            return None;
        }
        result.push(value);
    }
    Some(result)
}
fn quoted(data: &[u8], bytes: bool, ascii_only: bool) -> Option<Vec<u8>> {
    let quote = if data.contains(&b'\'') && !data.contains(&b'"') {
        b'"'
    } else {
        b'\''
    };
    let codes = if bytes {
        data.iter().map(|value| u32::from(*value)).collect()
    } else {
        decode_text(data)?
    };
    let mut result = Vec::new();
    if bytes {
        result.push(b'b');
    }
    result.push(quote);
    for code in codes {
        if code == u32::from(quote) || code == u32::from(b'\\') {
            result.push(b'\\');
            result.push(code as u8);
        } else if code == 10 {
            result.extend_from_slice(b"\\n");
        } else if code == 13 {
            result.extend_from_slice(b"\\r");
        } else if code == 9 {
            result.extend_from_slice(b"\\t");
        } else if code < 32
            || (127..=160).contains(&code)
            || ((bytes || ascii_only) && code >= 128)
            || (0xd800..=0xdfff).contains(&code)
            || matches!(code, 0x2028 | 0x2029 | 0xfeff)
            || code & 0xffff >= 0xfffe
        {
            let escape = if code <= 255 {
                format!("\\x{code:02x}")
            } else if code <= 65535 {
                format!("\\u{code:04x}")
            } else {
                format!("\\U{code:08x}")
            };
            result.extend_from_slice(escape.as_bytes());
        } else {
            let mut buffer = [0; 4];
            result.extend_from_slice(char::from_u32(code)?.encode_utf8(&mut buffer).as_bytes());
        }
    }
    result.push(quote);
    Some(result)
}
fn float_repr(value: f64) -> String {
    if value.is_nan() {
        return "nan".into();
    }
    let value = format!("{value:?}");
    if let Some((mantissa, exponent)) = value.split_once('e') {
        if let Ok(exponent) = exponent.parse::<i32>() {
            return format!("{mantissa}e{exponent:+03}");
        }
    }
    value
}
pub unsafe fn formatted_value(
    value: Value,
    representation: bool,
    ascii_only: bool,
) -> Option<Vec<u8>> {
    unsafe {
        if is_fixnum(value) {
            return Some(fixnum(value).to_string().into_bytes());
        }
        if value.is_null() {
            error("SystemError", "NULL formatting operand");
            return None;
        }
        Some(match &(*value).data {
            Data::None => b"None".to_vec(),
            Data::Bool(value) => {
                if *value {
                    b"True".to_vec()
                } else {
                    b"False".to_vec()
                }
            }
            Data::BigInt(value) => value.to_decimal().into_bytes(),
            Data::Float(value) => float_repr(*value).into_bytes(),
            Data::Text(data) if !representation => {
                if decode_text(data)?
                    .iter()
                    .any(|code| (0xd800..=0xdfff).contains(code))
                {
                    error("UnicodeEncodeError", "cannot encode surrogate to UTF-8");
                    return None;
                }
                data.clone()
            }
            Data::Text(data) => quoted(data, false, ascii_only)?,
            Data::Bytes(data) => quoted(data, true, ascii_only)?,
            Data::Args(values) | Data::List(values) => {
                let list = matches!(&(*value).data, Data::List(_));
                let mut result = vec![if list { b'[' } else { b'(' }];
                for (index, value) in values.iter().enumerate() {
                    if index > 0 {
                        result.extend_from_slice(b", ");
                    }
                    result.extend(formatted_value(*value, true, ascii_only)?);
                }
                if !list && values.len() == 1 {
                    result.push(b',');
                }
                result.push(if list { b']' } else { b')' });
                result
            }
            Data::Namespace(bindings) => {
                let mut result = vec![b'{'];
                for (index, (name, value)) in bindings.iter().enumerate() {
                    if index > 0 {
                        result.extend_from_slice(b", ");
                    }
                    result.extend(quoted(name.as_bytes(), false, ascii_only)?);
                    result.extend_from_slice(b": ");
                    result.extend(formatted_value(*value, true, ascii_only)?);
                }
                result.push(b'}');
                result
            }
            Data::Function { name, .. } => format!("<native function {name}>").into_bytes(),
            Data::Class(layout) => format!("<class '{}'>", layout.name).into_bytes(),
            Data::BoundMethod { .. } => b"<bound native method>".to_vec(),
            Data::StaticMethod(_) => b"<staticmethod>".to_vec(),
            Data::Type { name, .. } => format!("<class '{name}'>").into_bytes(),
            Data::Instance { ty, .. } | Data::NativeInstance { ty, .. } => match &(**ty).data {
                Data::Type { name, .. } => format!("<{name} object>").into_bytes(),
                Data::Class(layout) => format!("<{} object>", layout.name).into_bytes(),
                _ => unreachable!(),
            },
            Data::ArgumentView { .. } | Data::ReceiverArgs { .. } | Data::KeywordArgs { .. } => {
                error("SystemError", "argument view cannot escape a native call");
                return None;
            }
        })
    }
}

pub(crate) unsafe fn name_from_c<'a>(name: *const c_char) -> Option<&'a str> {
    unsafe {
        if name.is_null() {
            error("SystemError", "NULL name");
            return None;
        }
        match CStr::from_ptr(name).to_str() {
            Ok(name) => Some(name),
            Err(_) => {
                error("UnicodeDecodeError", "invalid name");
                None
            }
        }
    }
}
// FFI callers supply valid buffers and live owned/borrowed values. Null results
// signal the sticky runtime error; each successful returned value is owned.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_none() -> Value {
    unsafe { none_value() }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_bool(value: i32) -> Value {
    unsafe { bool_value(value != 0) }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_release(value: Value) {
    unsafe { release(value) };
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_literal_text(data: *const c_char, length: isize) -> Value {
    unsafe { literal_bytes(data, length, true) }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_literal_bytes(data: *const c_char, length: isize) -> Value {
    unsafe { literal_bytes(data, length, false) }
}
unsafe fn literal_bytes(data: *const c_char, length: isize, text: bool) -> Value {
    unsafe {
        if length < 0 || (data.is_null() && length != 0) {
            error("OverflowError", "invalid string buffer");
            return std::ptr::null_mut();
        }
        let bytes = if length == 0 {
            Vec::new()
        } else {
            std::slice::from_raw_parts(data.cast::<u8>(), length as usize).to_vec()
        };
        new_value(if text {
            Data::Text(bytes)
        } else {
            Data::Bytes(bytes)
        })
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_retain(value: Value) -> Value {
    unsafe { retain(value) }
}
#[unsafe(no_mangle)]
pub extern "C" fn mule_enter_frame() -> i32 {
    if has_error() {
        return -1;
    }
    let entered = DEPTH.with(|depth| {
        if depth.get() >= 1000 {
            false
        } else {
            depth.set(depth.get() + 1);
            true
        }
    });
    if entered {
        0
    } else {
        error("RecursionError", "maximum native recursion depth exceeded");
        -1
    }
}
#[unsafe(no_mangle)]
pub extern "C" fn mule_leave_frame() {
    DEPTH.with(|depth| {
        debug_assert!(depth.get() > 0, "native frame exits must match entries");
        depth.set(depth.get() - 1);
    });
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_function_matches(
    callable: Value,
    expected: *mut c_void,
    globals: Value,
) -> i32 {
    unsafe {
        if callable.is_null() || is_fixnum(callable) {
            return 0;
        }
        match &(*callable).data {
            Data::Function {
                entry,
                globals: captured,
                ..
            } => i32::from(*entry as *const () as *mut c_void == expected && *captured == globals),
            _ => 0,
        }
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_namespace() -> Value {
    unsafe { namespace() }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_namespace_bound(namespace: Value, name: *const c_char) -> i32 {
    unsafe {
        let Some(name) = name_from_c(name) else {
            return -1;
        };
        let value = lookup(namespace, name);
        if has_error() {
            -1
        } else {
            i32::from(!value.is_null())
        }
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_assertion_failure(message: Value) -> i32 {
    unsafe {
        if let Some(message) = formatted_value(message, false, false) {
            error("AssertionError", &String::from_utf8_lossy(&message));
        }
        -1
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_list() -> Value {
    unsafe { list() }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_list_append(list: Value, value: Value) -> i32 {
    unsafe { if list_append(list, value) { 0 } else { -1 } }
}

/// Copy and retain an evaluated buffer. Language evaluation and container
/// algorithms are compiled Mule code; this only constructs owned storage.
unsafe fn sequence_buffer(count: isize, values: *const Value) -> Option<Vec<Value>> {
    unsafe {
        if count < 0 || (count != 0 && values.is_null()) {
            error("SystemError", "invalid sequence buffer");
            return None;
        }
        let mut result = Vec::new();
        if result.try_reserve_exact(count as usize).is_err() {
            error("MemoryError", "sequence allocation failed");
            return None;
        }
        for index in 0..count as usize {
            result.push(retain(*values.add(index)));
        }
        Some(result)
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_sequence_list(count: isize, values: *const Value) -> Value {
    unsafe {
        sequence_buffer(count, values)
            .map_or(std::ptr::null_mut(), |values| new_value(Data::List(values)))
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_sequence_tuple(count: isize, values: *const Value) -> Value {
    unsafe {
        sequence_buffer(count, values)
            .map_or(std::ptr::null_mut(), |values| new_value(Data::Args(values)))
    }
}
/// Return an owned callable from the receiver's class method table, or None
/// when absent. Instance fields are intentionally not consulted.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_lookup_special(value: Value, name: *const c_char) -> Value {
    unsafe {
        let Some(name) = name_from_c(name) else {
            return std::ptr::null_mut();
        };
        if value.is_null() || is_fixnum(value) {
            return none_value();
        }
        let method = match &(*value).data {
            Data::Instance { ty, .. } | Data::NativeInstance { ty, .. } => match &(**ty).data {
                Data::Class(_) => crate::classes::special_binding(value, name),
                Data::Type { attributes, .. } => lookup(*attributes, name),
                _ => unreachable!(),
            },
            _ => crate::sequences::binding(value, name),
        };
        if method.is_null() {
            return none_value();
        }
        if !is_fixnum(method) {
            if let Data::StaticMethod(method) = &(*method).data {
                return retain(*method);
            }
        }
        new_value(Data::BoundMethod {
            receiver: retain(value),
            method: retain(method),
        })
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_render_text(value: Value, representation: i32) -> Value {
    unsafe {
        formatted_value(value, representation != 0, false)
            .map_or(std::ptr::null_mut(), |bytes| new_value(Data::Text(bytes)))
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_result(value: Value) -> Value {
    unsafe {
        if !value.is_null() && !is_fixnum(value) {
            if let Data::Text(_) = &(*value).data {
                return retain(value);
            }
        }
        error("TypeError", "formatting protocol must return str");
        std::ptr::null_mut()
    }
}
/// Physical Unicode storage access for compiled Mule text algorithms.
unsafe fn text_storage<'a>(value: Value) -> Option<&'a [u8]> {
    unsafe {
        if !value.is_null() && !is_fixnum(value) {
            if let Data::Text(bytes) = &(*value).data {
                return Some(bytes);
            }
        }
        error("TypeError", "text storage is required");
        None
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_length(value: Value) -> Value {
    unsafe {
        let Some(bytes) = text_storage(value) else {
            return std::ptr::null_mut();
        };
        decode_text(bytes).map_or(std::ptr::null_mut(), |characters| {
            numeric::integer_from_usize(characters.len())
        })
    }
}
unsafe fn text_position(value: Value, index: Value) -> Option<(u32, usize, usize)> {
    unsafe {
        let bytes = text_storage(value)?;
        let index = if is_fixnum(index) {
            usize::try_from(fixnum(index)).ok()
        } else {
            numeric::integer_export(index)?
                .to_u64()
                .and_then(|value| usize::try_from(value).ok())
        };
        let characters = decode_text(bytes)?;
        let Some(index) = index.filter(|index| *index < characters.len()) else {
            error("IndexError", "text storage index out of range");
            return None;
        };
        let encoded_size = |value: u32| {
            if value < 0x80 {
                1
            } else if value < 0x800 {
                2
            } else if value < 0x10000 {
                3
            } else {
                4
            }
        };
        let start = characters[..index]
            .iter()
            .map(|value| encoded_size(*value))
            .sum();
        let codepoint = characters[index];
        Some((codepoint, start, start + encoded_size(codepoint)))
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_character(value: Value, index: Value) -> Value {
    unsafe {
        let Some((_, start, end)) = text_position(value, index) else {
            return std::ptr::null_mut();
        };
        let Some(bytes) = text_storage(value) else {
            return std::ptr::null_mut();
        };
        new_value(Data::Text(bytes[start..end].to_vec()))
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_codepoint(value: Value, index: Value) -> Value {
    unsafe {
        text_position(value, index).map_or(std::ptr::null_mut(), |(codepoint, _, _)| {
            numeric::integer_from_usize(codepoint as usize)
        })
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_invalid_string_format() -> Value {
    error("ValueError", "unsupported string format specification");
    std::ptr::null_mut()
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_load_global(globals: Value, name: *const c_char) -> Value {
    unsafe {
        let Some(name) = name_from_c(name) else {
            return std::ptr::null_mut();
        };
        let value = lookup(globals, name);
        if !value.is_null() {
            return retain(value);
        }
        if has_error() {
            return std::ptr::null_mut();
        }
        let value = builtins::lookup_builtin(name);
        if !value.is_null() {
            return retain(value);
        }
        error("NameError", &format!("name '{name}' is not defined"));
        std::ptr::null_mut()
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_store_global(
    globals: Value,
    name: *const c_char,
    value: Value,
) -> i32 {
    unsafe { name_from_c(name).map_or(-1, |name| if bind(globals, name, value) { 0 } else { -1 }) }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_load_local(value: Value, name: *const c_char) -> Value {
    unsafe {
        if !value.is_null() {
            return retain(value);
        }
        let Some(name) = name_from_c(name) else {
            return std::ptr::null_mut();
        };
        error(
            "UnboundLocalError",
            &format!(
                "cannot access local variable '{name}' where it is not associated with a value"
            ),
        );
        std::ptr::null_mut()
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_binary(op: i32, left: Value, right: Value, inplace: i32) -> Value {
    unsafe { binary(op, left, right, inplace != 0) }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_binary_reuse(
    op: i32,
    left: Value,
    right: Value,
    inplace: i32,
    slot: *mut Value,
) -> Value {
    unsafe {
        if has_error() {
            return std::ptr::null_mut();
        }
        if let Some(result) = numeric::try_binary_reuse(op, left, right, slot) {
            return result;
        }
        binary(op, left, right, inplace != 0)
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_unary(op: i32, value: Value) -> Value {
    unsafe {
        if op == 3 {
            truth(value).map_or(std::ptr::null_mut(), |truth| bool_value(!truth))
        } else {
            numeric::unary(op, value)
        }
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_compare(op: i32, left: Value, right: Value) -> Value {
    unsafe { compare(op, left, right) }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_truth(value: Value) -> i32 {
    unsafe { truth(value).map_or(-1, i32::from) }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_call(callable: Value, count: isize, args: *const Value) -> Value {
    unsafe {
        if count < 0 || (count != 0 && args.is_null()) {
            error("SystemError", "invalid argument buffer");
            return std::ptr::null_mut();
        }
        let args = if count == 0 {
            &[]
        } else {
            std::slice::from_raw_parts(args, count as usize)
        };
        call(callable, args)
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_function(
    globals: Value,
    native: *mut c_void,
    name: *const c_char,
) -> Value {
    unsafe {
        let Some(name) = name_from_c(name) else {
            return std::ptr::null_mut();
        };
        if native.is_null() {
            error("SystemError", "NULL native entry");
            return std::ptr::null_mut();
        }
        function(
            globals,
            std::mem::transmute::<*mut c_void, Native>(native),
            name,
        )
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_check_args(args: Value, count: i32) -> i32 {
    unsafe {
        let Some(actual) = argument_count(args) else {
            return -1;
        };
        if crate::arguments::keyword_count(args) != 0 {
            error("TypeError", "unexpected keyword argument");
            return -1;
        }
        if count < 0 || actual != count as usize {
            error(
                "TypeError",
                &format!("expected {count} positional arguments, got {actual}"),
            );
            -1
        } else {
            0
        }
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_arg(args: Value, index: i32) -> Value {
    unsafe {
        if index < 0 {
            error("IndexError", "native argument index out of range");
            return std::ptr::null_mut();
        }
        if let Some(value) = argument(args, index as usize) {
            retain(value)
        } else {
            error("IndexError", "native argument index out of range");
            std::ptr::null_mut()
        }
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_run(native: *mut c_void) -> i32 {
    unsafe { builtins::run(native) }
}

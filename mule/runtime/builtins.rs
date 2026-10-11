//! Initial builtins implemented with Rust I/O and Mule values.
use crate::gmp::BigInt;
use crate::numeric;
use crate::text::Text;
use crate::object::{self, Data, Native, Object, Value};
use std::cell::Cell;
use std::ffi::c_void;
use std::io::{self, Write};

thread_local! { static BUILTINS: Cell<Value> = const { Cell::new(std::ptr::null_mut()) }; }
fn fail(kind: &str, message: &str) -> Value {
    object::error(kind, message);
    std::ptr::null_mut()
}
unsafe fn arity(args: Value, minimum: usize, maximum: usize) -> Option<usize> {
    unsafe {
        let count = object::argument_count(args)?;
        if count < minimum || count > maximum {
            object::error("TypeError", "invalid argument count");
            None
        } else {
            Some(count)
        }
    }
}
unsafe fn arg(args: Value, index: usize) -> Value {
    unsafe { object::argument(args, index).unwrap() }
}
unsafe fn text(value: Value) -> Option<Vec<u8>> {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            match &(*value).data {
                Data::Text(text) => Some(text.to_wtf8()),
                Data::Bytes(bytes) => Some(bytes.clone()),
                _ => None,
            }
        } else {
            None
        }
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_stdout_write(value: Value) -> Value {
    unsafe {
        if object::has_error() {
            return std::ptr::null_mut();
        }
        if value.is_null() || object::is_fixnum(value) {
            return fail("TypeError", "stdout storage write requires text");
        }
        let Data::Text(text) = &(*value).data else {
            return fail("TypeError", "stdout storage write requires text");
        };
        let Ok(bytes) = text.to_utf8() else {
            return fail("UnicodeEncodeError", "cannot encode surrogate to UTF-8");
        };
        if io::stdout().lock().write_all(&bytes).is_err() {
            return fail("OSError", "output failed");
        }
        object::none_value()
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_storage(value: Value) -> Value {
    unsafe {
        if object::has_error() {
            return std::ptr::null_mut();
        }
        object::bool_value(
            !value.is_null()
                && !object::is_fixnum(value)
                && matches!(&(*value).data, Data::Text(_)),
        )
    }
}
unsafe extern "C" fn abs(_: Value, args: Value) -> Value {
    unsafe {
        if arity(args, 1, 1).is_none() {
            return std::ptr::null_mut();
        }
        let value = arg(args, 0);
        if numeric::is_integer(value) {
            let Some(value) = numeric::integer_export(value) else {
                return std::ptr::null_mut();
            };
            return numeric::integer_import(&value.abs());
        }
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::Float(value) = &(*value).data {
                return object::new_value(Data::Float(value.abs()));
            }
        }
        fail("TypeError", "bad operand type for abs")
    }
}
unsafe extern "C" fn bool_(_: Value, args: Value) -> Value {
    unsafe {
        let Some(count) = arity(args, 0, 1) else {
            return std::ptr::null_mut();
        };
        if count == 0 {
            return object::bool_value(false);
        }
        object::truth(arg(args, 0)).map_or(std::ptr::null_mut(), |truth| object::bool_value(truth))
    }
}
unsafe extern "C" fn len(_: Value, args: Value) -> Value {
    unsafe {
        if arity(args, 1, 1).is_none() {
            return std::ptr::null_mut();
        }
        let value = arg(args, 0);
        if value.is_null() || object::is_fixnum(value) {
            return fail("TypeError", "object has no len");
        }
        let length = match &(*value).data {
            Data::Bytes(value) => value.len(),
            Data::Text(value) => value.len(),
            Data::Args(value) | Data::List(value) => value.len(),
            Data::Namespace(value) => value.len(),
            Data::Instance { .. } | Data::NativeInstance { .. } => {
                let result = object::special(value, "__len__", &[]);
                if result.is_null() {
                    if !object::has_error() {
                        return fail("TypeError", "object has no len");
                    }
                    return std::ptr::null_mut();
                }
                let Some(length) = object::length_result(result) else {
                    return std::ptr::null_mut();
                };
                length
            }
            _ => return fail("TypeError", "object has no len"),
        };
        numeric::integer_from_usize(length)
    }
}
unsafe extern "C" fn input(_: Value, args: Value) -> Value {
    unsafe {
        let Some(count) = arity(args, 0, 1) else {
            return std::ptr::null_mut();
        };
        if count != 0 {
            let Some(prompt) = object::formatted_value(arg(args, 0), false, false) else {
                return std::ptr::null_mut();
            };
            if io::stdout().lock().write_all(&prompt).is_err() {
                return fail("OSError", "output failed");
            }
        }
        if io::stdout().flush().is_err() {
            return fail("OSError", "output failed");
        }
        let mut line = String::new();
        match io::stdin().read_line(&mut line) {
            Ok(0) => fail("EOFError", "could not read input"),
            Ok(_) => {
                if line.ends_with('\n') {
                    line.pop();
                    if line.ends_with('\r') {
                        line.pop();
                    }
                }
                object::new_value(Data::Text(Text::from_rust_str(&line)))
            }
            Err(error) => fail(
                if error.kind() == io::ErrorKind::InvalidData {
                    "UnicodeDecodeError"
                } else {
                    "OSError"
                },
                "could not read input",
            ),
        }
    }
}
fn parse_integer(bytes: &[u8], mut base: i32) -> Option<BigInt> {
    let mut bytes = bytes.trim_ascii();
    let mut negative = false;
    if let Some((&sign, rest)) = bytes.split_first() {
        if sign == b'+' || sign == b'-' {
            negative = sign == b'-';
            bytes = rest;
        }
    }
    let mut prefixed = false;
    if bytes.len() >= 2 && bytes[0] == b'0' {
        let prefix = match bytes[1].to_ascii_lowercase() {
            b'x' => 16,
            b'o' => 8,
            b'b' => 2,
            _ => 0,
        };
        if prefix != 0 && (base == 0 || base == prefix) {
            base = prefix;
            bytes = &bytes[2..];
            prefixed = true;
        }
    }
    let autodetect = base == 0;
    if base == 0 {
        base = 10;
    }
    let mut digits = String::new();
    let mut previous = false;
    let mut nonzero = false;
    for (index, &byte) in bytes.iter().enumerate() {
        if byte == b'_' {
            if (!previous && !(prefixed && index == 0))
                || index + 1 == bytes.len()
                || bytes[index + 1] == b'_'
            {
                return None;
            }
            previous = false;
            continue;
        }
        let digit = char::from(byte).to_digit(base as u32)?;
        previous = true;
        nonzero |= digit != 0;
        digits.push(char::from(byte));
    }
    if digits.is_empty() || (autodetect && bytes.first() == Some(&b'0') && nonzero) {
        return None;
    }
    let result = BigInt::parse(&digits, base)?;
    Some(if negative { result.neg() } else { result })
}
unsafe extern "C" fn int(_: Value, args: Value) -> Value {
    unsafe {
        let Some(count) = arity(args, 0, 2) else {
            return std::ptr::null_mut();
        };
        if count == 0 {
            return object::small(0);
        }
        let value = arg(args, 0);
        let base = if count == 2 {
            let Some(base) = numeric::integer_export(arg(args, 1)).and_then(|value| value.to_i64())
            else {
                if object::has_error() {
                    return std::ptr::null_mut();
                }
                return fail("ValueError", "int base must be >= 2 and <= 36, or 0");
            };
            if base != 0 && !(2..=36).contains(&base) {
                return fail("ValueError", "int base must be >= 2 and <= 36, or 0");
            }
            base as i32
        } else {
            10
        };
        if let Some(bytes) = text(value) {
            return parse_integer(&bytes, base).map_or_else(
                || fail("ValueError", "invalid literal for int"),
                |value| numeric::integer_import(&value),
            );
        }
        if count == 2 {
            return fail("TypeError", "int with explicit base requires a string");
        }
        if numeric::is_integer(value) {
            return numeric::integer_export(value).map_or(std::ptr::null_mut(), |value| {
                numeric::integer_import(&value)
            });
        }
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::Float(value) = &(*value).data {
                if !value.is_finite() {
                    return fail(
                        if value.is_nan() {
                            "ValueError"
                        } else {
                            "OverflowError"
                        },
                        "cannot convert nonfinite float to integer",
                    );
                }
                return numeric::integer_import(&BigInt::from_f64(*value).unwrap());
            }
        }
        fail("TypeError", "int requires a number or string")
    }
}
unsafe extern "C" fn float(_: Value, args: Value) -> Value {
    unsafe {
        let Some(count) = arity(args, 0, 1) else {
            return std::ptr::null_mut();
        };
        if count == 0 {
            return object::new_value(Data::Float(0.0));
        }
        let value = arg(args, 0);
        if let Some(bytes) = text(value) {
            let mut normalized = Vec::new();
            for (index, &byte) in bytes.iter().enumerate() {
                if byte == b'_' {
                    if index == 0
                        || index + 1 == bytes.len()
                        || !bytes[index - 1].is_ascii_digit()
                        || !bytes[index + 1].is_ascii_digit()
                    {
                        return fail("ValueError", "could not convert string to float");
                    }
                } else {
                    normalized.push(byte);
                }
            }
            let value = std::str::from_utf8(normalized.trim_ascii())
                .ok()
                .and_then(|value| value.parse::<f64>().ok());
            return value.map_or_else(
                || fail("ValueError", "could not convert string to float"),
                |value| object::new_value(Data::Float(value)),
            );
        }
        numeric::number_double(value).map_or(std::ptr::null_mut(), |value| {
            object::new_value(Data::Float(value))
        })
    }
}
unsafe extern "C" fn tuple(_: Value, args: Value) -> Value {
    unsafe {
        if arity(args, 0, 0).is_none() {
            std::ptr::null_mut()
        } else {
            object::new_value(Data::Args(Vec::new()))
        }
    }
}
unsafe extern "C" fn list(_: Value, args: Value) -> Value {
    unsafe {
        if arity(args, 0, 0).is_none() {
            std::ptr::null_mut()
        } else {
            object::list()
        }
    }
}
pub unsafe fn namespace() -> Value {
    unsafe {
        BUILTINS.with(|storage| {
            if !storage.get().is_null() {
                return storage.get();
            }
            let namespace = object::namespace();
            let entries: &[(&str, Native)] = &[
                ("abs", abs),
                ("int", int),
                ("input", input),
                ("len", len),
                ("bool", bool_),
                ("float", float),
                ("tuple", tuple),
                ("list", list),
            ];
            for (name, entry) in entries {
                let function = object::function(std::ptr::null_mut(), *entry, name);
                object::bind(namespace, name, function);
                object::release(function);
            }
            storage.set(namespace);
            namespace
        })
    }
}
pub unsafe fn lookup_builtin(name: &str) -> Value {
    unsafe { object::lookup(namespace(), name) }
}
pub unsafe fn run(native: *mut c_void) -> i32 {
    unsafe { run_protocols(native, std::ptr::null_mut()) }
}
pub unsafe fn run_protocols(native: *mut c_void, protocols: *mut c_void) -> i32 {
    unsafe {
        object::clear_error();
        if native.is_null() {
            return {
                object::error("SystemError", "NULL module entry");
                1
            };
        }
        let globals = object::namespace();
        let name = object::new_value(Data::Text(Text::from_rust_str("__main__")));
        object::bind(globals, "__name__", name);
        object::release(name);
        object::bind(globals, "__builtins__", namespace());
        let mut args = Object {
            references: usize::MAX,
            data: Data::Args(Vec::new()),
        };
        let entry = std::mem::transmute::<*mut c_void, Native>(native);
        let result = if crate::sequences::initialize(protocols) {
            entry(globals, &mut args)
        } else {
            std::ptr::null_mut()
        };
        if result.is_null() && !object::has_error() {
            object::error(
                "SystemError",
                "native module returned NULL without an error",
            );
        }
        object::release(result);
        object::release(globals);
        if io::stdout().flush().is_err() {
            object::error("OSError", "output failed");
        }
        let status = if object::has_error() {
            let message = format!("{}: {}\n", object::error_kind(), object::error_message());
            let _ = io::stderr().lock().write_all(message.as_bytes());
            1
        } else {
            0
        };
        BUILTINS.with(|storage| object::release(storage.replace(std::ptr::null_mut())));
        crate::sequences::finish();
        status
    }
}

/// `from module import *`: bind the names in the module's `__all__`, or else
/// its names without a leading underscore, in the importing module's globals.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_import_star(module: Value, globals: Value) -> i32 {
    unsafe {
        if object::has_error() {
            return -1;
        }
        if module.is_null() || object::is_fixnum(module) {
            object::error("SystemError", "module namespace storage is required");
            return -1;
        }
        let Data::Namespace(bindings) = &(*module).data else {
            object::error("SystemError", "module namespace storage is required");
            return -1;
        };
        let public = object::lookup(module, "__all__");
        let selected: Vec<(String, Value)> = if public.is_null() {
            bindings
                .iter()
                .filter(|(name, _)| !name.starts_with('_'))
                .map(|(name, value)| (name.clone(), *value))
                .collect()
        } else {
            let names = if object::is_fixnum(public) {
                None
            } else {
                match &(*public).data {
                    Data::List(values) | Data::Args(values) => Some(values),
                    _ => None,
                }
            };
            let Some(names) = names else {
                object::error("TypeError", "__all__ must be a list or tuple of strings");
                return -1;
            };
            let mut selected = Vec::with_capacity(names.len());
            for name in names {
                let text = if object::is_fixnum(*name) {
                    None
                } else if let Data::Text(text) = &(**name).data {
                    text.to_rust_string()
                } else {
                    None
                };
                let Some(text) = text else {
                    object::error("TypeError", "__all__ must be a list or tuple of strings");
                    return -1;
                };
                let value = object::lookup(module, &text);
                if value.is_null() {
                    object::error(
                        "AttributeError",
                        &format!("module has no attribute '{text}'"),
                    );
                    return -1;
                }
                selected.push((text, value));
            }
            selected
        };
        // Retain the values before binding: rebinding a name in the importing
        // module could release the last other reference to a selected value.
        for (_, value) in &selected {
            object::retain(*value);
        }
        let mut status = 0;
        for (name, value) in &selected {
            if status == 0 && !object::bind(globals, name, *value) {
                status = -1;
            }
            object::release(*value);
        }
        status
    }
}

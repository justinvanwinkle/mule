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
                Data::Text(text, ..) => Some(text.to_wtf8()),
                Data::Bytes(bytes, ..) => Some(bytes.clone()),
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
        let Data::Text(text, ..) = &(*value).data else {
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
                && matches!(&(*value).data, Data::Text(..)),
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
            if let Data::Float(value, ..) = &(*value).data {
                return object::new_value(Data::Float(value.abs(), std::ptr::null_mut(), std::ptr::null_mut()));
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
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_fast_len(value: Value) -> Value {
    unsafe {
        if object::has_error() {
            return std::ptr::null_mut();
        }
        if value.is_null() || object::is_fixnum(value) {
            return fail("TypeError", "object has no len");
        }
        let length = match &(*value).data {
            Data::Bytes(value, ..) => value.len(),
            Data::Text(value, ..) => value.len(),
            Data::Args(value, ..) | Data::List(value, ..) => value.len(),
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
unsafe extern "C" fn len(_: Value, args: Value) -> Value {
    unsafe {
        if arity(args, 1, 1).is_none() {
            return std::ptr::null_mut();
        }
        mule_fast_len(arg(args, 0))
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
                object::new_value(Data::Text(Text::from_rust_str(&line), std::ptr::null_mut(), std::ptr::null_mut()))
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
unsafe fn parse_integer_value(bytes: &[u8], mut base: i32) -> Option<Value> {
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
    let mut previous = false;
    let mut nonzero = false;
    let mut has_digits = false;
    let mut small_acc: Option<i64> = Some(0);
    let base_i64 = i64::from(base);
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
        has_digits = true;
        nonzero |= digit != 0;
        if let Some(acc) = small_acc {
            small_acc = acc
                .checked_mul(base_i64)
                .and_then(|v| v.checked_add(i64::from(digit)));
        }
    }
    if !has_digits || (autodetect && bytes.first() == Some(&b'0') && nonzero) {
        return None;
    }
    if let Some(acc) = small_acc {
        let signed = if negative { -acc } else { acc };
        return Some(unsafe { numeric::integer_from_i64(signed) });
    }
    let mut digits = String::with_capacity(bytes.len());
    for &byte in bytes {
        if byte != b'_' {
            digits.push(char::from(byte));
        }
    }
    let result = BigInt::parse(&digits, base)?;
    let result = if negative { result.neg() } else { result };
    Some(unsafe { numeric::integer_import(&result) })
}
unsafe fn parse_int_bytes_base10(bytes: &[u8]) -> Value {
    let trimmed = bytes.trim_ascii();
    let (negative, digits) = match trimmed.split_first() {
        Some((&b'-', rest)) => (true, rest),
        Some((&b'+', rest)) => (false, rest),
        _ => (false, trimmed),
    };
    if !digits.is_empty() && digits.len() <= 18 {
        let mut acc: i64 = 0;
        let mut all_digits = true;
        for &byte in digits {
            let d = byte.wrapping_sub(b'0');
            if d >= 10 {
                all_digits = false;
                break;
            }
            acc = acc * 10 + i64::from(d);
        }
        if all_digits {
            return object::small(if negative { -acc } else { acc });
        }
    }
    unsafe {
        parse_integer_value(trimmed, 10)
            .unwrap_or_else(|| fail("ValueError", "invalid literal for int"))
    }
}
unsafe fn slice_bound_int(value: Value, default: i64, len: i64) -> Option<usize> {
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
            Data::Bool(flag) => Some(i64::from(*flag).clamp(0, len) as usize),
            Data::BigInt(integer, ..) => {
                if let Some(raw) = integer.to_i64() {
                    let adjusted = if raw < 0 { raw.saturating_add(len) } else { raw };
                    Some(adjusted.clamp(0, len) as usize)
                } else if integer.sign() < 0 {
                    Some(0)
                } else {
                    Some(len as usize)
                }
            }
            _ => None,
        }
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_parse_int_text(value: Value) -> Value {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::Text(text, ..) = &(*value).data {
                if let Some(bytes) = text.as_latin1() {
                    return parse_int_bytes_base10(bytes);
                }
                let bytes = text.to_wtf8();
                return parse_int_bytes_base10(&bytes);
            }
        }
        fail("TypeError", "int requires a number or string")
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_parse_int_slice(
    sequence: Value,
    start: Value,
    stop: Value,
) -> Value {
    unsafe {
        if !sequence.is_null() && !object::is_fixnum(sequence) {
            if let Data::Text(text, ..) = &(*sequence).data {
                let len = text.len() as i64;
                let (Some(s), Some(e)) = (
                    slice_bound_int(start, 0, len),
                    slice_bound_int(stop, len, len),
                ) else {
                    return fail("TypeError", "slice indices must be integers or None");
                };
                if let Some(bytes) = text.as_latin1() {
                    let slice = if s < e { &bytes[s..e] } else { &[] };
                    return parse_int_bytes_base10(slice);
                }
                if s >= e {
                    return parse_int_bytes_base10(&[]);
                }
                return match text.slice(s, e) {
                    Ok(sub) => {
                        let bytes = sub.to_wtf8();
                        parse_int_bytes_base10(&bytes)
                    }
                    Err(_) => fail("MemoryError", "text storage allocation failed"),
                };
            }
        }
        fail("TypeError", "int requires a number or string")
    }
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
        if count == 1 && object::is_fixnum(value) {
            return value;
        }
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
        if !value.is_null() && !object::is_fixnum(value) {
            match &(*value).data {
                Data::Text(text, ..) => {
                    if base == 10 {
                        if let Some(bytes) = text.as_latin1() {
                            return parse_int_bytes_base10(bytes);
                        }
                        let bytes = text.to_wtf8();
                        return parse_int_bytes_base10(&bytes);
                    }
                    let parsed = if let Some(bytes) = text.as_latin1() {
                        parse_integer_value(bytes, base)
                    } else {
                        let bytes = text.to_wtf8();
                        parse_integer_value(&bytes, base)
                    };
                    return parsed.unwrap_or_else(|| fail("ValueError", "invalid literal for int"));
                }
                Data::Bytes(bytes, ..) => {
                    return parse_integer_value(bytes, base)
                        .unwrap_or_else(|| fail("ValueError", "invalid literal for int"));
                }
                _ => (),
            }
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
            if let Data::Float(value, ..) = &(*value).data {
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
            return object::new_value(Data::Float(0.0, std::ptr::null_mut(), std::ptr::null_mut()));
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
                |value| object::new_value(Data::Float(value, std::ptr::null_mut(), std::ptr::null_mut())),
            );
        }
        numeric::number_double(value).map_or(std::ptr::null_mut(), |value| {
            object::new_value(Data::Float(value, std::ptr::null_mut(), std::ptr::null_mut()))
        })
    }
}
unsafe extern "C" fn tuple(_: Value, args: Value) -> Value {
    unsafe {
        if arity(args, 0, 0).is_none() {
            std::ptr::null_mut()
        } else {
            object::new_value(Data::Args(Vec::new(), std::ptr::null_mut(), std::ptr::null_mut()))
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
    crate::thread_identity::initialize_main_identity();
    let _registration = crate::threads::Registration::enter();
    unsafe {
        object::clear_error();
        if native.is_null() {
            return {
                object::error("SystemError", "NULL module entry");
                1
            };
        }
        let globals = object::namespace();
        let name = object::new_value(Data::Text(Text::from_rust_str("__main__"), std::ptr::null_mut(), std::ptr::null_mut()));
        object::bind(globals, "__name__", name);
        object::release(name);
        object::bind(globals, "__builtins__", namespace());
        let mut args = Object {
            references: std::sync::atomic::AtomicUsize::new(usize::MAX),
            tracked: false,
            gc_slot: 0,
            data: Data::Args(Vec::new(), std::ptr::null_mut(), std::ptr::null_mut()),
        };
        let entry = std::mem::transmute::<*mut c_void, Native>(native);
        let initialized = crate::sequences::initialize(protocols);
        if initialized {
            crate::signals::initialize();
        }
        let result = if initialized {
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
        let mut sigint_exit = false;
        let status = if object::has_error() {
            if initialized && !protocols.is_null() {
                // Uncaught primitive errors use the same exception instances
                // and traceback transfer as errors entering a Python handler.
                let exception = crate::sequences::mule_catch_exception();
                if !exception.is_null() {
                    crate::sequences::mule_raise_pending(exception);
                    object::release(exception);
                }
            }
            if object::error_kind() == "SystemExit" {
                exit_status()
            } else {
                let kind = object::error_kind();
                let detail = object::error_message();
                let message = if detail.is_empty() {
                    format!("{kind}\n")
                } else {
                    format!("{kind}: {detail}\n")
                };
                let mut stderr = io::stderr().lock();
                crate::traceback::print_pending(&mut stderr);
                let _ = stderr.write_all(message.as_bytes());
                let _ = stderr.flush();
                if kind == "KeyboardInterrupt" {
                    sigint_exit = true;
                    130
                } else {
                    1
                }
            }
        } else {
            0
        };
        crate::traceback::clear_code_cache();
        BUILTINS.with(|storage| object::release(storage.replace(std::ptr::null_mut())));
        object::release(MODULES.with(|slot| slot.replace(std::ptr::null_mut())));
        let early = EARLY_MODULES.with_borrow_mut(std::mem::take);
        for (_, module) in early {
            object::release(module);
        }
        crate::signals::finish();
        crate::sequences::finish();
        crate::classes::finish_builtin_types();
        if sigint_exit {
            crate::signals::terminate_with_sigint();
        }
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
                .map(|(name, value)| (name.into(), value))
                .collect()
        } else {
            let names = if object::is_fixnum(public) {
                None
            } else {
                match &(*public).data {
                    Data::List(values, ..) | Data::Args(values, ..) => Some(values),
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
                } else if let Data::Text(text, ..) = &(**name).data {
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

/// The `builtins` module: the namespace builtin name lookups consult.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_builtins_module() -> Value {
    unsafe { object::retain(namespace()) }
}

/// The builtins are installed by the compiled protocols; nothing to add.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_builtins_module_init(_: Value) -> Value {
    unsafe { object::none_value() }
}

thread_local! {
    // sys.modules once the sys module registers it, and the modules
    // initialized before that, which it receives on registration.
    static MODULES: Cell<Value> = const { Cell::new(std::ptr::null_mut()) };
    static EARLY_MODULES: std::cell::RefCell<Vec<(String, Value)>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

unsafe fn record_module(modules: Value, name: &str, module: Value) -> bool {
    unsafe {
        let key = object::new_value(Data::Text(Text::from_rust_str(name), std::ptr::null_mut(), std::ptr::null_mut()));
        let result = object::special(modules, "__setitem__", &[key, module]);
        object::release(key);
        object::release(result);
        !result.is_null()
    }
}

/// Record a module in sys.modules before its body runs.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_module_loaded(name: *const std::ffi::c_char, module: Value) {
    unsafe {
        let Some(name) = object::name_from_c(name) else {
            return;
        };
        let modules = MODULES.with(Cell::get);
        if modules.is_null() {
            EARLY_MODULES.with_borrow_mut(|early| early.push((name.into(), object::retain(module))));
            return;
        }
        let saved = object::save_error();
        if !record_module(modules, name, module) {
            object::clear_error();
        }
        object::restore_error(saved);
    }
}

/// Drop a failed initializer's registry reference while preserving its error.
/// Generated initialization calls this before releasing its own namespace.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_module_failed(name: *const std::ffi::c_char) {
    unsafe {
        let Some(name) = object::name_from_c(name) else {
            return;
        };
        let modules = MODULES.with(Cell::get);
        if modules.is_null() {
            let removed = EARLY_MODULES.with_borrow_mut(|early| {
                early.iter().position(|(entry, _)| entry == name)
                    .map(|index| early.remove(index).1)
            });
            if let Some(module) = removed {
                object::release(module);
            }
            return;
        }
        let saved = object::save_error();
        let key = object::new_value(Data::Text(Text::from_rust_str(name), std::ptr::null_mut(), std::ptr::null_mut()));
        let removed = object::special(modules, "pop", &[key, object::none_value()]);
        object::release(key);
        object::release(removed);
        object::clear_error();
        object::restore_error(saved);
    }
}

/// `register_modules(modules)`: the sys module's sys.modules dict.
pub(crate) unsafe extern "C" fn register_modules(_: Value, args: Value) -> Value {
    unsafe {
        let Some(modules) = object::argument(args, 0) else {
            return std::ptr::null_mut();
        };
        object::release(MODULES.with(|slot| slot.replace(object::retain(modules))));
        let early = EARLY_MODULES.with_borrow_mut(std::mem::take);
        for (name, module) in early {
            let recorded = record_module(modules, &name, module);
            object::release(module);
            if !recorded {
                return std::ptr::null_mut();
            }
        }
        object::none_value()
    }
}

/// Importing a module with no compiled source returns the module the program
/// installed under that name in sys.modules, or raises ModuleNotFoundError.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_import_missing(name: *const std::ffi::c_char) -> Value {
    unsafe {
        let Some(name) = object::name_from_c(name) else {
            return std::ptr::null_mut();
        };
        let modules = MODULES.with(Cell::get);
        if modules.is_null() {
            let found = EARLY_MODULES.with_borrow(|early| {
                early.iter().find(|(entry, _)| entry == name)
                    .map(|(_, module)| object::retain(*module))
            });
            if let Some(module) = found {
                return module;
            }
        } else {
            let key = object::new_value(Data::Text(Text::from_rust_str(name), std::ptr::null_mut(), std::ptr::null_mut()));
            let found = object::special(modules, "get", &[key, object::none_value()]);
            object::release(key);
            if found.is_null() {
                return found;
            }
            if found != object::none_value() {
                return found;
            }
            object::release(found);
        }
        object::error("ModuleNotFoundError", &format!("No module named '{name}'"));
        std::ptr::null_mut()
    }
}

/// The process status for an uncaught SystemExit, as CPython's
/// `handle_system_exit`: None is success, an integer is the status, and any
/// other code is printed to stderr with status 1.
unsafe fn exit_status() -> i32 {
    unsafe {
        let exception = crate::sequences::take_pending();
        object::clear_error();
        let result = object::special(exception, "__mule_process_status__", &[]);
        object::release(exception);
        if result.is_null() {
            object::clear_error();
            return 1;
        }
        let Some(status) = object::argument(result, 0).filter(|value| object::is_fixnum(*value)) else {
            object::release(result);
            object::clear_error();
            return 1;
        };
        let status = object::fixnum(status) as i32;
        if let Some(message) = object::argument(result, 1) {
            if message != object::none_value() {
                if let Some(text) = object::formatted_value(message, false, false) {
                    let mut text = text;
                    text.push(b'\n');
                    let _ = io::stderr().lock().write_all(&text);
                }
            }
        }
        object::release(result);
        status
    }
}

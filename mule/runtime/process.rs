//! Trusted process-lifetime foreign storage; compiled Python owns decoding policy.

use crate::numeric;
use crate::text::Text;
use crate::object::{self, Data, Native, Value};
use std::cell::Cell;
use std::ffi::{CStr, c_char};
use std::ptr::null_mut;

#[derive(Clone, Copy)]
struct Arguments {
    count: usize,
    values: *const *const c_char,
}

// Compiled Python currently runs on the native main thread only.
/// Unit finalizers in registration order. A module unit registers its
/// finalizer when its import first runs; `mule_run_finalizers` calls them in
/// reverse before the main unit releases its own state.
static FINALIZERS: std::sync::Mutex<Vec<unsafe extern "C" fn()>> =
    std::sync::Mutex::new(Vec::new());

#[unsafe(no_mangle)]
pub extern "C" fn mule_register_finalizer(finalizer: unsafe extern "C" fn()) {
    FINALIZERS.lock().unwrap().push(finalizer);
}

#[unsafe(no_mangle)]
pub extern "C" fn mule_run_finalizers() {
    let finalizers = std::mem::take(&mut *FINALIZERS.lock().unwrap());
    for finalizer in finalizers.into_iter().rev() {
        unsafe { finalizer() };
    }
}

thread_local! {
    static ARGUMENTS: Cell<Option<Arguments>> = const { Cell::new(None) };
}

fn fail(kind: &str, message: &str) -> Value {
    object::error(kind, message);
    null_mut()
}

/// Publish native main's arguments before compiled language initialization.
///
/// # Safety
/// For count > 0, values addresses count + 1 valid pointer slots with a final
/// NULL sentinel. Non-NULL entries address NUL-terminated byte buffers. All
/// storage must remain valid and unchanged for the process lifetime. This and
/// module initialization run on the native main thread. Guards cannot validate
/// arbitrary addresses; only native startup may publish this trusted storage.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_set_process_arguments(count: i32, values: *const *const c_char) {
    ARGUMENTS.set(None);
    if object::has_error() {
        return;
    }
    if count < 0 || (count > 0 && values.is_null()) {
        object::error("SystemError", "invalid native process argument vector");
        return;
    }
    if count > 0 && !unsafe { *values.add(count as usize) }.is_null() {
        object::error(
            "SystemError",
            "native process argument vector lacks NULL sentinel",
        );
        return;
    }
    ARGUMENTS.set(Some(Arguments {
        count: count as usize,
        values,
    }));
}

unsafe fn operands<const N: usize>(args: Value) -> Option<[Value; N]> {
    unsafe {
        if object::has_error() {
            return None;
        }
        let count = object::argument_count(args)?;
        if crate::arguments::keyword_count(args) != 0 || count != N {
            object::error(
                "TypeError",
                &format!("expected {N} positional-only arguments"),
            );
            return None;
        }
        let mut values = [null_mut(); N];
        for (index, value) in values.iter_mut().enumerate() {
            *value = object::argument(args, index)?;
        }
        Some(values)
    }
}

unsafe fn storage_index(index: Value, length: usize) -> Option<usize> {
    unsafe {
        let index = if object::is_fixnum(index) {
            usize::try_from(object::fixnum(index)).ok()
        } else {
            numeric::integer_export(index)?
                .to_u64()
                .and_then(|value| usize::try_from(value).ok())
        };
        let Some(index) = index.filter(|index| *index < length) else {
            object::error("IndexError", "foreign storage index out of range");
            return None;
        };
        Some(index)
    }
}

unsafe extern "C" fn read_pointer(_: Value, args: Value) -> Value {
    unsafe {
        let Some([vector, index]) = operands(args) else {
            return null_mut();
        };
        if vector.is_null() || object::is_fixnum(vector) {
            return fail(
                "TypeError",
                "read_pointer requires a foreign pointer vector",
            );
        }
        let Data::ForeignPointerVector { base, length } = &(*vector).data else {
            return fail(
                "TypeError",
                "read_pointer requires a foreign pointer vector",
            );
        };
        let Some(index) = storage_index(index, *length) else {
            return null_mut();
        };
        let address = *base.add(index);
        if address.is_null() {
            object::none_value()
        } else {
            object::new_value(Data::ForeignCString { address })
        }
    }
}

unsafe extern "C" fn read_c_string(_: Value, args: Value) -> Value {
    unsafe {
        let Some([pointer]) = operands(args) else {
            return null_mut();
        };
        if pointer.is_null() || object::is_fixnum(pointer) {
            return fail(
                "TypeError",
                "read_c_string requires a foreign C string pointer",
            );
        }
        let Data::ForeignCString { address } = &(*pointer).data else {
            return fail(
                "TypeError",
                "read_c_string requires a foreign C string pointer",
            );
        };
        object::new_value(Data::Bytes(CStr::from_ptr(*address).to_bytes().to_vec()))
    }
}

unsafe fn bytes_storage<'a>(value: Value) -> Option<&'a [u8]> {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::Bytes(bytes) = &(*value).data {
                return Some(bytes);
            }
        }
        object::error("TypeError", "byte storage requires bytes");
        None
    }
}

unsafe extern "C" fn byte_length(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return null_mut();
        };
        let Some(bytes) = bytes_storage(value) else {
            return null_mut();
        };
        numeric::integer_from_usize(bytes.len())
    }
}

unsafe extern "C" fn read_byte(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value, index]) = operands(args) else {
            return null_mut();
        };
        let Some(bytes) = bytes_storage(value) else {
            return null_mut();
        };
        let Some(index) = storage_index(index, bytes.len()) else {
            return null_mut();
        };
        object::small(i64::from(bytes[index]))
    }
}

unsafe extern "C" fn text_from_codepoints(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return null_mut();
        };
        if value.is_null() || object::is_fixnum(value) {
            return fail(
                "TypeError",
                "text_from_codepoints requires list or tuple storage",
            );
        }
        let (Data::List(values) | Data::Args(values)) = &(*value).data else {
            return fail(
                "TypeError",
                "text_from_codepoints requires list or tuple storage",
            );
        };
        let mut codes = Vec::new();
        if codes.try_reserve_exact(values.len()).is_err() {
            return fail("MemoryError", "text storage allocation failed");
        }
        for value in values {
            let code = if object::is_fixnum(*value) {
                u64::try_from(object::fixnum(*value)).ok()
            } else {
                let Some(integer) = numeric::integer_export(*value) else {
                    return null_mut();
                };
                integer.to_u64()
            };
            let Some(code) = code.filter(|code| *code <= 0x10ffff) else {
                return fail("ValueError", "text codepoint must be in range(0x110000)");
            };
            codes.push(code as u32);
        }
        match Text::from_codepoints(codes.into_iter()) {
            Ok(text) => object::new_value(Data::Text(text)),
            Err(_) => fail("MemoryError", "text storage allocation failed"),
        }
    }
}

unsafe extern "C" fn bytes_from_values(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return null_mut();
        };
        let values = if !value.is_null() && !object::is_fixnum(value) {
            match &(*value).data {
                Data::List(values) | Data::Args(values) => Some(values),
                _ => None,
            }
        } else {
            None
        };
        let Some(values) = values else {
            return fail("TypeError", "bytes_from_values requires list or tuple storage");
        };
        let mut bytes = Vec::new();
        if bytes.try_reserve_exact(values.len()).is_err() {
            return fail("MemoryError", "byte storage allocation failed");
        }
        for value in values {
            let byte = if object::is_fixnum(*value) {
                u8::try_from(object::fixnum(*value)).ok()
            } else {
                None
            };
            let Some(byte) = byte else {
                return fail("ValueError", "byte must be in range(0, 256)");
            };
            bytes.push(byte);
        }
        object::new_value(Data::Bytes(bytes))
    }
}

unsafe extern "C" {
    fn isatty(fd: i32) -> i32;
    fn write(fd: i32, buffer: *const u8, count: usize) -> isize;
}

/// `write_fd(fd, data)`: write bytes to a descriptor, returning the count
/// written. Standard output and error share the buffers `print` uses.
unsafe extern "C" fn write_fd(_: Value, args: Value) -> Value {
    unsafe {
        use std::io::Write;
        let Some([fd, data]) = operands(args) else {
            return null_mut();
        };
        if !object::is_fixnum(fd) {
            object::error("TypeError", "a file descriptor must be an integer");
            return null_mut();
        }
        let Data::Bytes(bytes) = &(*data).data else {
            object::error("TypeError", "a bytes-like object is required");
            return null_mut();
        };
        let written = match object::fixnum(fd) {
            1 => std::io::stdout().lock().write_all(bytes).map(|_| bytes.len()),
            2 => std::io::stderr().lock().write_all(bytes).map(|_| bytes.len()),
            descriptor => {
                let count = write(descriptor as i32, bytes.as_ptr(), bytes.len());
                if count < 0 {
                    Err(std::io::Error::last_os_error())
                } else {
                    Ok(count as usize)
                }
            }
        };
        match written {
            Ok(count) => numeric::integer_from_usize(count),
            Err(failure) => {
                object::error("OSError", &failure.to_string());
                null_mut()
            }
        }
    }
}

unsafe extern "C" fn flush_fd(_: Value, args: Value) -> Value {
    unsafe {
        use std::io::Write;
        let Some([fd]) = operands(args) else {
            return null_mut();
        };
        let flushed = match object::is_fixnum(fd).then(|| object::fixnum(fd)) {
            Some(1) => std::io::stdout().lock().flush(),
            Some(2) => std::io::stderr().lock().flush(),
            _ => Ok(()),
        };
        if let Err(failure) = flushed {
            object::error("OSError", &failure.to_string());
            return null_mut();
        }
        object::none_value()
    }
}

/// The exception being handled, or None (`sys.exception()`).
unsafe extern "C" fn active_exception(_: Value, args: Value) -> Value {
    unsafe {
        let Some([]) = operands::<0>(args) else {
            return null_mut();
        };
        crate::sequences::mule_active_exception()
    }
}

unsafe extern "C" fn isatty_fd(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd]) = operands(args) else {
            return null_mut();
        };
        if !object::is_fixnum(fd) {
            object::error("TypeError", "a file descriptor must be an integer");
            return null_mut();
        }
        object::bool_value(isatty(object::fixnum(fd) as i32) == 1)
    }
}

/// Populate an ordinary compiler-owned module namespace; no separate cache.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_native_module_init(globals: Value) -> Value {
    unsafe {
        if object::has_error() {
            return null_mut();
        }
        let Some(arguments) = ARGUMENTS.get() else {
            return fail(
                "SystemError",
                "native process arguments are not initialized",
            );
        };
        let argc = numeric::integer_from_usize(arguments.count);
        let argv = object::new_value(Data::ForeignPointerVector {
            base: arguments.values,
            length: if arguments.count == 0 {
                0
            } else {
                arguments.count + 1
            },
        });
        let bound = object::bind(globals, "argc", argc) && object::bind(globals, "argv", argv);
        object::release(argc);
        object::release(argv);
        if !bound {
            return null_mut();
        }
        let exports: [(&str, Native); 12] = [
            ("read_pointer", read_pointer),
            ("read_c_string", read_c_string),
            ("byte_length", byte_length),
            ("read_byte", read_byte),
            ("text_from_codepoints", text_from_codepoints),
            ("bytes_from_values", bytes_from_values),
            ("call_with", crate::arguments::call_with),
            ("write_fd", write_fd),
            ("flush_fd", flush_fd),
            ("isatty", isatty_fd),
            ("register_modules", crate::builtins::register_modules),
            ("active_exception", active_exception),
        ];
        for (name, entry) in exports {
            let function = object::function(null_mut(), entry, name);
            let bound = object::bind(globals, name, function);
            object::release(function);
            if !bound {
                return null_mut();
            }
        }
        if !crate::math::populate(globals)
            || !crate::classes::populate(globals)
            || !crate::hashing::populate(globals)
            || !crate::weakref::populate(globals)
            || !crate::coroutine::populate(globals)
        {
            return null_mut();
        }
        object::none_value()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    unsafe fn invoke(module: Value, name: &str, args: &[Value]) -> Value {
        unsafe { object::call(object::lookup(module, name), args) }
    }

    #[test]
    fn foreign_vector_preserves_bytes_empty_strings_and_null_sentinel() {
        unsafe {
            object::clear_error();
            // Leaked storage models native startup's process-lifetime ownership.
            let vector = Box::leak(Box::new([
                c"".as_ptr(),
                b"\xff\xed\xa0\x80\0".as_ptr().cast(),
                std::ptr::null(),
            ]));
            mule_set_process_arguments(2, vector.as_ptr());
            let module = object::namespace();
            assert_eq!(mule_native_module_init(module), object::none_value());
            assert_eq!(object::fixnum(object::lookup(module, "argc")), 2);
            let vector = object::lookup(module, "argv");
            let empty = invoke(module, "read_pointer", &[vector, object::small(0)]);
            assert!(matches!(&(*empty).data, Data::ForeignCString { .. }));
            let bytes = invoke(module, "read_c_string", &[empty]);
            assert!(matches!(&(*bytes).data, Data::Bytes(bytes) if bytes.is_empty()));
            assert_eq!(object::fixnum(invoke(module, "byte_length", &[bytes])), 0);
            object::release(bytes);
            object::release(empty);
            let pointer = invoke(module, "read_pointer", &[vector, object::small(1)]);
            let bytes = invoke(module, "read_c_string", &[pointer]);
            assert!(matches!(&(*bytes).data, Data::Bytes(bytes) if bytes == b"\xff\xed\xa0\x80"));
            assert_eq!(
                object::fixnum(invoke(module, "read_byte", &[bytes, object::small(0)])),
                255
            );
            assert_eq!(
                invoke(module, "read_pointer", &[vector, object::small(2)]),
                object::none_value()
            );
            for index in [-1, 3] {
                assert!(invoke(module, "read_pointer", &[vector, object::small(index)]).is_null());
                assert_eq!(object::error_kind(), "IndexError");
                object::clear_error();
            }
            for index in [-1, 4] {
                assert!(invoke(module, "read_byte", &[bytes, object::small(index)]).is_null());
                assert_eq!(object::error_kind(), "IndexError");
                object::clear_error();
            }
            object::release(bytes);
            object::release(pointer);
            object::release(module);
            ARGUMENTS.set(None);
        }
    }

    #[test]
    fn native_exports_guard_types_arity_and_remain_mutable() {
        unsafe {
            object::clear_error();
            mule_set_process_arguments(0, std::ptr::null());
            let module = object::namespace();
            assert_eq!(mule_native_module_init(module), object::none_value());
            let vector = object::lookup(module, "argv");
            assert!(invoke(module, "read_pointer", &[vector, object::small(0)]).is_null());
            assert_eq!(object::error_kind(), "IndexError");
            object::clear_error();
            let text = object::new_value(Data::Text(Text::empty()));
            for (name, args) in [
                ("byte_length", vec![text]),
                ("read_byte", vec![text, object::small(0)]),
                ("read_pointer", vec![object::small(0), object::small(0)]),
                ("read_c_string", vec![object::small(0)]),
                ("read_c_string", vec![object::none_value()]),
                ("byte_length", vec![]),
                ("text_from_codepoints", vec![text]),
                ("read_pointer", vec![vector, text]),
            ] {
                assert!(invoke(module, name, &args).is_null());
                assert_eq!(object::error_kind(), "TypeError", "{name}");
                object::clear_error();
            }
            assert!(object::bind(module, "byte_length", object::small(42)));
            assert_eq!(object::fixnum(object::lookup(module, "byte_length")), 42);
            object::release(text);
            object::release(module);
            ARGUMENTS.set(None);
        }
    }

    #[test]
    fn text_storage_encodes_list_and_tuple_codepoints_including_surrogates() {
        unsafe {
            object::clear_error();
            mule_set_process_arguments(0, std::ptr::null());
            let module = object::namespace();
            mule_native_module_init(module);
            let codes = [
                0, 0x7f, 0x80, 0x7ff, 0x800, 0xd800, 0xdfff, 0xffff, 0x10000, 0x10ffff,
            ];
            for tuple in [false, true] {
                let values = codes.map(object::small).to_vec();
                let sequence = object::new_value(if tuple {
                    Data::Args(values)
                } else {
                    Data::List(values)
                });
                let result = invoke(module, "text_from_codepoints", &[sequence]);
                let Data::Text(text) = &(*result).data else {
                    panic!("expected text")
                };
                assert_eq!(text.kind(), 4);
                assert_eq!(
                    text.codepoints().collect::<Vec<_>>(),
                    codes.map(|code| code as u32)
                );
                object::release(result);
                object::release(sequence);
            }
            for code in [-1, 0x110000] {
                let sequence = object::new_value(Data::List(vec![object::small(code)]));
                assert!(invoke(module, "text_from_codepoints", &[sequence]).is_null());
                assert_eq!(object::error_kind(), "ValueError");
                object::clear_error();
                object::release(sequence);
            }
            let sequence =
                object::new_value(Data::List(vec![object::new_value(Data::Text(Text::empty()))]));
            assert!(invoke(module, "text_from_codepoints", &[sequence]).is_null());
            assert_eq!(object::error_kind(), "TypeError");
            object::clear_error();
            object::release(sequence);
            object::release(module);
            ARGUMENTS.set(None);
        }
    }

    #[test]
    fn initialization_requires_published_valid_vector_and_preserves_sticky_errors() {
        unsafe {
            object::clear_error();
            ARGUMENTS.set(None);
            let module = object::namespace();
            assert!(mule_native_module_init(module).is_null());
            assert_eq!(object::error_kind(), "SystemError");
            object::clear_error();
            mule_set_process_arguments(1, std::ptr::null());
            assert_eq!(object::error_kind(), "SystemError");
            object::clear_error();
            let vector = [c"x".as_ptr(), c"not-null".as_ptr()];
            mule_set_process_arguments(1, vector.as_ptr());
            assert_eq!(object::error_kind(), "SystemError");
            object::clear_error();
            mule_set_process_arguments(0, std::ptr::null());
            object::error("ValueError", "existing failure");
            assert!(mule_native_module_init(module).is_null());
            assert_eq!(object::error_kind(), "ValueError");
            object::clear_error();
            object::release(module);
            ARGUMENTS.set(None);
        }
    }
}

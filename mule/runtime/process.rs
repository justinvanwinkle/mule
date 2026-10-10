//! Trusted process-lifetime foreign storage; compiled Python owns decoding policy.

use crate::numeric;
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
        let mut bytes = Vec::new();
        let Some(capacity) = values.len().checked_mul(4) else {
            return fail("MemoryError", "text storage allocation is too large");
        };
        if bytes.try_reserve_exact(capacity).is_err() {
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
            object::append_text_codepoint(&mut bytes, code as u32);
        }
        object::new_value(Data::Text(bytes))
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
        let exports: [(&str, Native); 5] = [
            ("read_pointer", read_pointer),
            ("read_c_string", read_c_string),
            ("byte_length", byte_length),
            ("read_byte", read_byte),
            ("text_from_codepoints", text_from_codepoints),
        ];
        for (name, entry) in exports {
            let function = object::function(null_mut(), entry, name);
            let bound = object::bind(globals, name, function);
            object::release(function);
            if !bound {
                return null_mut();
            }
        }
        if !crate::math::populate(globals) {
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
            let text = object::new_value(Data::Text(Vec::new()));
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
                let Data::Text(bytes) = &(*result).data else {
                    panic!("expected text")
                };
                assert_eq!(
                    bytes,
                    b"\x00\x7f\xc2\x80\xdf\xbf\xe0\xa0\x80\xed\xa0\x80\xed\xbf\xbf\xef\xbf\xbf\xf0\x90\x80\x80\xf4\x8f\xbf\xbf"
                );
                assert_eq!(
                    object::decode_text(bytes).unwrap(),
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
                object::new_value(Data::List(vec![object::new_value(Data::Text(Vec::new()))]));
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

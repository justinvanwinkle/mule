//! Physical numeric storage and binary64 operations for compiled math protocols.

use crate::numeric;
use crate::object::{self, Data, Native, Value};
use std::ffi::CString;
use std::ptr::null_mut;

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

unsafe extern "C" fn number_kind(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return null_mut();
        };
        let kind = if numeric::is_integer(value) {
            1
        } else if !value.is_null()
            && !object::is_fixnum(value)
            && matches!(&(*value).data, Data::Float(_))
        {
            2
        } else {
            0
        };
        object::small(kind)
    }
}

unsafe extern "C" fn to_double(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return null_mut();
        };
        let Some(value) = numeric::number_double(value) else {
            return null_mut();
        };
        object::new_value(Data::Float(value))
    }
}

unsafe extern "C" fn integer_value(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return null_mut();
        };
        crate::sequences::mule_integer_storage(value)
    }
}

unsafe extern "C" fn integer_bit_length(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return null_mut();
        };
        let bits = if object::is_fixnum(value) {
            let magnitude = object::fixnum(value).unsigned_abs();
            (u64::BITS - magnitude.leading_zeros()) as usize
        } else if !value.is_null() {
            match &(*value).data {
                Data::BigInt(value) => {
                    if value.sign() == 0 {
                        0
                    } else {
                        value.bits()
                    }
                }
                Data::Bool(value) => usize::from(*value),
                _ => {
                    object::error("TypeError", "integer storage is required");
                    return null_mut();
                }
            }
        } else {
            object::error("TypeError", "integer storage is required");
            return null_mut();
        };
        numeric::integer_from_usize(bits)
    }
}

unsafe extern "C" fn special_method(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value, name]) = operands(args) else {
            return null_mut();
        };
        if name.is_null() || object::is_fixnum(name) {
            object::error("TypeError", "method name storage requires text");
            return null_mut();
        }
        let Data::Text(bytes) = &(*name).data else {
            object::error("TypeError", "method name storage requires text");
            return null_mut();
        };
        let Ok(name) = CString::new(bytes.to_wtf8()) else {
            object::error("ValueError", "method name contains a NUL character");
            return null_mut();
        };
        object::mule_lookup_special(value, name.as_ptr())
    }
}

unsafe extern "C" fn sqrt(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return null_mut();
        };
        if value.is_null() || object::is_fixnum(value) {
            object::error("TypeError", "binary64 storage is required");
            return null_mut();
        }
        let Data::Float(value) = &(*value).data else {
            object::error("TypeError", "binary64 storage is required");
            return null_mut();
        };
        object::new_value(Data::Float(value.sqrt()))
    }
}

/// Bind physical operations into the existing compiler-owned native namespace.
pub unsafe fn populate(globals: Value) -> bool {
    unsafe {
        if object::has_error() {
            return false;
        }
        let exports: [(&str, Native); 6] = [
            ("math_number_kind", number_kind),
            ("math_to_double", to_double),
            ("math_integer_value", integer_value),
            ("math_integer_bit_length", integer_bit_length),
            ("math_special_method", special_method),
            ("math_sqrt", sqrt),
        ];
        for (name, entry) in exports {
            let function = object::function(null_mut(), entry, name);
            let bound = object::bind(globals, name, function);
            object::release(function);
            if !bound {
                return false;
            }
        }
        for (name, value) in [
            ("math_inf", f64::INFINITY),
            ("math_nan", f64::from_bits(0x7ff8_0000_0000_0000)),
        ] {
            let value = object::new_value(Data::Float(value));
            let bound = object::bind(globals, name, value);
            object::release(value);
            if !bound {
                return false;
            }
        }
        true
    }
}

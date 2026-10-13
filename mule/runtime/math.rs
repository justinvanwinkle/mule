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
            && matches!(&(*value).data, Data::Float(..))
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
        object::new_value(Data::Float(value, std::ptr::null_mut()))
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
                Data::BigInt(value, ..) => {
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
        let Data::Text(bytes, ..) = &(*name).data else {
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
        let Data::Float(value, ..) = &(*value).data else {
            object::error("TypeError", "binary64 storage is required");
            return null_mut();
        };
        object::new_value(Data::Float(value.sqrt(), std::ptr::null_mut()))
    }
}

/// Format a binary64 value for one percent/format conversion kind
/// (0=f, 1=e, 2=E, 3=g, 4=G, 5=percent, 6=shortest). Compiled Python owns
/// signs, width, grouping, and alignment.
unsafe extern "C" fn float_format(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value, kind, precision, alternate]) = operands(args) else {
            return null_mut();
        };
        let (Some(kind), Some(precision)) = (
            object::is_fixnum(kind).then(|| object::fixnum(kind)),
            object::is_fixnum(precision).then(|| object::fixnum(precision)),
        ) else {
            object::error("TypeError", "float format arguments must be integers");
            return null_mut();
        };
        let alternate = alternate == object::bool_value(true);
        let number = if !value.is_null() && !object::is_fixnum(value) {
            match &(*value).data {
                Data::Float(number, ..) => *number,
                _ => {
                    object::error("TypeError", "binary64 storage is required");
                    return null_mut();
                }
            }
        } else {
            object::error("TypeError", "binary64 storage is required");
            return null_mut();
        };
        if number.is_nan() {
            let text = if kind == 2 || kind == 4 { "NAN" } else { "nan" };
            return object::new_value(Data::Text(crate::text::Text::from_rust_str(text), std::ptr::null_mut()));
        }
        if number.is_infinite() {
            let text = match (number < 0.0, kind == 2 || kind == 4) {
                (true, true) => "-INF",
                (true, false) => "-inf",
                (false, true) => "INF",
                (false, false) => "inf",
            };
            return object::new_value(Data::Text(crate::text::Text::from_rust_str(text), std::ptr::null_mut()));
        }
        let precision = usize::try_from(precision).unwrap_or(6);
        let exponent_form = |number: f64, precision: usize, upper: bool| -> String {
            let formatted = format!("{:.*e}", precision, number.abs());
            let (mantissa, exponent) = formatted.split_once('e').expect("exponent");
            let exponent: i32 = exponent.parse().expect("exponent value");
            let letter = if upper { "E" } else { "e" };
            format!("{mantissa}{letter}{exponent:+03}")
        };
        let fixed = |number: f64, precision: usize| -> String { format!("{:.*}", precision, number.abs()) };
        let body = match kind {
            0 => fixed(number, precision),
            1 => exponent_form(number, precision, false),
            2 => exponent_form(number, precision, true),
            3 | 4 => {
                let upper = kind == 4;
                let total = if precision == 0 { 1 } else { precision };
                let exponent = if number == 0.0 {
                    0
                } else {
                    format!("{:.*e}", total - 1, number.abs())
                        .split_once('e')
                        .expect("exponent")
                        .1
                        .parse::<i32>()
                        .expect("exponent value")
                };
                if exponent < -4 || exponent >= total as i32 {
                    let mut text = exponent_form(number, total - 1, upper);
                    if !alternate {
                        if let Some((mantissa, rest)) = text.split_once(['e', 'E']) {
                            let letter = if upper { "E" } else { "e" };
                            let mantissa = mantissa.trim_end_matches('0').trim_end_matches('.');
                            text = format!("{mantissa}{letter}{rest}");
                        }
                    }
                    text
                } else {
                    let decimals = (total as i32 - 1 - exponent) as usize;
                    let mut text = fixed(number, decimals);
                    if !alternate && text.contains('.') {
                        text = text.trim_end_matches('0').trim_end_matches('.').to_string();
                    }
                    text
                }
            }
            5 => format!("{}%", fixed(number * 100.0, precision)),
            _ => {
                let mut text = format!("{}", number.abs());
                if alternate && !text.contains('.') {
                    text.push_str(".0");
                }
                text
            }
        };
        let body = if (kind == 0 || kind == 5 || kind == 6) && alternate && !body.contains(['.', 'e', 'E']) {
            let mut body = body;
            if kind == 5 {
                body.insert(body.len() - 1, '.');
            } else {
                body.push('.');
            }
            body
        } else {
            body
        };
        let signed = if number.is_sign_negative() {
            format!("-{body}")
        } else {
            body
        };
        object::new_value(Data::Text(crate::text::Text::from_rust_str(&signed), std::ptr::null_mut()))
    }
}

/// Bind physical operations into the existing compiler-owned native namespace.
pub unsafe fn populate(globals: Value) -> bool {
    unsafe {
        if object::has_error() {
            return false;
        }
        let exports: [(&str, Native); 7] = [
            ("math_number_kind", number_kind),
            ("math_to_double", to_double),
            ("math_integer_value", integer_value),
            ("math_integer_bit_length", integer_bit_length),
            ("math_special_method", special_method),
            ("math_sqrt", sqrt),
            ("math_float_format", float_format),
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
            let value = object::new_value(Data::Float(value, std::ptr::null_mut()));
            let bound = object::bind(globals, name, value);
            object::release(value);
            if !bound {
                return false;
            }
        }
        true
    }
}

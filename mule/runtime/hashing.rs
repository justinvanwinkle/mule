//! Physical hash inputs for compiled `hash`: scalar storage and identity.
//!
//! Integer and tuple hashing are compiled Python. Binary64 hashing follows
//! CPython's `_Py_HashDouble` exactly, because equal numbers must hash alike
//! across `int` and `float`. Text and bytes use FNV-1a over their encoded
//! bytes; CPython randomizes these per process, so only consistency with
//! equality is part of the contract.
use crate::object::{self, Data, Value};
use std::ptr::null_mut;

const MODULUS_BITS: i32 = 61;
const MODULUS: u64 = (1 << MODULUS_BITS) - 1;
const INFINITY_HASH: i64 = 314_159;
// CPython 3.12 and later hash None to this constant.
const NONE_HASH: i64 = 0xFCA8_6420;

fn fnv(bytes: &[u8]) -> i64 {
    if bytes.is_empty() {
        return 0;
    }
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    let hash = hash as i64;
    if hash == -1 { -2 } else { hash }
}

/// CPython's `_Py_HashDouble` for non-NaN values.
fn double_hash(value: f64) -> i64 {
    if value.is_infinite() {
        return if value > 0.0 { INFINITY_HASH } else { -INFINITY_HASH };
    }
    let (mut mantissa, mut exponent) = frexp(value);
    let sign: i64 = if mantissa < 0.0 {
        mantissa = -mantissa;
        -1
    } else {
        1
    };
    let mut x: u64 = 0;
    while mantissa != 0.0 {
        x = ((x << 28) & MODULUS) | (x >> (MODULUS_BITS - 28));
        mantissa *= 268_435_456.0;
        exponent -= 28;
        let y = mantissa as u64;
        mantissa -= y as f64;
        x += y;
        if x >= MODULUS {
            x -= MODULUS;
        }
    }
    let exponent = if exponent >= 0 {
        exponent % MODULUS_BITS
    } else {
        MODULUS_BITS - 1 - ((-1 - exponent) % MODULUS_BITS)
    };
    x = ((x << exponent) & MODULUS) | (x >> (MODULUS_BITS - exponent));
    let result = x as i64 * sign;
    if result == -1 { -2 } else { result }
}

/// C `frexp`: a mantissa in [0.5, 1) and a binary exponent.
fn frexp(value: f64) -> (f64, i32) {
    if value == 0.0 || !value.is_finite() {
        return (value, 0);
    }
    let bits = value.to_bits();
    let exponent = ((bits >> 52) & 0x7ff) as i32;
    if exponent == 0 {
        // Subnormal: scale into the normal range first.
        let (mantissa, exponent) = frexp(value * f64::from_bits(0x4350_0000_0000_0000));
        return (mantissa, exponent - 54);
    }
    let mantissa = f64::from_bits((bits & !(0x7ff << 52)) | (1022 << 52));
    (mantissa, exponent - 1022)
}

unsafe fn operand(args: Value) -> Option<Value> {
    unsafe {
        if object::has_error() {
            return None;
        }
        if object::argument_count(args)? != 1 || crate::arguments::keyword_count(args) != 0 {
            object::error("TypeError", "expected 1 positional argument");
            return None;
        }
        object::argument(args, 0)
    }
}

/// The hash of text, bytes, None or binary64 storage, or None for other values.
unsafe extern "C" fn storage_hash(_: Value, args: Value) -> Value {
    unsafe {
        let Some(value) = operand(args) else {
            return null_mut();
        };
        if value.is_null() || object::is_fixnum(value) {
            return object::none_value();
        }
        let hash = match &(*value).data {
            Data::None => NONE_HASH,
            Data::Text(text) => fnv(&text.to_wtf8()),
            Data::Bytes(bytes) => fnv(bytes),
            Data::Float(number) if number.is_nan() => identity(value),
            Data::Float(number) => double_hash(*number),
            _ => return object::none_value(),
        };
        integer(hash)
    }
}

unsafe fn integer(value: i64) -> Value {
    unsafe { crate::numeric::integer_import_owned(crate::gmp::BigInt::from_i64(value)) }
}

fn identity(value: Value) -> i64 {
    // CPython's default object hash: the address rotated by four bits.
    (value as usize).rotate_right(4) as i64
}

unsafe extern "C" fn identity_hash(_: Value, args: Value) -> Value {
    unsafe {
        let Some(value) = operand(args) else {
            return null_mut();
        };
        let hash = identity(value);
        integer(if hash == -1 { -2 } else { hash })
    }
}

/// `id(value)`: the value's address, which is unique while it is alive.
unsafe extern "C" fn identity_export(_: Value, args: Value) -> Value {
    unsafe {
        let Some(value) = operand(args) else {
            return null_mut();
        };
        crate::numeric::integer_from_usize(value as usize)
    }
}

pub unsafe fn populate(globals: Value) -> bool {
    unsafe {
        if object::has_error() {
            return false;
        }
        let exports: [(&str, object::Native); 3] = [
            ("storage_hash", storage_hash),
            ("identity_hash", identity_hash),
            ("identity", identity_export),
        ];
        for (name, entry) in exports {
            let function = object::function(null_mut(), entry, name);
            let bound = object::bind(globals, name, function);
            object::release(function);
            if !bound {
                return false;
            }
        }
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integral_doubles_hash_like_integers() {
        assert_eq!(double_hash(1.0), 1);
        assert_eq!(double_hash(-1.0), -2);
        assert_eq!(double_hash(2f64.powi(61)), 1);
        assert_eq!(double_hash(0.5), 1 << 60);
        assert_eq!(double_hash(1.5), (1 << 60) + 1);
    }
}

//! Physical hash inputs for compiled `hash`: scalar storage and identity.
//!
//! Integer and tuple hashing are compiled Python. Binary64 hashing follows
//! CPython's `_Py_HashDouble` exactly, because equal numbers must hash alike
//! across `int` and `float`. Text and bytes use FNV-1a over their encoded
//! bytes; CPython randomizes these per process, so only consistency with
//! equality is part of the contract.
use crate::object::{self, Data, Value};
use std::ptr::null_mut;

/// The hasher for the runtime's internal tables: attribute names fixed in
/// compiled source, and object addresses. SipHash defends against chosen
/// keys an attacker can supply; these keys are not chosen that way, and
/// SipHash's setup and finalization dominated every attribute miss. This is
/// the FxHash multiply-rotate used by rustc's own tables.
#[derive(Default, Clone, Copy)]
pub struct FastHasher(u64);

const FX_SEED: u64 = 0x51_7c_c1_b7_27_22_0a_95;

impl FastHasher {
    #[inline]
    fn add(&mut self, word: u64) {
        self.0 = (self.0.rotate_left(5) ^ word).wrapping_mul(FX_SEED);
    }
}

impl std::hash::Hasher for FastHasher {
    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        let mut chunks = bytes.chunks_exact(8);
        for chunk in &mut chunks {
            self.add(u64::from_le_bytes(chunk.try_into().expect("eight bytes")));
        }
        let rest = chunks.remainder();
        if !rest.is_empty() {
            let mut word = [0u8; 8];
            word[..rest.len()].copy_from_slice(rest);
            self.add(u64::from_le_bytes(word));
        }
    }

    #[inline]
    fn write_u8(&mut self, value: u8) {
        self.add(u64::from(value));
    }

    #[inline]
    fn write_usize(&mut self, value: usize) {
        self.add(value as u64);
    }

    #[inline]
    fn write_u64(&mut self, value: u64) {
        self.add(value);
    }

    #[inline]
    fn finish(&self) -> u64 {
        self.0
    }
}

pub type FastBuild = std::hash::BuildHasherDefault<FastHasher>;
/// A `HashMap` with the runtime's internal hasher.
pub type FastMap<K, V> = std::collections::HashMap<K, V, FastBuild>;

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
    let hash = (((hash ^ (hash >> 62)) as i64) << 1) >> 1;
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

fn fixnum_hash(value: i64) -> i64 {
    let modulus = MODULUS as i64;
    let result = if value > -modulus && value < modulus {
        value
    } else if value < 0 {
        -((-value) % modulus)
    } else {
        value % modulus
    };
    if result == -1 { -2 } else { result }
}

/// The hash of fixnum, bool, text, bytes, None or binary64 storage, or None for other values.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_storage_hash(value: Value) -> Value {
    unsafe {
        if object::has_error() {
            return null_mut();
        }
        if value.is_null() {
            return object::none_value();
        }
        if object::is_fixnum(value) {
            return integer(fixnum_hash(object::fixnum(value)));
        }
        let hash = match &(*value).data {
            Data::None => NONE_HASH,
            Data::Bool(b) => i64::from(*b),
            Data::Text(text, ..) => text.hash(),
            Data::Bytes(bytes, ..) => fnv(bytes),
            Data::Float(number, ty, ..) if ty.is_null() && number.is_nan() => identity(value),
            Data::Float(number, ty, ..) if ty.is_null() => double_hash(*number),
            _ => return object::none_value(),
        };
        integer(hash)
    }
}

unsafe extern "C" fn storage_hash(_: Value, args: Value) -> Value {
    unsafe {
        let Some(value) = operand(args) else {
            return null_mut();
        };
        mule_storage_hash(value)
    }
}

unsafe fn integer(value: i64) -> Value {
    unsafe { crate::numeric::integer_from_i64(value) }
}

fn identity(value: Value) -> i64 {
    // CPython's default object hash: the address rotated by four bits, masked to fixnum range.
    (((value as usize).rotate_right(4) as i64) << 1) >> 1
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_identity_hash(value: Value) -> Value {
    unsafe {
        if object::has_error() || value.is_null() {
            return null_mut();
        }
        let hash = identity(value);
        integer(if hash == -1 { -2 } else { hash })
    }
}

unsafe extern "C" fn identity_hash(_: Value, args: Value) -> Value {
    unsafe {
        let Some(value) = operand(args) else {
            return null_mut();
        };
        mule_identity_hash(value)
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

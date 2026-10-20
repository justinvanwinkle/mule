//! Tagged integer fast paths and exact GMP/double arithmetic.

use crate::gmp::BigInt;
use crate::object::{self, Data, Value};
use std::borrow::Cow;
use std::cmp::Ordering;
use std::ffi::{CStr, c_char};
use std::ptr::null_mut;

pub(crate) const MIN: i64 = -(1_i64 << 62);
pub(crate) const MAX: i64 = (1_i64 << 62) - 1;

fn fail(kind: &str, message: &str) -> Value {
    object::error(kind, message);
    null_mut()
}

unsafe fn small_integer(value: Value) -> Option<i64> {
    if object::is_fixnum(value) {
        return Some(unsafe { object::fixnum(value) });
    }
    if value.is_null() {
        return None;
    }
    match unsafe { &(*value).data } {
        Data::Bool(value) => Some(i64::from(*value)),
        _ => None,
    }
}

unsafe fn float_value(value: Value) -> Option<f64> {
    if value.is_null() || object::is_fixnum(value) {
        return None;
    }
    match unsafe { &(*value).data } {
        Data::Float(value, ..) => Some(*value),
        _ => None,
    }
}

pub unsafe fn is_integer(value: Value) -> bool {
    if object::is_fixnum(value) {
        return true;
    }
    !value.is_null() && matches!(unsafe { &(*value).data }, Data::Bool(_) | Data::BigInt(..))
}

pub unsafe fn integer_export(value: Value) -> Option<BigInt> {
    unsafe { integer_view(value) }.map(Cow::into_owned)
}

/// Borrow a boxed operand while its owning runtime references remain live.
/// Callers must not mutate/release that object until this view has been dropped.
unsafe fn integer_view<'a>(value: Value) -> Option<Cow<'a, BigInt>> {
    if let Some(value) = unsafe { small_integer(value) } {
        return Some(Cow::Owned(BigInt::from_i64(value)));
    }
    if !value.is_null() {
        if let Data::BigInt(integer, ..) = unsafe { &(*value).data } {
            return Some(Cow::Borrowed(integer));
        }
    }
    object::error("TypeError", "an integer is required");
    None
}

pub unsafe fn integer_import(integer: &BigInt) -> Value {
    if let Some(value) = integer.to_i64() {
        if (MIN..=MAX).contains(&value) {
            return object::small(value);
        }
    }
    unsafe { object::new_value(Data::BigInt(integer.clone(), std::ptr::null_mut(), std::ptr::null_mut())) }
}

/// Lengths and slot indices normally fit the immediate representation.
/// Do not construct a GMP temporary on this common path.
pub unsafe fn integer_from_usize(value: usize) -> Value {
    if value <= MAX as usize {
        object::small(value as i64)
    } else {
        unsafe { integer_import_owned(BigInt::from_u64(value as u64)) }
    }
}

pub unsafe fn integer_from_i64(value: i64) -> Value {
    if (MIN..=MAX).contains(&value) {
        object::small(value)
    } else {
        unsafe { integer_import_owned(BigInt::from_i64(value)) }
    }
}

/// Transfer a freshly computed GMP integer into its runtime box without copying.
pub unsafe fn integer_import_owned(integer: BigInt) -> Value {
    if let Some(value) = integer.to_i64() {
        if (MIN..=MAX).contains(&value) {
            return object::small(value);
        }
    }
    unsafe { object::new_value(Data::BigInt(integer, std::ptr::null_mut(), std::ptr::null_mut())) }
}

/// Acquire a unit-owned value from a compiler-emitted native GMP integer.
/// The source header and limbs remain borrowed immutable static storage.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_literal_integer(integer: *const BigInt) -> Value {
    if crate::gmp::bits_per_limb() != 64 {
        return fail("RuntimeError", "compiled integer constants require 64-bit GMP limbs");
    }
    if integer.is_null() {
        return fail("RuntimeError", "missing compiled integer constant");
    }
    unsafe { integer_import(&*integer) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_literal_float(value: f64) -> Value {
    unsafe { object::new_value(Data::Float(value, std::ptr::null_mut(), std::ptr::null_mut())) }
}

pub unsafe fn number_double(value: Value) -> Option<f64> {
    if let Some(value) = unsafe { small_integer(value) } {
        return Some(value as f64);
    }
    if let Some(value) = unsafe { float_value(value) } {
        return Some(value);
    }
    if !value.is_null() {
        if let Data::BigInt(integer, ..) = unsafe { &(*value).data } {
            return quotient_double(integer, &BigInt::from_u64(1));
        }
    }
    object::error("TypeError", "a real number is required");
    None
}

/// Round an exact rational to binary64 using integer ties-to-even rounding.
fn quotient_double(numerator: &BigInt, denominator: &BigInt) -> Option<f64> {
    let negative = (numerator.sign() < 0) != (denominator.sign() < 0);
    if denominator.sign() == 0 {
        object::error("ZeroDivisionError", "division by zero");
        return None;
    }
    let sign = u64::from(negative) << 63;
    if numerator.sign() == 0 {
        return Some(f64::from_bits(sign));
    }
    let mut a = numerator.abs();
    let mut b = denominator.abs();
    let abits = a.bits();
    let bbits = b.bits();
    let overflow = || {
        object::error(
            "OverflowError",
            "integer division result too large for a float",
        );
        None
    };
    if abits > bbits && abits - bbits > 1024 {
        return overflow();
    }
    if bbits > abits && bbits - abits > 1076 {
        return Some(f64::from_bits(sign));
    }
    let mut exponent = if abits >= bbits {
        (abits - bbits) as i64
    } else {
        -((bbits - abits) as i64)
    };
    let below = if exponent >= 0 {
        a.compare(&b.shifted_left(exponent as u64)).is_lt()
    } else {
        a.shifted_left((-exponent) as u64).compare(&b).is_lt()
    };
    if below {
        exponent -= 1;
    }
    if exponent > 1023 {
        return overflow();
    }
    let normal = exponent >= -1022;
    let scale = if normal { 52 - exponent } else { 1074 };
    if scale >= 0 {
        a = a.shifted_left(scale as u64);
    } else {
        b = b.shifted_left((-scale) as u64);
    }
    let (mut quotient, remainder) = a.div_rem_floor(&b);
    let halfway = remainder.shifted_left(1).compare(&b);
    if halfway.is_gt() || (halfway.is_eq() && quotient.is_odd()) {
        quotient = quotient.add(&BigInt::from_u64(1));
    }
    let mut significand = quotient
        .to_u64()
        .expect("rounded binary64 significand fits u64");
    let magnitude = if normal {
        if significand == 1_u64 << 53 {
            significand >>= 1;
            exponent += 1;
        }
        if exponent > 1023 {
            return overflow();
        }
        (((exponent + 1023) as u64) << 52) | (significand - (1_u64 << 52))
    } else {
        // Rounding up to 2^52 directly encodes the smallest normal number.
        significand
    };
    Some(f64::from_bits(sign | magnitude))
}

unsafe fn small_binary(op: i32, left: i64, right: i64) -> Option<Value> {
    let result: i128 = match op {
        0 => i128::from(left) + i128::from(right),
        1 => i128::from(left) - i128::from(right),
        2 => i128::from(left) * i128::from(right),
        3 => {
            if right == 0 {
                return Some(fail("ZeroDivisionError", "division by zero"));
            }
            if left.abs() <= 1_i64 << 53 && right.abs() <= 1_i64 << 53 {
                return Some(unsafe { mule_literal_float(left as f64 / right as f64) });
            }
            return None;
        }
        4 | 5 => {
            if right == 0 {
                return Some(fail(
                    "ZeroDivisionError",
                    "integer division or modulo by zero",
                ));
            }
            let mut quotient = left / right;
            let mut remainder = left % right;
            if remainder != 0 && (remainder < 0) != (right < 0) {
                quotient -= 1;
                remainder += right;
            }
            i128::from(if op == 4 { quotient } else { remainder })
        }
        8 | 9 => {
            if right < 0 {
                return Some(fail("ValueError", "negative shift count"));
            }
            if op == 9 {
                return Some(object::small(if right >= 63 {
                    i64::from(left < 0) * -1
                } else {
                    left >> right
                }));
            }
            if left == 0 {
                return Some(object::small(0));
            }
            if right >= 63 {
                return None;
            }
            i128::from(left) * (1_i128 << right)
        }
        10 => return Some(object::small(left & right)),
        11 => return Some(object::small(left ^ right)),
        12 => return Some(object::small(left | right)),
        _ => return None,
    };
    (i128::from(MIN)..=i128::from(MAX))
        .contains(&result)
        .then(|| object::small(result as i64))
}

fn float_divmod(left: f64, right: f64) -> (f64, f64) {
    let mut modulo = left % right;
    let mut division = (left - modulo) / right;
    if modulo != 0.0 {
        if (right < 0.0) != (modulo < 0.0) {
            modulo += right;
            division -= 1.0;
        }
    } else {
        modulo = 0.0_f64.copysign(right);
    }
    let quotient = if division != 0.0 {
        let mut floor = division.floor();
        if division - floor > 0.5 {
            floor += 1.0;
        }
        floor
    } else {
        0.0_f64.copysign(left / right)
    };
    (quotient, modulo)
}

unsafe fn float_binary(op: i32, lhs: Value, rhs: Value) -> Value {
    if op >= 8 || op == 6 {
        return fail("TypeError", "operation requires integer operands");
    }
    let Some(left) = (unsafe { number_double(lhs) }) else {
        return null_mut();
    };
    let Some(right) = (unsafe { number_double(rhs) }) else {
        return null_mut();
    };
    let result = match op {
        0 => left + right,
        1 => left - right,
        2 => left * right,
        3..=5 => {
            if right == 0.0 {
                return fail("ZeroDivisionError", "float division or modulo by zero");
            }
            if op == 3 {
                left / right
            } else {
                let (quotient, modulo) = float_divmod(left, right);
                if op == 4 { quotient } else { modulo }
            }
        }
        7 => {
            if left == 0.0 && right < 0.0 {
                return fail(
                    "ZeroDivisionError",
                    "zero cannot be raised to a negative power",
                );
            }
            if left < 0.0 && left.is_finite() && right.is_finite() && right != right.floor() {
                return fail("ValueError", "complex power results are not supported");
            }
            let result = left.powf(right);
            if result.is_infinite() && left.is_finite() && right.is_finite() {
                return fail("OverflowError", "numerical result out of range");
            }
            result
        }
        _ => return fail("TypeError", "unsupported numeric operation"),
    };
    unsafe { mule_literal_float(result) }
}

unsafe fn integer_binary(op: i32, lhs: Value, rhs: Value) -> Value {
    if let (Some(left), Some(right)) =
        (unsafe { small_integer(lhs) }, unsafe { small_integer(rhs) })
    {
        if let Some(result) = unsafe { small_binary(op, left, right) } {
            return result;
        }
    }
    if op == 2 {
        if let Some(right) = unsafe { small_integer(rhs) } {
            let Some(left) = (unsafe { integer_view(lhs) }) else {
                return null_mut();
            };
            return unsafe { integer_import_owned(left.mul_i64(right)) };
        }
        if let Some(left) = unsafe { small_integer(lhs) } {
            let Some(right) = (unsafe { integer_view(rhs) }) else {
                return null_mut();
            };
            return unsafe { integer_import_owned(right.mul_i64(left)) };
        }
    }
    let Some(a) = (unsafe { integer_view(lhs) }) else {
        return null_mut();
    };
    let Some(b) = (unsafe { integer_view(rhs) }) else {
        return null_mut();
    };
    let integer = match op {
        0 => a.add(&b),
        1 => a.sub(&b),
        2 => a.mul(&b),
        3 => {
            return quotient_double(&a, &b)
                .map_or(null_mut(), |value| unsafe { mule_literal_float(value) });
        }
        4 | 5 => {
            if b.sign() == 0 {
                return fail("ZeroDivisionError", "integer division or modulo by zero");
            }
            if op == 4 {
                a.floor_div(&b)
            } else {
                a.remainder(&b)
            }
        }
        7 => {
            if b.sign() < 0 {
                return unsafe { float_binary(op, lhs, rhs) };
            }
            if b.sign() == 0 {
                BigInt::from_u64(1)
            } else if a.sign() == 0 {
                BigInt::zero()
            } else if a.abs().compare(&BigInt::from_u64(1)).is_eq() {
                BigInt::from_i64(if a.sign() < 0 && b.is_odd() { -1 } else { 1 })
            } else {
                let Some(exponent) = b.to_u64() else {
                    return fail("OverflowError", "exponent too large");
                };
                a.pow(exponent)
            }
        }
        8 | 9 => {
            if b.sign() < 0 {
                return fail("ValueError", "negative shift count");
            }
            if a.sign() == 0 {
                BigInt::zero()
            } else if let Some(shift) = b.to_u64() {
                if op == 8 {
                    if shift > i64::MAX as u64 || a.bits() as u64 > i64::MAX as u64 - shift {
                        return fail("OverflowError", "shift count too large");
                    }
                    a.shifted_left(shift)
                } else {
                    a.shifted_right(shift)
                }
            } else if op == 9 {
                BigInt::from_i64(if a.sign() < 0 { -1 } else { 0 })
            } else {
                return fail("OverflowError", "shift count too large");
            }
        }
        10 => a.bitand(&b),
        11 => a.xor(&b),
        12 => a.or(&b),
        _ => return fail("TypeError", "unsupported numeric operation"),
    };
    unsafe { integer_import_owned(integer) }
}

pub unsafe fn binary(op: i32, left: Value, right: Value) -> Value {
    let valid = unsafe {
        (is_integer(left) || float_value(left).is_some())
            && (is_integer(right) || float_value(right).is_some())
    };
    if !valid {
        return fail("TypeError", "numeric operands required");
    }
    if (10..=12).contains(&op) && !object::is_fixnum(left) && !object::is_fixnum(right) {
        if let (Data::Bool(a), Data::Bool(b)) = unsafe { (&(*left).data, &(*right).data) } {
            return unsafe {
                object::bool_value(match op {
                    10 => *a & *b,
                    11 => *a ^ *b,
                    _ => *a | *b,
                })
            };
        }
    }
    if unsafe { float_value(left).is_some() || float_value(right).is_some() } {
        unsafe { float_binary(op, left, right) }
    } else {
        unsafe { integer_binary(op, left, right) }
    }
}

/// Reuse only an exclusively owned boxed integer, or the two references known
/// to be its local slot and this operation's owned left operand. A slot pointer
/// is supplied only for an immediately following store back to that local.
pub unsafe fn try_binary_reuse(
    op: i32,
    left: Value,
    right: Value,
    slot: *mut Value,
) -> Option<Value> {
    unsafe {
        if !(0..=2).contains(&op)
            || left.is_null()
            || right.is_null()
            || left == right
            || object::is_fixnum(left)
            || !matches!(&(*left).data, Data::BigInt(..))
        {
            return None;
        }
        // Bool arithmetic has separate representation rules. Only exact ints
        // participate here; the generic path still handles mixed numeric kinds.
        if !object::is_fixnum(right) && !matches!(&(*right).data, Data::BigInt(..)) {
            return None;
        }
        let references = (*left).reference_count();
        if references != 1 && !(references == 2 && !slot.is_null() && *slot == left) {
            return None;
        }
        let demoted = if op == 2 && object::is_fixnum(right) {
            let multiplier = object::fixnum(right);
            let Data::BigInt(integer, ..) = &mut (*left).data else {
                unreachable!()
            };
            integer.mul_assign_i64(multiplier);
            integer.to_i64().filter(|value| (MIN..=MAX).contains(value))
        } else {
            // `right != left` and the reference proof exclude a shared GMP
            // object; keep only the other operand borrowed during mutation.
            let other = integer_view(right)?;
            let Data::BigInt(integer, ..) = &mut (*left).data else {
                unreachable!()
            };
            match op {
                0 => integer.add_assign(&other),
                1 => integer.sub_assign(&other),
                2 => integer.mul_assign(&other),
                _ => unreachable!(),
            }
            integer.to_i64().filter(|value| (MIN..=MAX).contains(value))
        };
        Some(demoted.map_or_else(|| object::retain(left), object::small))
    }
}

pub unsafe fn unary(op: i32, value: Value) -> Value {
    if let Some(real) = unsafe { float_value(value) } {
        return match op {
            0 => unsafe { object::retain(value) },
            1 => unsafe { mule_literal_float(-real) },
            _ => fail("TypeError", "integer operand required"),
        };
    }
    if !unsafe { is_integer(value) } {
        return fail("TypeError", "numeric operand required");
    }
    if let Some(integer) = unsafe { small_integer(value) } {
        match op {
            0 => return object::small(integer),
            1 if integer != MIN => return object::small(-integer),
            2 => return object::small(!integer),
            _ => {}
        }
    } else if op == 0 {
        return unsafe { object::retain(value) };
    }
    let Some(integer) = (unsafe { integer_view(value) }) else {
        return null_mut();
    };
    let integer = match op {
        1 => integer.neg(),
        2 => integer.invert(),
        0 => integer.into_owned(),
        _ => return fail("TypeError", "unsupported unary operation"),
    };
    unsafe { integer_import_owned(integer) }
}

fn ordering(value: Ordering) -> i32 {
    match value {
        Ordering::Less => -1,
        Ordering::Equal => 0,
        Ordering::Greater => 1,
    }
}

pub unsafe fn compare(left: Value, right: Value) -> Option<i32> {
    let af = unsafe { float_value(left) };
    let bf = unsafe { float_value(right) };
    if let (Some(a), Some(b)) = (af, bf) {
        return Some(a.partial_cmp(&b).map_or(2, ordering));
    }
    if unsafe { is_integer(left) && is_integer(right) } {
        if let (Some(a), Some(b)) = (unsafe { small_integer(left) }, unsafe {
            small_integer(right)
        }) {
            return Some(ordering(a.cmp(&b)));
        }
        let left = unsafe { integer_view(left) }?;
        let right = unsafe { integer_view(right) }?;
        return Some(ordering(left.compare(&right)));
    }
    let (real, integer, reverse) = if let Some(real) = af {
        (real, right, true)
    } else if let Some(real) = bf {
        (real, left, false)
    } else {
        object::error("TypeError", "numeric operands required");
        return None;
    };
    if !unsafe { is_integer(integer) } {
        object::error("TypeError", "numeric operands required");
        return None;
    }
    if real.is_nan() {
        return Some(2);
    }
    let comparison = if real.is_infinite() {
        if real > 0.0 { -1 } else { 1 }
    } else {
        ordering(unsafe { integer_view(integer) }?.compare_f64(real))
    };
    Some(if reverse { -comparison } else { comparison })
}

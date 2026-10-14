//! Borrowed-value assertions for checked compiler annotation boundaries.

use crate::gmp::BigInt;
use crate::object::{self, Data, Value};
use std::cmp::Ordering;
use std::ffi::{CStr, c_char};

enum Integer<'a> {
    Small(i64),
    Big(&'a BigInt),
}

impl Integer<'_> {
    fn compare_decimal(&self, bound: &str) -> Option<Ordering> {
        match self {
            Self::Small(value) => match bound.parse::<i64>() {
                Ok(bound) => Some(value.cmp(&bound)),
                Err(_) => {
                    let bound = BigInt::from_decimal(bound)?;
                    // Every tagged integer fits i64; an out-of-i64 bound is
                    // therefore ordered by its sign without allocating a value.
                    if bound.to_i64().is_none() {
                        Some(if bound.sign() < 0 {
                            Ordering::Greater
                        } else {
                            Ordering::Less
                        })
                    } else {
                        Some(value.cmp(&bound.to_i64()?))
                    }
                }
            },
            Self::Big(value) => Some(value.compare(&BigInt::from_decimal(bound)?)),
        }
    }
}

unsafe fn annotation_name<'a>(name: *const c_char) -> &'a str {
    if name.is_null() {
        "annotation"
    } else {
        unsafe { CStr::from_ptr(name) }
            .to_str()
            .unwrap_or("annotation")
    }
}

fn failed(name: &str) -> i32 {
    object::error("TypeError", &format!("expected {name}"));
    -1
}

/// Check an integer category and inclusive bounds without retaining or consuming
/// the value. Null bound pointers mean the corresponding side is unbounded.
/// Booleans are intentionally excluded: the compiler may use an accepted narrow
/// integer contract to select tagged integer instructions.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_assert_integer(
    value: Value,
    minimum: *const c_char,
    maximum: *const c_char,
    name: *const c_char,
) -> i32 {
    let name = unsafe { annotation_name(name) };
    let value = if object::is_fixnum(value) {
        Integer::Small(unsafe { object::fixnum(value) })
    } else if value.is_null() {
        return failed(name);
    } else {
        match unsafe { &(*value).data } {
            Data::BigInt(value, ..) => Integer::Big(value),
            _ => return failed(name),
        }
    };
    for (bound, excluded) in [(minimum, Ordering::Less), (maximum, Ordering::Greater)] {
        if bound.is_null() {
            continue;
        }
        let bound = match unsafe { CStr::from_ptr(bound) }.to_str() {
            Ok(bound) => bound,
            Err(_) => {
                object::error("RuntimeError", "invalid compiler annotation bound");
                return -1;
            }
        };
        match value.compare_decimal(bound) {
            Some(ordering) if ordering == excluded => return failed(name),
            Some(_) => {}
            None => {
                object::error("RuntimeError", "invalid compiler annotation bound");
                return -1;
            }
        }
    }
    0
}

/// Check the floating-point category without transferring ownership. Width
/// selection belongs to compiler lowering, rather than this category assertion.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_assert_float(value: Value, name: *const c_char) -> i32 {
    if !value.is_null()
        && !object::is_fixnum(value)
        && matches!(unsafe { &(*value).data }, Data::Float(..))
    {
        0
    } else {
        failed(unsafe { annotation_name(name) })
    }
}

/// Validate the physical union used by Python's float annotation. No conversion
/// occurs, so arbitrary-precision integers and boolean identities are retained.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_assert_real(value: Value, name: *const c_char) -> i32 {
    if !value.is_null() && (object::is_fixnum(value)
        || matches!(unsafe { &(*value).data }, Data::BigInt(..) | Data::Bool(..) | Data::Float(..)))
    {
        0
    } else {
        failed(unsafe { annotation_name(name) })
    }
}

//! Borrowed-value assertions for checked compiler annotation boundaries.

use crate::gmp::BigInt;
use crate::object::{self, Data, Value};
use std::cmp::Ordering;
use std::ffi::{CStr, c_char};

enum Integer<'a> {
    Small(i64),
    Big(&'a BigInt),
}

impl<'a> Integer<'a> {
    unsafe fn borrowed(value: Value) -> Option<Self> {
        if object::is_fixnum(value) {
            Some(Self::Small(unsafe { object::fixnum(value) }))
        } else if value.is_null() {
            None
        } else {
            match unsafe { &(*value).data } {
                Data::BigInt(value, ..) => Some(Self::Big(value)),
                _ => None,
            }
        }
    }

    fn compare(&self, bound: &Self) -> Ordering {
        match (self, bound) {
            (Self::Small(value), Self::Small(bound)) => value.cmp(bound),
            (Self::Big(value), Self::Big(bound)) => value.compare(bound),
            (Self::Big(value), Self::Small(bound)) => value.compare_i64(*bound),
            (Self::Small(value), Self::Big(bound)) => bound.compare_i64(*value).reverse(),
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
/// the value or numeric bounds. Null means the corresponding side is unbounded.
/// Booleans are intentionally excluded: the compiler may use an accepted narrow
/// integer contract to select tagged integer instructions.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_assert_integer(
    value: Value,
    minimum: Value,
    maximum: Value,
    name: *const c_char,
) -> i32 {
    let Some(value) = (unsafe { Integer::borrowed(value) }) else {
        return failed(unsafe { annotation_name(name) });
    };
    for (bound, excluded) in [(minimum, Ordering::Less), (maximum, Ordering::Greater)] {
        if bound.is_null() {
            continue;
        }
        let Some(bound) = (unsafe { Integer::borrowed(bound) }) else {
            object::error("RuntimeError", "invalid compiler integer bound");
            return -1;
        };
        if value.compare(&bound) == excluded {
            return failed(unsafe { annotation_name(name) });
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
    if !value.is_null()
        && (object::is_fixnum(value)
            || matches!(
                unsafe { &(*value).data },
                Data::BigInt(..) | Data::Bool(..) | Data::Float(..)
            ))
    {
        0
    } else {
        failed(unsafe { annotation_name(name) })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::numeric::integer_import_owned;
    use std::ptr::null_mut;

    #[test]
    fn numeric_bounds_compare_all_storage_pairs_and_unbounded_sides() {
        unsafe {
            let wide = BigInt::from_i64(1).shifted_left(127);
            let values = [
                integer_import_owned(wide.neg()),
                object::small(-1),
                object::small(0),
                object::small(1),
                integer_import_owned(wide),
            ];
            for (index, value) in values.iter().enumerate() {
                for (bound_index, bound) in values.iter().enumerate() {
                    let lower = mule_assert_integer(*value, *bound, null_mut(), c"test".as_ptr());
                    assert_eq!(lower == 0, index >= bound_index);
                    object::clear_error();
                    let upper = mule_assert_integer(*value, null_mut(), *bound, c"test".as_ptr());
                    assert_eq!(upper == 0, index <= bound_index);
                    object::clear_error();
                }
                assert_eq!(
                    mule_assert_integer(*value, *value, *value, c"test".as_ptr()),
                    0
                );
            }
            assert_eq!(
                mule_assert_integer(
                    object::bool_value(true),
                    null_mut(),
                    null_mut(),
                    c"test".as_ptr()
                ),
                -1
            );
            object::clear_error();
            for value in values {
                object::release(value);
            }
        }
    }
}

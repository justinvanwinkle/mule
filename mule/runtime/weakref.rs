//! Weak reference lifetime support: the physical link compiled `weakref.ref`
//! objects hold to their referent.
//!
//! A handle borrows its referent. Each referent's handles are registered by
//! address; when the referent's last reference is released, its handles are
//! cleared first and their callbacks then run, as CPython's
//! `PyObject_ClearWeakRefs` does before deallocation. Any pending error is
//! saved around each callback; an error a callback raises is reported on
//! stderr and discarded, matching CPython's unraisable-exception handling.
use crate::object::{self, Data, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::ptr::null_mut;

thread_local! {
    // Referent address to live handles. Empty in programs without weak references.
    static REGISTRY: RefCell<HashMap<usize, Vec<Value>>> = RefCell::new(HashMap::new());
}

pub fn has_references(value: Value) -> bool {
    REGISTRY.with_borrow(|registry| !registry.is_empty() && registry.contains_key(&(value as usize)))
}

/// Forget a handle being freed while its referent is still alive.
pub fn unregister(referent: Value, handle: Value) {
    if referent.is_null() {
        return;
    }
    REGISTRY.with_borrow_mut(|registry| {
        if let Some(handles) = registry.get_mut(&(referent as usize)) {
            handles.retain(|candidate| *candidate != handle);
            if handles.is_empty() {
                registry.remove(&(referent as usize));
            }
        }
    });
}

/// Clear every handle to `referent`, then call their callbacks with their
/// owning reference objects, most recently created first.
pub unsafe fn clear(referent: Value) {
    unsafe {
        let handles = REGISTRY.with_borrow_mut(|registry| registry.remove(&(referent as usize)));
        let Some(handles) = handles else {
            return;
        };
        let mut pending = Vec::new();
        for handle in &handles {
            if let Data::WeakHandle {
                referent, callback, owner,
            } = &mut (**handle).data
            {
                *referent = null_mut();
                if !callback.is_null() && !owner.is_null() && !crate::gc::unreachable(*owner) {
                    pending.push((object::retain(*owner), object::retain(*callback)));
                }
            }
        }
        for (owner, callback) in pending.into_iter().rev() {
            let saved = object::save_error();
            let result = object::call(callback, &[owner]);
            if result.is_null() {
                let message = format!(
                    "Exception ignored in weak reference callback: {}: {}\n",
                    object::error_kind(),
                    object::error_message()
                );
                let _ = std::io::stderr().lock().write_all(message.as_bytes());
                object::clear_error();
            }
            object::release(result);
            object::restore_error(saved);
            object::release(callback);
            object::release(owner);
        }
    }
}

/// Whether a value has independently owned heap storage that a handle may
/// borrow. The compiled reference constructor owns language-level eligibility.
unsafe fn referenceable(value: Value) -> bool {
    unsafe {
        !value.is_null()
            && !object::is_fixnum(value)
            && (*value).reference_count() != usize::MAX
    }
}

unsafe fn operands<const N: usize>(args: Value) -> Option<[Value; N]> {
    unsafe {
        if object::has_error() {
            return None;
        }
        let count = object::argument_count(args)?;
        if count != N || crate::arguments::keyword_count(args) != 0 {
            object::error("TypeError", &format!("expected {N} positional arguments"));
            return None;
        }
        let mut values = [null_mut(); N];
        for (index, value) in values.iter_mut().enumerate() {
            *value = object::argument(args, index)?;
        }
        Some(values)
    }
}

/// `weak_handle(referent, callback, owner)`: a new handle; callback may be None.
unsafe extern "C" fn weak_handle(_: Value, args: Value) -> Value {
    unsafe {
        let Some([referent, callback, owner]) = operands(args) else {
            return null_mut();
        };
        if !referenceable(referent) {
            let name = object::type_name(referent).unwrap_or_else(|| "object".into());
            object::error(
                "TypeError",
                &format!("cannot create weak reference to '{name}' object"),
            );
            return null_mut();
        }
        let callback = if callback == object::none_value() {
            null_mut()
        } else {
            object::retain(callback)
        };
        let handle = object::new_value(Data::WeakHandle {
            referent,
            callback,
            owner,
        });
        REGISTRY.with_borrow_mut(|registry| {
            registry.entry(referent as usize).or_default().push(handle);
        });
        handle
    }
}

unsafe fn handle_referent(handle: Value) -> Option<Value> {
    unsafe {
        if !handle.is_null() && !object::is_fixnum(handle) {
            if let Data::WeakHandle { referent, .. } = &(*handle).data {
                return Some(*referent);
            }
        }
        object::error("TypeError", "weak handle storage is required");
        None
    }
}

/// The referent, or None once it has been freed.
unsafe extern "C" fn weak_target(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle]) = operands(args) else {
            return null_mut();
        };
        match handle_referent(handle) {
            Some(referent) if referent.is_null() => object::none_value(),
            Some(referent) => object::retain(referent),
            None => null_mut(),
        }
    }
}

/// The compiled reference objects currently referring to `referent`.
unsafe extern "C" fn weak_owners(_: Value, args: Value) -> Value {
    unsafe {
        let Some([referent]) = operands(args) else {
            return null_mut();
        };
        let owners: Vec<Value> = REGISTRY.with_borrow(|registry| {
            registry.get(&(referent as usize)).map_or_else(Vec::new, |handles| {
                handles
                    .iter()
                    .filter_map(|handle| match &(**handle).data {
                        Data::WeakHandle { owner, .. } if !owner.is_null() => Some(*owner),
                        _ => None,
                    })
                    .collect()
            })
        });
        object::mule_sequence_list(owners.len() as isize, owners.as_ptr())
    }
}

pub unsafe fn populate(globals: Value) -> bool {
    unsafe {
        if object::has_error() {
            return false;
        }
        let exports: [(&str, object::Native); 3] = [
            ("weak_handle", weak_handle),
            ("weak_target", weak_target),
            ("weak_owners", weak_owners),
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

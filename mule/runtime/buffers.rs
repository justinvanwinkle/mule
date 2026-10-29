//! Owned lifetime guards for exported buffers. Resize policy stays in Python.

use crate::object::{self, Data, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr::null_mut;

thread_local! {
    static EXPORTS: RefCell<HashMap<usize, usize>> = RefCell::new(HashMap::new());
}

pub fn release(owner: Value) {
    unsafe {
        if let Data::ByteStorage(buffer) = &mut (*owner).data {
            buffer.exports = buffer.exports.checked_sub(1).expect("live buffer lease");
            return;
        }
    }
    EXPORTS.with_borrow_mut(|exports| {
        let count = exports.get_mut(&(owner as usize)).expect("live buffer lease");
        *count -= 1;
        if *count == 0 { exports.remove(&(owner as usize)); }
    });
}

#[inline]
pub fn has_exports(owner: Value) -> bool {
    unsafe {
        if let Some(storage) = byte_storage(owner) {
            let Data::ByteStorage(buffer) = &(*storage).data else {
                unreachable!()
            };
            return buffer.exports != 0;
        }
    }
    EXPORTS.with_borrow(|exports| {
        !exports.is_empty() && exports.get(&(owner as usize)).copied().unwrap_or(0) != 0
    })
}

/// Borrow the single authoritative mutable-buffer owner, without a class lookup.
pub unsafe fn byte_storage(owner: Value) -> Option<Value> {
    unsafe {
        if owner.is_null() || object::is_fixnum(owner) {
            return None;
        }
        match &(*owner).data {
            Data::ByteStorage(_) => Some(owner),
            Data::Instance { fields, .. } if !fields.is_empty() => {
                let storage = fields[0];
                if !storage.is_null()
                    && !object::is_fixnum(storage)
                    && matches!(&(*storage).data, Data::ByteStorage(_))
                {
                    Some(storage)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

unsafe fn operand(args: Value) -> Option<Value> {
    unsafe {
        if object::has_error() { return None; }
        if object::argument_count(args)? != 1 || crate::arguments::keyword_count(args) != 0 {
            object::error("TypeError", "buffer primitive requires one argument"); return None;
        }
        object::argument(args, 0)
    }
}

unsafe extern "C" fn lease(_: Value, args: Value) -> Value {
    unsafe {
        let Some(owner) = operand(args) else { return null_mut(); };
        if owner.is_null() || object::is_fixnum(owner) {
            object::error("TypeError", "buffer lifetime requires heap storage"); return null_mut();
        }
        if let Some(storage) = byte_storage(owner) {
            let Data::ByteStorage(buffer) = &mut (*storage).data else {
                unreachable!()
            };
            buffer.exports += 1;
            return object::new_value(Data::BufferLease {
                owner: object::retain(storage),
            });
        }
        EXPORTS.with_borrow_mut(|exports| { *exports.entry(owner as usize).or_insert(0) += 1; });
        object::new_value(Data::BufferLease { owner: object::retain(owner),
        })
    }
}

unsafe extern "C" fn exports(_: Value, args: Value) -> Value {
    unsafe {
        let Some(owner) = operand(args) else { return null_mut(); };
        if let Some(storage) = byte_storage(owner) {
            let Data::ByteStorage(buffer) = &(*storage).data else {
                unreachable!()
            };
            return numeric_exports(buffer.exports);
        }
        let count = EXPORTS.with_borrow(|exports| exports.get(&(owner as usize)).copied().unwrap_or(0));
        object::small(count as i64)
    }
}

fn numeric_exports(count: usize) -> Value {
    unsafe { crate::numeric::integer_from_usize(count)
    }
}

pub unsafe fn populate(globals: Value) -> bool {
    unsafe {
        let exports: [(&str, object::Native); 2] = [("buffer_lease", lease), ("buffer_exports", exports)];
        for (name, entry) in exports {
            let function = object::function(null_mut(), entry, name);
            let bound = object::bind(globals, name, function);
            object::release(function);
            if !bound { return false; }
        }
        true
    }
}

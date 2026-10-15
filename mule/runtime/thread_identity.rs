//! POSIX thread cookies and Linux kernel thread identities.
//! Main startup publishes its identity before any compiled module runs.

use std::sync::atomic::{AtomicUsize, Ordering};

unsafe extern "C" {
    fn pthread_self() -> usize;
    fn gettid() -> std::ffi::c_int;
}

static MAIN_IDENT: AtomicUsize = AtomicUsize::new(0);

pub fn initialize_main_identity() {
    MAIN_IDENT.store(current_ident(), Ordering::Release);
}

pub fn is_main_thread() -> bool {
    let main = MAIN_IDENT.load(Ordering::Acquire);
    main != 0 && main == current_ident()
}

fn current_ident() -> usize {
    unsafe { pthread_self() }
}

fn current_native_id() -> usize {
    unsafe { gettid() as usize }
}

unsafe fn identity_argument_count(args: crate::object::Value) -> bool {
    unsafe {
        if crate::object::has_error() {
            return false;
        }
        if crate::object::argument_count(args) == Some(0)
            && crate::arguments::keyword_count(args) == 0
        {
            return true;
        }
        crate::object::error("TypeError", "thread identity primitives take no arguments");
        false
    }
}

unsafe extern "C" fn thread_ident(
    _: crate::object::Value,
    args: crate::object::Value,
) -> crate::object::Value {
    unsafe {
        if !identity_argument_count(args) {
            return std::ptr::null_mut();
        }
        crate::numeric::integer_from_usize(current_ident())
    }
}

unsafe extern "C" fn thread_native_id(
    _: crate::object::Value,
    args: crate::object::Value,
) -> crate::object::Value {
    unsafe {
        if !identity_argument_count(args) {
            return std::ptr::null_mut();
        }
        crate::numeric::integer_from_usize(current_native_id())
    }
}

unsafe extern "C" fn main_thread_ident(
    _: crate::object::Value,
    args: crate::object::Value,
) -> crate::object::Value {
    unsafe {
        if !identity_argument_count(args) {
            return std::ptr::null_mut();
        }
        let ident = MAIN_IDENT.load(Ordering::Acquire);
        if ident == 0 {
            crate::object::error(
                "SystemError",
                "native main thread identity is not initialized",
            );
            return std::ptr::null_mut();
        }
        crate::numeric::integer_from_usize(ident)
    }
}

pub unsafe fn populate(globals: crate::object::Value) -> bool {
    unsafe {
        let exports: [(&str, crate::object::Native); 3] = [
            ("thread_ident", thread_ident),
            ("thread_native_id", thread_native_id),
            ("main_thread_ident", main_thread_ident),
        ];
        for (name, entry) in exports {
            let function = crate::object::function(std::ptr::null_mut(), entry, name);
            let bound = crate::object::bind(globals, name, function);
            crate::object::release(function);
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
    use std::thread;

    #[test]
    fn native_identities_are_stable_distinct_and_main_identity_is_shared() {
        initialize_main_identity();
        let parent = (current_ident(), current_native_id());
        assert!(parent.0 > 0 && parent.1 > 0);
        let child = thread::spawn(move || {
            let child = (current_ident(), current_native_id());
            assert!(child.0 > 0 && child.1 > 0);
            assert_ne!(child.0, parent.0);
            assert_ne!(child.1, parent.1);
            assert_eq!(MAIN_IDENT.load(Ordering::Acquire), parent.0);
            assert_eq!((current_ident(), current_native_id()), child);
        });
        child.join().unwrap();
        assert_eq!((current_ident(), current_native_id()), parent);
    }
}

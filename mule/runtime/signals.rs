//! POSIX signal dispositions and mask storage. Language handlers are not run here.

use crate::object::{self, Data, Value};
use std::ptr::null_mut;

#[repr(C)]
struct SignalSet { words: [u64; 16] }

#[repr(C)]
struct SignalAction {
    handler: usize,
    mask: SignalSet,
    flags: i32,
    restorer: usize,
}

unsafe extern "C" {
    fn signal(number: i32, handler: usize) -> usize;
    fn sigaction(number: i32, action: *const SignalAction, old: *mut SignalAction) -> i32;
    fn raise(number: i32) -> i32;
    fn sigemptyset(set: *mut SignalSet) -> i32;
    fn sigfillset(set: *mut SignalSet) -> i32;
    fn sigaddset(set: *mut SignalSet, number: i32) -> i32;
    fn sigismember(set: *const SignalSet, number: i32) -> i32;
    fn pthread_sigmask(how: i32, set: *const SignalSet, old: *mut SignalSet) -> i32;
    fn sigpending(set: *mut SignalSet) -> i32;
    fn sigwait(set: *const SignalSet, number: *mut i32) -> i32;
    fn __libc_current_sigrtmin() -> i32;
    fn __libc_current_sigrtmax() -> i32;
}

unsafe fn operands<const N: usize>(args: Value) -> Option<[Value; N]> {
    unsafe {
        if object::has_error() { return None; }
        if object::argument_count(args)? != N || crate::arguments::keyword_count(args) != 0 {
            object::error("TypeError", "invalid signal primitive argument count");
            return None;
        }
        let mut values = [null_mut(); N];
        for (index, value) in values.iter_mut().enumerate() {
            *value = object::argument(args, index)?;
        }
        Some(values)
    }
}

unsafe fn integer(value: Value) -> Option<i32> {
    unsafe {
        if object::is_fixnum(value) {
            if let Ok(value) = i32::try_from(object::fixnum(value)) { return Some(value); }
        } else if !value.is_null() {
            if let Data::BigInt(number, ..) = &(*value).data {
                if let Some(number) = number.to_i64() {
                    if let Ok(number) = i32::try_from(number) { return Some(number); }
                }
            }
        }
        object::error("TypeError", "signal primitive requires a native integer");
        None
    }
}

fn os_error(code: i32) -> Value {
    object::error("OSError", &std::io::Error::from_raw_os_error(code).to_string());
    null_mut()
}

fn last_error() -> Value {
    os_error(std::io::Error::last_os_error().raw_os_error().unwrap_or(22))
}

unsafe fn mask(values: Value) -> Option<SignalSet> {
    unsafe {
        let mut result = SignalSet { words: [0; 16] };
        if sigemptyset(&mut result) < 0 { last_error(); return None; }
        for index in 0..object::argument_count(values)? {
            let number = integer(object::argument(values, index)?)?;
            if sigaddset(&mut result, number) < 0 { last_error(); return None; }
        }
        Some(result)
    }
}

unsafe fn members(set: &SignalSet) -> Value {
    unsafe {
        let values: Vec<Value> = (1..=__libc_current_sigrtmax())
            .filter(|number| sigismember(set, *number) == 1)
            .map(|number| object::small(i64::from(number))).collect();
        object::mule_sequence_list(values.len() as isize, values.as_ptr())
    }
}

unsafe extern "C" fn limits(_: Value, args: Value) -> Value {
    unsafe {
        if operands::<0>(args).is_none() { return null_mut(); }
        let values = [object::small(i64::from(__libc_current_sigrtmin())), object::small(i64::from(__libc_current_sigrtmax()))];
        object::mule_sequence_tuple(2, values.as_ptr())
    }
}

unsafe extern "C" fn disposition(_: Value, args: Value) -> Value {
    unsafe {
        let Some([number, handler]) = operands(args) else { return null_mut(); };
        let Some(number) = integer(number) else { return null_mut(); };
        let old = if handler == object::none_value() {
            let mut old = SignalAction { handler: 0, mask: SignalSet { words: [0;16] }, flags: 0, restorer: 0 };
            if sigaction(number, std::ptr::null(), &mut old) < 0 { return last_error(); }
            old.handler
        } else {
            let Some(handler) = integer(handler) else { return null_mut(); };
            if handler != 0 && handler != 1 {
                object::error("ValueError", "native disposition must be default or ignore");
                return null_mut();
            }
            let old = signal(number, handler as usize);
            if old == usize::MAX { return last_error(); }
            old
        };
        if old <= 1 { object::small(old as i64) } else { object::none_value() }
    }
}

unsafe extern "C" fn raise_signal(_: Value, args: Value) -> Value {
    unsafe {
        let Some([number]) = operands(args) else { return null_mut(); };
        let Some(number) = integer(number) else { return null_mut(); };
        let error = raise(number);
        if error < 0 { last_error() } else if error != 0 { os_error(error) } else { object::none_value() }
    }
}

unsafe extern "C" fn signal_mask(_: Value, args: Value) -> Value {
    unsafe {
        let Some([how, values]) = operands(args) else { return null_mut(); };
        let Some(how) = integer(how) else { return null_mut(); };
        let Some(set) = mask(values) else { return null_mut(); };
        let mut old = SignalSet { words: [0;16] };
        let error = pthread_sigmask(how, &set, &mut old);
        if error != 0 { os_error(error) } else { members(&old) }
    }
}

unsafe extern "C" fn pending(_: Value, args: Value) -> Value {
    unsafe {
        if operands::<0>(args).is_none() { return null_mut(); }
        let mut set = SignalSet { words: [0;16] };
        if sigpending(&mut set) < 0 { last_error() } else { members(&set) }
    }
}

unsafe extern "C" fn valid(_: Value, args: Value) -> Value {
    unsafe {
        if operands::<0>(args).is_none() { return null_mut(); }
        let mut set = SignalSet { words: [0;16] };
        if sigfillset(&mut set) < 0 { last_error() } else { members(&set) }
    }
}

unsafe extern "C" fn wait(_: Value, args: Value) -> Value {
    unsafe {
        let Some([values]) = operands(args) else { return null_mut(); };
        let Some(set) = mask(values) else { return null_mut(); };
        let mut number = 0;
        let error = sigwait(&set, &mut number);
        if error != 0 { os_error(error) } else { object::small(i64::from(number)) }
    }
}

pub unsafe fn populate(globals: Value) -> bool {
    unsafe {
        let exports: [(&str, object::Native);7] = [
            ("signal_limits", limits), ("signal_disposition", disposition),
            ("raise_signal", raise_signal), ("signal_mask", signal_mask),
            ("pending_signals", pending), ("valid_signals", valid), ("wait_signal", wait),
        ];
        for (name, entry) in exports {
            let function = object::function(null_mut(), entry, name);
            let bound = object::bind(globals, name, function);
            object::release(function);
            if !bound { return false; }
        }
        true
    }
}

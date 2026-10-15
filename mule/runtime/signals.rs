//! POSIX signal dispositions and mask storage with deferred safepoint delivery.

use crate::object::{self, Data, Value};
use std::cell::{Cell, RefCell};
use std::ptr::null_mut;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};

#[repr(C)]
struct SignalSet { words: [u64; 16] }

#[repr(C)]
struct SignalAction {
    handler: usize,
    mask: SignalSet,
    flags: i32,
    restorer: usize,
}

static PENDING_SIGNALS: [AtomicU64; 2] = [const { AtomicU64::new(0) }; 2];
static TRAPPED_SIGNALS: [AtomicBool; 128] = [const { AtomicBool::new(false) }; 128];
static DEFAULT_INT_ACTIVE: AtomicBool = AtomicBool::new(false);

thread_local! {
    static HANDLERS: RefCell<[Value; 128]> = const { RefCell::new([null_mut(); 128]) };
    static DELIVERING: Cell<bool> = const { Cell::new(false) };
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

extern "C" fn handle_signal(number: i32) {
    if (1..128).contains(&number) {
        let index = number as usize;
        PENDING_SIGNALS[index / 64].fetch_or(1u64 << (index % 64), Ordering::Release);
        crate::threads::request_safepoint();
    }
}

fn clear_pending_bit(number: i32) {
    if (1..128).contains(&number) {
        let index = number as usize;
        PENDING_SIGNALS[index / 64].fetch_and(!(1u64 << (index % 64)), Ordering::AcqRel);
    }
}

pub fn has_pending_signals() -> bool {
    PENDING_SIGNALS[0].load(Ordering::Acquire) != 0
        || PENDING_SIGNALS[1].load(Ordering::Acquire) != 0
}

unsafe fn install_trap(number: i32, old: &mut SignalAction) -> i32 {
    unsafe {
        let mut action = SignalAction {
            handler: handle_signal as *const () as usize,
            mask: SignalSet { words: [0; 16] },
            flags: 0,
            restorer: 0,
        };
        sigemptyset(&mut action.mask);
        sigaction(number, &action, old)
    }
}

pub unsafe fn initialize() {
    unsafe {
        PENDING_SIGNALS[0].store(0, Ordering::Release);
        PENDING_SIGNALS[1].store(0, Ordering::Release);
        let mut old = SignalAction {
            handler: 0,
            mask: SignalSet { words: [0; 16] },
            flags: 0,
            restorer: 0,
        };
        if sigaction(2, std::ptr::null(), &mut old) == 0 && old.handler == 0 {
            if install_trap(2, &mut old) == 0 {
                TRAPPED_SIGNALS[2].store(true, Ordering::Release);
                DEFAULT_INT_ACTIVE.store(true, Ordering::Release);
            }
        }
    }
}

pub unsafe fn finish() {
    unsafe {
        DEFAULT_INT_ACTIVE.store(false, Ordering::Release);
        PENDING_SIGNALS[0].store(0, Ordering::Release);
        PENDING_SIGNALS[1].store(0, Ordering::Release);
        for (number, trapped) in TRAPPED_SIGNALS.iter().enumerate().skip(1) {
            if trapped.swap(false, Ordering::AcqRel) {
                signal(number as i32, 0);
            }
        }
        HANDLERS.with_borrow_mut(|handlers| {
            for slot in handlers.iter_mut() {
                object::release(std::mem::replace(slot, null_mut()));
            }
        });
        DELIVERING.with(|flag| flag.set(false));
        crate::threads::clear_safepoint_if_idle();
    }
}

pub unsafe fn terminate_with_sigint() {
    unsafe {
        signal(2, 0);
        raise(2);
    }
}

unsafe fn deliver_signal(number: i32) {
    unsafe {
        let handler = HANDLERS.with_borrow(|handlers| {
            let value = handlers[number as usize];
            if value.is_null() { null_mut() } else { object::retain(value) }
        });
        if !handler.is_null() {
            let frame = crate::traceback::frame_at(0);
            let frame_arg = if frame.is_null() { object::none_value() } else { frame };
            let result = object::call(handler, &[object::small(i64::from(number)), frame_arg]);
            object::release(result);
            object::release(frame);
            object::release(handler);
        } else if number == 2 && DEFAULT_INT_ACTIVE.load(Ordering::Acquire) {
            let globals = crate::sequences::mule_protocol_globals();
            let class = if globals.is_null() {
                null_mut()
            } else {
                object::lookup(globals, "KeyboardInterrupt")
            };
            if class.is_null() {
                object::error("KeyboardInterrupt", "");
            } else {
                crate::sequences::mule_raise_pending(class);
                object::release(class);
            }
        }
    }
}

pub unsafe fn check_signals() -> bool {
    unsafe {
        if object::has_error() {
            return false;
        }
        if !has_pending_signals() || !crate::thread_identity::is_main_thread() {
            return true;
        }
        if DELIVERING.with(Cell::get) {
            crate::threads::clear_safepoint_if_idle();
            return true;
        }
        DELIVERING.with(|flag| flag.set(true));
        let words = [
            PENDING_SIGNALS[0].swap(0, Ordering::AcqRel),
            PENDING_SIGNALS[1].swap(0, Ordering::AcqRel),
        ];
        crate::threads::clear_safepoint_if_idle();
        for (word_index, mut bits) in words.into_iter().enumerate() {
            while bits != 0 {
                let bit = bits.trailing_zeros() as usize;
                bits &= bits - 1;
                let number = word_index * 64 + bit;
                deliver_signal(number as i32);
                if object::has_error() {
                    if bits != 0 {
                        PENDING_SIGNALS[word_index].fetch_or(bits, Ordering::Release);
                    }
                    if word_index == 0 && words[1] != 0 {
                        PENDING_SIGNALS[1].fetch_or(words[1], Ordering::Release);
                    }
                    DELIVERING.with(|flag| flag.set(false));
                    return false;
                }
            }
        }
        DELIVERING.with(|flag| flag.set(false));
        if has_pending_signals() {
            crate::threads::request_safepoint();
        }
        !object::has_error()
    }
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
        let index = number as usize;
        if handler == object::none_value() {
            if (1..128).contains(&index) {
                let stored = HANDLERS.with_borrow(|handlers| handlers[index]);
                if !stored.is_null() {
                    return object::retain(stored);
                }
                if number == 2 && DEFAULT_INT_ACTIVE.load(Ordering::Acquire) {
                    return object::small(2);
                }
            }
            let mut old = SignalAction { handler: 0, mask: SignalSet { words: [0;16] }, flags: 0, restorer: 0 };
            if sigaction(number, std::ptr::null(), &mut old) < 0 { return last_error(); }
            return if old.handler <= 1 { object::small(old.handler as i64) } else { object::none_value() };
        }
        if !crate::thread_identity::is_main_thread() {
            object::error("ValueError", "signal only works in main thread of the main interpreter");
            return null_mut();
        }
        if object::is_fixnum(handler) {
            let code = object::fixnum(handler);
            if code != 0 && code != 1 {
                object::error("ValueError", "native disposition must be default or ignore");
                return null_mut();
            }
            let old = signal(number, code as usize);
            if old == usize::MAX { return last_error(); }
            clear_pending_bit(number);
            if (1..128).contains(&index) {
                TRAPPED_SIGNALS[index].store(false, Ordering::Release);
            }
            let previous = if (1..128).contains(&index) {
                HANDLERS.with_borrow_mut(|handlers| std::mem::replace(&mut handlers[index], null_mut()))
            } else {
                null_mut()
            };
            let default_int = number == 2 && DEFAULT_INT_ACTIVE.swap(false, Ordering::AcqRel);
            crate::threads::clear_safepoint_if_idle();
            return if !previous.is_null() {
                previous
            } else if default_int {
                object::small(2)
            } else if old <= 1 {
                object::small(old as i64)
            } else {
                object::none_value()
            };
        }
        let mut old = SignalAction { handler: 0, mask: SignalSet { words: [0;16] }, flags: 0, restorer: 0 };
        if install_trap(number, &mut old) < 0 {
            return last_error();
        }
        if (1..128).contains(&index) {
            TRAPPED_SIGNALS[index].store(true, Ordering::Release);
        }
        let previous = if (1..128).contains(&index) {
            HANDLERS.with_borrow_mut(|handlers| std::mem::replace(&mut handlers[index], object::retain(handler)))
        } else {
            null_mut()
        };
        let default_int = number == 2 && DEFAULT_INT_ACTIVE.swap(false, Ordering::AcqRel);
        if !previous.is_null() {
            previous
        } else if default_int {
            object::small(2)
        } else if old.handler <= 1 {
            object::small(old.handler as i64)
        } else {
            object::none_value()
        }
    }
}

unsafe extern "C" fn raise_signal(_: Value, args: Value) -> Value {
    unsafe {
        let Some([number]) = operands(args) else { return null_mut(); };
        let Some(number) = integer(number) else { return null_mut(); };
        let error = raise(number);
        if error < 0 {
            last_error()
        } else if error != 0 {
            os_error(error)
        } else if !check_signals() {
            null_mut()
        } else {
            object::none_value()
        }
    }
}

unsafe extern "C" fn signal_mask(_: Value, args: Value) -> Value {
    unsafe {
        let Some([how, values]) = operands(args) else { return null_mut(); };
        let Some(how) = integer(how) else { return null_mut(); };
        let Some(set) = mask(values) else { return null_mut(); };
        let mut old = SignalSet { words: [0;16] };
        let error = pthread_sigmask(how, &set, &mut old);
        if error != 0 {
            os_error(error)
        } else if !check_signals() {
            null_mut()
        } else {
            members(&old)
        }
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
        if error != 0 {
            os_error(error)
        } else {
            clear_pending_bit(number);
            crate::threads::clear_safepoint_if_idle();
            object::small(i64::from(number))
        }
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

//! OS fatal-signal diagnostics over immutable compiler-emitted labels.
//! Signal handlers write native bytes; no Python code, locks or allocation.

use crate::object::{self, Data, Value};
use crate::traceback::CodeInfo;
use std::cell::RefCell;
use std::ffi::{CStr, c_char, c_void};
use std::ptr::{null, null_mut};
use std::sync::Mutex;
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicUsize, Ordering};

struct Frame { label: *const c_char, parent: *mut Frame }
struct Header { bytes: Vec<u8> }
static ACTIVE: AtomicBool = AtomicBool::new(false);
static FD: AtomicI32 = AtomicI32::new(2);
static C_STACK: AtomicBool = AtomicBool::new(false);
static HEAD: AtomicPtr<Frame> = AtomicPtr::new(null_mut());
static HEADER: AtomicPtr<Header> = AtomicPtr::new(null_mut());
static HEADER_ROOT: Mutex<Option<Box<Header>>> = Mutex::new(None);
const SIGNALS: [i32; 5] = [11, 8, 6, 7, 4];
static PREVIOUS: [AtomicUsize; 5] = [const { AtomicUsize::new(0) }; 5];
thread_local! {
    static FRAMES: RefCell<Vec<Box<Frame>>> = const { RefCell::new(Vec::new()) };
}

unsafe extern "C" {
    fn signal(number: i32, handler: usize) -> usize;
    fn raise(number: i32) -> i32;
    fn write(fd: i32, bytes: *const u8, length: usize) -> isize;
    fn backtrace(frames: *mut *mut c_void, size: i32) -> i32;
    fn backtrace_symbols_fd(frames: *const *mut c_void, size: i32, fd: i32);
    fn pthread_self() -> usize;
}

#[inline(always)]
pub fn enabled() -> bool { ACTIVE.load(Ordering::Relaxed) }

#[inline(always)]
pub fn push(code: *const CodeInfo) {
    if enabled() {
        push_slow(code);
    }
}

#[cold]
#[inline(never)]
fn push_slow(code: *const CodeInfo) {
    FRAMES.with_borrow_mut(|frames| {
        let mut node = Box::new(Frame {
            label: unsafe { (*code).fault_label },
            parent: HEAD.load(Ordering::Relaxed),
        });
        HEAD.store(&mut *node, Ordering::SeqCst);
        frames.push(node);
    });
}

#[inline(always)]
pub fn pop() {
    if enabled() {
        pop_slow();
    }
}

#[cold]
#[inline(never)]
fn pop_slow() {
    FRAMES.with_borrow_mut(|frames| {
        if let Some(node) = frames.pop() {
            HEAD.store(node.parent, Ordering::SeqCst);
        }
    });
}

pub fn replace(codes: impl Iterator<Item = *const CodeInfo>) {
    let mut frames = Vec::new();
    let mut parent = null_mut();
    for code in codes {
        let mut node = Box::new(Frame { label: unsafe { (*code).fault_label }, parent });
        parent = &mut *node;
        frames.push(node);
    }
    HEAD.store(parent, Ordering::SeqCst);
    FRAMES.with_borrow_mut(|current| { *current = frames; });
}

unsafe fn output(fd: i32, bytes: &[u8]) {
    unsafe {
        let mut offset = 0;
        while offset < bytes.len() {
            let count = write(fd, bytes.as_ptr().add(offset), bytes.len() - offset);
            if count <= 0 { break; }
            offset += count as usize;
        }
    }
}

unsafe fn dump(fd: i32, header: &[u8]) {
    unsafe {
        output(fd, header);
        let mut node = HEAD.load(Ordering::SeqCst);
        let mut count = 0;
        while !node.is_null() && count < 100 {
            let label = (*node).label;
            if !label.is_null() { output(fd, CStr::from_ptr(label).to_bytes()); }
            node = (*node).parent;
            count += 1;
        }
        if !node.is_null() { output(fd, b"  ...\n"); }
    }
}

unsafe fn dump_c(fd: i32) {
    unsafe {
        let mut frames = [null_mut(); 64];
        let count = backtrace(frames.as_mut_ptr(), frames.len() as i32);
        output(fd, b"\nNative C stack (most recent call first):\n");
        backtrace_symbols_fd(frames.as_ptr(), count, fd);
    }
}

unsafe extern "C" fn fatal(number: i32) {
    unsafe {
        let fd = FD.load(Ordering::Relaxed);
        let message: &[u8] = match number {
            11 => b"Fatal Python error: Segmentation fault\n\n",
            8 => b"Fatal Python error: Floating point exception\n\n",
            6 => b"Fatal Python error: Aborted\n\n",
            7 => b"Fatal Python error: Bus error\n\n",
            _ => b"Fatal Python error: Illegal instruction\n\n",
        };
        output(fd, message);
        let header = HEADER.load(Ordering::SeqCst);
        if !header.is_null() { dump(fd, &(*header).bytes); }
        if C_STACK.load(Ordering::Relaxed) { dump_c(fd); }
        let index = SIGNALS.iter().position(|signal| *signal == number).unwrap_or(0);
        let previous = PREVIOUS[index].load(Ordering::Relaxed);
        signal(number, if previous == 1 { 0 } else { previous });
        raise(number);
    }
}

unsafe fn operands<const N: usize>(args: Value) -> Option<[Value; N]> {
    unsafe {
        if object::has_error() { return None; }
        if object::argument_count(args) != Some(N) || crate::arguments::keyword_count(args) != 0 {
            object::error("TypeError", "invalid fault diagnostic arguments");
            return None;
        }
        let mut values = [null_mut(); N];
        for (index, value) in values.iter_mut().enumerate() {
            *value = object::argument(args, index)?;
        }
        Some(values)
    }
}

unsafe fn descriptor(value: Value) -> Option<i32> {
    unsafe { crate::numeric::integer_export(value)?.to_i64().and_then(|fd| i32::try_from(fd).ok()) }
}

unsafe fn bytes(value: Value) -> Option<&'static [u8]> {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::Bytes(bytes, ..) = &(*value).data { return Some(bytes); }
        }
        object::error("TypeError", "fault diagnostic header requires bytes");
        None
    }
}

unsafe extern "C" fn fault_enable(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd, header, c_stack]) = operands(args) else { return null_mut(); };
        let (Some(fd), Some(bytes), Some(c_stack)) = (descriptor(fd), bytes(header), object::truth(c_stack)) else { return null_mut(); };
        // Warm libc's unwinder before it can be called by a fatal handler.
        if c_stack {
            let mut warm = [null_mut(); 1];
            backtrace(warm.as_mut_ptr(), 1);
        }
        let mut header = Box::new(Header { bytes: bytes.to_vec() });
        HEADER.store(&mut *header, Ordering::SeqCst);
        *HEADER_ROOT.lock().unwrap() = Some(header);
        FD.store(fd, Ordering::Relaxed);
        C_STACK.store(c_stack, Ordering::Relaxed);
        if !ACTIVE.swap(true, Ordering::SeqCst) {
            crate::traceback::refresh_fault_frames();
            for (index, number) in SIGNALS.iter().enumerate() {
                let previous = signal(*number, fatal as *const () as usize);
                if previous == usize::MAX {
                    object::error("OSError", &std::io::Error::last_os_error().to_string());
                    return null_mut();
                }
                PREVIOUS[index].store(previous, Ordering::Relaxed);
            }
        }
        object::none_value()
    }
}

unsafe extern "C" fn fault_disable(_: Value, args: Value) -> Value {
    unsafe {
        let Some([]) = operands(args) else { return null_mut(); };
        let previous = ACTIVE.swap(false, Ordering::SeqCst);
        if previous {
            for (index, number) in SIGNALS.iter().enumerate() {
                signal(*number, PREVIOUS[index].load(Ordering::Relaxed));
            }
        }
        HEADER.store(null_mut(), Ordering::SeqCst);
        *HEADER_ROOT.lock().unwrap() = None;
        replace(std::iter::empty());
        object::bool_value(previous)
    }
}

unsafe extern "C" fn fault_dump(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd, header]) = operands(args) else { return null_mut(); };
        let (Some(fd), Some(header)) = (descriptor(fd), bytes(header)) else { return null_mut(); };
        replace(crate::traceback::fault_codes().into_iter());
        dump(fd, header);
        if !enabled() { replace(std::iter::empty()); }
        object::none_value()
    }
}

unsafe extern "C" fn fault_c_stack(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd]) = operands(args) else { return null_mut(); };
        let Some(fd) = descriptor(fd) else { return null_mut(); };
        dump_c(fd);
        object::none_value()
    }
}

unsafe extern "C" fn fault_enabled(_: Value, args: Value) -> Value {
    unsafe {
        let Some([]) = operands(args) else { return null_mut(); };
        object::bool_value(enabled())
    }
}

unsafe extern "C" fn native_thread_id(_: Value, args: Value) -> Value {
    unsafe {
        let Some([]) = operands(args) else { return null_mut(); };
        crate::numeric::integer_from_usize(pthread_self())
    }
}

pub unsafe fn populate(globals: Value) -> bool {
    unsafe {
        for (name, entry) in [
            ("fault_enable", fault_enable as object::Native),
            ("fault_disable", fault_disable as object::Native),
            ("fault_dump", fault_dump as object::Native),
            ("fault_c_stack", fault_c_stack as object::Native),
            ("fault_enabled", fault_enabled as object::Native),
            ("native_thread_id", native_thread_id as object::Native),
        ] {
            let function = object::function(null_mut(), entry, name);
            let bound = object::bind(globals, name, function);
            object::release(function);
            if !bound { return false; }
        }
        true
    }
}

//! Traceback and frame records for exceptions in flight.
//!
//! Python exposes a raised exception's path as values: `__traceback__` is a
//! chain of traceback entries from the frame handling it toward the frame
//! that raised it, and each entry's frame names its code and line. Compiled
//! code reports errors through null results, so each function records its
//! entry where an error reaches its failure path: the line of the failing
//! operation, its code metadata, globals, and the activation it ran in. The
//! entries accumulate here while the error is pending and move to the
//! exception when a handler catches it. CPython builds the same chain while
//! unwinding (`PyTraceBack_Here`). SBCL instead walks the stack with its
//! compiled debug information when a backtrace is requested; Python needs the
//! entries to exist as values while the exception is still being handled.
//!
//! Frames are created only for exceptions in flight, so they carry what the
//! failure path knows: no local variables, and `f_back` is linked when the
//! exception reaches the calling frame.
use crate::object::{self, Data, Value};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::ffi::{CStr, c_char};
use std::io::Write;
use std::ptr::null_mut;

/// A compiled function's code metadata, emitted by the compiler as constant data.
#[repr(C)]
pub struct CodeInfo {
    pub name: *const c_char,
    pub qualname: *const c_char,
    pub filename: *const c_char,
    pub firstlineno: i32,
    pub fault_label: *const c_char,
}

impl CodeInfo {
    fn text(field: *const c_char) -> &'static str {
        if field.is_null() {
            return "<unknown>";
        }
        unsafe { CStr::from_ptr(field) }.to_str().unwrap_or("<unknown>")
    }

    pub fn name(&self) -> &'static str {
        Self::text(self.name)
    }

    pub fn qualname(&self) -> &'static str {
        Self::text(self.qualname)
    }

    pub fn filename(&self) -> &'static str {
        Self::text(self.filename)
    }
}

thread_local! {
    static CODES: RefCell<HashMap<usize, (Value, Value)>> = RefCell::new(HashMap::new());
    /// The pending error's traceback, outermost entry first, or NULL.
    static PENDING: Cell<Value> = const { Cell::new(null_mut()) };
    /// Live activations, innermost last: code metadata and retained globals.
    static FRAMES: RefCell<Vec<(*const CodeInfo, Value)>> = const { RefCell::new(Vec::new()) };
}

unsafe fn code_info(receiver: Value) -> *const CodeInfo {
    unsafe {
        if receiver.is_null() || object::is_fixnum(receiver) { return std::ptr::null(); }
        match &(*receiver).data {
            Data::Function { code_info, .. } => *code_info,
            Data::Frame { code, .. } => *code,
            _ => std::ptr::null(),
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_set_function_metadata(function: Value, info: *const CodeInfo, metadata: Value) {
    unsafe {
        let Data::Function { code_info, .. } = &mut (*function).data else { unreachable!() };
        *code_info = info;
        CODES.with_borrow_mut(|codes| {
            let entry = codes.entry(info as usize).or_insert((null_mut(), null_mut()));
            if entry.0.is_null() { entry.0 = object::retain(metadata); }
        });
    }
}

pub unsafe fn code_metadata(receiver: Value, cached: bool) -> Value {
    unsafe {
        let info = code_info(receiver);
        let value = CODES.with_borrow(|codes| codes.get(&(info as usize)).map_or(null_mut(), |entry| if cached { entry.1 } else { entry.0 }));
        object::retain(if value.is_null() { object::none_value() } else { value })
    }
}

pub unsafe fn cache_code(receiver: Value, code: Value) -> Value {
    unsafe {
        let info = code_info(receiver);
        if info.is_null() {
            object::error("TypeError", "native code metadata is unavailable");
            return null_mut();
        }
        CODES.with_borrow_mut(|codes| {
            let entry = codes.entry(info as usize).or_insert((null_mut(), null_mut()));
            object::release(std::mem::replace(&mut entry.1, object::retain(code)));
        });
        object::none_value()
    }
}

pub fn clear_code_cache() {
    let codes = CODES.with_borrow_mut(std::mem::take);
    for (metadata, code) in codes.into_values() {
        unsafe { object::release(metadata); object::release(code); }
    }
}

/// Enter a compiled function activation.
#[unsafe(no_mangle)]
pub extern "C" fn mule_frame_push(code: *const CodeInfo, globals: Value) {
    unsafe { object::retain(globals) };
    FRAMES.with_borrow_mut(|frames| frames.push((code, globals)));
    crate::faults::push(code);
}

/// Leave the innermost compiled function activation.
#[unsafe(no_mangle)]
pub extern "C" fn mule_frame_pop() {
    let popped = FRAMES.with_borrow_mut(|frames| frames.pop());
    crate::faults::pop();
    if let Some((_, globals)) = popped {
        unsafe { object::release(globals) };
    }
}

/// Move suspended activations into the currently running stack, preserving the
/// ownership of their retained globals. The returned boundary belongs to the
/// resumer; yielded activations move back out at that boundary.
pub fn enter_coroutine_frames(frames: &mut Vec<(*const CodeInfo, Value)>) -> usize {
    let boundary = FRAMES.with_borrow_mut(|active| {
        let boundary = active.len();
        active.append(frames);
        boundary
    });
    refresh_fault_frames();
    boundary
}

pub fn leave_coroutine_frames(boundary: usize) -> Vec<(*const CodeInfo, Value)> {
    let suspended = FRAMES.with_borrow_mut(|active| active.split_off(boundary));
    refresh_fault_frames();
    suspended
}

pub fn refresh_fault_frames() {
    if crate::faults::enabled() {
        FRAMES.with_borrow(|frames| crate::faults::replace(frames.iter().map(|(code, _)| *code)));
    }
}

pub fn fault_codes() -> Vec<*const CodeInfo> {
    FRAMES.with_borrow(|frames| frames.iter().map(|(code, _)| *code).collect())
}

/// A frame value for the activation `depth` steps out from the innermost
/// one, with its `back` chain built from the activations outside it, or NULL
/// when the stack is not that deep. The frame's line is its code's first
/// line: compiled bodies do not record a current line for live frames.
pub unsafe fn frame_at(depth: usize) -> Value {
    unsafe {
        let records: Vec<(*const CodeInfo, Value)> = FRAMES.with_borrow(|frames| frames.clone());
        if depth >= records.len() {
            return null_mut();
        }
        let mut back = null_mut();
        for (code, globals) in records[..records.len() - depth].iter() {
            back = object::new_value(Data::Frame {
                code: *code,
                line: i64::from((*(*code)).firstlineno),
                globals: object::retain(*globals),
                back,
            });
        }
        back
    }
}

/// Remove the pending error's traceback, owned.
pub fn take() -> Value {
    PENDING.with(|pending| pending.replace(null_mut()))
}

/// Make `traceback` (owned, or NULL) the pending error's traceback.
pub fn restore(traceback: Value) {
    PENDING.with(|pending| unsafe { object::release(pending.replace(traceback)) });
}

/// A new error starts with an empty traceback.
pub fn clear() {
    restore(null_mut());
}

/// Continue the traceback an exception already carries when it is raised
/// again, as CPython does. Call it before the exception becomes pending:
/// attribute access does not run while an error is pending.
pub unsafe fn resume(exception: Value) {
    unsafe {
        let traceback = crate::classes::mule_get_attr(exception, c"__traceback__".as_ptr());
        if traceback.is_null() {
            // Not an exception carrying a traceback; nothing to continue.
            object::clear_error();
        }
        let continued = if is_traceback(traceback) { object::retain(traceback) } else { null_mut() };
        object::release(traceback);
        restore(continued);
    }
}

fn is_traceback(value: Value) -> bool {
    !value.is_null() && !object::is_fixnum(value) && matches!(unsafe { &(*value).data }, Data::Traceback { .. })
}

/// Record that the pending error passed through a compiled function at
/// `line`. `activation` identifies the call; ordinary propagation records
/// one entry per activation. An explicit raise adds a new entry even when
/// the existing traceback already starts in this activation. `marker` is a
/// compiler-planned constant highlight, never parsed by the runtime.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_traceback_here(
    code: *const CodeInfo,
    line: i32,
    globals: Value,
    activation: *const u8,
    explicit_raise: i32,
    marker: *const c_char,
) {
    unsafe {
        if !object::has_error() {
            return;
        }
        let head = PENDING.with(Cell::get);
        if let Some(Data::Traceback { activation: recorded, .. }) = (!head.is_null()).then(|| &(*head).data) {
            if explicit_raise == 0 && *recorded == activation as usize {
                return;
            }
        }
        let frame = object::new_value(Data::Frame {
            code,
            line: i64::from(line),
            globals: object::retain(globals),
            back: null_mut(),
        });
        if let Some(Data::Traceback { frame: inner, .. }) = (!head.is_null()).then(|| &(*head).data) {
            if let Data::Frame { back, .. } = &mut (**inner).data {
                *back = object::retain(frame);
            }
        }
        // The new entry takes over the reference the pending slot held.
        let traceback = object::new_value(Data::Traceback {
            frame,
            line: i64::from(line),
            marker,
            next: PENDING.with(|pending| pending.replace(null_mut())),
            activation: activation as usize,
        });
        PENDING.with(|pending| pending.set(traceback));
    }
}

/// Field `index` of a traceback (`tb_frame`, `tb_lineno`, `tb_next`) or a
/// frame (code metadata as `(name, qualname, filename, firstlineno)`,
/// `f_lineno`, `f_globals`, `f_back`), owned.
pub unsafe fn field(value: Value, index: i64) -> Value {
    unsafe {
        let optional = |value: Value| object::retain(if value.is_null() { object::none_value() } else { value });
        if value.is_null() || object::is_fixnum(value) {
            object::error("TypeError", "expected a traceback or frame");
            return null_mut();
        }
        match (&(*value).data, index) {
            (Data::Traceback { frame, .. }, 0) => object::retain(*frame),
            (Data::Traceback { line, .. }, 1) => object::small(*line),
            (Data::Traceback { next, .. }, 2) => optional(*next),
            (Data::Frame { code, .. }, 0) => {
                let code = &**code;
                let values = [
                    text(code.name()),
                    text(code.qualname()),
                    text(code.filename()),
                    object::small(i64::from(code.firstlineno)),
                ];
                let tuple = object::mule_sequence_tuple(values.len() as isize, values.as_ptr());
                for value in values {
                    object::release(value);
                }
                tuple
            }
            (Data::Frame { line, .. }, 1) => object::small(*line),
            (Data::Frame { globals, .. }, 2) => optional(*globals),
            (Data::Frame { back, .. }, 3) => optional(*back),
            _ => {
                object::error("IndexError", "no such traceback field");
                null_mut()
            }
        }
    }
}

fn text(value: &str) -> Value {
    unsafe { object::new_value(Data::Text(crate::text::Text::from_rust_str(value), std::ptr::null_mut(), std::ptr::null_mut())) }
}

/// Write the pending error's traceback as CPython prints an uncaught one.
pub fn print_pending(output: &mut impl Write) {
    let head = PENDING.with(Cell::get);
    if head.is_null() {
        return;
    }
    let _ = writeln!(output, "Traceback (most recent call last):");
    let show_ranges = std::env::var_os("PYTHONNODEBUGRANGES").is_none();
    let mut entry = head;
    while let Some(Data::Traceback { frame, line, next, marker, .. }) = (!entry.is_null()).then(|| unsafe { &(*entry).data }) {
        if let Data::Frame { code, .. } = unsafe { &(**frame).data } {
            let code = unsafe { &**code };
            let _ = writeln!(output, "  File \"{}\", line {line}, in {}", code.filename(), code.name());
            if let Some(source) = source_line(code.filename(), *line) {
                let _ = writeln!(output, "    {source}");
                if show_ranges && !marker.is_null() {
                    let _ = writeln!(output, "    {}", CodeInfo::text(*marker));
                }
            }
        }
        entry = *next;
    }
}

fn source_line(filename: &str, line: i64) -> Option<String> {
    let index = usize::try_from(line).ok()?.checked_sub(1)?;
    let text = std::fs::read_to_string(filename).ok()?;
    let source = text.lines().nth(index)?.trim();
    (!source.is_empty()).then(|| source.to_owned())
}

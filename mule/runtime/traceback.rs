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
use crate::hashing::FastMap as HashMap;
use std::ffi::{CStr, c_char};
use std::io::Write;
use std::ptr::null_mut;
use std::sync::atomic::{AtomicPtr, Ordering, compiler_fence};

/// A compiled function's code metadata, emitted by the compiler as constant data.
#[repr(C)]
pub struct CodeInfo {
    pub name: *const c_char,
    pub qualname: *const c_char,
    pub filename: *const c_char,
    pub firstlineno: i32,
    pub fault_label: *const c_char,
}

/// A compiler-owned native stack record. Code and globals are borrowed from
/// the running callable/module; materialized Python frames retain globals.
#[repr(C)]
pub struct Activation {
    pub parent: *mut Activation,
    pub code: *const CodeInfo,
    pub globals: Value,
}

/// Records remain in their coroutine's mapped stack while it is suspended.
#[derive(Default)]
pub struct DetachedFrames {
    head: *mut Activation,
    bottom: *mut Activation,
}

impl DetachedFrames {
    pub fn clear(&mut self) {
        self.head = null_mut();
        self.bottom = null_mut();
    }
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

    pub fn filename(&self) -> String {
        let raw = Self::text(self.filename);
        let Some(file_name) = std::path::Path::new(raw).file_name() else {
            return raw.to_owned();
        };
        if let Ok(exe) = std::env::current_exe() {
            let is_step_cache = exe
                .parent()
                .and_then(|p| p.parent())
                .and_then(|p| p.file_name())
                .is_some_and(|n| n == "steps");
            if !is_step_cache {
                if let Some(dir) = exe.parent() {
                    let candidate = dir.join(file_name);
                    if candidate.is_file() {
                        return candidate.to_string_lossy().into_owned();
                    }
                }
            }
        }
        if let Some(argv0) = std::env::args_os().next() {
            if argv0 != "-c" && argv0 != "-" {
                let p = std::path::PathBuf::from(argv0);
                if p.file_name() == Some(file_name) && p.is_file() {
                    return p.to_string_lossy().into_owned();
                }
                if let Some(dir) = p.parent() {
                    let candidate = dir.join(file_name);
                    if candidate.is_file() {
                        return candidate.to_string_lossy().into_owned();
                    }
                }
            }
        }
        raw.to_owned()
    }
}

thread_local! {
    static CODES: RefCell<HashMap<usize, (Value, Value)>> = RefCell::new(HashMap::with_hasher(crate::hashing::FastBuild::new()));
    /// The pending error's traceback, outermost entry first, or NULL.
    static PENDING: Cell<Value> = const { Cell::new(null_mut()) };
    /// Constant TLS has no lazy initialization or destructor on signal access.
    static ACTIVE_FRAME: AtomicPtr<Activation> = const { AtomicPtr::new(null_mut()) };
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

/// Physical TLS storage for compiler-emitted atomic link/unlink operations.
/// Reacquire this address after any suspension; it belongs to the current thread.
#[unsafe(no_mangle)]
#[inline(never)]
pub extern "C" fn mule_frame_head_slot() -> *mut *mut Activation {
    // A stackful coroutine can resume on another native thread. Keep this
    // physical lookup distinct across suspension even with whole-program LTO.
    compiler_fence(Ordering::SeqCst);
    ACTIVE_FRAME.with(|head| head.as_ptr())
}

pub fn active_head() -> *mut Activation {
    unsafe { AtomicPtr::from_ptr(mule_frame_head_slot()).load(Ordering::Acquire) }
}

fn publish_head(value: *mut Activation) {
    unsafe { AtomicPtr::from_ptr(mule_frame_head_slot()).store(value, Ordering::Release); }
}

/// Attach a private coroutine chain above its resumer's live records.
/// All record storage remains owned by the corresponding mapped native stack.
pub unsafe fn enter_coroutine_frames(frames: &mut DetachedFrames) -> *mut Activation {
    unsafe {
        let boundary = active_head();
        if !frames.head.is_null() {
            (*frames.bottom).parent = boundary;
            publish_head(frames.head);
            frames.clear();
        }
        boundary
    }
}

/// Detach before mutating any parent link or allowing stack storage to disappear.
pub unsafe fn leave_coroutine_frames(boundary: *mut Activation) -> DetachedFrames {
    unsafe {
        let head = active_head();
        publish_head(boundary);
        if head == boundary {
            return DetachedFrames::default();
        }
        let mut bottom = head;
        loop {
            assert!(!bottom.is_null(), "coroutine frame chain lost its resumer boundary");
            if (*bottom).parent == boundary {
                break;
            }
            bottom = (*bottom).parent;
        }
        (*bottom).parent = null_mut();
        DetachedFrames { head, bottom }
    }
}

/// A frame value for the activation `depth` steps out from the innermost
/// one, with its `back` chain built from the activations outside it, or NULL
/// when the stack is not that deep. The frame's line is its code's first
/// line: compiled bodies do not record a current line for live frames.
pub unsafe fn frame_at(depth: usize) -> Value {
    unsafe {
        let mut cursor = active_head();
        for _ in 0..depth {
            if cursor.is_null() { return null_mut(); }
            cursor = (*cursor).parent;
        }
        let mut records = Vec::new();
        while !cursor.is_null() {
            records.push(((*cursor).code, (*cursor).globals));
            cursor = (*cursor).parent;
        }
        let mut back = null_mut();
        for (code, globals) in records.iter().rev() {
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
        let traceback = crate::classes::get_exception_traceback(exception);
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

/// Materialize `Data::Frame` records along a traceback chain on first access
/// to `tb_frame` or `tb_next`, linking each inner frame's `f_back` to the
/// outer frame.
unsafe fn ensure_frame(tb: Value) -> Value {
    unsafe {
        let mut cursor = tb;
        let mut outer_frame: Value = null_mut();
        while let Some(Data::Traceback {
            frame,
            code,
            globals,
            line,
            next,
            ..
        }) = (!cursor.is_null()).then(|| &(*cursor).data)
        {
            let mut current_frame = frame.get();
            if current_frame.is_null() {
                current_frame = object::new_value(Data::Frame {
                    code: *code,
                    line: *line,
                    globals: object::retain(*globals),
                    back: null_mut(),
                });
                frame.set(current_frame);
            }
            if !outer_frame.is_null() {
                if let Data::Frame { back, .. } = &mut (*current_frame).data {
                    if back.is_null() {
                        *back = object::retain(outer_frame);
                    }
                }
            }
            outer_frame = current_frame;
            cursor = *next;
        }
        let Data::Traceback { frame, .. } = &(*tb).data else {
            return null_mut();
        };
        frame.get()
    }
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
        // The new entry takes over the reference the pending slot held.
        let traceback = object::new_value(Data::Traceback {
            frame: Cell::new(null_mut()),
            code,
            globals: object::retain(globals),
            line: i64::from(line),
            marker,
            next: PENDING.with(|pending| pending.replace(null_mut())),
            activation: activation as usize,
        });
        if let Some(Data::Traceback { frame: inner, .. }) = (!head.is_null()).then(|| &(*head).data) {
            if !inner.get().is_null() {
                let outer_frame = ensure_frame(traceback);
                if let Data::Frame { back, .. } = &mut (*inner.get()).data {
                    if back.is_null() {
                        *back = object::retain(outer_frame);
                    }
                }
            }
        }
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
            (Data::Traceback { .. }, 0) => object::retain(ensure_frame(value)),
            (Data::Traceback { line, .. }, 1) => object::small(*line),
            (Data::Traceback { next, .. }, 2) => {
                if !next.is_null() {
                    let _ = ensure_frame(value);
                }
                optional(*next)
            }
            (Data::Frame { code, .. }, 0) => {
                let code = &**code;
                let fname = code.filename();
                let values = [
                    text(code.name()),
                    text(code.qualname()),
                    text(&fname),
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
    while let Some(Data::Traceback { code, line, next, marker, .. }) = (!entry.is_null()).then(|| unsafe { &(*entry).data }) {
        let code = unsafe { &**code };
        let fname = code.filename();
        let _ = writeln!(output, "  File \"{fname}\", line {line}, in {}", code.name());
        if let Some(source) = source_line(&fname, *line) {
            let _ = writeln!(output, "    {source}");
            if show_ranges && !marker.is_null() {
                let _ = writeln!(output, "    {}", CodeInfo::text(*marker));
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

#[cfg(test)]
mod tests {
    use super::*;

    fn record(parent: *mut Activation) -> Activation {
        Activation { parent, code: std::ptr::null(), globals: null_mut() }
    }

    #[test]
    fn activation_layout_matches_compiler_storage() {
        assert_eq!(std::mem::size_of::<Activation>(), 24);
        assert_eq!(std::mem::align_of::<Activation>(), 8);
        assert_eq!(std::mem::offset_of!(Activation, parent), 0);
        assert_eq!(std::mem::offset_of!(Activation, code), 8);
        assert_eq!(std::mem::offset_of!(Activation, globals), 16);
    }

    #[test]
    fn detached_nested_chains_reconnect_to_the_current_resumer() {
        unsafe {
            assert!(active_head().is_null());
            let mut first = record(null_mut());
            let mut second = record(null_mut());
            let mut outer = record(&mut first);
            let mut inner = record(&mut outer);
            ACTIVE_FRAME.with(|head| head.store(&mut inner, Ordering::Release));
            let mut nested = leave_coroutine_frames(&mut outer);
            assert_eq!(active_head(), &mut outer as *mut Activation);
            assert!(inner.parent.is_null());
            let mut suspended = leave_coroutine_frames(&mut first);
            assert_eq!(active_head(), &mut first as *mut Activation);
            assert!(outer.parent.is_null());

            ACTIVE_FRAME.with(|head| head.store(&mut second, Ordering::Release));
            let boundary = enter_coroutine_frames(&mut suspended);
            assert_eq!(boundary, &mut second as *mut Activation);
            assert_eq!(outer.parent, boundary);
            let nested_boundary = enter_coroutine_frames(&mut nested);
            assert_eq!(nested_boundary, &mut outer as *mut Activation);
            assert_eq!(inner.parent, nested_boundary);
            let _nested = leave_coroutine_frames(nested_boundary);
            let _outer = leave_coroutine_frames(boundary);
            let mut empty = DetachedFrames::default();
            assert_eq!(enter_coroutine_frames(&mut empty), boundary);
            assert!(leave_coroutine_frames(boundary).head.is_null());
            assert_eq!(active_head(), boundary);
            ACTIVE_FRAME.with(|head| head.store(null_mut(), Ordering::Release));
        }
    }
}

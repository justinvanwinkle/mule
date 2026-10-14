//! Generator execution on separate native stacks.
//!
//! A generator body runs on its own stack. `resume` switches to that stack
//! and `suspend` (the `yield` primitive) switches back, transferring one
//! value each way. Mule reports exceptions through error results rather than
//! unwinding, so a thrown exception is a pending error at the point where
//! `suspend` returns, and the body's ordinary error paths run its `finally`
//! blocks and release its references. The generator protocol itself
//! (`send`, `throw`, `close`, `StopIteration`) is compiled Python.
//!
//! SBCL has no generators; its threads are the nearest runtime mechanism,
//! and this is the same OS-level service: a stack, a saved register context
//! and a switch. The context switch saves the System V AMD64 callee-saved
//! registers and the floating-point control state.
use crate::object::{self, Data, Value};
use std::cell::Cell;
use std::ffi::c_void;
use std::io::Write;
use std::ptr::null_mut;

#[cfg(not(target_arch = "x86_64"))]
compile_error!("Mule generators currently implement the x86-64 context switch only");

const STACK_SIZE: usize = 8 << 20;
const GUARD_SIZE: usize = 4096;

const CREATED: u8 = 0;
const SUSPENDED: u8 = 1;
const RUNNING: u8 = 2;
const FINISHED: u8 = 3;

unsafe extern "C" {
    fn mmap(address: *mut c_void, length: usize, protection: i32, flags: i32, fd: i32, offset: i64)
    -> *mut c_void;
    fn munmap(address: *mut c_void, length: usize) -> i32;
    fn mprotect(address: *mut c_void, length: usize, protection: i32) -> i32;
    fn mule_context_switch(save: *mut *mut u8, target: *mut u8);
}

const PROT_NONE: i32 = 0;
const PROT_READ: i32 = 1;
const PROT_WRITE: i32 = 2;
const MAP_PRIVATE: i32 = 0x02;
const MAP_ANONYMOUS: i32 = 0x20;
const MAP_NORESERVE: i32 = 0x4000;
const MAP_STACK: i32 = 0x20000;
const MAP_FAILED: *mut c_void = !0usize as *mut c_void;

// mule_context_switch(save: *mut *mut u8 [rdi], target: *mut u8 [rsi]) saves
// the callee-saved state on the current stack, stores the stack pointer
// through `save`, and resumes the context whose stack pointer is `target`.
// A new context starts in mule_coroutine_start with the coroutine in r12.
std::arch::global_asm!(
    ".globl mule_context_switch",
    ".type mule_context_switch,@function",
    "mule_context_switch:",
    "push rbp",
    "push rbx",
    "push r12",
    "push r13",
    "push r14",
    "push r15",
    "sub rsp, 8",
    "stmxcsr [rsp]",
    "fnstcw [rsp + 4]",
    "mov [rdi], rsp",
    "mov rsp, rsi",
    "ldmxcsr [rsp]",
    "fldcw [rsp + 4]",
    "add rsp, 8",
    "pop r15",
    "pop r14",
    "pop r13",
    "pop r12",
    "pop rbx",
    "pop rbp",
    "ret",
    ".size mule_context_switch, . - mule_context_switch",
    ".globl mule_coroutine_start",
    ".type mule_coroutine_start,@function",
    "mule_coroutine_start:",
    "mov rdi, r12",
    "and rsp, -16",
    "call {main}",
    "ud2",
    ".size mule_coroutine_start, . - mule_coroutine_start",
    main = sym coroutine_main,
);

unsafe extern "C" {
    fn mule_coroutine_start();
}

thread_local! {
    static CURRENT: Cell<*mut Coroutine> = const { Cell::new(null_mut()) };
}

pub struct Coroutine {
    stack: *mut u8,
    /// The suspended coroutine's stack pointer.
    context: *mut u8,
    /// The resumer's stack pointer while the coroutine runs.
    resumer: *mut u8,
    state: u8,
    function: Value,
    arguments: Value,
    /// The value passed by the latest switch: yielded, sent or returned.
    transfer: Value,
    /// The coroutine's own native frames, outside the resumer's depth.
    depth: usize,
    frames: Vec<(*const crate::traceback::CodeInfo, Value)>,
    handlers: Vec<Value>,
    parent: *mut Coroutine,
}

unsafe extern "C" fn coroutine_main(coroutine: *mut Coroutine) -> ! {
    unsafe {
        let arguments = match &(*(*coroutine).arguments).data {
            Data::Args(values, ..) => values.clone(),
            _ => Vec::new(),
        };
        let result = object::call((*coroutine).function, &arguments);
        (*coroutine).transfer = result;
        (*coroutine).state = FINISHED;
        mule_context_switch(&mut (*coroutine).context, (*coroutine).resumer);
        unreachable!("a finished coroutine is never resumed");
    }
}

impl Coroutine {
    unsafe fn new(function: Value, arguments: Value) -> Option<Box<Self>> {
        unsafe {
            let stack = mmap(
                null_mut(),
                STACK_SIZE,
                PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE | MAP_STACK,
                -1,
                0,
            );
            if stack == MAP_FAILED {
                object::error("MemoryError", "generator stack allocation failed");
                return None;
            }
            // The lowest page faults on overflow instead of corrupting memory.
            mprotect(stack, GUARD_SIZE, PROT_NONE);
            let mut coroutine = Box::new(Coroutine {
                stack: stack.cast(),
                context: null_mut(),
                resumer: null_mut(),
                state: CREATED,
                function: object::retain(function),
                arguments: object::retain(arguments),
                transfer: null_mut(),
                depth: 0,
                frames: Vec::new(),
                handlers: Vec::new(),
                parent: null_mut(),
            });
            // The initial frame mule_context_switch restores: control words,
            // r15..r12 (r12 carries the coroutine), rbx, rbp and the start address.
            let top = (stack as usize + STACK_SIZE) & !15;
            let frame = (top - 9 * 8) as *mut u64;
            let control: u64 = 0x1F80 | (0x037F << 32);
            let words = [
                control,
                0,
                0,
                0,
                &mut *coroutine as *mut Coroutine as u64,
                0,
                0,
                mule_coroutine_start as *const () as u64,
            ];
            for (index, word) in words.iter().enumerate() {
                *frame.add(index) = *word;
            }
            coroutine.context = frame.cast();
            Some(coroutine)
        }
    }

    /// Switch into the coroutine with `value` (owned by the caller, borrowed
    /// here) or, when `value` is NULL, with the pending error thrown into it.
    /// Returns the yielded or returned value, or NULL with an error.
    unsafe fn resume(&mut self, value: Value) -> Value {
        unsafe {
            match self.state {
                RUNNING => {
                    object::error("ValueError", "generator already executing");
                    return null_mut();
                }
                FINISHED => {
                    object::error("SystemError", "resumed a finished coroutine");
                    return null_mut();
                }
                _ => (),
            }
            self.transfer = object::retain(value);
            self.state = RUNNING;
            self.parent = CURRENT.with(|current| current.replace(self));
            let base = object::frame_depth();
            let frame_boundary = crate::traceback::enter_coroutine_frames(&mut self.frames);
            let handler_boundary = crate::sequences::enter_coroutine_handlers(&mut self.handlers);
            object::set_frame_depth(base + self.depth);
            mule_context_switch(&mut self.resumer, self.context);
            self.handlers = crate::sequences::leave_coroutine_handlers(handler_boundary);
            self.frames = crate::traceback::leave_coroutine_frames(frame_boundary);
            self.depth = object::frame_depth() - base;
            object::set_frame_depth(base);
            CURRENT.with(|current| current.set(self.parent));
            self.parent = null_mut();
            std::mem::replace(&mut self.transfer, null_mut())
        }
    }
}

impl Drop for Coroutine {
    fn drop(&mut self) {
        unsafe {
            // A suspended generator still owns references in its frames:
            // close it by throwing GeneratorExit, as CPython's finalizer does.
            if self.state == SUSPENDED {
                let saved = object::save_error();
                object::error("GeneratorExit", "");
                let result = self.resume(null_mut());
                let leaked = !result.is_null() && self.state != FINISHED;
                object::release(result);
                if leaked {
                    report("RuntimeError: generator ignored GeneratorExit");
                } else if object::has_error() {
                    let kind = object::error_kind();
                    if kind != "GeneratorExit" && kind != "StopIteration" {
                        report(&format!("{kind}: {}", object::error_message()));
                    }
                }
                object::clear_error();
                object::restore_error(saved);
                if leaked {
                    // Its frames can never unwind; keep the stack mapped.
                    object::release(self.function);
                    object::release(self.arguments);
                    return;
                }
            }
            object::release(self.transfer);
            object::release(self.function);
            object::release(self.arguments);
            for (_, globals) in self.frames.drain(..) {
                object::release(globals);
            }
            for exception in self.handlers.drain(..) {
                object::release(exception);
            }
            munmap(self.stack.cast(), STACK_SIZE);
        }
    }
}

fn report(message: &str) {
    let text = format!("Exception ignored while closing generator: {message}\n");
    let _ = std::io::stderr().lock().write_all(text.as_bytes());
}

unsafe fn coroutine<'a>(handle: Value) -> Option<&'a mut Coroutine> {
    unsafe {
        if !handle.is_null() && !object::is_fixnum(handle) {
            if let Data::Coroutine(coroutine) = &mut (*handle).data {
                return Some(coroutine);
            }
        }
        object::error("TypeError", "coroutine storage is required");
        None
    }
}

/// The `yield` primitive: suspend the running coroutine with `value` and
/// return the value it is resumed with, or NULL when an exception is thrown in.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_coroutine_yield(value: Value) -> Value {
    unsafe {
        if object::has_error() {
            return null_mut();
        }
        let current = CURRENT.with(Cell::get);
        if current.is_null() {
            object::error("SystemError", "yield outside a running generator");
            return null_mut();
        }
        let coroutine = &mut *current;
        coroutine.transfer = object::retain(value);
        coroutine.state = SUSPENDED;
        mule_context_switch(&mut coroutine.context, coroutine.resumer);
        coroutine.state = RUNNING;
        std::mem::replace(&mut coroutine.transfer, null_mut())
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

/// `coroutine_new(function, arguments)`: a coroutine that will call
/// `function(*arguments)` on its own stack when first resumed.
unsafe extern "C" fn coroutine_new(_: Value, args: Value) -> Value {
    unsafe {
        let Some([function, arguments]) = operands(args) else {
            return null_mut();
        };
        if !matches!(&(*arguments).data, Data::Args(..)) {
            object::error("TypeError", "coroutine arguments must be a tuple");
            return null_mut();
        }
        match Coroutine::new(function, arguments) {
            Some(coroutine) => object::new_value(Data::Coroutine(coroutine)),
            None => null_mut(),
        }
    }
}

/// `coroutine_resume(handle, value)`: send a value; returns what it yields or returns.
unsafe extern "C" fn coroutine_resume(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle, value]) = operands(args) else {
            return null_mut();
        };
        let Some(coroutine) = coroutine(handle) else {
            return null_mut();
        };
        coroutine.resume(value)
    }
}

/// `coroutine_throw(handle, exception)`: resume with `exception` raised at
/// the suspended `yield`.
unsafe extern "C" fn coroutine_throw(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle, exception]) = operands(args) else {
            return null_mut();
        };
        let Some(coroutine) = coroutine(handle) else {
            return null_mut();
        };
        if coroutine.state == RUNNING {
            object::error("ValueError", "generator already executing");
            return null_mut();
        }
        crate::traceback::resume(exception);
        crate::sequences::restore_pending(object::retain(exception));
        coroutine.resume(null_mut())
    }
}

/// 0 created, 1 suspended, 2 running, 3 finished.
unsafe extern "C" fn coroutine_state(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle]) = operands(args) else {
            return null_mut();
        };
        let Some(coroutine) = coroutine(handle) else {
            return null_mut();
        };
        object::small(i64::from(coroutine.state))
    }
}

/// Mark a created or suspended coroutine finished without running it, for
/// a generator closed before it started.
unsafe extern "C" fn coroutine_abandon(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle]) = operands(args) else {
            return null_mut();
        };
        let Some(coroutine) = coroutine(handle) else {
            return null_mut();
        };
        if coroutine.state == CREATED {
            coroutine.state = FINISHED;
        }
        object::none_value()
    }
}

/// Bind the generator stack operations into the compiler-owned native namespace.
pub unsafe fn populate(globals: Value) -> bool {
    unsafe {
        if object::has_error() {
            return false;
        }
        let yield_function = object::function(null_mut(), yield_export, "coroutine_yield");
        let bound = object::bind(globals, "coroutine_yield", yield_function);
        object::release(yield_function);
        if !bound {
            return false;
        }
        let exports: [(&str, object::Native); 5] = [
            ("coroutine_new", coroutine_new),
            ("coroutine_resume", coroutine_resume),
            ("coroutine_throw", coroutine_throw),
            ("coroutine_state", coroutine_state),
            ("coroutine_abandon", coroutine_abandon),
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

unsafe extern "C" fn yield_export(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return null_mut();
        };
        mule_coroutine_yield(value)
    }
}

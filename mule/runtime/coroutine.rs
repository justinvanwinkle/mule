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
//! and a switch. The context switch saves the target ABI's callee-saved
//! registers and the floating-point control state.
use crate::object::{self, Data, Value};
use std::cell::{Cell, RefCell};
use std::ffi::c_void;
use std::io::Write;
use std::ptr::null_mut;
use std::sync::Mutex;

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
compile_error!("Mule generators currently implement x86-64 and AArch64 context switches only");

const STACK_SIZE: usize = 8 << 20;
const GUARD_SIZE: usize = 4096;
const LOCAL_STACK_POOL_CAP: usize = 32;
const GLOBAL_STACK_POOL_CAP: usize = 64;

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
#[cfg(target_arch = "x86_64")]
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

// mule_context_switch(save: *mut *mut u8 [x0], target: *mut u8 [x1]) saves
// the AAPCS64 callee-saved registers (x19..x30, d8..d15) and fpcr on the
// current stack, stores the stack pointer through `save`, and resumes the
// context whose stack pointer is `target`. A new context starts in
// mule_coroutine_start with the coroutine in x19.
#[cfg(target_arch = "aarch64")]
std::arch::global_asm!(
    ".globl mule_context_switch",
    ".type mule_context_switch,%function",
    "mule_context_switch:",
    "stp x29, x30, [sp, #-176]!",
    "stp x19, x20, [sp, #16]",
    "stp x21, x22, [sp, #32]",
    "stp x23, x24, [sp, #48]",
    "stp x25, x26, [sp, #64]",
    "stp x27, x28, [sp, #80]",
    "stp d8, d9, [sp, #96]",
    "stp d10, d11, [sp, #112]",
    "stp d12, d13, [sp, #128]",
    "stp d14, d15, [sp, #144]",
    "mrs x9, fpcr",
    "str x9, [sp, #160]",
    "mov x9, sp",
    "str x9, [x0]",
    "mov sp, x1",
    "ldr x9, [sp, #160]",
    "msr fpcr, x9",
    "ldp d14, d15, [sp, #144]",
    "ldp d12, d13, [sp, #128]",
    "ldp d10, d11, [sp, #112]",
    "ldp d8, d9, [sp, #96]",
    "ldp x27, x28, [sp, #80]",
    "ldp x25, x26, [sp, #64]",
    "ldp x23, x24, [sp, #48]",
    "ldp x21, x22, [sp, #32]",
    "ldp x19, x20, [sp, #16]",
    "ldp x29, x30, [sp], #176",
    "ret",
    ".size mule_context_switch, . - mule_context_switch",
    ".globl mule_coroutine_start",
    ".type mule_coroutine_start,%function",
    "mule_coroutine_start:",
    "mov x0, x19",
    "bl {main}",
    "brk #0x1",
    ".size mule_coroutine_start, . - mule_coroutine_start",
    main = sym coroutine_main,
);

unsafe extern "C" {
    fn mule_coroutine_start();
}

static GLOBAL_STACK_POOL: Mutex<Vec<usize>> = Mutex::new(Vec::new());

struct LocalStackPool {
    stacks: Vec<*mut u8>,
}

impl Drop for LocalStackPool {
    fn drop(&mut self) {
        if self.stacks.is_empty() {
            return;
        }
        let mut remaining = std::mem::take(&mut self.stacks);
        if let Ok(mut global) = GLOBAL_STACK_POOL.lock() {
            while global.len() < GLOBAL_STACK_POOL_CAP {
                let Some(stack) = remaining.pop() else {
                    break;
                };
                global.push(stack as usize);
            }
        }
        for stack in remaining {
            unsafe {
                munmap(stack.cast(), STACK_SIZE);
            }
        }
    }
}

thread_local! {
    static CURRENT: Cell<*mut Coroutine> = const { Cell::new(null_mut()) };
    static LOCAL_STACK_POOL: RefCell<LocalStackPool> = const {
        RefCell::new(LocalStackPool { stacks: Vec::new() })
    };
}

/// Acquire a coroutine control stack from the thread-local or global recycle bin,
/// falling back to `mmap` + `mprotect` on a miss, following SBCL's
/// `get_recyclebin_item` (`src/runtime/thread.c`).
unsafe fn get_recyclebin_stack() -> *mut u8 {
    if let Some(stack) = LOCAL_STACK_POOL
        .try_with(|pool| pool.borrow_mut().stacks.pop())
        .ok()
        .flatten()
    {
        return stack;
    }
    if let Ok(mut global) = GLOBAL_STACK_POOL.lock() {
        if let Some(addr) = global.pop() {
            return addr as *mut u8;
        }
    }
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
            return null_mut();
        }
        // The lowest page faults on overflow instead of corrupting memory.
        mprotect(stack, GUARD_SIZE, PROT_NONE);
        stack.cast()
    }
}

/// Return a finished coroutine's control stack to the recycle bin, or `munmap`
/// it when both bins are at capacity, following SBCL's `put_recyclebin_item`
/// (`src/runtime/thread.c`).
unsafe fn put_recyclebin_stack(stack: *mut u8) {
    if stack.is_null() {
        return;
    }
    let stored_local = LOCAL_STACK_POOL
        .try_with(|pool| {
            let mut pool = pool.borrow_mut();
            if pool.stacks.len() < LOCAL_STACK_POOL_CAP {
                pool.stacks.push(stack);
                true
            } else {
                false
            }
        })
        .unwrap_or(false);
    if stored_local {
        return;
    }
    if let Ok(mut global) = GLOBAL_STACK_POOL.lock() {
        if global.len() < GLOBAL_STACK_POOL_CAP {
            global.push(stack as usize);
            return;
        }
    }
    unsafe {
        munmap(stack.cast(), STACK_SIZE);
    }
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
    frames: crate::traceback::DetachedFrames,
    handlers: Vec<Value>,
    parent: *mut Coroutine,
    yield_from: Value,
    locals_ptr: *const Value,
    locals_len: usize,
}

unsafe extern "C" fn coroutine_main(coroutine: *mut Coroutine) -> ! {
    unsafe {
        let (ptr, len) = match &(*(*coroutine).arguments).data {
            Data::Args(values, ..) => (values.as_ptr(), values.len()),
            _ => (std::ptr::NonNull::<Value>::dangling().as_ptr().cast_const(), 0),
        };
        let arguments = std::slice::from_raw_parts(ptr, len);
        let result = object::call((*coroutine).function, arguments);
        (*coroutine).transfer = result;
        (*coroutine).state = FINISHED;
        (*coroutine).locals_ptr = null_mut();
        (*coroutine).locals_len = 0;
        object::release(std::mem::replace(&mut (*coroutine).yield_from, null_mut()));
        mule_context_switch(&mut (*coroutine).context, (*coroutine).resumer);
        unreachable!("a finished coroutine is never resumed");
    }
}

impl Coroutine {
    unsafe fn new(function: Value, arguments: Value) -> Option<Box<Self>> {
        unsafe {
            let stack = get_recyclebin_stack();
            if stack.is_null() {
                object::error("MemoryError", "generator stack allocation failed");
                return None;
            }
            let mut coroutine = Box::new(Coroutine {
                stack,
                context: null_mut(),
                resumer: null_mut(),
                state: CREATED,
                function: object::retain(function),
                arguments: object::retain(arguments),
                transfer: null_mut(),
                depth: 0,
                frames: crate::traceback::DetachedFrames::default(),
                handlers: Vec::new(),
                parent: null_mut(),
                yield_from: null_mut(),
                locals_ptr: null_mut(),
                locals_len: 0,
            });
            let top = (stack as usize + STACK_SIZE) & !15;
            #[cfg(target_arch = "x86_64")]
            {
                // The initial frame mule_context_switch restores: control words,
                // r15..r12 (r12 carries the coroutine), rbx, rbp and the start address.
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
            }
            #[cfg(target_arch = "aarch64")]
            {
                // The initial frame mule_context_switch restores: x29, x30 (start address),
                // x19..x28 (x19 carries the coroutine), d8..d15 and fpcr.
                let frame = (top - 22 * 8) as *mut u64;
                let mut words = [0u64; 22];
                words[1] = mule_coroutine_start as *const () as u64;
                words[2] = &mut *coroutine as *mut Coroutine as u64;
                for (index, word) in words.iter().enumerate() {
                    *frame.add(index) = *word;
                }
                coroutine.context = frame.cast();
            }
            Some(coroutine)
        }
    }

    unsafe fn recycle_stack(&mut self) {
        let stack = std::mem::replace(&mut self.stack, null_mut());
        if !stack.is_null() {
            unsafe {
                put_recyclebin_stack(stack);
            }
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
            crate::sequences::leave_coroutine_handlers(handler_boundary, &mut self.handlers);
            self.frames = crate::traceback::leave_coroutine_frames(frame_boundary);
            self.depth = object::frame_depth() - base;
            object::set_frame_depth(base);
            CURRENT.with(|current| current.set(self.parent));
            self.parent = null_mut();
            if self.state == FINISHED {
                self.recycle_stack();
            }
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
                    object::release(self.yield_from);
                    return;
                }
            }
            object::release(self.transfer);
            object::release(self.function);
            object::release(self.arguments);
            object::release(self.yield_from);
            self.locals_ptr = null_mut();
            self.locals_len = 0;
            self.frames.clear();
            for exception in self.handlers.drain(..) {
                object::release(exception);
            }
            self.recycle_stack();
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

/// Advance `handle` with `None` for normal generator/coroutine iteration.
/// Returns `1` and writes the yielded value to `*out` when suspended;
/// returns `0` and writes the return value (`None` or owned value) to `*out`
/// when finished without error; returns `-1` when an error is pending.
pub(crate) unsafe fn step_coroutine_none(handle: Value, out: *mut Value) -> i32 {
    unsafe {
        let Some(coroutine) = coroutine(handle) else {
            return -1;
        };
        if coroutine.state == FINISHED {
            *out = object::none_value();
            return 0;
        }
        let result = coroutine.resume(object::none_value());
        if result.is_null() {
            return -1;
        }
        *out = result;
        if coroutine.state == FINISHED { 0 } else { 1 }
    }
}

///Return whether `handle` is in the `FINISHED` state.
pub(crate) unsafe fn is_coroutine_finished(handle: Value) -> bool {
    unsafe {
        if !handle.is_null() && !object::is_fixnum(handle) {
            if let Data::Coroutine(coroutine) = &(*handle).data {
                return coroutine.state == FINISHED;
            }
        }
        false
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

/// Register the current coroutine's stack-allocated local variable snapshot buffer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_coroutine_set_locals(ptr: *const Value, len: i64) {
    let current = CURRENT.with(Cell::get);
    if !current.is_null() {
        unsafe {
            (*current).locals_ptr = ptr;
            (*current).locals_len = len.max(0) as usize;
        }
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
            coroutine.recycle_stack();
        }
        object::none_value()
    }
}

unsafe fn clear_current_yieldfrom() {
    let current = CURRENT.with(Cell::get);
    if !current.is_null() {
        unsafe {
            let old = std::mem::replace(&mut (*current).yield_from, null_mut());
            if !old.is_null() {
                object::release(old);
            }
        }
    }
}

unsafe fn set_current_yieldfrom(value: Value) {
    let current = CURRENT.with(Cell::get);
    if !current.is_null() {
        unsafe {
            let new_val = if value.is_null() || value == object::none_value() {
                null_mut()
            } else {
                object::retain(value)
            };
            let old = std::mem::replace(&mut (*current).yield_from, new_val);
            if !old.is_null() {
                object::release(old);
            }
        }
    }
}

unsafe extern "C" fn coroutine_set_yieldfrom(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return null_mut();
        };
        set_current_yieldfrom(value);
        object::none_value()
    }
}

unsafe extern "C" fn coroutine_has_yieldfrom(_: Value, args: Value) -> Value {
    unsafe {
        let Some([]) = operands(args) else {
            return null_mut();
        };
        let current = CURRENT.with(Cell::get);
        let has = !current.is_null() && !(*current).yield_from.is_null();
        object::bool_value(has)
    }
}

/// `generator.__next__(self)` implemented directly over the coroutine handle.
unsafe extern "C" fn coroutine_next(_: Value, args: Value) -> Value {
    unsafe {
        let Some([gen_instance]) = operands(args) else {
            return null_mut();
        };
        if gen_instance.is_null() || object::is_fixnum(gen_instance) {
            object::error("TypeError", "expected generator object");
            return null_mut();
        }
        let Data::Instance { fields, .. } = &(*gen_instance).data else {
            object::error("TypeError", "expected generator object");
            return null_mut();
        };
        if fields.is_empty() {
            object::error("TypeError", "expected generator object");
            return null_mut();
        }
        let handle = fields[0];
        let mut out: Value = null_mut();
        let status = step_coroutine_none(handle, &mut out);
        if status == 1 {
            return out;
        }
        if status == 0 {
            crate::sequences::raise_stop_iteration_value(out);
            return null_mut();
        }
        if is_coroutine_finished(handle) {
            crate::sequences::convert_stop_iteration_to_runtime_error();
        }
        null_mut()
    }
}

/// Drive a subgenerator or coroutine `handle` until it finishes and returns its
/// final value, forwarding yielded values and sent responses directly.
unsafe extern "C" fn coroutine_delegate(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle, initial_sent, yieldfrom_obj, is_coro_val]) = operands(args) else {
            return null_mut();
        };
        let is_coro = is_coro_val == object::bool_value(true);
        let mut sent = object::retain(initial_sent);
        loop {
            let Some(coro) = coroutine(handle) else {
                object::release(sent);
                clear_current_yieldfrom();
                return null_mut();
            };
            if coro.state == FINISHED {
                object::release(sent);
                clear_current_yieldfrom();
                if is_coro {
                    object::error("RuntimeError", "cannot reuse already awaited coroutine");
                    return null_mut();
                }
                return object::none_value();
            }
            if coro.state == CREATED && sent != object::none_value() {
                object::release(sent);
                clear_current_yieldfrom();
                if is_coro {
                    object::error(
                        "TypeError",
                        "can't send non-None value to a just-started coroutine",
                    );
                } else {
                    object::error(
                        "TypeError",
                        "can't send non-None value to a just-started generator",
                    );
                }
                return null_mut();
            }
            clear_current_yieldfrom();
            let value = coro.resume(sent);
            object::release(sent);
            if value.is_null() {
                if coro.state == FINISHED {
                    crate::sequences::convert_stop_iteration_to_runtime_error();
                }
                return null_mut();
            }
            if coro.state == FINISHED {
                return value;
            }
            set_current_yieldfrom(yieldfrom_obj);
            let next_sent = mule_coroutine_yield(value);
            object::release(value);
            if next_sent.is_null() {
                return null_mut();
            }
            sent = next_sent;
        }
    }
}

unsafe extern "C" fn coroutine_yieldfrom(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle]) = operands(args) else {
            return null_mut();
        };
        let Some(coroutine) = coroutine(handle) else {
            return null_mut();
        };
        if coroutine.state == FINISHED || coroutine.yield_from.is_null() {
            object::none_value()
        } else {
            object::retain(coroutine.yield_from)
        }
    }
}

unsafe extern "C" fn coroutine_locals(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle]) = operands(args) else {
            return null_mut();
        };
        let Some(coroutine) = coroutine(handle) else {
            return null_mut();
        };
        if (coroutine.state != SUSPENDED && coroutine.state != RUNNING)
            || coroutine.locals_ptr.is_null()
        {
            return object::none_value();
        }
        let missing = crate::arguments::mule_binding_missing();
        let mut values = Vec::with_capacity(coroutine.locals_len);
        for index in 0..coroutine.locals_len {
            let val = *coroutine.locals_ptr.add(index);
            values.push(if val.is_null() { missing } else { val });
        }
        object::mule_sequence_tuple(values.len() as isize, values.as_ptr())
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
        let exports: [(&str, object::Native); 11] = [
            ("coroutine_new", coroutine_new),
            ("coroutine_resume", coroutine_resume),
            ("coroutine_throw", coroutine_throw),
            ("coroutine_state", coroutine_state),
            ("coroutine_abandon", coroutine_abandon),
            ("coroutine_set_yieldfrom", coroutine_set_yieldfrom),
            ("coroutine_has_yieldfrom", coroutine_has_yieldfrom),
            ("coroutine_yieldfrom", coroutine_yieldfrom),
            ("coroutine_locals", coroutine_locals),
            ("coroutine_next", coroutine_next),
            ("coroutine_delegate", coroutine_delegate),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::traceback::{Activation, active_head, mule_frame_head_slot};
    use std::sync::atomic::{AtomicPtr, Ordering};

    unsafe fn publish(slot: *mut *mut Activation, value: *mut Activation) {
        unsafe { AtomicPtr::from_ptr(slot).store(value, Ordering::Release); }
    }

    unsafe extern "C" fn migrating_body(_: Value, _: Value) -> Value {
        unsafe {
            let initial_slot = mule_frame_head_slot();
            let mut record = Activation {
                parent: active_head(), code: std::ptr::null(), globals: null_mut(),
            };
            publish(initial_slot, &mut record);
            let sent = mule_coroutine_yield(object::small(11));
            assert_eq!(sent, object::small(7));
            let resumed_slot = mule_frame_head_slot();
            assert_ne!(initial_slot, resumed_slot);
            assert_eq!(active_head(), &mut record as *mut Activation);
            // The bottom record's parent changes when another thread resumes it.
            publish(resumed_slot, record.parent);
            object::small(22)
        }
    }

    #[test]
    fn suspended_native_stack_resumes_with_new_thread_head_and_parent() {
        unsafe {
            assert!(active_head().is_null());
            let function = object::function(null_mut(), migrating_body, "migrating_body");
            let arguments = object::new_value(Data::Args(crate::value_buffer::ValueBuffer::new(), null_mut(), null_mut()));
            let mut coroutine = Coroutine::new(function, arguments).unwrap();
            object::release(function);
            object::release(arguments);
            assert_eq!(coroutine.resume(object::small(0)), object::small(11));
            assert!(active_head().is_null());

            // The original thread does not touch the coroutine until join.
            // This exercises the physical stack service, not Python thread APIs.
            let address = (&mut *coroutine as *mut Coroutine) as usize;
            std::thread::spawn(move || {
                let coroutine = &mut *(address as *mut Coroutine);
                let mut resumer = Activation {
                    parent: null_mut(), code: std::ptr::null(), globals: null_mut(),
                };
                publish(mule_frame_head_slot(), &mut resumer);
                assert_eq!(coroutine.resume(object::small(7)), object::small(22));
                assert_eq!(active_head(), &mut resumer as *mut Activation);
                publish(mule_frame_head_slot(), null_mut());
            }).join().unwrap();
            assert_eq!(coroutine.state, FINISHED);
            assert!(active_head().is_null());
            drop(coroutine);
        }
    }
}

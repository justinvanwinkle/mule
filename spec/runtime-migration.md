# Runtime migration: Rust to compiled Mule Python, C23, and pure-Mule substrate

Status: **proposed** migration roadmap and architectural evaluation. Currently,
Mule links `mule/runtime/*.rs` (~20.8k lines of Rust) with compiled protocol
modules in `stdlib/_*.py` (~16k lines of Mule Python). Approved implementation
languages remain Python and Rust (`AGENTS.md`); replacing the remaining foreign
runtime substrate with C23 or pure Mule LLVM primitives requires explicit user
approval before introducing any C source files into the active build.

---

## 1. Decomposing `mule/runtime/*.rs`

The current Rust runtime (`mule/runtime/*.rs`, ~20,800 lines across 28 files)
combines two distinct responsibilities:

1. **Language-level bootstrap debt (~75% of current Rust code):**
   Class and attribute dispatch (`mule/runtime/classes.rs`, 4,009 lines),
   high-level object operations and operator fallbacks (`mule/runtime/object.rs`,
   3,950 lines), sequence and text helpers (`mule/runtime/sequences.rs`,
   2,092 lines; `mule/runtime/text.rs`, 731 lines), process/OS wrapper conversion
   (`mule/runtime/process.rs`, 1,837 lines), and remaining builtins
   (`mule/runtime/builtins.rs`, 862 lines). Per `spec/python-changes.md` and
   `AGENTS.md`, language-level protocols belong in compiled Mule Python
   (`stdlib/_*.py`) and direct LLVM VOPs in `mule/compiler/vops.py`, matching how
   SBCL compiles its Lisp runtime in `~/checkout/sbcl/src/code/*.lisp`.
2. **Low-level runtime substrate (~25% of current Rust code, analogous to SBCL's `~/checkout/sbcl/src/runtime/*.c`):**
   Physical memory allocation (`mule/runtime/native_storage.rs`), reference
   counting and nonmoving cycle collection (`mule/runtime/gc.rs`), stackful
   generator context switching (`mule/runtime/coroutine.rs`), signal and hardware
   fault handling (`mule/runtime/signals.rs`, `mule/runtime/faults.rs`), raw OS
   and libc calls (`mule/runtime/networking.rs`, `mule/runtime/multiplex.rs`,
   `mule/runtime/spawn.rs`, `mule/runtime/terminal.rs`, `mule/runtime/threads.rs`,
   `mule/runtime/thread_identity.rs`), GMP bindings (`mule/runtime/gmp.rs`), and
   future CPython C-API compatibility shims (`spec/cpython-interface.md`).

---

## 2. Foreign Runtime Evaluation: C23 / C++ / Zig vs. Rust

As language-level algorithms move out of `mule/runtime/*.rs` into compiled Mule
Python, Rust's collection ergonomics matter less and its friction at the
LLVM/ABI boundary matters more.

### Where C23 (compiled with `clang -std=c23`) simplifies the foreign substrate

1. **Single compiler toolchain and bitcode inlining without full LTO:**
   - In `mule/compiler/driver.py` (`build_runtime` and `lto_linker`), Mule
     currently invokes `rustc` with `-C linker-plugin-lto -C lto=fat` and locates
     `rustc`'s internal `bin/gcc-ld/ld.lld` so the linker's LLVM version matches
     `rustc`'s bitcode format. Any LLVM major-version skew between system `clang`
     and `rustc` breaks cross-language LTO.
   - The host uses Clang 21 (`/usr/lib/llvm-21/bin/clang`), which supports
     `-std=c23`. Compiling a C23 runtime with the same `clang` that compiles
     Mule's `.ll` output eliminates `rustc` from the build and guarantees
     bitcode compatibility.
   - A proposed C23 runtime compiled with `clang -std=c23 -O3 -emit-llvm -c`
     would produce LLVM bitcode from the same LLVM installation. Inlining its
     primitives into each compilation unit without full LTO would require a new
     bitcode integration stage. The current driver compiles each LLVM unit whole
     through Clang and does not link runtime bitcode into individual units.
2. **Physical object layout transparency (`PyVarObject` vs. Rust `enum Data`):**
   - `Object` in `mule/runtime/object.rs` currently embeds a Rust `pub data: Data`
     enum holding `Vec<Value>`, `String`, and `HashMap`. Because Rust enum,
     `Vec`, and `String` layouts are opaque to `mule/compiler/vops.py` and sized
     to the largest variant, every object carries layout bloat or double
     indirection (`Object` -> `Vec`/`String` header -> heap buffer), and
     `vops.py` must emit `extern "C"` calls (`mule_storage_read`,
     `mule_text_length`, etc.) for basic payload reads.
   - In C23 (or compiler-known physical layouts), strings, bytes, and tuples can
     use single-allocation `PyVarObject`-style flexible array tails, and lists
     can store `{ size_t len, cap; Value *items; }` directly in the object
     header at fixed byte offsets verified with `static_assert(offsetof(...))`.
     `vops.py` can then emit direct LLVM `getelementptr` and `load`/`store`
     instructions.
3. **Standard C23 language features for runtime primitives:**
   - `<stdckdint.h>` (`ckd_add`, `ckd_sub`, `ckd_mul`) lowers directly to
     `@llvm.sadd.with.overflow.i64` and `@llvm.smul.with.overflow.i64`,
     replacing Rust's `checked_add`/`checked_mul` fast paths in
     `mule/runtime/numeric.rs`.
   - `<stdbit.h>` (`stdc_leading_zeros`, `stdc_bit_width`, `stdc_count_ones`)
     provides standard bit intrinsics for `mule/runtime/math.rs` and
     `mule/runtime/hashing.rs`.
   - Fixed-width enums (`enum ObjectKind : uint8_t`) guarantee exact tag byte
     width and offset for direct LLVM loads in `vops.py`.
   - `constexpr`, `auto`, `typeof`, `nullptr`, `bool`, `thread_local`,
     `_Atomic`, `[[nodiscard]]`, `[[noreturn]]`, and `= {}` zero-initialization
     avoid C99/C11 boilerplate without introducing hidden C++ control flow.
   - Direct `#include` of POSIX and library headers (`<signal.h>`,
     `<sys/mman.h>`, `<sys/epoll.h>`, `<termios.h>`, `<gmp.h>`) replaces hundreds
     of lines of hand-declared `#[repr(C)]` FFI structs in
     `mule/runtime/signals.rs`, `coroutine.rs`, `networking.rs`, `terminal.rs`,
     and `process.rs`.
4. **CPython C extension ABI (`spec/cpython-interface.md`):**
   - Implementing `Python.h` source and ABI compatibility requires exact C
     struct layouts (`PyObject`, `PyVarObject`, `PyTypeObject`), C header macros,
     and C variadic functions (`PyArg_ParseTuple`, `PyErr_Format`,
     `Py_BuildValue`) using `<stdarg.h>` (`va_list`).
5. **No borrow-checker ceremony over cyclic, intrusive graphs:**
   - Reference-counted objects with cyclic GC, interior mutability across
     reentrant Python callbacks, and raw stack/register inspection require raw
     pointers (`*mut Object` / `Value`) across almost every function in
     `mule/runtime/`, where Rust `unsafe` blocks add ceremony without safety.

### Language comparison summary

| Dimension | C23 (`clang -std=c23`, like SBCL) | C++23 (`-fno-exceptions -fno-rtti`) | Zig | Rust (Current) |
| --- | --- | --- | --- | --- |
| **Toolchain & LTO** | Same `clang` as `.ll`; zero version skew | Same `clang` as `.ll`; zero version skew | Extra compiler dependency; own LLVM version | Extra compiler; `rustc`/`clang` LLVM skew |
| **Object / `PyVarObject` layouts** | Native (flexible array + explicit offsets) | Native + `constexpr` offset assertions | Native | Opaque `enum Data` or manual pointer math in `unsafe` |
| **`Python.h` & C varargs (`va_list`)** | Native | Native (`extern "C"`) | C macros/varargs need wrappers | Poor for defining C variadics |
| **Risk of language logic staying in foreign code** | **Lowest**: minimal stdlib discourages writing Python protocols in C | **Medium**: `std::vector`/`std::unordered_map` tempt keeping containers in C++ | Low | **High** (`Vec`/`HashMap`/`String` already used across `mule/runtime/`) |
| **Atomics / low-level ergonomics** | `<stdatomic.h>`, `<stdckdint.h>`, `<stdbit.h>` | `std::atomic`, scoped guards, templates | Builtin atomics, `defer` | `std::sync::atomic`, `Drop` |

### Why not port all 20.8k lines of Rust to C23 first?

Roughly 15 files (~4,500 lines) in `mule/runtime/` are true low-level substrate
(`native_storage.rs`, `annotations.rs`, `buffers.rs`, `coroutine.rs`,
`faults.rs`, `signals.rs`, `gmp.rs`, `math.rs`, `numeric.rs`, `hashing.rs`,
`multiplex.rs`, `networking.rs`, `spawn.rs`, `terminal.rs`,
`thread_identity.rs`, `threads.rs`, `traceback.rs`, `gc.rs`, `weakref.rs`).
However, four files (~11,900 lines)—`classes.rs`, `object.rs`, `sequences.rs`,
and `process.rs`—rely heavily on Rust's `Vec`, `String`, and `HashMap` to
implement language-level Python behavior that belongs in `stdlib/_*.py`. Porting
those four files directly to C23 would mean writing temporary C hash tables and
string builders for code slated to be replaced by compiled Mule Python.

Instead, migrating language-level Rust into compiled Mule Python first shrinks
the foreign runtime to a small physical kernel, which can then be converted to
C23 or pure Mule LLVM primitives over the existing `extern "C"` (`mule_*`) ABI
module by module.

---

## 3. Pure Mule Runtime Feasibility and Required Compiler Capabilities

Because Mule emits **LLVM IR** and links with `clang` against system libraries
(`-lgmp -lutil -lrt -lpthread -lm -ldl` in `mule/compiler/driver.py`), Mule can
go further than SBCL toward a pure self-hosted runtime.

### Why SBCL keeps ~5% C while Mule can reach ~99–100% Mule

SBCL writes bignums (`src/code/bignum.lisp`), hash tables, strings, streams,
CLOS, and package tables in Common Lisp using `sb-vm` VOPs and
`sb-sys:system-area-pointer` (SAP), while keeping `src/runtime/*.c` (~6k lines
for x86-64 Linux) because SBCL's compiler emits machine bytes directly and uses
C to consume OS headers (`sigaction`, `ucontext_t`, `pthread`, `mmap`) and host
its stop-the-world GC tables.

Mule's backend emits LLVM IR text (`mule/compiler/codegen.py`). Anything C
expresses—struct layouts, raw pointers, atomic instructions, `extern` calls to
`libc`/`libgmp`/`pthread`, and module-level inline assembly (`module asm`)—can
be emitted directly as LLVM IR by Mule:

- **Generators (`mule/runtime/coroutine.rs`):** A small x86-64 and AArch64
  context switch (`mule_context_switch`) plus `mmap`/`munmap`/`mprotect` calls
  and a state struct. LLVM IR supports `module asm` directly in `codegen.py`.
- **Signals (`mule/runtime/signals.rs`):** `handle_signal` is a 5-line
  `extern "C" fn(i32)` that sets an atomic bit in `PENDING_SIGNALS` and sets the
  safepoint flag.
- **Cycle GC (`mule/runtime/gc.rs`):** 172 lines of trial-deletion graph
  traversal over object headers.
- **OS, networking, and GMP (`process.rs`, `networking.rs`, `gmp.rs`):** Thin
  wrappers around POSIX `libc` calls and `__gmpz_*` symbols that construct
  Python objects or raise `OSError`.

### Four compiler capabilities required for a pure Mule runtime

To write the low-level runtime substrate in Mule Python (`stdlib/_runtime/*.py`),
the compiler needs four systems-programming capabilities (analogous to SBCL's
`sb-vm`, `sb-sys:system-area-pointer`, and `sb-alien`):

1. **Full unboxed machine types and raw pointer primitives:**
   Extend `stdlib/mule_types.py` and `mule/compiler/vops.py` beyond `int64`/`float64`
   to lower `int8`, `uint8`, `int16`, `uint16`, `int32`, `uint32`, `uint64`, and
   typed/untyped raw pointers (`ptr[T]`, `sizeof(T)`, `offsetof(T, field)`),
   with VOPs for raw `load`, `store`, pointer offset (`getelementptr`), and
   atomics (`atomic_load`, `atomic_store`, `atomic_fetch_add`, `atomic_cmpxchg`).
2. **Unboxed, zero-overhead systems functions (`@mule_types.c_abi` or `safety=0, speed=3`):**
   Allow runtime modules to define functions whose parameters, locals, and
   results are raw machine types (`i64`, `i32`, `ptr`, `void`) with no
   reference-counting prologue/epilogue and no Python error-edge checks,
   compiling 1-to-1 to the LLVM C ABI. This enables compiling `retain`,
   `release`, allocator wrappers, and POSIX signal callbacks in Mule.
3. **Direct foreign C symbol declarations (SBCL `define-alien-routine`):**
   Allow system modules to declare `libc`, `pthread`, and `libgmp` symbols
   (`malloc`, `realloc`, `free`, `memcpy`, `mmap`, `epoll_wait`,
   `__errno_location`, `__gmpz_add`, etc.) and emit direct LLVM `call`
   instructions without a foreign wrapper function.
4. **Unified physical object layouts in LLVM:**
   Replace Rust's `enum Data` with `@mule_types.optimize(speed=3)` physical object
   layouts (`ObjectHeader`, `VarObjectHeader`, `AsciiStr`, `ListObject`,
   `DictObject`, `BigIntObject`). Once `str`, `list`, `tuple`, `dict`, and `int`
   have compiler-known physical layouts, both `stdlib/_*.py` and user code with
   type proofs can inline length reads, slot/byte loads, and tag checks directly
   in LLVM IR.

---

## 4. Phased Rust-to-Mule Migration Roadmap

Six main areas of language-level code remain in `mule/runtime/*.rs`. They
should be migrated into compiled Mule Python (`stdlib/_*.py`) and IR1/IR2
lowering in order from most self-contained to deepest structural change:

| Priority | Rust file(s) | Lines to migrate | What currently lives in Rust | Target home in Mule |
| --- | --- | --- | --- | --- |
| **1. First** | `mule/runtime/builtins.rs` | ~600 lines | `mule_import_star` (`from m import *`), `input()`, `len()`, `int()`/`float()` string and underscore parsing, `sys.modules` lookup hooks | `stdlib/_sequences.py`, `stdlib/_object.py` |
| **2. Second** | `mule/runtime/sequences.rs` (`:851–1672, 2063–2650`) | ~1,400 lines | Duplicate Rust implementations of `str.find`, `str.split`, `str.partition`, `str.startswith`/`endswith`, slice equality (`mule_text_slice_eq*`), and hardcoded positional field inspection of `RangeIterator`, `StorageIterator`, `_generator`, and `bytearray` (`mule_fast_next_item`, `mule_fast_iterate*`, `mule_fast_unpack*`) | `stdlib/_str.py`, `stdlib/_sequences.py` + IR1 transforms in `mule/compiler/seqtran.py` |
| **3. Third** | `mule/runtime/object.rs` (`:1845–3165`) | ~1,300 lines | `call`/`call_with_receiver`, `truth`/`length_result`, `dispatch_binary` (`__add__`/`__radd__`/`__iadd__`), `compare_slow_inner`, `contains`, `compare_sequence_storage`, and `formatted_value`/`quoted` (`repr` quoting fallback) | `stdlib/_sequences.py`, `stdlib/_format.py` |
| **4. Fourth** | `mule/runtime/process.rs` (`:1456–1606` and wrappers) | ~1,200 lines | `os_walk` directory tree traversal (`os.walk` and `os.fspath` dispatch in Rust), plus `_time` and `_posix` argument validation, tuple/list construction, and `sleep`/`localtime`/`gmtime` wrappers around `libc` | `stdlib/os.py`, `stdlib/time.py`, `stdlib/posix.py` |
| **5. Fifth** | `mule/runtime/classes.rs` | ~3,500 lines | C3 MRO linearization, class construction, descriptor and attribute lookup order, `mule_resolve_super_method`, `isinstance`/`issubclass` | `stdlib/_object.py`, `stdlib/_native_class.py` |
| **6. Sixth** | `mule/runtime/capi.rs` | ~7,500 lines | `PyType_FromModuleAndSpec` / `PyType_FromSpecWithBases`, `c_callable_entry` slot/method/member trampolines, `PyObject_Call`, `PyDict_Next`, `PyErr_*`, `PyUnicode_FromFormatV`, `mule_VaParseTupleAndKeywords`, `Py_VaBuildValue` (claimed to be in `stdlib/_capi.py`, which does not exist) | `stdlib/_capi.py` |

*(In addition, 26 functions across `mule/runtime/object.rs`, `sequences.rs`,
`value_buffer.rs`, `classes.rs`, `gc.rs`, and `faults.rs` still carry
`#[inline(always)]`, violating the inlining policy in `AGENTS.md`.)*

### Priority 1 breakdown: `mule/runtime/builtins.rs`

Almost all Python builtins are already installed by `stdlib/_sequences.py`
(`_install_builtin`), leaving `mule/runtime/builtins.rs` with four self-contained
pieces of language logic to migrate first:

1. **`mule_import_star` (`mule/runtime/builtins.rs:625–697`):**
   75 lines of Rust implementing `from module import *` (reading `__all__`,
   validating a sequence of `str` or filtering `_`-prefixed attributes from the
   module namespace, looking up each attribute, and binding it in the caller's
   namespace). Moving this to `stdlib/_sequences.py` replaces the Rust helper
   with compiled Python using existing namespace and attribute primitives.
2. **`input([prompt])` (`mule/runtime/builtins.rs:148–186`):**
   Currently hardcoded to Rust's `std::io::stdin().read_line()` and
   `std::io::stdout()`, bypassing Python's `sys.stdin` and `sys.stdout`.
   Migrating `input` into compiled Python deletes the Rust builtin and respects
   redirected `sys.stdin` / `sys.stdout`.
3. **`len(obj)` (`mule/runtime/builtins.rs:109–147`):**
   Moving the generic `len` builtin function into `stdlib/_sequences.py` (while
   preserving the compiler's inline/known-call fast paths for proven container
   types) removes the bootstrap `len` entry from `builtins::namespace()`.
4. **`int()` and `float()` string parsing (`mule/runtime/builtins.rs:187–475`):**
   `stdlib/_object.py` currently captures `_convert_int = int` and
   `_convert_float = float` from `builtins.rs` before defining the `int` and
   `float` classes because ASCII whitespace trimming, sign handling, base-prefix
   (`0x`/`0o`/`0b`) detection, and underscore validation live in Rust. Moving
   the parsing and validation protocol into compiled Python reduces the underlying
   runtime primitive to raw digit/float conversion.

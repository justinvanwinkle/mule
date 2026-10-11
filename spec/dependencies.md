# Dependency policy

Status: user requirements and approved dependencies.

Mule implementation languages are Python and Rust. The compiler is written in
Python with eventual self hosting required; the native runtime is written in Rust.
LLVM IR remains the code generation target. Historical archived code and the
explicitly requested comparison programs are separate from implementation code.

Mule is an independent Python implementation. Generated programs, Mule's runtime,
and native test executables must never link CPython. Using CPython's headers or
runtime implementation, and source/bytecode execution fallbacks, is prohibited.
The Python compiler host and separately launched CPython reference benchmarks
do not change this rule. A future CPython-compatible C extension interface must
be implemented by Mule, rather than obtained by embedding CPython.

Every new third-party library must be discussed with the user before inclusion.
Libraries linked into the compiler/runtime/generated programs must be LGPL or
more permissively licensed. GPL dependencies are permitted for non-linked uses.
License eligibility does not replace discussing the dependency with the user.

## Approved library

GMP was proposed for arbitrary-precision integer arithmetic and explicitly
approved by the user. Use its LGPLv3-or-later license option. The installed
development version used for this implementation is 6.3.0. GMP's
[official copying conditions](https://gmplib.org/manual/Copying) offer LGPLv3
or GPLv2, with later versions of either license permitted.

GMP is dynamically linked. Mule owns integer representations, native small-value
operations, overflow promotion, dispatch, and lifetime management; GMP provides
arbitrary-precision arithmetic for values outside the native range. There is
no direct dependency on CPython's object representation or integer code.

Clang/LLVM remain the existing code generation tools. The system C and math
runtime remain existing platform services. Rust's standard library and required
platform services accompany the Rust runtime. No external Rust crates are used.
GMP bindings are handwritten in Rust; no additional third-party runtime library
was added during the replacement.

## Approved Python standard library sources

The user explicitly requested the released Python 3.14 Python standard library
sources in `stdlib/`, with optional native accelerator imports removed, replacing
an earlier Python 3.15.0 import. The Python 3.14.7 source release, matching the
host CPython used as the conformance oracle, is vendored under its PSF and historical licenses;
notices are preserved in `stdlib/LICENSE.cpython`, and the release and its
adaptations are described in `stdlib/README.md`.

This approval covers Python library source reuse. It does not permit linking
CPython, copying its C runtime, using its object ABI internally, or delegating
generated-program execution to a host evaluator. Native primitives without an
upstream Python implementation must be provided by Mule under the existing
compiler/runtime boundary and dependency approval policy.

SBCL's `src/cold/build-order.lisp-expr` specifies language-level sources compiled
for the target; `src/cold/warm.lisp` and `make-target-2-load.lisp` load target
language implementation sources during bootstrap. Mule likewise intends to
compile its language-level library with its own compiler. Reusing the released
Python algorithms rather than SBCL's Common Lisp standard library is necessary
to provide Python's APIs and semantics. Vendoring these sources supplies those
algorithms; target compilation and compiled-module loading remain separate work.

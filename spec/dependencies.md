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

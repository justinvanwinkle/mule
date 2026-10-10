//! Batch arithmetic benchmark; no compiler/runtime object machinery is involved.

use std::ffi::{CStr, c_char, c_int, c_ulong, c_void};
use std::hint::black_box;
use std::mem::MaybeUninit;
use std::time::Instant;

#[cfg(width128)]
const LIMBS: usize = 2;
#[cfg(not(width128))]
const LIMBS: usize = 4;
const BITS: c_ulong = (LIMBS * 64) as c_ulong;
type Words = [u64; LIMBS];
type Kernel = unsafe extern "C" fn(*mut Words, *const Words, *const Words, usize);

#[repr(C)]
struct Mpz {
    allocated: c_int,
    size: c_int,
    limbs: *mut c_ulong,
}

#[link(name = "gmp")]
unsafe extern "C" {
    static __gmp_version: *const c_char;
    fn __gmpz_init2(value: *mut Mpz, bits: c_ulong);
    fn __gmpz_clear(value: *mut Mpz);
    fn __gmpz_import(
        value: *mut Mpz,
        count: usize,
        order: c_int,
        size: usize,
        endian: c_int,
        nails: usize,
        data: *const c_void,
    );
    fn __gmpz_add(out: *mut Mpz, a: *const Mpz, b: *const Mpz);
    fn __gmpz_xor(out: *mut Mpz, a: *const Mpz, b: *const Mpz);
    fn __gmpz_mul(out: *mut Mpz, a: *const Mpz, b: *const Mpz);
    fn __gmpz_tdiv_q(out: *mut Mpz, a: *const Mpz, b: *const Mpz);
    fn __gmpz_fdiv_r_2exp(out: *mut Mpz, a: *const Mpz, bits: c_ulong);
    fn __gmpn_add_n(out: *mut u64, a: *const u64, b: *const u64, n: isize) -> u64;
    fn __gmpn_xor_n(out: *mut u64, a: *const u64, b: *const u64, n: isize);
    fn __gmpn_mul_n(out: *mut u64, a: *const u64, b: *const u64, n: isize);
    fn __gmpn_tdiv_qr(
        q: *mut u64,
        r: *mut u64,
        fraction: isize,
        a: *const u64,
        an: isize,
        b: *const u64,
        bn: isize,
    );
    fn llvm_add(out: *mut Words, a: *const Words, b: *const Words, n: usize);
    fn llvm_xor(out: *mut Words, a: *const Words, b: *const Words, n: usize);
    fn llvm_mul(out: *mut Words, a: *const Words, b: *const Words, n: usize);
    fn llvm_div(out: *mut Words, a: *const Words, b: *const Words, n: usize);
}

struct Integer(Mpz);

impl Integer {
    fn new(words: &Words) -> Self {
        let mut value = MaybeUninit::uninit();
        unsafe {
            // A full-width product plus the extra limb GMP may reserve.
            __gmpz_init2(value.as_mut_ptr(), BITS * 2 + 64);
            __gmpz_import(
                value.as_mut_ptr(),
                LIMBS,
                -1,
                8,
                0,
                0,
                words.as_ptr().cast(),
            );
            Self(value.assume_init())
        }
    }

    fn words(&self) -> Words {
        let mut result = [0; LIMBS];
        let count = self.0.size as usize;
        assert!(count <= LIMBS);
        unsafe { std::ptr::copy_nonoverlapping(self.0.limbs, result.as_mut_ptr(), count) };
        result
    }
}

impl Drop for Integer {
    fn drop(&mut self) {
        unsafe { __gmpz_clear(&mut self.0) };
    }
}

trait Operation {
    const LLVM: Kernel;
    fn mpn(out: &mut Words, a: &Words, b: &Words);
    fn mpz(out: &mut Integer, a: &Integer, b: &Integer);
}

struct Add;
struct Xor;
struct Mul;
struct Div;

impl Operation for Add {
    const LLVM: Kernel = llvm_add;
    fn mpn(out: &mut Words, a: &Words, b: &Words) {
        unsafe { __gmpn_add_n(out.as_mut_ptr(), a.as_ptr(), b.as_ptr(), LIMBS as isize) };
    }
    fn mpz(out: &mut Integer, a: &Integer, b: &Integer) {
        unsafe {
            __gmpz_add(&mut out.0, &a.0, &b.0);
            let ptr = std::ptr::addr_of_mut!(out.0);
            __gmpz_fdiv_r_2exp(ptr, ptr, BITS);
        }
    }
}

impl Operation for Xor {
    const LLVM: Kernel = llvm_xor;
    fn mpn(out: &mut Words, a: &Words, b: &Words) {
        unsafe { __gmpn_xor_n(out.as_mut_ptr(), a.as_ptr(), b.as_ptr(), LIMBS as isize) };
    }
    fn mpz(out: &mut Integer, a: &Integer, b: &Integer) {
        unsafe { __gmpz_xor(&mut out.0, &a.0, &b.0) };
    }
}

impl Operation for Mul {
    const LLVM: Kernel = llvm_mul;
    fn mpn(out: &mut Words, a: &Words, b: &Words) {
        let mut product = [0; LIMBS * 2];
        unsafe { __gmpn_mul_n(product.as_mut_ptr(), a.as_ptr(), b.as_ptr(), LIMBS as isize) };
        out.copy_from_slice(&product[..LIMBS]);
    }
    fn mpz(out: &mut Integer, a: &Integer, b: &Integer) {
        unsafe {
            __gmpz_mul(&mut out.0, &a.0, &b.0);
            let ptr = std::ptr::addr_of_mut!(out.0);
            __gmpz_fdiv_r_2exp(ptr, ptr, BITS);
        }
    }
}

impl Operation for Div {
    const LLVM: Kernel = llvm_div;
    fn mpn(out: &mut Words, a: &Words, b: &Words) {
        // Datasets have full-width dividends and half- or full-width divisors.
        let divisor_size = if b[LIMBS - 1] != 0 { LIMBS } else { LIMBS / 2 };
        let mut remainder = [0; LIMBS];
        *out = [0; LIMBS];
        unsafe {
            __gmpn_tdiv_qr(
                out.as_mut_ptr(),
                remainder.as_mut_ptr(),
                0,
                a.as_ptr(),
                LIMBS as isize,
                b.as_ptr(),
                divisor_size as isize,
            );
        }
    }
    fn mpz(out: &mut Integer, a: &Integer, b: &Integer) {
        unsafe { __gmpz_tdiv_q(&mut out.0, &a.0, &b.0) };
    }
}

fn run<O: Operation>(backend: &str, a: &[Words], b: &[Words], rounds: u64) -> (u128, Vec<Words>) {
    let mut output = vec![[0; LIMBS]; a.len()];
    match backend {
        "llvm" => {
            let start = Instant::now();
            for _ in 0..rounds {
                unsafe { O::LLVM(output.as_mut_ptr(), a.as_ptr(), b.as_ptr(), a.len()) };
                black_box(&mut output);
            }
            (start.elapsed().as_nanos(), output)
        }
        "mpn" => {
            let start = Instant::now();
            for _ in 0..rounds {
                for ((out, left), right) in output.iter_mut().zip(a).zip(b) {
                    O::mpn(out, left, right);
                }
                black_box(&mut output);
            }
            (start.elapsed().as_nanos(), output)
        }
        "mpz" => {
            let left: Vec<_> = a.iter().map(Integer::new).collect();
            let right: Vec<_> = b.iter().map(Integer::new).collect();
            let mut results: Vec<_> = a.iter().map(|_| Integer::new(&[0; LIMBS])).collect();
            let start = Instant::now();
            for _ in 0..rounds {
                for ((out, lhs), rhs) in results.iter_mut().zip(&left).zip(&right) {
                    O::mpz(out, lhs, rhs);
                }
                black_box(&mut results);
            }
            let elapsed = start.elapsed().as_nanos();
            for (out, result) in output.iter_mut().zip(&results) {
                *out = result.words();
            }
            (elapsed, output)
        }
        _ => panic!("unknown backend {backend}"),
    }
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    if args.len() == 2 && args[1] == "version" {
        println!("{}", unsafe {
            CStr::from_ptr(__gmp_version).to_str().unwrap()
        });
        return;
    }
    let data = std::fs::read(&args[3]).unwrap();
    assert!(!data.is_empty() && data.len().is_multiple_of(LIMBS * 16));
    let mut a = Vec::new();
    let mut b = Vec::new();
    for pair in data.chunks_exact(LIMBS * 16) {
        let words: Vec<_> = pair
            .chunks_exact(8)
            .map(|word| u64::from_le_bytes(word.try_into().unwrap()))
            .collect();
        a.push(words[..LIMBS].try_into().unwrap());
        b.push(words[LIMBS..].try_into().unwrap());
    }
    black_box(&mut a);
    black_box(&mut b);
    let rounds = args[4].parse().unwrap();
    let (elapsed, output) = match args[2].as_str() {
        "add" => run::<Add>(&args[1], &a, &b, rounds),
        "xor" => run::<Xor>(&args[1], &a, &b, rounds),
        "mul" => run::<Mul>(&args[1], &a, &b, rounds),
        "div" => run::<Div>(&args[1], &a, &b, rounds),
        other => panic!("unknown operation {other}"),
    };
    let checksum = output
        .iter()
        .flatten()
        .enumerate()
        .fold(0_u64, |sum, (index, word)| {
            sum.wrapping_add(word.rotate_left((index % 64) as u32))
        });
    if let Some(path) = args.get(5) {
        let bytes: Vec<_> = output
            .iter()
            .flatten()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        std::fs::write(path, bytes).unwrap();
    }
    println!("{elapsed} {} {checksum}", a.len() as u64 * rounds);
}

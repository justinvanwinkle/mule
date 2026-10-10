//! Owning Rust integers over GMP's public integer ABI.

use std::cmp::Ordering;
use std::ffi::{CStr, CString, c_char, c_double, c_int, c_long, c_ulong};
use std::mem::MaybeUninit;

#[repr(C)]
struct Mpz {
    allocated: c_int,
    size: c_int,
    limbs: *mut c_ulong,
}

#[link(name = "gmp")]
unsafe extern "C" {
    fn __gmpz_init(value: *mut Mpz);
    fn __gmpz_clear(value: *mut Mpz);
    fn __gmpz_set(result: *mut Mpz, value: *const Mpz);
    fn __gmpz_set_si(result: *mut Mpz, value: c_long);
    fn __gmpz_set_ui(result: *mut Mpz, value: c_ulong);
    fn __gmpz_set_d(result: *mut Mpz, value: c_double);
    fn __gmpz_set_str(result: *mut Mpz, value: *const c_char, base: c_int) -> c_int;
    fn __gmpz_get_str(result: *mut c_char, base: c_int, value: *const Mpz) -> *mut c_char;
    fn __gmpz_get_si(value: *const Mpz) -> c_long;
    fn __gmpz_get_ui(value: *const Mpz) -> c_ulong;
    fn __gmpz_fits_slong_p(value: *const Mpz) -> c_int;
    fn __gmpz_fits_ulong_p(value: *const Mpz) -> c_int;
    fn __gmpz_sizeinbase(value: *const Mpz, base: c_int) -> usize;
    fn __gmpz_tstbit(value: *const Mpz, bit: c_ulong) -> c_int;
    fn __gmpz_cmp(left: *const Mpz, right: *const Mpz) -> c_int;
    fn __gmpz_cmp_d(left: *const Mpz, right: c_double) -> c_int;
    fn __gmpz_abs(result: *mut Mpz, value: *const Mpz);
    fn __gmpz_neg(result: *mut Mpz, value: *const Mpz);
    fn __gmpz_com(result: *mut Mpz, value: *const Mpz);
    fn __gmpz_add(result: *mut Mpz, left: *const Mpz, right: *const Mpz);
    fn __gmpz_sub(result: *mut Mpz, left: *const Mpz, right: *const Mpz);
    fn __gmpz_mul(result: *mut Mpz, left: *const Mpz, right: *const Mpz);
    fn __gmpz_mul_si(result: *mut Mpz, left: *const Mpz, right: c_long);
    fn __gmpz_fdiv_q(result: *mut Mpz, left: *const Mpz, right: *const Mpz);
    fn __gmpz_fdiv_r(result: *mut Mpz, left: *const Mpz, right: *const Mpz);
    fn __gmpz_fdiv_qr(quotient: *mut Mpz, remainder: *mut Mpz, left: *const Mpz, right: *const Mpz);
    fn __gmpz_and(result: *mut Mpz, left: *const Mpz, right: *const Mpz);
    fn __gmpz_xor(result: *mut Mpz, left: *const Mpz, right: *const Mpz);
    fn __gmpz_ior(result: *mut Mpz, left: *const Mpz, right: *const Mpz);
    fn __gmpz_mul_2exp(result: *mut Mpz, value: *const Mpz, bits: c_ulong);
    fn __gmpz_fdiv_q_2exp(result: *mut Mpz, value: *const Mpz, bits: c_ulong);
    fn __gmpz_pow_ui(result: *mut Mpz, value: *const Mpz, exponent: c_ulong);
}

/// A GMP integer whose allocation is initialized, cloned, and freed by Rust.
pub struct BigInt(Mpz);

impl BigInt {
    pub fn zero() -> Self {
        let mut value = MaybeUninit::<Mpz>::uninit();
        unsafe {
            __gmpz_init(value.as_mut_ptr());
            Self(value.assume_init())
        }
    }

    pub fn from_i64(value: i64) -> Self {
        let mut result = Self::zero();
        unsafe { __gmpz_set_si(&mut result.0, value as c_long) };
        result
    }

    pub fn from_u64(value: u64) -> Self {
        let mut result = Self::zero();
        unsafe { __gmpz_set_ui(&mut result.0, value as c_ulong) };
        result
    }

    pub fn parse(value: &str, base: i32) -> Option<Self> {
        if base != 0 && !(2..=62).contains(&base) {
            return None;
        }
        let value = CString::new(value.strip_prefix('+').unwrap_or(value)).ok()?;
        let mut result = Self::zero();
        (unsafe { __gmpz_set_str(&mut result.0, value.as_ptr(), base) } == 0).then_some(result)
    }

    pub fn from_decimal(value: &str) -> Option<Self> {
        Self::parse(value, 10)
    }

    pub fn from_f64(value: f64) -> Option<Self> {
        if !value.is_finite() {
            return None;
        }
        let mut result = Self::zero();
        unsafe { __gmpz_set_d(&mut result.0, value) };
        Some(result)
    }

    pub fn from_f64_trunc(value: f64) -> Option<Self> {
        Self::from_f64(value)
    }

    pub fn to_decimal(&self) -> String {
        // GMP writes into Rust-owned storage, so no allocator is crossed on free.
        let capacity = unsafe { __gmpz_sizeinbase(&self.0, 10) } + 2;
        let mut bytes = vec![0_u8; capacity];
        unsafe { __gmpz_get_str(bytes.as_mut_ptr().cast(), 10, &self.0) };
        unsafe { CStr::from_ptr(bytes.as_ptr().cast()) }
            .to_str()
            .expect("GMP decimal output is ASCII")
            .to_owned()
    }

    pub fn to_i64(&self) -> Option<i64> {
        (unsafe { __gmpz_fits_slong_p(&self.0) } != 0)
            .then(|| unsafe { __gmpz_get_si(&self.0) } as i64)
    }

    pub fn to_u64(&self) -> Option<u64> {
        (unsafe { __gmpz_fits_ulong_p(&self.0) } != 0)
            .then(|| unsafe { __gmpz_get_ui(&self.0) } as u64)
    }

    pub fn sign(&self) -> i32 {
        self.0.size.signum()
    }

    pub fn bits(&self) -> usize {
        unsafe { __gmpz_sizeinbase(&self.0, 2) }
    }

    pub fn is_odd(&self) -> bool {
        unsafe { __gmpz_tstbit(&self.0, 0) != 0 }
    }

    pub fn compare(&self, other: &Self) -> Ordering {
        unsafe { __gmpz_cmp(&self.0, &other.0) }.cmp(&0)
    }

    /// The caller must exclude NaN, which GMP's comparison does not accept.
    pub fn compare_f64(&self, other: f64) -> Ordering {
        assert!(!other.is_nan(), "NaN has no integer ordering");
        unsafe { __gmpz_cmp_d(&self.0, other) }.cmp(&0)
    }

    fn unary(&self, operation: unsafe extern "C" fn(*mut Mpz, *const Mpz)) -> Self {
        let mut result = Self::zero();
        unsafe { operation(&mut result.0, &self.0) };
        result
    }

    fn binary(
        &self,
        other: &Self,
        operation: unsafe extern "C" fn(*mut Mpz, *const Mpz, *const Mpz),
    ) -> Self {
        let mut result = Self::zero();
        unsafe { operation(&mut result.0, &self.0, &other.0) };
        result
    }

    pub fn abs(&self) -> Self {
        self.unary(__gmpz_abs)
    }
    pub fn neg(&self) -> Self {
        self.unary(__gmpz_neg)
    }
    pub fn invert(&self) -> Self {
        self.unary(__gmpz_com)
    }
    pub fn add(&self, other: &Self) -> Self {
        self.binary(other, __gmpz_add)
    }
    pub fn sub(&self, other: &Self) -> Self {
        self.binary(other, __gmpz_sub)
    }
    pub fn mul(&self, other: &Self) -> Self {
        self.binary(other, __gmpz_mul)
    }
    pub fn mul_i64(&self, other: i64) -> Self {
        let mut result = Self::zero();
        unsafe { __gmpz_mul_si(&mut result.0, &self.0, other as c_long) };
        result
    }

    // GMP permits the output to alias either operand. Take one raw pointer to
    // our exclusive value rather than overlapping Rust mutable/shared borrows.
    pub fn add_assign(&mut self, other: &Self) {
        let value = std::ptr::addr_of_mut!(self.0);
        unsafe { __gmpz_add(value, value, &other.0) };
    }
    pub fn sub_assign(&mut self, other: &Self) {
        let value = std::ptr::addr_of_mut!(self.0);
        unsafe { __gmpz_sub(value, value, &other.0) };
    }
    pub fn mul_assign(&mut self, other: &Self) {
        let value = std::ptr::addr_of_mut!(self.0);
        unsafe { __gmpz_mul(value, value, &other.0) };
    }
    pub fn mul_assign_i64(&mut self, other: i64) {
        let value = std::ptr::addr_of_mut!(self.0);
        unsafe { __gmpz_mul_si(value, value, other as c_long) };
    }
    pub fn floor_div(&self, other: &Self) -> Self {
        assert_ne!(
            other.sign(),
            0,
            "integer division requires a nonzero divisor"
        );
        self.binary(other, __gmpz_fdiv_q)
    }
    pub fn remainder(&self, other: &Self) -> Self {
        assert_ne!(
            other.sign(),
            0,
            "integer remainder requires a nonzero divisor"
        );
        self.binary(other, __gmpz_fdiv_r)
    }
    pub fn bitand(&self, other: &Self) -> Self {
        self.binary(other, __gmpz_and)
    }
    pub fn xor(&self, other: &Self) -> Self {
        self.binary(other, __gmpz_xor)
    }
    pub fn or(&self, other: &Self) -> Self {
        self.binary(other, __gmpz_ior)
    }

    pub fn div_rem_floor(&self, other: &Self) -> (Self, Self) {
        assert_ne!(
            other.sign(),
            0,
            "integer division requires a nonzero divisor"
        );
        let mut quotient = Self::zero();
        let mut remainder = Self::zero();
        unsafe { __gmpz_fdiv_qr(&mut quotient.0, &mut remainder.0, &self.0, &other.0) };
        (quotient, remainder)
    }

    pub fn shifted_left(&self, bits: u64) -> Self {
        let mut result = Self::zero();
        unsafe { __gmpz_mul_2exp(&mut result.0, &self.0, bits as c_ulong) };
        result
    }

    pub fn shifted_right(&self, bits: u64) -> Self {
        let mut result = Self::zero();
        unsafe { __gmpz_fdiv_q_2exp(&mut result.0, &self.0, bits as c_ulong) };
        result
    }

    pub fn pow(&self, exponent: u64) -> Self {
        let mut result = Self::zero();
        unsafe { __gmpz_pow_ui(&mut result.0, &self.0, exponent as c_ulong) };
        result
    }
}

impl Clone for BigInt {
    fn clone(&self) -> Self {
        let mut result = Self::zero();
        unsafe { __gmpz_set(&mut result.0, &self.0) };
        result
    }
}

impl Drop for BigInt {
    fn drop(&mut self) {
        unsafe { __gmpz_clear(&mut self.0) };
    }
}

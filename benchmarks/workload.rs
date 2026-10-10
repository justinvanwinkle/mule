//! Typed Rust version of the scalar integer/search workload in workload.py.

use std::hint::black_box;
use std::io::{self, Read};

fn queens(mask: u64, columns: u64, left: u64, right: u64) -> u64 {
    if columns == mask {
        return 1;
    }
    let mut available = mask & !(columns | left | right);
    let mut total = 0;
    while available != 0 {
        let bit = available & available.wrapping_neg();
        available ^= bit;
        total += queens(
            mask,
            columns | bit,
            ((left | bit) << 1) & mask,
            (right | bit) >> 1,
        );
    }
    total
}

fn gcd(mut a: u64, mut b: u64) -> u64 {
    while b != 0 {
        let remainder = a % b;
        a = b;
        b = remainder;
    }
    a
}

fn is_prime(n: u64) -> bool {
    if n < 2 {
        return false;
    }
    if n % 2 == 0 {
        return n == 2;
    }
    if n % 3 == 0 {
        return n == 3;
    }
    let mut divisor = 5;
    while divisor * divisor <= n {
        if n % divisor == 0 || n % (divisor + 2) == 0 {
            return false;
        }
        divisor += 6;
    }
    true
}

fn integer_kernel(iterations: u64) -> u64 {
    let mut state = 123456789;
    let mut checksum = 0;
    let mut index = 0;
    while index < iterations {
        state = (state * 1664525 + 1013904223) & 2147483647;
        let candidate = 10000 + state % 90000;
        let common = gcd(candidate, (state >> 8) % 10000 + 1);
        if is_prime(candidate) {
            checksum = (checksum * 33 + candidate + common) % 1000000007;
        } else {
            checksum = (checksum * 33 + (candidate ^ common)) % 1000000007;
        }
        index += 1;
    }
    checksum
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).expect("read stdin");
    let mut lines = input.lines();
    let mut parameter = || -> i64 {
        lines
            .next()
            .expect("three input lines are required")
            .trim()
            .parse()
            .expect("input must be an integer")
    };
    let n = parameter();
    let repetitions = parameter();
    let iterations = parameter();
    if !(1..=15).contains(&n) || repetitions < 0 || iterations < 0 {
        println!("invalid parameters");
        return;
    }
    let mask = (1_u64 << n) - 1;
    let mut solutions = 0;
    let mut repeat = 0;
    while repeat < repetitions as u64 {
        solutions += queens(black_box(mask), 0, 0, 0);
        repeat += 1;
    }
    println!("{} {}", solutions, integer_kernel(iterations as u64));
}

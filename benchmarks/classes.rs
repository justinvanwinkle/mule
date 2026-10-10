//! Matching native struct workloads; input and output prevent constant folding.
use std::io::{self, Read};

struct Counter {
    value: i64,
}

impl Counter {
    fn add(&mut self, amount: i64) {
        self.value += amount;
    }

    fn mix(&mut self, amount: i64) {
        self.value = (self.value ^ amount) + 3;
    }
}

fn count(limit: i64, nonlinear: bool) -> i64 {
    let mut counter = Counter { value: 0 };
    let mut index = 0_i64;
    if nonlinear {
        while index < limit {
            counter.mix(3);
            index += 1;
        }
    } else {
        while index < limit {
            counter.add(3);
            index += 1;
        }
    }
    counter.value
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let limit: i64 = input.trim().parse().unwrap();
    let nonlinear = std::env::args().nth(1).as_deref() == Some("nonlinear");
    println!("{}", count(limit, nonlinear));
}

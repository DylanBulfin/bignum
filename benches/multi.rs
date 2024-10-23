use std::hint::black_box;

use bignum::{myu128::MyU128, old_methods, BigNum};
use criterion::{criterion_group, criterion_main, Criterion};
use rand::{distributions::Uniform, prelude::Distribution, thread_rng};

fn add_old(n: BigNum, m: BigNum) -> BigNum {
    old_methods::add_old(black_box(n), black_box(m))
}

fn add_u128(n: BigNum, m: BigNum) -> BigNum {
    old_methods::add_u128(black_box(n), black_box(m))
}

fn add_curr(n: BigNum, m: BigNum) -> BigNum {
    black_box(n) + black_box(m)
}

fn myu128_add1(lhss: &[MyU128], rhss: &[MyU128]) -> Vec<MyU128> {
    let mut res = Vec::new();

    for (&lhs, &rhs) in lhss.iter().zip(rhss.iter()) {
        res.push(black_box(lhs).add1(black_box(rhs)));
    }

    res
}

fn myu128_add2(lhss: &[MyU128], rhss: &[MyU128]) -> Vec<MyU128> {
    let mut res = Vec::new();

    for (&lhs, &rhs) in lhss.iter().zip(rhss.iter()) {
        res.push(black_box(lhs).add2(black_box(rhs)));
    }

    res
}

fn u128_add_baseline(lhss: &[u128], rhss: &[u128]) -> Vec<u128> {
    let mut res = Vec::new();

    for (&lhs, &rhs) in lhss.iter().zip(rhss.iter()) {
        res.push(black_box(lhs) + black_box(rhs));
    }

    res
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("add_old", |b| {
        b.iter(|| {
            add_old(
                black_box(BigNum::new(0xffff_ffff_ffff_ffff, 100)),
                black_box(BigNum::new(0xffff_ffff_ffff_ffff, 95)),
            )
        })
    });

    c.bench_function("add_u128", |b| {
        b.iter(|| {
            add_u128(
                black_box(BigNum::new(0xffff_ffff_ffff_ffff, 100)),
                black_box(BigNum::new(0xffff_ffff_ffff_ffff, 95)),
            )
        })
    });

    c.bench_function("add_curr", |b| {
        b.iter(|| {
            add_curr(
                black_box(BigNum::new(0xffff_ffff_ffff_ffff, 100)),
                black_box(BigNum::new(0xffff_ffff_ffff_ffff, 95)),
            )
        })
    });

    // This range ensures that the sum of two numbers can't overflow
    let uniform: Uniform<u128> = Uniform::new(0, 1 << 127);

    let lhss: Vec<u128> = (0..1000)
        .map(|_| uniform.sample(&mut thread_rng()))
        .collect();
    let rhss: Vec<u128> = (0..1000)
        .map(|_| uniform.sample(&mut thread_rng()))
        .collect();

    c.bench_function("u128_add_baseline", |b| {
        b.iter(|| u128_add_baseline(&lhss, &rhss))
    });

    let lhss: Vec<MyU128> = lhss.into_iter().map(MyU128::from_u128).collect();
    let rhss: Vec<MyU128> = rhss.into_iter().map(MyU128::from_u128).collect();

    c.bench_function("MyU128.add1", |b| b.iter(|| myu128_add1(&lhss, &rhss)));
    c.bench_function("MyU128.add2", |b| b.iter(|| myu128_add2(&lhss, &rhss)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

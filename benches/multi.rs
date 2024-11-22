#![cfg(feature = "bench")]
use std::{hint::black_box, iter::from_fn};

use bignum::{
    //bignumold::{old_impl::BigNumOld, old_methods},
    create_default_base,
    Base,
    BigNumBase,
    Binary,
    Decimal,
    Hexadecimal,
    Octal,
};
use criterion::{criterion_group, criterion_main, Criterion};
use rand::{distributions::Uniform, prelude::Distribution, thread_rng};

create_default_base!(Base61, 61);
create_default_base!(Base300, 300);

macro_rules! do_rand_add_test {
    (
        base = $base:ident,
        low = $low:expr,
        high = $high:expr,
    ) => {
        type BigNum = BigNumBase<$base>;
        let min_exp = $base::new().exp_range().min() as i32;

        let rand: Uniform<BigNum> = Uniform::new_inclusive($low, $high);
        let rng = &mut thread_rng();

        let sig_dist: Uniform<u64> = Uniform::new_inclusive(0, u64::MAX);
        let exp_diff_dist: Uniform<i32> = Uniform::new_inclusive(-min_exp, min_exp);

        let vals = black_box(from_fn(|| Some(rand.sample(rng))).take(1000)).collect::<Vec<_>>();

        for val in vals {
            let exp_diff: i32 = exp_diff_dist.sample(rng);

            let rhs = if exp_diff < 0 {
                BigNum::new(sig_dist.sample(rng), val.exp - ((-exp_diff) as u64))
            } else {
                BigNum::new(sig_dist.sample(rng), val.exp + exp_diff as u64)
            };

            let _ = val + rhs;
        }
    };
}

macro_rules! do_rand_sub_test {
    (
        base = $base:ident,
        low = $low:expr,
        high = $high:expr,
    ) => {
        type BigNum = BigNumBase<$base>;
        let min_exp = $base::new().exp_range().min() as u32;

        let rand: Uniform<BigNum> = Uniform::new_inclusive($low, $high);
        let rng = &mut thread_rng();

        // Need the custom BigNum to be strictly greater than the one we get from rand,
        // so we restrict it to a valid sig range.
        let sig_dist: Uniform<u64> = Uniform::new_inclusive(1 << 63, u64::MAX);
        let exp_diff_dist: Uniform<u32> = Uniform::new_inclusive(1, min_exp);

        let vals = black_box(from_fn(|| Some(rand.sample(rng))).take(1000)).collect::<Vec<_>>();

        for val in vals {
            let rhs = BigNum::new(
                sig_dist.sample(rng),
                val.exp + exp_diff_dist.sample(rng) as u64,
            );

            let _ = rhs - val;
        }
    };
}

fn bignumred_binary_add_rand() {
    do_rand_add_test!(
        base = Binary,
        low = BigNum::from(0),
        high = BigNum::new(u64::MAX, u64::MAX),
    );
}

fn bignumred_decimal_add_rand() {
    do_rand_add_test!(
        base = Decimal,
        low = BigNum::from(0),
        high = BigNum::new(10u64.pow(19) - 1, u64::MAX),
    );
}

fn bignumred_hexadecimal_add_rand() {
    do_rand_add_test!(
        base = Hexadecimal,
        low = BigNum::from(0),
        high = BigNum::new(u64::MAX, u64::MAX),
    );
}

fn bignumred_binary_add_rand_lim() {
    do_rand_add_test!(
        base = Binary,
        low = BigNum::from(0),
        high = BigNum::from(u64::MAX),
    );
}

fn bignumred_decimal_add_rand_lim() {
    do_rand_add_test!(
        base = Decimal,
        low = BigNum::from(0),
        high = BigNum::from(10u64.pow(19) - 1),
    );
}

fn bignumred_hexadecimal_add_rand_lim() {
    do_rand_add_test!(
        base = Hexadecimal,
        low = BigNum::from(0),
        high = BigNum::from(u64::MAX),
    );
}

fn bignumred_binary_sub_rand() {
    do_rand_sub_test!(
        base = Binary,
        low = BigNum::from(0),
        high = BigNum::new(u64::MAX, u64::MAX),
    );
}

fn criterion_benchmark(c: &mut Criterion) {
    //c.bench_function("BigNum Binary Add Rand", |b| {
    //    b.iter(bignumred_binary_add_rand)
    //});

    //c.bench_function("BigNum Decimal Add Rand", |b| {
    //    b.iter(bignumred_decimal_add_rand)
    //});

    //c.bench_function("BigNum Hexadecimal Add Rand", |b| {
    //    b.iter(bignumred_hexadecimal_add_rand)
    //});

    //c.bench_function("BigNum Binary Add Rand Limited", |b| {
    //    b.iter(bignumred_binary_add_rand_lim)
    //});

    //c.bench_function("BigNum Decimal Add Rand Limited", |b| {
    //    b.iter(bignumred_decimal_add_rand_lim)
    //});

    //c.bench_function("BigNum Hexadecimal Add Rand Limited", |b| {
    //    b.iter(bignumred_hexadecimal_add_rand_lim)
    //});
    //

    c.bench_function("BigNum Binary Sub Rand", |b| {
        b.iter(bignumred_binary_sub_rand)
    });
}
criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

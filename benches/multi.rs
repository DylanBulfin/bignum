//#![cfg(feature = "bench")]
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
        use bignum::traits::{Pred, Succ};

        type BigNum = BigNumBase<$base>;

        let mut lhs = $high;
        let mut rhs = $low;

        for _ in 0..10000 {
            let c = black_box(lhs - rhs);

            lhs = lhs.pred();
            rhs = rhs.succ();

            let _ = c;
        }
    };
}

macro_rules! do_rand_mul_test {
    (
        base = $base:ident,
        low = $low:expr,
        high = $high:expr,
    ) => {
        use bignum::traits::{Pred, Succ};

        type BigNum = BigNumBase<$base>;

        let mut lhs = $low;
        let mut rhs = $high;

        for _ in 0..10000 {
            let c = black_box(lhs * rhs);

            lhs = lhs.succ();
            rhs = rhs.pred();

            let _ = c;
        }
    };
}

macro_rules! do_rand_div_test {
    (
        base = $base:ident,
        low = $low:expr,
        high = $high:expr,
    ) => {
        use bignum::traits::{Pred, Succ};

        type BigNum = BigNumBase<$base>;

        let mut lhs = $high;
        let mut rhs = $low;

        for _ in 0..10000 {
            let c = black_box(lhs / rhs);

            lhs = lhs.pred();
            rhs = rhs.succ();

            let _ = c;
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

fn bignumred_binary_sub_rand() {
    do_rand_sub_test!(
        base = Binary,
        low = BigNum::from(0),
        high = BigNum::new(u64::MAX, u64::MAX),
    );
}

fn bignumred_binary_mul_rand() {
    do_rand_mul_test!(
        base = Binary,
        low = BigNum::from(0),
        high = BigNum::new(u64::MAX, u64::MAX / 3),
    );
}

fn bignumred_binary_div_rand() {
    do_rand_div_test!(
        base = Binary,
        low = BigNum::from(1),
        high = BigNum::new(u64::MAX, u64::MAX),
    );
}

fn bignumred_base300_sub_rand() {
    do_rand_sub_test!(
        base = Base300,
        low = BigNum::from(0),
        high = BigNum::new(1, u64::MAX),
    );
}

fn bignumred_base300_mul_rand() {
    do_rand_mul_test!(
        base = Base300,
        low = BigNum::from(0),
        high = BigNum::new(1, u64::MAX / 3),
    );
}

fn bignumred_base300_div_rand() {
    do_rand_div_test!(
        base = Base300,
        low = BigNum::from(1),
        high = BigNum::new(1, u64::MAX),
    );
}

fn bignumred_base300_add_rand() {
    do_rand_add_test!(
        base = Base300,
        low = BigNum::from(0),
        high = BigNum::new(1, u64::MAX),
    );
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("BigNum Binary Add Rand", |b| {
        b.iter(bignumred_binary_add_rand)
    });

    c.bench_function("BigNum Binary Sub Rand", |b| {
        b.iter(bignumred_binary_sub_rand)
    });

    c.bench_function("BigNum Binary Mul Rand", |b| {
        b.iter(bignumred_binary_mul_rand)
    });

    c.bench_function("BigNum Binary Div Rand", |b| {
        b.iter(bignumred_binary_div_rand)
    });

    // Base-300 benches
    c.bench_function("BigNum Base300 Add Rand", |b| {
        b.iter(bignumred_base300_add_rand)
    });

    c.bench_function("BigNum Base300 Sub Rand", |b| {
        b.iter(bignumred_base300_sub_rand)
    });

    c.bench_function("BigNum Base300 Mul Rand", |b| {
        b.iter(bignumred_base300_mul_rand)
    });

    c.bench_function("BigNum Base300 Div Rand", |b| {
        b.iter(bignumred_base300_div_rand)
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

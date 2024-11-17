use std::hint::black_box;

use bignum::{
    bignumold::{old_impl::BigNumOld, old_methods},
    BigNum,
};
use criterion::{criterion_group, criterion_main, Criterion};
use rand::{distributions::Uniform, prelude::Distribution, thread_rng};

fn bignumold_orig_add(n: BigNumOld, m: BigNumOld) -> BigNumOld {
    old_methods::add_old(black_box(n), black_box(m))
}

fn bignumold_u128_add(n: BigNumOld, m: BigNumOld) -> BigNumOld {
    old_methods::add_u128(black_box(n), black_box(m))
}

fn bignumold_final_add(n: BigNumOld, m: BigNumOld) -> BigNumOld {
    black_box(n) + black_box(m)
}

fn bignum_binary_add(n: BigNum, m: BigNum) -> BigNum {
    black_box(n) + black_box(m)
}
fn bignum_arbitrary_add(n: BigNum, m: BigNum) -> BigNum {
    black_box(n) + black_box(m)
}

fn criterion_benchmark(c: &mut Criterion) {
    //c.bench_function("BigNumOld Original Add", |b| {
    //    b.iter(|| {
    //        bignumold_orig_add(
    //            black_box(BigNumOld::new(0xffff_ffff_ffff_ffff, 100)),
    //            black_box(BigNumOld::new(0xffff_ffff_ffff_ffff, 95)),
    //        )
    //    })
    //});
    //
    //c.bench_function("BigNumOld U128 Add", |b| {
    //    b.iter(|| {
    //        bignumold_u128_add(
    //            black_box(BigNumOld::new(0xffff_ffff_ffff_ffff, 100)),
    //            black_box(BigNumOld::new(0xffff_ffff_ffff_ffff, 95)),
    //        )
    //    })
    //});
    //
    //c.bench_function("BigNumOld Final Add", |b| {
    //    b.iter(|| {
    //        bignumold_final_add(
    //            black_box(BigNumOld::new(0xffff_ffff_ffff_ffff, 100)),
    //            black_box(BigNumOld::new(0xffff_ffff_ffff_ffff, 95)),
    //        )
    //    })
    //});

    c.bench_function("BigNum Binary Add", |b| {
        b.iter(|| {
            bignum_binary_add(
                black_box(BigNum::new_bin(0xffff_ffff_ffff_ffff, 100)),
                black_box(BigNum::new_bin(0xffff_ffff_ffff_ffff, 95)),
            )
        })
    });
    c.bench_function("BigNum Octal Add", |b| {
        b.iter(|| {
            bignum_binary_add(
                black_box(BigNum::new_oct(0xffff_ffff_ffff_ffff, 100)),
                black_box(BigNum::new_oct(0xffff_ffff_ffff_ffff, 95)),
            )
        })
    });
    c.bench_function("BigNum Decimal Add", |b| {
        b.iter(|| {
            bignum_binary_add(
                black_box(BigNum::new_dec(0xffff_ffff_ffff_ffff, 100)),
                black_box(BigNum::new_dec(0xffff_ffff_ffff_ffff, 95)),
            )
        })
    });
    c.bench_function("BigNum Hexadecimal Add", |b| {
        b.iter(|| {
            bignum_binary_add(
                black_box(BigNum::new_hex(0xffff_ffff_ffff_ffff, 100)),
                black_box(BigNum::new_hex(0xffff_ffff_ffff_ffff, 95)),
            )
        })
    });

    c.bench_function("BigNum Arbitrary Add", |b| {
        b.iter(|| {
            bignum_binary_add(
                black_box(BigNum::new(0xffff_ffff_ffff_ffff, 100, 53)),
                black_box(BigNum::new(0xffff_ffff_ffff_ffff, 95, 53)),
            )
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

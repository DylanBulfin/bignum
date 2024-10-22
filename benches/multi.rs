use bignum::BigNum;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn multi16(n: BigNum, m: u16) -> BigNum {
    n * m
}

fn multi64(n: BigNum, m: u64) -> BigNum {
    n * m
}

fn add_old(n: BigNum, m: BigNum) -> BigNum {
    bignum::add_old(n, m)
}

fn add_new(n: BigNum, m: BigNum) -> BigNum {
    bignum::add_new(n, m)
}

fn add_newer(n: BigNum, m: BigNum) -> BigNum {
    bignum::add_newer(n, m)
}

//I've made my decision for now, leaving in case I need to test more later
fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("multi16", |b| {
        b.iter(|| {
            multi16(
                black_box(BigNum::new(0xffff_ffff_ffff_ffff, 185)),
                black_box(0x1fd4),
            )
        })
    });

    c.bench_function("multi64", |b| {
        b.iter(|| {
            multi64(
                black_box(BigNum::new(0xffff_ffff_ffff_ffff, 185)),
                black_box(0x1fd4),
            )
        })
    });

    c.bench_function("add_old", |b| {
        b.iter(|| {
            add_old(
                black_box(BigNum::new(0xffff_ffff_ffff_ffff, 100)),
                black_box(BigNum::new(0xffff_ffff_ffff_ffff, 95))
            )
        })
    });

    c.bench_function("add_new", |b| {
        b.iter(|| {
            add_new(
                black_box(BigNum::new(0xffff_ffff_ffff_ffff, 100)),
                black_box(BigNum::new(0xffff_ffff_ffff_ffff, 95))
            )
        })
    });

    c.bench_function("add_newer", |b| {
        b.iter(|| {
            add_newer(
                black_box(BigNum::new(0xffff_ffff_ffff_ffff, 100)),
                black_box(BigNum::new(0xffff_ffff_ffff_ffff, 95))
            )
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

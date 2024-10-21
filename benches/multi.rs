use bignum::BigNum;
use criterion::{criterion_group, criterion_main, Criterion};
use std::hint::black_box;

fn multi16(n: BigNum, m: u16) -> BigNum {
    n * m
}

fn multi64(n: BigNum, m: u64) -> BigNum {
    n * m
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
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

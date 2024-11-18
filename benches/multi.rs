use std::hint::black_box;

use bignum::{
    //bignumold::{old_impl::BigNumOld, old_methods},
    create_default_base,
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

fn bignumred_binary_add(n: BigNumBase<Binary>, m: BigNumBase<Binary>) -> BigNumBase<Binary> {
    black_box(n) + black_box(m)
}

fn bignumred_octal_add(n: BigNumBase<Octal>, m: BigNumBase<Octal>) -> BigNumBase<Octal> {
    black_box(n) + black_box(m)
}

fn bignumred_decimal_add(n: BigNumBase<Decimal>, m: BigNumBase<Decimal>) -> BigNumBase<Decimal> {
    black_box(n) + black_box(m)
}

fn bignumred_hexadecimal_add(
    n: BigNumBase<Hexadecimal>,
    m: BigNumBase<Hexadecimal>,
) -> BigNumBase<Hexadecimal> {
    black_box(n) + black_box(m)
}

fn bignumred_arbitrary_add(n: BigNumBase<Base61>, m: BigNumBase<Base61>) -> BigNumBase<Base61> {
    black_box(n) + black_box(m)
}
fn bignumred_arbitrary_add2(n: BigNumBase<Base300>, m: BigNumBase<Base300>) -> BigNumBase<Base300> {
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
            bignumred_binary_add(
                black_box(BigNumBase::new(0xffff_ffff_ffff_ffff, 100)),
                black_box(BigNumBase::new(0xffff_ffff_ffff_ffff, 95)),
            )
        })
    });
    c.bench_function("BigNum Octal Add", |b| {
        b.iter(|| {
            bignumred_octal_add(
                black_box(BigNumBase::new(0xffff_ffff_ffff_ffff, 100)),
                black_box(BigNumBase::new(0xffff_ffff_ffff_ffff, 95)),
            )
        })
    });
    c.bench_function("BigNum Decimal Add", |b| {
        b.iter(|| {
            bignumred_decimal_add(
                black_box(BigNumBase::new(0xffff_ffff_ffff_ffff, 100)),
                black_box(BigNumBase::new(0xffff_ffff_ffff_ffff, 95)),
            )
        })
    });
    c.bench_function("BigNum Hexadecimal Add", |b| {
        b.iter(|| {
            bignumred_hexadecimal_add(
                black_box(BigNumBase::new(0xffff_ffff_ffff_ffff, 100)),
                black_box(BigNumBase::new(0xffff_ffff_ffff_ffff, 95)),
            )
        })
    });
    //c.bench_function("BigNum Arbitrary Add", |b| {
    //    b.iter(|| {
    //        bignumred_arbitrary_add(
    //            black_box(BigNumBase::new(0xffff_ffff_ffff_ffff, 100)),
    //            black_box(BigNumBase::new(0xffff_ffff_ffff_ffff, 95)),
    //        )
    //    })
    //});
    c.bench_function("BigNum Arbitrary Add2", |b| {
        b.iter(|| {
            bignumred_arbitrary_add2(
                black_box(BigNumBase::new(0xffff_ffff_ffff_ffff, 100)),
                black_box(BigNumBase::new(0xffff_ffff_ffff_ffff, 95)),
            )
        })
    });

    //c.bench_function("BigNum Binary Add", |b| {
    //    b.iter(|| {
    //        bignumred_binary_add(
    //            black_box(BigNumBase::new(0xffff_ffff_ffff_ffff, 100)),
    //            black_box(BigNumBase::new(0xffff_ffff_ffff_ffff, 95)),
    //        )
    //    })
    //});

    //c.bench_function("BigNumSecond Octal Add", |b| {
    //    b.iter(|| {
    //        bignum_binary_add(
    //            black_box(BigNumSecond::new_oct(0xffff_ffff_ffff_ffff, 100)),
    //            black_box(BigNumSecond::new_oct(0xffff_ffff_ffff_ffff, 95)),
    //        )
    //    })
    //});
    //c.bench_function("BigNumSecond Decimal Add", |b| {
    //    b.iter(|| {
    //        bignum_binary_add(
    //            black_box(BigNumSecond::new_dec(0xffff_ffff_ffff_ffff, 100)),
    //            black_box(BigNumSecond::new_dec(0xffff_ffff_ffff_ffff, 95)),
    //        )
    //    })
    //});
    //c.bench_function("BigNumSecond Hexadecimal Add", |b| {
    //    b.iter(|| {
    //        bignum_binary_add(
    //            black_box(BigNumSecond::new_hex(0xffff_ffff_ffff_ffff, 100)),
    //            black_box(BigNumSecond::new_hex(0xffff_ffff_ffff_ffff, 95)),
    //        )
    //    })
    //});
    //
    //c.bench_function("BigNumSecond Arbitrary Add", |b| {
    //    b.iter(|| {
    //        bignum_binary_add(
    //            black_box(BigNumSecond::new(0xffff_ffff_ffff_ffff, 100, 53)),
    //            black_box(BigNumSecond::new(0xffff_ffff_ffff_ffff, 95, 53)),
    //        )
    //    })
    //});
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

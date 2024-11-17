use bignum::BigNum;

fn main() {
    for _ in 0..1000000 {
        let a = BigNum::new_bin(u64::MAX, 100);
        let b = BigNum::new_bin(u64::MAX, 95);

        let _ = a + b;
    }
}

use bignum::BigNumBase;

fn calculate_ranges(base: u16) -> ((u32, u32), (u64, u64)) {
    let mut powers = vec![];

    let mut exp = 0u32;
    let mut sig: u128 = 1;

    while sig <= u64::MAX as u128 {
        powers.push(sig as u64);

        exp += 1;
        sig *= base as u128;
    }

    let max = sig / (base as u128);
    let min = max / (base as u128);

    ((exp - 2, exp - 1), (min as u64, (max - 1) as u64))
}

fn main() {
    println!("{:?}", calculate_ranges(61));
}

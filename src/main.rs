fn main() {
    let mut exp = 0;
    let mut sig = 1u128;

    while sig <= u64::MAX as u128 {
        exp += 1;
        sig *= 3;
    }

    print!("{:x}\n{:020x}\n{:020x}", exp, sig / 3, u64::MAX);

    assert_eq!(sig / 3, 3u128.pow(exp - 1));
}

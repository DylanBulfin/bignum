#[cfg(test)]
mod tests {
    use bignum_proc_macro::create_efficient_base;

    #[test]
    fn testing() {
        create_efficient_base!(123);
        create_efficient_base!(34524);

        for (i, power) in BASE_123_POWERS.iter().enumerate() {
            assert_eq!(3u64.pow(i as u32), *power);
        }

        for (i, power) in BASE_34524_POWERS.iter().enumerate() {
            assert_eq!(34524u64.pow(i as u32), *power);
        }
        for (i, power) in BASE_34524_U128_POWERS.iter().enumerate() {
            assert_eq!(34524u128.pow(i as u32), *power);
        }
    }
}

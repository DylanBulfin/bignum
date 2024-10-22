/// Get the highest exp x such that n > 2^x
pub fn get_exp_u64(n: u64) -> u64 {
    (0..64).rev().find(|i| 1 << i & n != 0).unwrap_or(0)
}

/// Get the highest exp x such that n > 2^x
pub fn get_exp_u128(n: u128) -> u128 {
    (0..128).rev().find(|i| 1 << i & n != 0).unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exp_u64() {
        assert_eq!(get_exp_u64(2), 1);
        assert_eq!(get_exp_u64(4), 2);
        assert_eq!(get_exp_u64(7), 2);

        assert_eq!(get_exp_u64(u64::MAX), 63);
        assert_eq!(get_exp_u64(0), 0);
        assert_eq!(get_exp_u64(1), 0);
    }

    #[test]
    fn test_exp_u128() {
        assert_eq!(get_exp_u128(2), 1);
        assert_eq!(get_exp_u128(4), 2);
        assert_eq!(get_exp_u128(7), 2);

        assert_eq!(get_exp_u128(u128::MAX), 127);
        assert_eq!(get_exp_u128(0), 0);
        assert_eq!(get_exp_u128(1), 0);

        for i in 0..128 {
            assert_eq!(get_exp_u128(1 << i), i);
        }
        for i in 1..128 {
            // Checks numbers made up of all 1s
            assert_eq!(get_exp_u128((1 << i) - 1), i - 1);
        }
    }
}

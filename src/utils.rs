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
    use std::u64;

    use super::*;

    #[test]
    fn get_exp() {
        assert_eq!(super::get_exp_u64(2), 1);
        assert_eq!(super::get_exp_u64(4), 2);
        assert_eq!(super::get_exp_u64(7), 2);

        assert_eq!(super::get_exp_u64(u64::MAX), 63);
        assert_eq!(super::get_exp_u64(0), 0);
        assert_eq!(super::get_exp_u64(1), 0);
    }
}

pub fn get_exp_u64(mut n: u64) -> u64 {
    let mut pow = 63;

    while pow > 0 && n & 0x8000_0000_0000_0000 == 0 {
        pow -= 1;
        n <<= 1;
    }

    pow
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

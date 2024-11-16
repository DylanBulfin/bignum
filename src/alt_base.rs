use std::{cmp::Ordering, ops::Add};

use crate::{check_bases, error::BigNumResult};

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub struct BigNum {
    sig: u64,
    exp: u64,
    base: u16,
    range: (u8, u8),
}

//const OCTAL_POWERS: [u64; 23] = [
//
//];

impl BigNum {
    // TODO make these proper unsafe
    pub fn new_unsafe(sig: u64, exp: u64, base: u16) -> Self {
        if base < 2 {
            panic!("Expected base at least 2, found: {}", base);
        }

        Self {
            sig,
            exp,
            base,
            range: BigNum::calculate_sig_range(base),
        }
    }

    pub fn new_bin_unsafe(sig: u64, exp: u64) -> Self {
        Self::new_unsafe(sig, exp, 2)
    }

    pub fn new_dec_unsafe(sig: u64, exp: u64) -> Self {
        Self::new_unsafe(sig, exp, 10)
    }

    pub fn new(sig: u64, exp: u64, base: u16) -> Self {
        Self::try_from_parts(sig, exp, base).unwrap()
    }

    pub fn new_bin(sig: u64, exp: u64) -> Self {
        Self::try_from_parts_bin(sig, exp).unwrap()
    }

    pub fn try_from_parts_bin(sig: u64, exp: u64) -> BigNumResult<Self> {
        if sig == 0 && exp != 0 {
            Err(format!("Can't have sig of 0 with exp of {}", exp).into())
        } else if exp == 0 {
            Ok(Self::new_bin_unchecked(sig, 0))
        } else if sig >= 1 << 63 {
            Ok(Self::new_bin_unchecked(sig, exp))
        } else {
            let mag = sig.ilog2() as u64;

            if mag.saturating_add(exp) < 63 {
                Ok(Self::new_bin_unchecked(sig << exp, 0))
            } else {
                let adj = 63 - mag;

                Ok(Self::new_bin_unchecked(sig << adj, exp - adj))
            }
        }
    }

    pub fn try_from_parts(sig: u64, exp: u64, base: u16) -> BigNumResult<Self> {
        if sig == 0 && exp != 0 {
            Err(format!("Can't have sig of 0 with exp of {}", exp).into())
        } else if base == 2 {
            Self::try_from_parts_bin(sig, exp)
        } else if base == 8 {
            unimplemented!()
        } else if base == 10 {
            unimplemented!()
        } else if base == 16 {
            unimplemented!()
        } else {
            let range = Self::calculate_sig_range(base);

            //if sig > range.1 {
            //    // No adjustment needed
            //    Ok(Self::new(sig, exp, base))
            //} else {
            //}

            unimplemented!()
        }
    }

    /// Calculate the range of the significand for this `BigNum`
    pub fn calculate_sig_range(base: u16) -> (u8, u8) {
        if base == 2 {
            (63, 64)
        } else if base == 8 {
            (20, 21)
        } else if base == 10 {
            (18, 19)
        } else if base == 16 {
            (14, 15)
        } else {
            let mut exp = 0;
            let mut sig: u128 = 1;

            while sig < u64::MAX as u128 {
                exp += 1;
                sig *= base as u128;
            }

            (exp - 2, exp - 1)
        }
    }
}

impl Ord for BigNum {
    fn cmp(&self, other: &Self) -> Ordering {
        check_bases!(self, other, "compare");
        match self.exp.cmp(&other.exp) {
            Ordering::Equal => (),
            ord => return ord,
        }
        self.base.cmp(&other.base)
    }
}

impl PartialOrd for BigNum {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Add for BigNum {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        check_bases!(self, rhs, "add");

        let (max, min) = if self > rhs { (self, rhs) } else { (rhs, self) };
        let shift = max.exp - min.exp;

        let result = max.sig.wrapping_add(min.sig >> shift);

        if result < max.sig {
            // Wrapping occurred, handle this
            Self::try_from_parts_bin((1 << 63) + (result >> 1), max.exp + 1).unwrap()
        } else {
            Self::try_from_parts_bin(result, max.exp).unwrap()
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        alt_base::BigNum,
        error::{BigNumError, BigNumTestResult},
    };

    #[test]
    fn from_parts_bin_test() -> BigNumTestResult {
        // Check that adjustment is correct, especially around edge cases
        assert_eq!(BigNum::try_from_parts_bin(1, 0)?, BigNum::new(1, 0, 2));
        assert_eq!(
            BigNum::try_from_parts_bin(0b100, 2)?,
            BigNum::new(0b10000, 0, 2)
        );
        assert_eq!(
            BigNum::try_from_parts_bin(1 << 62, 20)?,
            BigNum::new(1 << 63, 19, 2)
        );
        assert_eq!(
            BigNum::try_from_parts_bin(1 << 62, 20)?,
            BigNum::new(1 << 63, 19, 2)
        );

        Ok(())
    }

    #[test]
    fn add_binary_test() -> BigNumTestResult {
        assert_eq!(
            BigNum::new(0x100, 0, 2) + BigNum::new(0x0100_0000, 4, 2),
            BigNum::new_unchecked(0x1000_0100, 0, 2)
        );
        assert_eq!(
            BigNum::new(0x1000_0000, 32, 2) + BigNum::new(0x0100_0000, 4, 2),
            BigNum::new_unchecked(0x1000_0000_1000_0000, 0, 2)
        );
        assert_eq!(
            BigNum::new(0xFFFF_FFFF, 32, 2) + BigNum::new(0x8000_0000, 1, 2),
            BigNum::new_unchecked(0x8000_0000_0000_0000, 1, 2)
        );

        Ok(())
    }
}

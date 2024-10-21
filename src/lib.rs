use std::{
    cmp::Ordering,
    ops::{Add, AddAssign, Sub},
};

use utils::get_exp_u64;

mod utils;

// Equal to 2^63
const MIN_BASE_VAL: u64 = 0x8000_0000_0000_0000;

/// Representation of large number. Formula is base * (2 ^ exp)
#[derive(Debug, Clone, Copy)]
pub struct BigNum {
    base: u64,
    exp: u64,
    // This field keeps me from accidentally constructing this struct manually
    invalidate: bool,
}

impl BigNum {
    /// Create a BigNum instance directly (e.g. not through the From trait)
    pub fn new(base: u64, exp: u64) -> Self {
        if base == 0 && exp != 0 {
            panic!("Invalid BigNum: base is 0 but exp is not")
        }
        if base < MIN_BASE_VAL && exp != 0 {
            panic!("Invalid BigNum: exp is non-zero but base in invalid form")
        }
        BigNum {
            base,
            exp,
            invalidate: false,
        }
    }

    /// The exponent x such that self = c * 2^x for some c between 0 and 1
    pub fn get_full_exp(&self) -> u64 {
        if self.exp == 0 {
            utils::get_exp_u64(self.base)
        } else {
            // Panics when self.exp + 63 > u64::MAX
            self.exp + 63
        }
    }
}

impl From<u64> for BigNum {
    fn from(value: u64) -> Self {
        BigNum::new(value, 0)
    }
}

impl From<u32> for BigNum {
    fn from(value: u32) -> Self {
        BigNum::new(value as u64, 0)
    }
}

impl From<u16> for BigNum {
    fn from(value: u16) -> Self {
        BigNum::new(value as u64, 0)
    }
}

impl From<u8> for BigNum {
    fn from(value: u8) -> Self {
        BigNum::new(value as u64, 0)
    }
}

impl From<i64> for BigNum {
    fn from(value: i64) -> Self {
        BigNum::new(value as u64, 0)
    }
}

impl From<i32> for BigNum {
    fn from(value: i32) -> Self {
        BigNum::new(value as u64, 0)
    }
}

impl From<i16> for BigNum {
    fn from(value: i16) -> Self {
        BigNum::new(value as u64, 0)
    }
}

impl From<i8> for BigNum {
    fn from(value: i8) -> Self {
        BigNum::new(value as u64, 0)
    }
}

impl PartialEq for BigNum {
    fn eq(&self, other: &Self) -> bool {
        self.base == other.base && self.exp == other.exp
    }
}

impl PartialOrd for BigNum {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.exp.partial_cmp(&other.exp) {
            Some(Ordering::Equal) => {}
            ord => return ord,
        }
        self.base.partial_cmp(&other.base)
    }
}

impl Add for BigNum {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if self.exp == 0 && rhs.exp == 0 {
            // Both numbers are in compact form, first try normal addition
            let result = self.base.wrapping_add(rhs.base);

            // If remainder is less than either base, overflow occurred
            if result < self.base || result < rhs.base {
                Self::new(MIN_BASE_VAL + (result >> 1), 1)
            } else {
                Self::new(result, 0)
            }
        } else {
            // At least one of the numbers is in expanded form, first find which is bigger
            let (min, max) = if self > rhs { (rhs, self) } else { (self, rhs) };

            // Calculate how much we need to shift the smaller number to align
            let shift = if min.exp == 0 {
                max.exp
            } else {
                max.get_full_exp() - min.get_full_exp()
            };

            if shift >= 64 || shift > min.get_full_exp() {
                // Shifting will leave us with 0 so don't bother, return max
                max
            } else {
                // Now we can add them, and handle any overflow
                let res = max.base.wrapping_add(min.base >> shift);

                // If result is less than either base, overflow occurred
                if res < max.base {
                    // Wrapping occurred, need to fix things up
                    Self::new(MIN_BASE_VAL + (res >> 1), max.exp + 1)
                } else {
                    Self::new(res, max.exp)
                }
            }
        }
    }
}

impl Add<u64> for BigNum {
    type Output = Self;

    fn add(self, rhs: u64) -> Self::Output {
        self + BigNum::from(rhs)
    }
}

impl AddAssign for BigNum {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl AddAssign<u64> for BigNum {
    fn add_assign(&mut self, rhs: u64) {
        *self = *self + rhs;
    }
}

impl Sub for BigNum {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        if rhs > self {
            // We can't have negative numbers
            panic!("Attempt to subtract with overflow")
        }

        if rhs.exp == 0 && self.exp == 0 {
            // Both are in compact form, and since self > rhs we know we won't underflow
            Self::new(self.base - rhs.base, 0)
        } else {
            // Find how much we need to shift the smaller number to align
            let shift = if rhs.exp == 0 {
                self.exp
            } else {
                self.get_full_exp() - rhs.get_full_exp()
            };

            if shift >= 64 || shift > rhs.get_full_exp() {
                if self.base == MIN_BASE_VAL && (shift == 64 || shift == rhs.get_full_exp() + 1) {
                    // Base is at the minimum value so we need to handle edge case
                    // E.g. BigNum::new(0x8000_0000_0000_0000, 1) - BigNum::from(1)
                    // shift = 1, get_full_exp = 0, so normally we would skip
                    // But since base is at min value we need to decrease exp and normalize
                    BigNum::new(u64::MAX, self.exp - 1)
                } else {
                    // Shifting will leave us with 0 so don't bother, return self
                    self
                }
            } else {
                let res = self.base - (rhs.base >> shift);

                // We know that underflow won't happen, but if numbers are equal
                // we need to handle 0 case
                if res == 0 {
                    Self::new(0, 0)
                } else {
                    // If new resulting base is not in range we need to fix
                    let adjustment = 63 - get_exp_u64(res);

                    Self::new(res << adjustment, self.exp - adjustment)
                }
            }
        }
    }
}

impl Sub<u64> for BigNum {
    type Output = Self;

    fn sub(self, rhs: u64) -> Self::Output {
        self - BigNum::from(rhs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}

    #[test]
    fn add() {
        let a: BigNum = 1.into();
        let b: BigNum = 1000000000001u64.into();

        assert_eq!(a + b, 1000000000002u64.into());

        let c: BigNum = u64::MAX.into();
        assert_eq!(a + c, BigNum::new(0x8000_0000_0000_0000, 1));

        let d = BigNum::new(0x8000_0000_0000_0000, 1);
        let e: BigNum = 2.into();
        let f: BigNum = 4.into();
        assert_eq!(a + d, d);
        assert_eq!(d + e, BigNum::new(0x8000_0000_0000_0001, 1));
        assert_eq!(d + f, BigNum::new(0x8000_0000_0000_0002, 1));

        let g = BigNum::new(0x8000_0000_0000_0000, 10000);
        let h = BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 9937);
        assert_eq!(g + h, BigNum::new(0x8000_0000_0000_0001, 10000));

        let i = BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 10000);
        let j = BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 9937);
        assert_eq!(i + j, BigNum::new(0x8000_0000_0000_0000, 10001));

        let k = BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 10000);
        let l = BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 10000);
        assert_eq!(k + l, BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 10001));
    }

    #[test]
    fn sub() {
        let a: BigNum = 1000000000001u64.into();
        let b: BigNum = 1.into();

        assert_eq!(a - b, 1000000000000u64.into());

        let c: BigNum = 0x8000_0000_0000_0000u64.into();
        assert_eq!(c - b, 0x7FFF_FFFF_FFFF_FFFFu64.into());

        let d = BigNum::new(0x8000_0000_0000_0000, 1);
        let e: BigNum = 2.into();
        let f: BigNum = 4.into();
        assert_eq!(d - b, BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 0));
        assert_eq!(d - e, BigNum::new(0xFFFF_FFFF_FFFF_FFFE, 0));
        assert_eq!(d - f, BigNum::new(0xFFFF_FFFF_FFFF_FFFC, 0));

        let g = BigNum::new(0x8000_0000_0000_0001, 10000);
        let h = BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 9937);
        assert_eq!(g - h, BigNum::new(0x8000_0000_0000_0000, 10000));

        assert_eq!(a - a, 0u64.into());
        assert_eq!(b - b, 0u64.into());
        assert_eq!(c - c, 0u64.into());
        assert_eq!(d - d, 0u64.into());
        assert_eq!(e - e, 0u64.into());
        assert_eq!(f - f, 0u64.into());
        assert_eq!(g - g, 0u64.into());
        assert_eq!(h - h, 0u64.into());
    }
}

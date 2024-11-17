use std::{cmp::Ordering, ops::Add};

use crate::impl_for_type;

#[derive(Clone, Copy)]
pub struct CustomBase {
    test1: [u8; 10000000000],
}

/// This trait is used to indicate that a type is a valid base for a BigNum. It
/// contains metadata and functions that can be used to efficiently handle arbitrary
/// bases. Importantly you must ensure the following (base is just an instance of the
/// type, since some methods don't have static versions):
/// - `EXP_RANGE.1 = EXP.RANGE.0 + 1`
/// - `EXP_RANGE.0 > 0`
/// - `SIG_RANGE.0 = base.pow(EXP_RANGE.0)`
/// - `SIG_RANGE.1 = base.pow(EXP_RANGE.1) - 1`
/// - `base.pow(n) = NUMBER.exp(n)` for all `n <= EXP_RANGE.1`
/// - `base.divide(lhs, exp) = lhs / NUMBER.exp(n)` for all `n <= EXP_RANGE.1`
/// - `base.multiply(lhs, exp) = lhs * NUMBER.exp(n)` for all `n <= EXP_RANGE.1`
/// - `base.get_mag(n)` should return the highest exponent `x` such that
///     `n >= base.pow(x)`, for all `n <= EXP_RANGE.1`
/// - `SIG_RANGE.0 * NUMBER > u64::MAX`
///     - This restriction allows us to conveniently handle some construction cases
///
/// Some of these calculations have the potential to overflow a `u64` so you may need to
/// think of other ways to compute them if you plan to verify them manually.
///
/// Additionally, the implementers will be copied on every math operation and in some
/// other contexts, so ensure that they are lightweight. E.g. even though
/// ```
/// #[derive(Clone, Copy)]
/// pub struct CustomBase {
///     metadata: [u8; 10000000000],
/// }
/// ```
/// is valid, it's ill-advised here. If you need a table of powers I would recommend a
/// global const array that you reference in the `pow` method
pub trait Base: Copy {
    /// This contains the numeric value of the type. E.g. for binary 2, for decimal 10,
    /// etc
    const NUMBER: u16;
    /// This is the (non-inclusive) range of the significand's exponent. E.g. since for
    /// binary the range of the significand is [2^63, 2^64), this is equal to (63, 64).
    /// This is irrelevant for 'compact' values, e.g. where `n.exp == 0`.
    const EXP_RANGE: (u32, u32);
    /// This is the inclusive range of values for the significand. E.g. since for binary
    /// the range of the significand is [2^63, 2^64) this is equal to `(2^63, (2^64) - 1)`
    /// This is irrelevant for 'compact' values, e.g. where `n.exp == 0`
    const SIG_RANGE: (u64, u64);

    /// This is a function that computes `Self::NUMBER ^ exp`. It has a default
    /// implementation that computes the value directly. It is recommended to override
    /// this behavior if there is a trick to the exponentiation (like how for binary
    /// `2^n = (1 << n)`). You can also create a gloabl const lookup table and reference
    /// that.
    fn pow(&self, exp: u32) -> u64 {
        (Self::NUMBER as u64).pow(exp)
    }

    /// Function that can create an instance of this Base. Users should never have to
    /// manually create instances of this type. This is called implicitly on every
    /// call to `BigNumRed<Self>::new()` so it should be as lightweight as possible. Note
    /// that it is not called when creating a BigNumRed<Self> from another, like when
    /// performing an addition. In this case it is simply copied
    fn new() -> Self;

    /// This is a function that computes `lhs / (Self::NUMBER ^ exp)`. There is a default
    /// implementation that obtains the value of `Self::NUMBER ^ exp` via the `pow` method
    /// for this type, and does a division. It is recommended to override this method if
    /// there is a trick for the division (like how in binary,
    /// `lhs / (2 ^ exp) = lhs >> exp`, or in octal `lhs / (8 ^ exp) = lhs >> (3 * exp)`
    fn divide(&self, lhs: u64, exp: u32) -> u64 {
        lhs / self.pow(exp)
    }
    /// This is a function that computes `lhs * (Self::NUMBER ^ exp)`. There is a default
    /// implementation that obtains the value of `Self::NUMBER ^ exp` via the `pow` method
    /// for this type, and does a multiplication. It is recommended to override this
    /// method if there is a trick for the division (like how in binary,
    /// `lhs * (2 ^ exp) = lhs << exp`, or in octal `lhs * (8 ^ exp) = lhs << (3 * exp)`
    fn multiply(&self, lhs: u64, exp: u32) -> u64 {
        lhs / self.pow(exp)
    }

    /// This is a function that computes the highest power `x` such that
    /// `sig >= (Self::NUMBER ^ x)`. There is a default implementation that uses
    /// `Self::pow` to get sequential powers until it finds an invalid one. It is
    /// recommended to override this if there is a more efficient way (like how for binary
    /// the magnitude of `n` is given by `n.ilog2()`, or if you store a table of powers
    /// you can iterate over that).
    fn get_mag(&self, sig: u64) -> u64 {
        let mut exp = 0;
        let mut curr = 1;

        while sig >= curr {
            exp += 1;
            curr *= Self::NUMBER as u64;
        }

        exp - 1
    }

    /// This method just fetches `Self::NUMBER` but is provided as an instance method for
    /// convenience. Overriding it is undefined behavior
    fn as_number(&self) -> u16 {
        Self::NUMBER
    }

    /// This method just fetches `Self::EXP_RANGE` but is provided as an instance method
    /// for convenience. Overriding it is undefined behavior
    fn exp_range(&self) -> (u32, u32) {
        Self::EXP_RANGE
    }

    /// this method just fetches `self::sig_range` but is provided as an instance method
    /// for convenience. Overriding it is undefined behavior
    fn sig_range(&self) -> (u64, u64) {
        Self::SIG_RANGE
    }

    /// This method just fetches `Self::SIG_RANGE.0` but is provided as an instance method
    /// for convenience. Overriding it is undefined behavior
    fn min_sig(&self) -> u64 {
        Self::SIG_RANGE.0
    }

    /// This method just fetches `Self::SIG_RANGE.1` but is provided as an instance method
    /// for convenience. Overriding it is undefined behavior
    fn max_sig(&self) -> u64 {
        Self::SIG_RANGE.1
    }

    /// This method just fetches `Self::EXP_RANGE.0` but is provided as an instance method
    /// for convenience. Overriding it is undefined behavior
    fn min_exp(&self) -> u32 {
        Self::EXP_RANGE.0
    }

    /// This method just fetches `Self::EXP_RANGE.1` but is provided as an instance method
    /// for convenience. Overriding it is undefined behavior
    fn max_exp(&self) -> u32 {
        Self::EXP_RANGE.1
    }
}

// Since these types are powers of two I can implement Base efficiently with no metadata
#[derive(Clone, Copy, Debug)]
pub struct Binary;
#[derive(Clone, Copy, Debug)]
pub struct Octal;
#[derive(Clone, Copy, Debug)]
pub struct Hexadecimal;

impl Base for Binary {
    const NUMBER: u16 = 2;
    const EXP_RANGE: (u32, u32) = (63, 64);
    const SIG_RANGE: (u64, u64) = (1 << 63, u64::MAX);

    fn new() -> Self {
        Self
    }

    fn pow(&self, exp: u32) -> u64 {
        1 << exp
    }

    fn divide(&self, lhs: u64, exp: u32) -> u64 {
        lhs >> exp
    }
    fn multiply(&self, lhs: u64, exp: u32) -> u64 {
        lhs << exp
    }

    fn get_mag(&self, sig: u64) -> u64 {
        sig.ilog2() as u64
    }
}

impl Base for Octal {
    const NUMBER: u16 = 8;
    const EXP_RANGE: (u32, u32) = (20, 21);
    const SIG_RANGE: (u64, u64) = (1 << 60, (1 << 63) - 1);

    fn new() -> Self {
        Self
    }

    fn pow(&self, exp: u32) -> u64 {
        1 << (3 * exp)
    }

    fn divide(&self, lhs: u64, exp: u32) -> u64 {
        lhs >> (3 * exp)
    }
    fn multiply(&self, lhs: u64, exp: u32) -> u64 {
        lhs << (3 * exp)
    }

    fn get_mag(&self, sig: u64) -> u64 {
        (sig.ilog2() / 3) as u64
    }
}

impl Base for Hexadecimal {
    const NUMBER: u16 = 16;
    const EXP_RANGE: (u32, u32) = (15, 16);
    const SIG_RANGE: (u64, u64) = (1 << 60, u64::MAX);

    fn new() -> Self {
        Self
    }

    fn pow(&self, exp: u32) -> u64 {
        1 << (exp << 2)
    }

    fn divide(&self, lhs: u64, exp: u32) -> u64 {
        lhs >> (exp << 2)
    }
    fn multiply(&self, lhs: u64, exp: u32) -> u64 {
        lhs << (exp << 2)
    }

    fn get_mag(&self, sig: u64) -> u64 {
        (sig.ilog2() as u64) << 2
    }
}

#[derive(Clone, Copy, Debug)]
pub struct BigNumRed<T>
where
    T: Base,
{
    sig: u64,
    exp: u64,
    base: T,
}

impl<T> BigNumRed<T>
where
    T: Base,
{
    pub fn new(sig: u64, exp: u64) -> Self {
        let base = T::new();

        if sig >= base.min_sig() && sig <= base.max_sig() {
            Self { sig, exp, base }
        } else if sig > base.max_sig() {
            // Since we know `base.max_sig() * base.as_number() > u64::MAX`, we also know
            // that `sig / base.as_number() <= base.max_sig()`
            Self {
                sig: sig / base.as_number() as u64,
                exp: exp + 1,
                base,
            }
        } else if exp == 0 {
            Self { sig, exp, base }
        } else if sig == 0 {
            panic!("Unable to create BigNum with exp of {} and sig of 0", exp);
        } else {
            let mag = base.get_mag(sig);

            if mag.saturating_add(exp) <= base.min_exp() as u64 {
                Self {
                    sig: sig * base.pow(exp as u32),
                    exp: 0,
                    base,
                }
            } else {
                let adj = base.min_exp() as u64 - mag;

                Self {
                    sig: sig * base.pow(adj as u32),
                    exp: exp - adj,
                    base,
                }
            }
        }
    }

    // Creates a BigNumRed directly from values, panicking if not possible. This is mostly
    // for testing but may be more performant on inputs that are guaranteed valid
    pub fn new_raw(sig: u64, exp: u64) -> Self {
        let base = T::new();

        if sig > base.max_sig() || exp != 0 && (sig > base.max_sig() || sig < base.min_sig()) {
            panic!(
                "Unable to create BigNumRed with sig {} and exp {}",
                sig, exp
            );
        } else {
            Self { sig, exp, base }
        }
    }
}

impl<T> PartialEq for BigNumRed<T>
where
    T: Base,
{
    fn eq(&self, other: &Self) -> bool {
        self.sig == other.sig && self.exp == other.exp
    }
}

impl<T> Eq for BigNumRed<T> where T: Base {}

impl<T> Ord for BigNumRed<T>
where
    T: Base,
{
    fn cmp(&self, other: &Self) -> Ordering {
        match self.exp.cmp(&other.exp) {
            Ordering::Less => Ordering::Less,
            Ordering::Greater => Ordering::Greater,
            Ordering::Equal => match self.sig.cmp(&other.sig) {
                Ordering::Less => Ordering::Less,
                Ordering::Greater => Ordering::Greater,
                Ordering::Equal => Ordering::Equal,
            },
        }
    }
}

impl<T> PartialOrd for BigNumRed<T>
where
    T: Base,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

macro_rules! impl_from_types {
    ($($ty:ty),+) => {
        $(
            impl<T> From<$ty> for BigNumRed<T> where T: Base {
                fn from(value: $ty) -> Self {
                    Self::new(value as u64, 0)
                }
            }
        )+
    };
}

impl_from_types!(u64, u32);

impl<T> Add for BigNumRed<T>
where
    T: Base,
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let base = self.base;

        let (max, min) = if self > rhs { (self, rhs) } else { (rhs, self) };
        let shift = max.exp - min.exp;

        if shift >= base.max_exp() as u64 {
            // This shift is guaranteed to result in 0 on lhs, no need to compute
            return max;
        }

        let result = max.sig.wrapping_add(base.divide(min.sig, shift as u32));

        let (sig, exp) = if result < max.sig {
            // Wrapping occurred, handle it
            (base.min_sig() + base.divide(result, 1), max.exp + 1)
        } else if base.as_number() != 2 && result > base.max_sig() {
            (base.divide(result, 1), max.exp + 1)
        } else {
            (result, max.exp)
        };

        Self {
            sig,
            exp,
            base: self.base,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{error::BigNumTestResult, redesign::Binary};

    type BigNum = BigNumRed<Binary>;

    #[test]
    fn new_binary_test() -> BigNumTestResult {
        // Check that adjustment is correct, especially around edge cases
        assert_eq!(BigNum::new(1, 0), BigNum::new_raw(1, 0));
        assert_eq!(BigNum::new(0b100, 2), BigNum::new_raw(0b10000, 0));
        assert_eq!(BigNum::new(1 << 62, 20), BigNum::new_raw(1 << 63, 19));
        assert_eq!(BigNum::new(1 << 62, 20), BigNum::new_raw(1 << 63, 19));

        Ok(())
    }

    #[test]
    fn add_binary_test() -> BigNumTestResult {
        assert_eq!(
            BigNum::new(0x100, 0) + BigNum::new(0x0100_0000, 4),
            BigNum::new_raw(0x1000_0100, 0)
        );
        assert_eq!(
            BigNum::new(0x1000_0000, 32) + BigNum::new(0x0100_0000, 4),
            BigNum::new_raw(0x1000_0000_1000_0000, 0)
        );
        assert_eq!(
            BigNum::new(0xFFFF_FFFF, 32) + BigNum::new(0x8000_0000, 1),
            BigNum::new_raw(0x8000_0000_0000_0000, 1)
        );
        assert_eq!(
            BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 1) + 0x1u32,
            BigNum::new_raw(0xFFFF_FFFF_FFFF_FFFF, 1)
        );
        assert_eq!(
            BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 1) + 0x2u32,
            BigNum::new_raw(0x8000_0000_0000_0000)
        );

        Ok(())
    }
}

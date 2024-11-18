use std::{
    cmp::Ordering,
    ops::{Add, AddAssign},
};

use consts::{DEC_EXP_RANGE, DEC_POWERS, DEC_SIG_RANGE, HEX_EXP_RANGE, HEX_SIG_RANGE};

pub(crate) mod consts;
pub(crate) mod error;
pub mod macros;

#[derive(Clone, Copy, Debug)]
pub struct ExpRange(u32, u32);

impl ExpRange {
    pub const fn new(min: u32, max: u32) -> Self {
        Self(min, max)
    }

    pub const fn from(range: (u32, u32)) -> Self {
        Self(range.0, range.1)
    }

    pub fn min(&self) -> u32 {
        self.0
    }

    pub fn max(&self) -> u32 {
        self.1
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SigRange(u64, u64);

impl SigRange {
    pub const fn new(min: u64, max: u64) -> Self {
        Self(min, max)
    }

    pub const fn from(range: (u64, u64)) -> Self {
        Self(range.0, range.1)
    }

    pub fn min(&self) -> u64 {
        self.0
    }

    pub fn max(&self) -> u64 {
        self.1
    }
}

/// This trait is used to indicate that a type is a valid base for a BigNumBase. It
/// contains metadata and functions that can be used to efficiently handle arbitrary
/// bases. Importantly you must ensure the following (base is just an instance of the
/// type, since some methods don't have static versions):
/// - `exp_range().max() = exp_range().min() + 1`
/// - `exp_range().min() > 0`
/// - `sig_range().min() = base.pow(exp_range().min())`
/// - `sig_range().max() = base.pow(exp_range().max()) - 1`
/// - `base.pow(n) = NUMBER.exp(n)` for all `n <= exp_range().max()`
/// - `base.divide(lhs, exp) = lhs / NUMBER.exp(n)` for all `n <= exp_range().max()`
/// - `base.multiply(lhs, exp) = lhs * NUMBER.exp(n)` for all `n <= exp_range().max()`
/// - `base.get_mag(n)` should return the highest exponent `x` such that
///     `n >= base.pow(x)`, for all `n <= exp_range().max()`
/// - `sig_range().min() * NUMBER > u64::MAX`
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
/// global const array that you reference in the `pow` method. The recommended format for
/// a non-performance critical simple Base definition and implementation is:
/// ```
/// use bignum::{ExpRange, SigRange, Base};
///
/// #[derive(Clone, Copy)]
/// pub struct Base13 {
///     exp_range: ExpRange,
///     sig_range: SigRange
/// }
///
/// impl Base for Base13{
///     const NUMBER: u16 = 13;
///
///     fn new() -> Self {
///         let (exp_range, sig_range) = Self::calculate_ranges();
///         Self {exp_range, sig_range}
///     }
///
///     fn exp_range(&self) -> ExpRange {
///         self.exp_range
///     }
///
///     fn sig_range(&self) -> SigRange {
///         self.sig_range
///     }
/// }
/// ```
/// In fact there's a macro to do just this
pub trait Base: Copy {
    /// This contains the numeric value of the type. E.g. for binary 2, for decimal 10,
    /// etc
    const NUMBER: u16;

    /// Function that can create an instance of this Base. Users should never have to
    /// manually create instances of this type. This is called implicitly on every
    /// call to `BigNumBase<Self>::new()` so it should be as lightweight as possible. Note
    /// that it is not called when creating a BigNumBase<Self> from another, like when
    /// performing an addition. In this case it is simply copied
    fn new() -> Self;

    fn exp_range(&self) -> ExpRange;

    fn sig_range(&self) -> SigRange;

    /// This is a function that computes `Self::NUMBER ^ exp`. It has a default
    /// implementation that computes the value directly. It is recommended to override
    /// this behavior if there is a trick to the exponentiation (like how for binary
    /// `2^n = (1 << n)`). You can also create a gloabl const lookup table and reference
    /// that.
    fn pow(&self, exp: u32) -> u64 {
        (Self::NUMBER as u64).pow(exp)
    }

    fn calculate_ranges() -> (ExpRange, SigRange) {
        let mut exp = 0u32;
        let mut curr = 1u128;

        while curr <= u64::MAX as u128 {
            exp += 1;
            curr *= Self::NUMBER as u128;
        }

        let max_sig = (curr / Self::NUMBER as u128) as u64;
        let min_sig = max_sig / Self::NUMBER as u64;

        (ExpRange(exp - 2, exp - 1), SigRange(min_sig, max_sig))
    }

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
        lhs * self.pow(exp)
    }

    /// This is a function that computes the highest power `x` such that
    /// `sig >= (Self::NUMBER ^ x)`. There is a default implementation that simply checks
    /// each multiple in order. It is recommended to override this if there is a more
    /// efficient way (like how for binary the magnitude of `n` is given by `n.ilog2()`,
    /// or if you store a table of powers you can iterate over that).
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
}

// Since these types are powers of two I can implement Base efficiently with no metadata
#[derive(Clone, Copy, Debug)]
pub struct Binary;
#[derive(Clone, Copy, Debug)]
pub struct Octal;
#[derive(Clone, Copy, Debug)]
pub struct Hexadecimal;

#[derive(Clone, Copy, Debug)]
pub struct Decimal;

impl Base for Binary {
    const NUMBER: u16 = 2;
    fn new() -> Self {
        Self
    }

    fn exp_range(&self) -> ExpRange {
        ExpRange(63, 64)
    }

    fn sig_range(&self) -> SigRange {
        SigRange(1 << 63, u64::MAX)
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

    fn new() -> Self {
        Self
    }

    fn exp_range(&self) -> ExpRange {
        ExpRange(20, 21)
    }

    fn sig_range(&self) -> SigRange {
        SigRange(1 << 60, (1 << 63) - 1)
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

    fn new() -> Self {
        Self
    }

    fn exp_range(&self) -> ExpRange {
        //ExpRange(15, 16)
        ExpRange::from(HEX_EXP_RANGE)
    }

    fn sig_range(&self) -> SigRange {
        //SigRange(1 << 60, u64::MAX)
        SigRange::from(HEX_SIG_RANGE)
    }

    fn pow(&self, exp: u32) -> u64 {
        //1 << (exp << 2)

    }

    //fn divide(&self, lhs: u64, exp: u32) -> u64 {
    //    lhs >> (exp << 2)
    //}
    //
    //fn multiply(&self, lhs: u64, exp: u32) -> u64 {
    //    lhs << (exp << 2)
    //}

    fn get_mag(&self, sig: u64) -> u64 {
        (sig.ilog2() as u64) << 2
    }
}

impl Base for Decimal {
    const NUMBER: u16 = 10;

    fn new() -> Self {
        Self
    }

    fn exp_range(&self) -> ExpRange {
        ExpRange(DEC_EXP_RANGE.0, DEC_EXP_RANGE.1)
    }

    fn sig_range(&self) -> SigRange {
        SigRange(DEC_SIG_RANGE.0, DEC_SIG_RANGE.1)
    }

    fn pow(&self, exp: u32) -> u64 {
        DEC_POWERS[exp as usize]
    }

    fn get_mag(&self, sig: u64) -> u64 {
        sig.ilog10() as u64
    }
}

#[derive(Clone, Copy, Debug)]
pub struct BigNumBase<T>
where
    T: Base,
{
    sig: u64,
    exp: u64,
    base: T,
}

impl<T> BigNumBase<T>
where
    T: Base,
{
    pub fn new(sig: u64, exp: u64) -> Self {
        let base = T::new();
        let SigRange(min_sig, max_sig) = base.sig_range();
        let ExpRange(min_exp, _) = base.exp_range();

        if sig >= min_sig && sig <= max_sig {
            Self { sig, exp, base }
        } else if sig > max_sig {
            // Since we know `max_sig * base.as_number() > u64::MAX`, we also know
            // that `sig / base.as_number() <= max_sig`
            Self {
                sig: base.divide(sig, 1),
                exp: exp + 1,
                base,
            }
        } else if exp == 0 {
            Self { sig, exp, base }
        } else if sig == 0 {
            panic!(
                "Unable to create BigNumBase with exp of {} and sig of 0",
                exp
            );
        } else {
            let mag = base.get_mag(sig);

            if mag.saturating_add(exp) <= min_exp as u64 {
                Self {
                    sig: base.multiply(sig, exp as u32),
                    exp: 0,
                    base,
                }
            } else {
                let adj = min_exp as u64 - mag;

                Self {
                    sig: base.multiply(sig, adj as u32),
                    exp: exp - adj,
                    base,
                }
            }
        }
    }

    // Creates a BigNumBase directly from values, panicking if not possible. This is mostly
    // for testing but may be more performant on inputs that are guaranteed valid
    pub fn new_raw(sig: u64, exp: u64) -> Self {
        let base = T::new();
        let SigRange(min_sig, max_sig) = base.sig_range();

        if sig > max_sig || exp != 0 && sig < min_sig {
            panic!(
                "Unable to create BigNumBase with sig {} and exp {}",
                sig, exp
            );
        } else {
            Self { sig, exp, base }
        }
    }
}

impl<T> PartialEq for BigNumBase<T>
where
    T: Base,
{
    fn eq(&self, other: &Self) -> bool {
        self.sig == other.sig && self.exp == other.exp
    }
}

impl<T> Eq for BigNumBase<T> where T: Base {}

impl<T> Ord for BigNumBase<T>
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

impl<T> PartialOrd for BigNumBase<T>
where
    T: Base,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

macro_rules! impl_for_types {
    ($($ty:ty),+) => {
        $(
            impl<T> From<$ty> for BigNumBase<T> where T: Base {
                fn from(value: $ty) -> Self {
                    Self::new(value as u64, 0)
                }
            }

            impl<T> Add<$ty> for BigNumBase<T> where T: Base {
                type Output = Self;

                fn add(self, rhs: $ty) -> Self::Output {
                    self + BigNumBase::from(rhs)
                }
            }

            impl<T> Add<BigNumBase<T>> for $ty where T: Base {
                type Output = BigNumBase<T>;

                fn add(self, rhs: BigNumBase<T>) -> Self::Output {
                    rhs + BigNumBase::from(self)
                }
            }

            impl<T> AddAssign<$ty> for BigNumBase<T> where T: Base {
                fn add_assign(&mut self, rhs: $ty) {
                    *self = *self + BigNumBase::from(rhs);
                }
            }
        )+
    };
}

impl_for_types!(u64, u32, i32);

impl<T> Add for BigNumBase<T>
where
    T: Base,
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let base = self.base;
        let SigRange(min_sig, max_sig) = base.sig_range();
        let ExpRange(_, max_exp) = base.exp_range();

        let (max, min) = if self > rhs { (self, rhs) } else { (rhs, self) };
        let shift = max.exp - min.exp;

        if shift >= max_exp as u64 {
            // This shift is guaranteed to result in 0 on lhs, no need to compute
            return max;
        }

        let result = max.sig.wrapping_add(base.divide(min.sig, shift as u32));

        let (sig, exp) = if result < max.sig {
            // Wrapping occurred, handle it
            (min_sig + base.divide(result, 1), max.exp + 1)
        } else if base.as_number() != 2 && result > max_sig {
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

impl<T> AddAssign for BigNumBase<T>
where
    T: Base,
{
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{error::BigNumTestResult, Binary};

    type BigNum = BigNumBase<Binary>;

    #[test]
    fn new_binary_test() {
        // Check that adjustment is correct, especially around edge cases
        assert_eq!(BigNum::new(1, 0), BigNum::new_raw(1, 0));
        assert_eq!(BigNum::new(0b100, 2), BigNum::new_raw(0b10000, 0));
        assert_eq!(BigNum::new(1 << 62, 20), BigNum::new_raw(1 << 63, 19));
        assert_eq!(BigNum::new(1 << 62, 20), BigNum::new_raw(1 << 63, 19));
    }

    #[test]
    fn add_binary_test() {
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
            BigNum::new_raw(0x8000_0000_0000_0000, 2)
        );
    }

    #[test]
    fn add_hex_test() {
        type BigNum = BigNumBase<Hexadecimal>;
        assert_eq!(
            BigNum::from(0xFFFF_FFFF_FFFF_FFFFu64) + 1u32,
            BigNum::new_raw(0x1000_0000_0000_0000, 1)
        );
        assert_eq!(
            BigNum::from(0xFFFF_FFFF_FFFF_FFFEu64) + 1u32,
            BigNum::new_raw(0xFFFF_FFFF_FFFF_FFFF, 0)
        );
        assert_eq!(
            BigNum::new(0xFFFF_FFFF_FFFF_FFFEu64, 10) + 0x0100_0000_0000u64,
            BigNum::new_raw(0xFFFF_FFFF_FFFF_FFFF, 10)
        );
        assert_eq!(
            BigNum::new(0xFFFF_FFFF_FFFF_FFFFu64, 0xFFFF_FFFF_FFFF_0000) + 0x0100_0000_0000u64,
            BigNum::new_raw(0xFFFF_FFFF_FFFF_FFFF, 0xFFFF_FFFF_FFFF_0000)
        );
        assert_eq!(
            BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 0xFFFF_FFFF)
                + BigNum::new(0x1FFF_FFFF_FFFF_FFFF, 0xFFFF_FFF0),
            BigNum::new_raw(0x1000_0000_0000_0000, 0x1_0000_0000)
        );
        assert_eq!(
            BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 0xFFFF_FFFF)
                + BigNum::new(0x1FFF_FFFF_FFFF_FFFF, 0xFFFF_FFEF),
            BigNum::new_raw(0xFFFF_FFFF_FFFF_FFFF, 0xFFFF_FFFF)
        );
    }

    #[test]
    fn add_decimal_test() {
        type BigNum = BigNumBase<Decimal>;

        assert_eq!(
            BigNum::from(1) + BigNum::new(1243123123, 3),
            BigNum::new_raw(1243123123001, 0)
        );
    }

    #[test]
    fn add_arbitrary_test() {
        create_default_base!(Base61, 61);
        type BigNum = BigNumBase<Base61>;

        let SigRange(min_sig, max_sig) = Base61::calculate_ranges().1;

        assert_eq!(
            BigNum::from(0xFFFF_FFFF_FFFF_FFFEu64) + 1,
            BigNum::new_raw(((u64::MAX as u128 + 1) / 61u128) as u64, 1)
        );
        assert_eq!(BigNum::from(1u64) + 1, BigNum::new_raw(2, 0));
        assert_eq!(
            //BigNum::new(max_sig, 10, BASE) + BigNum::new(1, 10, BASE),
            BigNum::new(max_sig, 10) + BigNum::new(1, 10),
            BigNum::new_raw(min_sig, 11)
        );
        assert_eq!(
            BigNum::new(max_sig, 10) + BigNum::new(61u64, 9),
            BigNum::new_raw(min_sig, 11)
        );
    }
}

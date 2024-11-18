use std::{
    cmp::Ordering,
    fmt::Debug,
    ops::{Add, AddAssign, Sub, SubAssign},
};

use consts::{
    BIN_EXP_RANGE, BIN_POWERS, BIN_SIG_RANGE, DEC_EXP_RANGE, DEC_POWERS, DEC_SIG_RANGE,
    HEX_EXP_RANGE, HEX_POWERS, HEX_SIG_RANGE,
};

pub(crate) mod consts;
pub(crate) mod error;
pub mod macros;
pub mod random;

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
/// #[derive(Clone, Copy, Debug)]
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
/// #[derive(Clone, Copy, Debug)]
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
pub trait Base: Copy + Debug {
    /// This contains the numeric value of the type. E.g. for binary 2, for decimal 10,
    /// etc
    const NUMBER: u16;

    /// Function that can create an instance of this Base. Users should never have to
    /// manually create instances of this type. This is called implicitly on every
    /// call to `BigNumBase<Self>::new()` so it should be as lightweight as possible. Note
    /// that it is not called when creating a BigNumBase<Self> from another, like when
    /// performing an addition. In this case it is simply copied
    fn new() -> Self;

    /// Function that fetches the non-inclusive range of the exponent for the significand
    /// in the BigNum with this base. E.g. the range for binary is [63, 64), since the
    /// range of the significand is [2^63, 2^64)
    fn exp_range(&self) -> ExpRange;

    /// Function that fetches the inclusive range for the significand in the BigNum with
    /// this base. E.g. for binary the range of the significand is [2^63, 2^64 - 1]
    fn sig_range(&self) -> SigRange;

    /// This is a function that computes `Self::NUMBER ^ exp`. It has a default
    /// implementation that computes the value directly. It is recommended to override
    /// this behavior if there is a trick to the exponentiation (like how for binary
    /// `2^n = (1 << n)`). You can also create a gloabl const lookup table and reference
    /// that.
    fn pow(exp: u32) -> u64 {
        (Self::NUMBER as u64).pow(exp)
    }

    /// This function calculates the ranges for the exponent and the significand. It is
    /// not particularly efficient so if performance is a concern you should not use it.
    /// It mainly exists to facilitate the `create_default_base!` macro. It is recommended
    /// to store the ranges in a const and return them directly in the `exp_range` and
    /// `sig_range` methods if convenient.
    fn calculate_ranges() -> (ExpRange, SigRange) {
        if Self::NUMBER.is_power_of_two() && Self::NUMBER.ilog2().is_power_of_two() {
            // This is a special case where sig_max = u64::MAX. We have to handle it
            // specially to avoid overflowing the u64
            let pow = Self::NUMBER.ilog2();
            let exp = 64 / pow;
            let sig = Self::pow(exp - 1);

            (ExpRange(exp - 1, exp), SigRange(sig, u64::MAX))
        } else {
            let exp = u64::MAX.ilog(Self::NUMBER as u64);
            (
                ExpRange(exp - 1, exp),
                SigRange(Self::pow(exp - 1), Self::pow(exp) - 1),
            )
        }
    }

    /// This is a function that computes `lhs / (Self::NUMBER ^ exp)`. There is a default
    /// implementation that obtains the value of `Self::NUMBER ^ exp` via the `pow` method
    /// for this type, and does a division. It is recommended to override this method if
    /// there is a trick for the division (like how in binary,
    /// `lhs / (2 ^ exp) = lhs >> exp`, or in octal `lhs / (8 ^ exp) = lhs >> (3 * exp)`
    fn divide(lhs: u64, exp: u32) -> u64 {
        lhs / Self::pow(exp)
    }
    /// This is a function that computes `lhs * (Self::NUMBER ^ exp)`. There is a default
    /// implementation that obtains the value of `Self::NUMBER ^ exp` via the `pow` method
    /// for this type, and does a multiplication. It is recommended to override this
    /// method if there is a trick for the division (like how in binary,
    /// `lhs * (2 ^ exp) = lhs << exp`, or in octal `lhs * (8 ^ exp) = lhs << (3 * exp)`
    fn multiply(lhs: u64, exp: u32) -> u64 {
        lhs * Self::pow(exp)
    }

    /// This is a function that computes the highest power `x` such that
    /// `sig >= (Self::NUMBER ^ x)`. There is a default implementation that uses `ilog`,
    /// and it is recommended to use this unless there is a special way to find the
    /// magnitude (e.g. binary and decimal have specialized `ilog` implementations).
    /// As a special case, bases that are powers of 2 or 10 can use log arithmetic to
    /// convert. I tried this with octal and hexadecimal but it had no noticeable impact.
    fn get_mag(sig: u64) -> u32 {
        sig.ilog(Self::NUMBER as u64)
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
        //ExpRange(63, 64)
        ExpRange::from(BIN_EXP_RANGE)
    }

    fn sig_range(&self) -> SigRange {
        //SigRange(1 << 63, u64::MAX)
        SigRange::from(BIN_SIG_RANGE)
    }

    fn pow(exp: u32) -> u64 {
        //1 << exp
        BIN_POWERS[exp as usize]
    }

    fn divide(lhs: u64, exp: u32) -> u64 {
        lhs >> exp
    }

    fn multiply(lhs: u64, exp: u32) -> u64 {
        lhs << exp
    }

    fn get_mag(sig: u64) -> u32 {
        sig.ilog2()
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

    fn pow(exp: u32) -> u64 {
        1 << (3 * exp)
    }

    fn divide(lhs: u64, exp: u32) -> u64 {
        lhs >> (3 * exp)
    }

    fn multiply(lhs: u64, exp: u32) -> u64 {
        lhs << (3 * exp)
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

    fn pow(exp: u32) -> u64 {
        //1 << (exp << 2)
        HEX_POWERS[exp as usize]
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

    fn pow(exp: u32) -> u64 {
        DEC_POWERS[exp as usize]
    }

    fn get_mag(sig: u64) -> u32 {
        sig.ilog10()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct BigNumBase<T>
where
    T: Base,
{
    pub sig: u64,
    pub exp: u64,
    pub base: T,
}

impl<T> BigNumBase<T>
where
    T: Base,
{
    /// Creates a BigNumBase from values, normalizing if needed. You should always use
    /// this unless you have a specific need
    pub fn new(sig: u64, exp: u64) -> Self {
        let base = T::new();

        Self::new_with_base(sig, exp, base)
    }

    /// Creates a BigNumBase directly from values, panicking if not possible. This is mostly
    /// for testing but may be more performant on inputs that are guaranteed valid
    pub fn new_raw(sig: u64, exp: u64) -> Self {
        let base = T::new();

        if Self::is_valid(sig, exp, base.sig_range()) {
            Self { sig, exp, base }
        } else {
            panic!(
                "Unable to create BigNumBase with sig 
{:x} and exp 
{}
min_sig:
{:x},
max_sig:
{:x}",
                sig,
                exp,
                base.sig_range().0,
                base.sig_range().1
            );
        }
    }

    // This is a helper function that creates a BigNumBase from values and a base. It
    // shouldn't be called directly, but is used by other internal methods
    fn new_with_base(sig: u64, exp: u64, base: T) -> Self {
        let SigRange(min_sig, max_sig) = base.sig_range();
        let ExpRange(min_exp, _) = base.exp_range();

        if sig >= min_sig && sig <= max_sig {
            Self { sig, exp, base }
        } else if sig > max_sig {
            // Since we know `max_sig * base.as_number() > u64::MAX`, we also know
            // that `sig / base.as_number() <= max_sig`
            Self {
                sig: T::divide(sig, 1),
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
            let mag = T::get_mag(sig);

            if mag.saturating_add(exp as u32) <= min_exp {
                Self {
                    sig: T::multiply(sig, exp as u32),
                    exp: 0,
                    base,
                }
            } else {
                let adj = min_exp - mag;

                Self {
                    sig: T::multiply(sig, adj),
                    exp: exp - adj as u64,
                    base,
                }
            }
        }
    }

    // Returns true if the values are valid for the current base
    pub fn is_valid(sig: u64, exp: u64, range: SigRange) -> bool {
        sig <= range.max() && (exp == 0 || sig >= range.min())
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

        let result = max.sig.wrapping_add(T::divide(min.sig, shift as u32));

        let (sig, exp) = if result < max.sig {
            // Wrapping occurred, handle it
            (min_sig + T::divide(result, 1), max.exp + 1)
        } else if T::NUMBER != 2 && result > max_sig {
            (T::divide(result, 1), max.exp + 1)
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

impl<T> Sub for BigNumBase<T>
where
    T: Base,
{
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        let base = self.base;
        let SigRange(min_sig, max_sig) = base.sig_range();
        let ExpRange(min_exp, max_exp) = base.exp_range();

        let (max, min) = if self >= rhs {
            (self, rhs)
        } else {
            panic!(
                "Attempt to subtract 
{:?} from 
{:?}",
                rhs, self
            )
        };

        let shift = max.exp - min.exp;

        if shift >= max_exp as u64 {
            // This shift is guaranteed to result in 0 on rhs, no need to compute
            return max;
        }

        let result = max.sig.wrapping_sub(T::divide(min.sig, shift as u32));

        let (res_sig, res_exp) = if result > max.sig {
            // Wrapping occurred, handle it by decrementing the exponent
            (result, max.exp - 1)
        } else {
            (result, max.exp)
        };

        if res_sig == 0 {
            Self {
                sig: 0,
                exp: 0,
                base,
            }
        } else if res_exp == 0 || res_sig >= min_sig {
            Self {
                sig: res_sig,
                exp: res_exp,
                base,
            }
        } else {
            // This operation can result in arbitrary loss in magnitude so we have to
            // calculate the differential directly
            let mag = T::get_mag(res_sig);
            let adj = min_exp - mag;

            if adj as u64 == res_exp {
                Self {
                    sig: T::multiply(res_sig, adj),
                    exp: 0,
                    base,
                }
            } else if adj as u64 >= res_exp {
                // Have to adjust by more than exp so we will have a compact result
                // TODO Verify this again, pretty sure it's right but I can't figure out
                // why the -1 is there
                let diff = adj as u64 - res_exp - 1;

                Self {
                    sig: T::multiply(res_sig, diff as u32),
                    exp: 0,
                    base,
                }
            } else {
                Self {
                    sig: T::multiply(res_sig, adj),
                    exp: res_exp - adj as u64,
                    base,
                }
            }
        }
    }
}

impl<T> SubAssign for BigNumBase<T>
where
    T: Base,
{
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{error::BigNumTestResult, Binary};

    // Overriding it for better error messages
    macro_rules! assert_eq_bignum {
        ($lhs:expr, $rhs:expr) => {
            if $lhs != $rhs {
                panic!("assertion failed: 
sig:
0x{:x} 
vs
0x{:x}

exp:
{}
vs
{}

base:
{}

min_sig:
0x{:x}
max_sig:
0x{:x}
min_exp:
{}
max_exp:
{}
"
            , $lhs.sig,
            $rhs.sig, $lhs.exp, $rhs.exp, $lhs.base.as_number(), $lhs.base.sig_range().min(), $lhs.base.sig_range().max(), $lhs.base.exp_range().min(), $lhs.base.exp_range().max());
            }
        };
    }

    #[test]
    fn new_binary_test() {
        type BigNum = BigNumBase<Binary>;
        // Check that adjustment is correct, especially around edge cases
        assert_eq_bignum!(BigNum::new(1, 0), BigNum::new_raw(1, 0));
        assert_eq_bignum!(BigNum::new(0b100, 2), BigNum::new_raw(0b10000, 0));
        assert_eq_bignum!(BigNum::new(1 << 62, 20), BigNum::new_raw(1 << 63, 19));
        assert_eq_bignum!(BigNum::new(1 << 62, 20), BigNum::new_raw(1 << 63, 19));
    }

    #[test]
    fn add_binary_test() {
        type BigNum = BigNumBase<Binary>;
        assert_eq_bignum!(
            BigNum::new(0x100, 0) + BigNum::new(0x0100_0000, 4),
            BigNum::new_raw(0x1000_0100, 0)
        );
        assert_eq_bignum!(
            BigNum::new(0x1000_0000, 32) + BigNum::new(0x0100_0000, 4),
            BigNum::new_raw(0x1000_0000_1000_0000, 0)
        );
        assert_eq_bignum!(
            BigNum::new(0xFFFF_FFFF, 32) + BigNum::new(0x8000_0000, 1),
            BigNum::new_raw(0x8000_0000_0000_0000, 1)
        );
        assert_eq_bignum!(
            BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 1) + 0x1u32,
            BigNum::new_raw(0xFFFF_FFFF_FFFF_FFFF, 1)
        );
        assert_eq_bignum!(
            BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 1) + 0x2u32,
            BigNum::new_raw(0x8000_0000_0000_0000, 2)
        );
    }

    #[test]
    fn add_hex_test() {
        type BigNum = BigNumBase<Hexadecimal>;
        assert_eq_bignum!(
            BigNum::from(0xFFFF_FFFF_FFFF_FFFFu64) + 1u32,
            BigNum::new_raw(0x1000_0000_0000_0000, 1)
        );
        assert_eq_bignum!(
            BigNum::from(0xFFFF_FFFF_FFFF_FFFEu64) + 1u32,
            BigNum::new_raw(0xFFFF_FFFF_FFFF_FFFF, 0)
        );
        assert_eq_bignum!(
            BigNum::new(0xFFFF_FFFF_FFFF_FFFEu64, 10) + 0x0100_0000_0000u64,
            BigNum::new_raw(0xFFFF_FFFF_FFFF_FFFF, 10)
        );
        assert_eq_bignum!(
            BigNum::new(0xFFFF_FFFF_FFFF_FFFFu64, 0xFFFF_FFFF_FFFF_0000) + 0x0100_0000_0000u64,
            BigNum::new_raw(0xFFFF_FFFF_FFFF_FFFF, 0xFFFF_FFFF_FFFF_0000)
        );
        assert_eq_bignum!(
            BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 0xFFFF_FFFF)
                + BigNum::new(0x1FFF_FFFF_FFFF_FFFF, 0xFFFF_FFF0),
            BigNum::new_raw(0x1000_0000_0000_0000, 0x1_0000_0000)
        );
        assert_eq_bignum!(
            BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 0xFFFF_FFFF)
                + BigNum::new(0x1FFF_FFFF_FFFF_FFFF, 0xFFFF_FFEF),
            BigNum::new_raw(0xFFFF_FFFF_FFFF_FFFF, 0xFFFF_FFFF)
        );
    }

    #[test]
    fn add_decimal_test() {
        type BigNum = BigNumBase<Decimal>;

        assert_eq_bignum!(
            BigNum::from(1) + BigNum::new(1243123123, 3),
            BigNum::new_raw(1243123123001, 0)
        );
        assert_eq_bignum!(
            BigNum::from(1000) + BigNum::new(10u64.pow(19) - 1, 3),
            BigNum::new_raw(10u64.pow(18), 4)
        );
        assert_eq_bignum!(
            BigNum::new(10u64.pow(19) - 1, 13) + BigNum::new(10u64.pow(18), 3),
            BigNum::new_raw(10u64.pow(18) + 10u64.pow(7) - 1, 14)
        );
    }

    #[test]
    fn add_arbitrary_test() {
        create_default_base!(Base61, 61);
        type BigNum = BigNumBase<Base61>;

        let SigRange(min_sig, max_sig) = Base61::calculate_ranges().1;

        assert_eq_bignum!(
            BigNum::from(0xFFFF_FFFF_FFFF_FFFEu64) + 1u64,
            BigNum::new_raw(((u64::MAX as u128 + 1) / 61u128) as u64, 1)
        );
        assert_eq_bignum!(BigNum::from(1u64) + 1u64, BigNum::new_raw(2, 0));
        assert_eq_bignum!(
            //BigNum::new(max_sig, 10, BASE) + BigNum::new(1, 10, BASE),
            BigNum::new(max_sig, 10) + BigNum::new(1, 10),
            BigNum::new_raw(min_sig, 11)
        );
        assert_eq_bignum!(
            BigNum::new(max_sig, 10) + BigNum::new(61u64, 9),
            BigNum::new_raw(min_sig, 11)
        );
    }

    #[test]
    fn sub_binary_test() {
        type BigNum = BigNumBase<Binary>;

        assert_eq_bignum!(
            BigNum::new(0x100, 32) - BigNum::new(0x0080_0000_0000, 0),
            BigNum::new_raw(0x0080_0000_0000, 0)
        );
        assert_eq_bignum!(
            BigNum::new(0x1000_0000_0000_0000, 0) - BigNum::new(0x0010_0000_0000_0000, 8),
            BigNum::from(0)
        );
        assert_eq_bignum!(
            BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 48) - BigNum::new(0x8000_0000_0000_0000, 16),
            BigNum::new(0xFFFF_FFFF_7FFF_FFFF, 48)
        );
        assert_eq_bignum!(
            BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 48) - BigNum::new(0xFFFF_FFFF_0000_0000, 48),
            BigNum::new(0xFFFF_FFFF_0000_0000, 16)
        );
        assert_eq_bignum!(
            BigNum::new(0x8000_0000_0000_0000, 48) - BigNum::new(0xFFFF_FFFF_0000_0000, 16),
            BigNum::new(0xFFFF_FFFE_0000_0002, 47)
        );
    }

    // Runs some non-base specific tests
    macro_rules! base_agnostic_tests {
        (*; $base:ident) => {{
            base_agnostic_tests!(add $base);
            base_agnostic_tests!(sub $base);
        }};
        (sub $base:ident) => {{
            type BigNum = BigNumBase<$base>;
            let SigRange(min_sig, max_sig) = $base::calculate_ranges().1;

            assert_eq_bignum!(
                BigNum::new(min_sig, 1) - $base::NUMBER as u64,
                BigNum::new_raw(max_sig - ($base::NUMBER as u64 - 1), 0)
            );
            assert_eq_bignum!(
                BigNum::new(max_sig, 1) - max_sig,
                BigNum::new_raw(max_sig - max_sig / $base::NUMBER as u64, 1)
            );
            assert_eq_bignum!(
                BigNum::new(12341098709128730491, 11234) - BigNum::new(12341098709128730491, 11234),
                BigNum::from(0)
            );
            assert_eq_bignum!(
                BigNum::from(max_sig) - BigNum::from(max_sig),
                BigNum::from(0)
            );
        }};
        (add $base:ident) => {{
            type BigNum = BigNumBase<$base>;
            let SigRange(min_sig, max_sig) = $base::calculate_ranges().1;

            assert_eq_bignum!(
                BigNum::new(min_sig, 1) + $base::NUMBER as u64,
                BigNum::new_raw(min_sig + 1, 1)
            );
            assert_eq_bignum!(
                BigNum::new(max_sig, 1) + $base::NUMBER as u64,
                BigNum::new_raw(min_sig, 2)
            );
            assert_eq_bignum!(
                BigNum::from(min_sig) + BigNum::from(min_sig),
                BigNum::from(min_sig * 2)
            );
            assert_eq_bignum!(
                BigNum::new(max_sig, 142) + BigNum::new(min_sig, 140),
                BigNum::new(min_sig + $base::divide(max_sig, 4), 143)
            );
        }};
    }

    macro_rules! create_and_test_base {
        (*; $base:ident, $num:literal) => {
            create_default_base!($base, $num);
            base_agnostic_tests!(sub $base);
            base_agnostic_tests!(add $base);
        };
        (sub $base:ident, $num:literal) => {
            create_default_base!($base, $num);
            base_agnostic_tests!(sub $base);
        };
        (add $base:ident, $num:literal) => {
            create_default_base!($base, $num);
            base_agnostic_tests!(add $base);
        };
    }

    // I won't test each individual base since the logic is the same, but I will test
    // binary and arbitrary
    #[test]
    fn sub_arbitrary_test() {
        create_default_base!(Base61, 61);
        type BigNum = BigNumBase<Base61>;

        let SigRange(min_sig, max_sig) = Base61::calculate_ranges().1;

        // This is an example of how subtraction results in a loss of precision. I may
        // do a lossless_sub trait at some point that casts both sigs to u128 before
        // calculating

        assert_eq_bignum!(
            BigNum::new(min_sig, 1) - 61u64,
            BigNum::new_raw(max_sig - 60, 0)
        );
        assert_eq_bignum!(
            BigNum::new(max_sig, 1) - max_sig,
            BigNum::new_raw(max_sig - max_sig / 61, 1)
        );
        assert_eq_bignum!(
            BigNum::new(12341098709128730491, 11234) - BigNum::new(12341098709128730491, 11234),
            BigNum::from(0)
        )
    }

    #[test]
    fn sub_many_test() {
        // Not doing Binary or Hex since these tests assume max_sig + 1 fits in u64
        create_and_test_base!(*; Base61, 61);
        create_and_test_base!(*; Base11142, 11142);
        create_and_test_base!(*; Base942, 942);
        create_and_test_base!(*; Base3292, 3292);
        create_and_test_base!(*; Base1234, 1234);
        create_and_test_base!(*; Base5678, 5678);
        create_and_test_base!(*; Base9101, 9101);
        create_and_test_base!(*; Base2345, 2345);
        create_and_test_base!(*; Base6789, 6789);
        create_and_test_base!(*; Base1112, 1112);
        create_and_test_base!(*; Base3456, 3456);
        create_and_test_base!(*; Base7890, 7890);
        create_and_test_base!(*; Base1357, 1357);
        create_and_test_base!(*; Base2468, 2468);

        base_agnostic_tests!(*; Octal);
        base_agnostic_tests!(*; Decimal);
    }
}

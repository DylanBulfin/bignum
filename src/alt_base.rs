use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::HashMap,
    fmt::{Debug, Display, Write},
    ops::{Add, AddAssign, Deref, Sub},
    sync::LazyLock,
    u64,
};

use crate::{
    check_bases, impl_from_with_base,
    powers::{
        BIN_EXP_RANGE, BIN_SIG_RANGE, DEC_EXP_RANGE, DEC_POWERS, DEC_SIG_RANGE, HEX_EXP_RANGE,
        HEX_POWERS, HEX_SIG_RANGE, OCT_EXP_RANGE, OCT_POWERS, OCT_SIG_RANGE,
    },
};

/// This trait is not recommended for use with arbitrary bases if performance is a concern
/// since it involves recalculating the BaseData struct each time you create a new one.
pub trait FromWithBase<T>: Sized {
    fn from_with_base(val: T, base: u16) -> Self;

    fn from_bin(val: T) -> Self {
        Self::from_with_base(val, 2)
    }
    fn from_oct(val: T) -> Self {
        Self::from_with_base(val, 8)
    }
    fn from_dec(val: T) -> Self {
        Self::from_with_base(val, 10)
    }
    fn from_hex(val: T) -> Self {
        Self::from_with_base(val, 16)
    }
}

/// This trait is not recommended for use with arbitrary bases if performance is a concern
/// since it involves recalculating the BaseData struct each time you create a new one.
pub trait IntoWithBase<T>: Sized {
    fn into_with_base(self, base: u16) -> T;

    fn into_bin(self) -> T {
        self.into_with_base(2)
    }
    fn into_oct(self) -> T {
        self.into_with_base(8)
    }
    fn into_dec(self) -> T {
        self.into_with_base(10)
    }
    fn into_hex(self) -> T {
        self.into_with_base(16)
    }
}

impl_from_with_base!(*);

/// Holds runtime data for a base. This includes a table of valid powers, and ranges of
/// the significand. This type is Copy but since it does have a non-trivial amount of data
/// we still try to use references where it is convenient.
#[derive(Clone, Copy, Debug)]
pub struct BaseData {
    base: u64,
    /// This field must be able to hold all valid powers in base-3 and above. Since
    /// `3^28 < u64::MAX < 3^29` we need 29 elements total
    powers: [Option<u64>; 29],
    sig_range: (u64, u64),
    /// These are `u32` to make `pow` calls more convenient
    exp_range: (u32, u32),
}
impl BaseData {
    pub fn new(base: u16) -> Self {
        match base {
            2 | 8 | 10 | 16 => panic!("Attempted to create BaseData for special base {}", base),
            _ => {
                let base = base as u64;

                let mut powers = [None; 29];

                let mut exp = 0u32;
                let mut sig: u128 = 1;

                while sig <= u64::MAX as u128 {
                    powers[exp as usize] = Some(sig as u64);

                    exp += 1;
                    sig *= base as u128;
                }

                let max = sig / (base as u128);
                let min = max / (base as u128);

                Self {
                    base,
                    powers,
                    exp_range: (exp - 2, exp - 1),
                    sig_range: (min as u64, (max - 1) as u64),
                }
            }
        }
    }

    pub fn powers(&self) -> &[Option<u64>; 29] {
        &self.powers
    }

    pub fn sig_range(&self) -> (u64, u64) {
        self.sig_range
    }

    pub fn exp_range(&self) -> (u32, u32) {
        self.exp_range
    }

    pub fn pow(&self, exp: u32) -> u64 {
        if let Some(Some(n)) = self.powers.get(exp as usize) {
            *n
        } else {
            panic!(
                "Error while getting exp {} for base {}. Data: {:?}",
                exp, self.base, self
            );
        }
    }

    /// Max value for sig field, inclusive
    pub fn max_sig(&self) -> u64 {
        self.sig_range.1
    }
    /// Min value for sig field
    pub fn min_sig(&self) -> u64 {
        self.sig_range.0
    }

    /// Max value for exp field, inclusive
    pub fn max_exp(&self) -> u32 {
        self.exp_range.1
    }
    /// Min value for exp field
    pub fn min_exp(&self) -> u32 {
        self.exp_range.0
    }
}

#[derive(Clone, Copy)]
pub struct BigNum {
    sig: u64,
    exp: u64,
    base: u16,
    /// This holds useful data about a base, is None for special bases (2, 8, 10, 16)
    data: Option<BaseData>,
}

// I use a special format for the various constructors:
// - new_*_raw => Creates a BigNum directly from parts if possible, otherwise panics
//      - May remove check/panic in the future
// - new_* => Creates a BigNum, normalizing the `sig` and `exp`
// - get_mag_* => Get the highest power of the base that is lower than the input val
//      - E.g. `get_mag_spec(0b1000, 2) = 3` because `0b1000 = 2^3`
// - *spec* => These methods relate to special bases (2, 8, 10, 16).
// - *arbitrary* => These methods relate to non-special bases
impl BigNum {
    /// Create a new `BigNum` from given parts. This constructor will normalize the values
    /// silently so keep that in mind when using it. If you pass a non-special base it
    /// will calculate the table from scratch. When creating a BigNum with a certain base
    /// when you already have a BigNum of the same base, you should probably use
    /// `new_with_template` as it avoids recalculating the metadata values in BaseData
    pub fn new(sig: u64, exp: u64, base: u16) -> Self {
        match base {
            2 => Self::new_bin(sig, exp),
            8 | 10 | 16 => Self::new_spec(sig, exp, base),
            _ => {
                let data = BaseData::new(base);

                Self::new_arbitrary(sig, exp, &data)
            }
        }
    }
    pub fn new_oct(sig: u64, exp: u64) -> Self {
        Self::new(sig, exp, 8)
    }
    pub fn new_dec(sig: u64, exp: u64) -> Self {
        Self::new(sig, exp, 10)
    }
    pub fn new_hex(sig: u64, exp: u64) -> Self {
        Self::new(sig, exp, 16)
    }

    pub fn new_with_template(sig: u64, exp: u64, temp: &Self) -> Self {
        match temp.base {
            2 | 8 | 10 | 16 => panic!(
                "Calling new_with_template with special temp.base {} is invalid.",
                temp.base
            ),
            _ => Self::new_arbitrary(
                sig,
                exp,
                &temp.data.unwrap_or_else(|| {
                    panic!(
                        "Unable to get data from BigNum with temp.base {}",
                        temp.base
                    )
                }),
            ),
        }
    }

    pub fn is_special_base(&self) -> bool {
        matches!(self.base, 2 | 8 | 10 | 16)
    }

    pub fn min_sig(&self) -> u64 {
        match self.base {
            2 => BIN_SIG_RANGE.0,
            8 => OCT_SIG_RANGE.0,
            10 => DEC_SIG_RANGE.0,
            16 => HEX_SIG_RANGE.0,
            _ => self
                .data
                .unwrap_or_else(|| {
                    panic!("Unable to get BaseData for value with base {}", self.base)
                })
                .min_sig(),
        }
    }
    pub fn max_sig(&self) -> u64 {
        match self.base {
            2 => BIN_SIG_RANGE.1,
            8 => OCT_SIG_RANGE.1,
            10 => DEC_SIG_RANGE.1,
            16 => HEX_SIG_RANGE.1,
            _ => self
                .data
                .unwrap_or_else(|| {
                    panic!("Unable to get BaseData for value with base {}", self.base)
                })
                .max_sig(),
        }
    }
    pub fn min_exp(&self) -> u32 {
        match self.base {
            2 => BIN_EXP_RANGE.0,
            8 => OCT_EXP_RANGE.0,
            10 => DEC_EXP_RANGE.0,
            16 => HEX_EXP_RANGE.0,
            _ => self
                .data
                .unwrap_or_else(|| {
                    panic!("Unable to get BaseData for value with base {}", self.base)
                })
                .min_exp(),
        }
    }
    pub fn max_exp(&self) -> u32 {
        match self.base {
            2 => BIN_EXP_RANGE.1,
            8 => OCT_EXP_RANGE.1,
            10 => DEC_EXP_RANGE.1,
            16 => HEX_EXP_RANGE.1,
            _ => self
                .data
                .unwrap_or_else(|| {
                    panic!("Unable to get BaseData for value with base {}", self.base)
                })
                .max_exp(),
        }
    }

    /// Panics if it recieves an invalid value
    fn new_spec_raw(sig: u64, exp: u64, base: u16) -> Self {
        if base == 2 {
            return Self::new_bin_raw(sig, exp);
        }

        let (min_sig, max_sig) = match base {
            8 => (OCT_SIG_RANGE.0, OCT_SIG_RANGE.1),
            10 => (DEC_SIG_RANGE.0, DEC_SIG_RANGE.1),
            16 => (HEX_SIG_RANGE.0, HEX_SIG_RANGE.1),
            _ => panic!("Invalid special base in new_spec_raw: {}", base),
        };

        if sig > max_sig || exp != 0 && sig < min_sig {
            panic!(
                "Base-{} BigNum with sig {} and exp {} is invalid",
                base, sig, exp
            );
        } else {
            Self {
                sig,
                exp,
                base,
                data: None,
            }
        }
    }

    fn new_bin_raw(sig: u64, exp: u64) -> Self {
        if exp != 0 && sig < BIN_SIG_RANGE.0 {
            panic!("Binary BigNum with sig {} and exp {} is invalid", sig, exp);
        } else {
            Self {
                sig,
                exp,
                base: 2,
                data: None,
            }
        }
    }

    fn new_arbitrary_raw(sig: u64, exp: u64, data: &BaseData) -> Self {
        if sig > data.max_sig() || exp != 0 && sig < data.min_sig() {
            panic!(
                "Base-{} BigNum with sig {} and exp {} is invalid. min_sig: {}, max_sig: {}",
                data.base,
                sig,
                exp,
                data.min_sig(),
                data.max_sig()
            );
        }

        Self {
            sig,
            exp,
            base: data.base as u16,
            data: Some(*data),
        }
    }

    fn new_bin(sig: u64, exp: u64) -> Self {
        if exp == 0 {
            Self::new_bin_raw(sig, 0)
        } else if sig >= 1 << 63 {
            Self::new_bin_raw(sig, exp)
        } else {
            let mag = sig.ilog2() as u64;

            if mag.saturating_add(exp) < 63 {
                Self::new_bin_raw(sig << exp, 0)
            } else {
                let adj = 63 - mag;

                Self::new_bin_raw(sig << adj, exp - adj)
            }
        }
    }

    fn new_spec(sig: u64, exp: u64, base: u16) -> Self {
        if base == 2 {
            return Self::new_bin(sig, exp);
        }

        let (min_exp, min_sig, powers) = match base {
            8 => (
                OCT_EXP_RANGE.0 as u64,
                OCT_SIG_RANGE.0,
                OCT_POWERS.as_slice(),
            ),
            10 => (
                DEC_EXP_RANGE.0 as u64,
                DEC_SIG_RANGE.0,
                DEC_POWERS.as_slice(),
            ),
            16 => (
                HEX_EXP_RANGE.0 as u64,
                HEX_SIG_RANGE.0,
                HEX_POWERS.as_slice(),
            ),
            _ => panic!("Invalid special base: {}", base),
        };

        if exp == 0 {
            Self::new_spec_raw(sig, 0, base)
        } else if sig >= min_sig {
            Self::new_spec_raw(sig, exp, base)
        } else {
            let mag = Self::get_mag_spec(sig, base);

            if mag.saturating_add(exp) < min_exp {
                Self::new_spec_raw(sig * powers[exp as usize], 0, base)
            } else {
                let adj = min_exp - mag;

                Self::new_spec_raw(sig * powers[adj as usize], exp - adj, base)
            }
        }
    }

    fn new_arbitrary(sig: u64, exp: u64, data: &BaseData) -> Self {
        let (min_exp, min_sig, max_sig) = (data.min_exp() as u64, data.min_sig(), data.max_sig());

        let (sig, exp) = if sig > max_sig {
            (sig / data.base, exp + 1)
        } else {
            (sig, exp)
        };

        if exp == 0 {
            Self::new_arbitrary_raw(sig, 0, data)
        } else if sig == 0 {
            panic!(
                "Unable to create a base-{} BigNum with exp of {} and sig of {}",
                data.base, exp, sig
            );
        } else if sig >= min_sig {
            Self::new_arbitrary_raw(sig, exp, data)
        } else {
            let mag = Self::get_mag_arbitrary(sig, data);

            if mag.saturating_add(exp) < min_exp {
                Self::new_arbitrary_raw(sig * data.pow(exp as u32), 0, data)
            } else {
                let adj = min_exp - mag;

                Self::new_arbitrary_raw(sig * data.pow(adj as u32), exp - adj, data)
            }
        }
    }

    fn get_mag_spec(sig: u64, base: u16) -> u64 {
        match base {
            2 => sig.ilog2() as u64,
            8 => sig.ilog2() as u64 / 3,
            10 => sig.ilog10() as u64,
            16 => sig.ilog2() as u64 / 4,
            _ => panic!("{} is not a valid special base value", base),
        }
    }

    fn get_mag_arbitrary(sig: u64, data: &BaseData) -> u64 {
        data.powers
            .iter()
            .enumerate()
            .find(|(_, v)| sig < v.unwrap())
            .unwrap_or_else(|| {
                panic!(
                    "Unable to find base-{} magnitude for value {}",
                    data.base, sig
                )
            })
            .0 as u64
            - 1
    }

    /// Helper function that gets the n-th power of self's base
    fn pow(&self, n: u32) -> u64 {
        match self.base {
            2 => 1 << n,
            8 => OCT_POWERS[n as usize],
            10 => DEC_POWERS[n as usize],
            16 => HEX_POWERS[n as usize],
            _ => {
                if let Some(d) = self.data {
                    d.pow(n)
                } else {
                    panic!("Unable to get BaseData for base-{} BigNum", self.base);
                }
            }
        }
    }
}

// Want to skip the data field as it's the same for every BigNum of a certain base. May
// want to rethink the design later, moving the table to a global cache is likely more 
// elegant
impl Debug for BigNum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BigNum")
            .field("sig", &self.sig)
            .field("exp", &self.exp)
            .field("base", &self.base)
            .finish()
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

impl Eq for BigNum {}

impl PartialEq for BigNum {
    fn eq(&self, other: &Self) -> bool {
        self.sig == other.sig && self.exp == other.exp && self.base == other.base
    }
}

impl Add for BigNum {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        check_bases!(self, rhs, "add");

        let (max, min) = if self > rhs { (self, rhs) } else { (rhs, self) };
        let shift = max.exp - min.exp;

        if shift >= self.max_exp() as u64 {
            // This shift is guaranteed to result in 0 on lhs, no need to compute
            return self;
        }

        let result = max
            .sig
            .wrapping_add(min.sig.saturating_div(min.pow(shift as u32)));

        let (sig, exp) = if result < max.sig {
            // Wrapping occurred, handle it
            (self.min_sig() + (result / self.base as u64), max.exp + 1)
        } else {
            (result, max.exp)
        };

        if !self.is_special_base() {
            Self::new_with_template(sig, exp, &self)
        } else {
            Self::new(sig, exp, self.base)
        }
    }
}

impl Sub for BigNum {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        check_bases!(self, rhs, "sub");

        let (max, min) = if self >= rhs {
            (self, rhs)
        } else {
            panic!("Unable to subtract {} from {}", rhs, self)
        };
        let shift = max.exp - min.exp;

        if shift >= self.max_exp() as u64 {
            // This shift is guaranteed to result in 0 on lhs, no need to compute
            return self;
        }

        let result = max
            .sig
            .wrapping_add(min.sig.saturating_div(min.pow(shift as u32)));

        let (sig, exp) = if result < max.sig {
            // Wrapping occurred, handle it
            (self.min_sig() + (result / self.base as u64), max.exp + 1)
        } else {
            (result, max.exp)
        };

        if !self.is_special_base() {
            Self::new_with_template(sig, exp, &self)
        } else {
            Self::new(sig, exp, self.base)
        }
    }
}

impl AddAssign for BigNum {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

#[cfg(test)]
mod tests {

    use std::u32;

    use crate::{
        alt_base::{BaseData, BigNum, IntoWithBase},
        error::{BigNumError, BigNumTestResult},
        impl_for_type,
    };

    impl_for_type!([u8, u16, u32, u64, i8, i16, i32, i64], 2);

    #[test]
    fn from_test() -> BigNumTestResult {
        assert_eq!(BigNum::from(1u64), BigNum::new_bin_raw(1, 0));
        assert_eq!(BigNum::from(12234u16), BigNum::new_bin_raw(12234, 0));
        assert_eq!(BigNum::from(200i32), BigNum::new_bin_raw(200, 0));
        assert_eq!(BigNum::from(100i8), BigNum::new_bin_raw(100, 0));

        Ok(())
    }

    #[test]
    fn new_bin_test() -> BigNumTestResult {
        // Check that adjustment is correct, especially around edge cases
        assert_eq!(BigNum::new_bin(1, 0), BigNum::new_bin_raw(1, 0));
        assert_eq!(BigNum::new_bin(0b100, 2), BigNum::new_bin_raw(0b10000, 0));
        assert_eq!(
            BigNum::new_bin(1 << 62, 20),
            BigNum::new_bin_raw(1 << 63, 19)
        );
        assert_eq!(
            BigNum::new_bin(1 << 62, 20),
            BigNum::new_bin_raw(1 << 63, 19)
        );

        Ok(())
    }

    #[test]
    fn new_oct_test() -> BigNumTestResult {
        // Check that adjustment is correct, especially around edge cases
        assert_eq!(BigNum::new_oct(1, 0), BigNum::new_spec_raw(1, 0, 8));
        assert_eq!(
            BigNum::new_oct(0o100, 2),
            BigNum::new_spec_raw(0o10000, 0, 8)
        );
        assert_eq!(
            BigNum::new_oct(1 << 62, 20),
            BigNum::new_spec_raw(1 << 62, 20, 8)
        );
        assert_eq!(
            BigNum::new_oct(1 << 59, 20),
            BigNum::new_spec_raw(1 << 62, 19, 8)
        );

        Ok(())
    }

    #[test]
    fn new_dec_test() -> BigNumTestResult {
        // Check that adjustment is correct, especially around edge cases
        assert_eq!(BigNum::new_dec(1, 0), BigNum::new_spec_raw(1, 0, 10));
        assert_eq!(BigNum::new_dec(100, 2), BigNum::new_spec_raw(10000, 0, 10));
        assert_eq!(
            BigNum::new_dec(10u64.pow(18), 20),
            BigNum::new_spec_raw(10u64.pow(18), 20, 10)
        );
        assert_eq!(
            BigNum::new_dec(10u64.pow(17), 20),
            BigNum::new_spec_raw(10u64.pow(18), 19, 10)
        );

        Ok(())
    }

    #[test]
    fn new_hex_test() -> BigNumTestResult {
        // Check that adjustment is correct, especially around edge cases
        assert_eq!(BigNum::new_hex(1, 0), BigNum::new_spec_raw(1, 0, 16));
        assert_eq!(
            BigNum::new_hex(0x100, 2),
            BigNum::new_spec_raw(0x10000, 0, 16)
        );
        assert_eq!(
            BigNum::new_hex(1 << 60, 20),
            BigNum::new_spec_raw(1 << 60, 20, 16)
        );
        assert_eq!(
            BigNum::new_hex(u64::MAX, 20),
            BigNum::new_spec_raw(u64::MAX, 20, 16)
        );
        assert_eq!(
            BigNum::new_hex(1 << 59, 20),
            BigNum::new_spec_raw(1 << 63, 19, 16)
        );

        Ok(())
    }

    #[test]
    fn new_arbitrary_test() -> BigNumTestResult {
        let template = BigNum::new(0, 0, 7);
        let data = BaseData::new(7);

        assert_eq!(
            BigNum::new(0, 0, 7),
            BigNum::new_with_template(0, 0, &template)
        );
        assert_eq!(
            BigNum::new(0x15, 0, 7),
            BigNum::new_with_template(0x15, 0, &template)
        );
        assert_eq!(
            BigNum::new(0xFFFF_FFFF, 14, 7),
            BigNum::new_with_template(0xFFFF_FFFF, 14, &template),
        );
        assert_eq!(
            BigNum::new(124092, 2, 7),
            BigNum::new_arbitrary_raw(124092 * 49, 0, &data),
        );
        assert_eq!(
            BigNum::new(12342124098u64, 10, 7),
            BigNum::new_arbitrary_raw(12342124098u64 * data.pow(10), 0, &data)
        );
        assert_eq!(
            BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 0, 7),
            BigNum::new_arbitrary_raw(0xFFFF_FFFF_FFFF_FFFF / data.base, 1, &data)
        );

        Ok(())
    }

    #[test]
    fn add_binary_test() -> BigNumTestResult {
        assert_eq!(
            BigNum::new(0x100, 0, 2) + BigNum::new(0x0100_0000, 4, 2),
            BigNum::new_spec_raw(0x1000_0100, 0, 2)
        );
        assert_eq!(
            BigNum::new(0x1000_0000, 32, 2) + BigNum::new(0x0100_0000, 4, 2),
            BigNum::new_spec_raw(0x1000_0000_1000_0000, 0, 2)
        );
        assert_eq!(
            BigNum::new(0xFFFF_FFFF, 32, 2) + BigNum::new(0x8000_0000, 1, 2),
            BigNum::new_spec_raw(0x8000_0000_0000_0000, 1, 2)
        );
        assert_eq!(
            BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 1, 2) + 0x1u32,
            BigNum::new_spec_raw(0xFFFF_FFFF_FFFF_FFFF, 1, 2)
        );
        assert_eq!(
            BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 1, 2) + 0x2u32,
            BigNum::new_spec_raw(0x8000_0000_0000_0000, 2, 2)
        );

        Ok(())
    }

    #[test]
    fn add_hex_test() -> BigNumTestResult {
        assert_eq!(
            0xFFFF_FFFF_FFFF_FFFFu64.into_hex() + 1.into_hex(),
            BigNum::new_spec_raw(0x1000_0000_0000_0000, 1, 16)
        );
        assert_eq!(
            0xFFFF_FFFF_FFFF_FFFEu64.into_hex() + 1.into_hex(),
            BigNum::new_spec_raw(0xFFFF_FFFF_FFFF_FFFF, 0, 16)
        );
        assert_eq!(
            BigNum::new_hex(0xFFFF_FFFF_FFFF_FFFEu64, 10) + 0x0100_0000_0000u64.into_hex(),
            BigNum::new_spec_raw(0xFFFF_FFFF_FFFF_FFFF, 10, 16)
        );
        assert_eq!(
            BigNum::new_hex(0xFFFF_FFFF_FFFF_FFFFu64, 0xFFFF_FFFF_FFFF_0000)
                + 0x0100_0000_0000u64.into_hex(),
            BigNum::new_spec_raw(0xFFFF_FFFF_FFFF_FFFF, 0xFFFF_FFFF_FFFF_0000, 16)
        );
        assert_eq!(
            BigNum::new_hex(0xFFFF_FFFF_FFFF_FFFF, 0xFFFF_FFFF)
                + BigNum::new_hex(0x1FFF_FFFF_FFFF_FFFF, 0xFFFF_FFF0),
            BigNum::new_spec_raw(0x1000_0000_0000_0000, 0x1_0000_0000, 16)
        );
        assert_eq!(
            BigNum::new_hex(0xFFFF_FFFF_FFFF_FFFF, 0xFFFF_FFFF)
                + BigNum::new_hex(0x1FFF_FFFF_FFFF_FFFF, 0xFFFF_FFEF),
            BigNum::new_spec_raw(0xFFFF_FFFF_FFFF_FFFF, 0xFFFF_FFFF, 16)
        );

        Ok(())
    }

    #[test]
    fn add_arbitrary_test() -> BigNumTestResult {
        let data = BaseData::new(61);
        assert_eq!(
            0xFFFF_FFFF_FFFF_FFFEu64.into_with_base(61) + 1.into_with_base(61),
            BigNum::new_arbitrary_raw(((u64::MAX as u128 + 1) / 61) as u64, 1, &data)
        );
        assert_eq!(
            1u64.into_with_base(61) + 1.into_with_base(61),
            BigNum::new_arbitrary_raw(2, 0, &data)
        );
        assert_eq!(
            BigNum::new(data.max_sig(), 10, 61) + BigNum::new(1, 10, 61),
            BigNum::new_arbitrary_raw(data.min_sig(), 11, &data)
        );
        assert_eq!(
            BigNum::new(data.max_sig(), 10, 61) + BigNum::new(61, 9, 61),
            BigNum::new_arbitrary_raw(data.min_sig(), 10, &data)
        );

        Ok(())
    }
}

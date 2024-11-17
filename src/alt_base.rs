use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::HashMap,
    fmt::{Debug, Display, Write},
    ops::{Add, AddAssign, Deref, Sub},
    sync::{LazyLock, Mutex},
    u64,
};

use crate::{
    check_bases, impl_from_with_base,
    powers::{
        BIN_EXP_RANGE, BIN_SIG_RANGE, DEC_EXP_RANGE, DEC_POWERS, DEC_SIG_RANGE, HEX_EXP_RANGE,
        HEX_POWERS, HEX_SIG_RANGE, OCT_EXP_RANGE, OCT_POWERS, OCT_SIG_RANGE,
    },
};

static BASEDATA_CACHE: LazyLock<Mutex<HashMap<u16, BaseData>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

macro_rules! basedata_cache_lock {
    ($base: expr) => {
        BASEDATA_CACHE
            .lock()
            .expect("Unable to obtain lock on BASEDATA_CACHE")
    };
}

macro_rules! basedata_val {
    ($lock: ident, $base: expr) => {
        $lock
            .get(&$base)
            .unwrap_or_else(|| panic!("Unable to access metadata for base {}", $base))
    };
}

macro_rules! ensure_cached {
    ($base: expr) => {{
        let mut cache = BASEDATA_CACHE
            .lock()
            .expect("Unable to obtain lock on BASEDATA_CACHE");

        cache.entry($base).or_insert(BaseData::new($base));
        std::mem::drop(cache);
    }};
}

fn get_cached_pow(exp: u32, base: u16) -> u64 {
    let lock = basedata_cache_lock!(base);

    let ret = basedata_val!(lock, base).pow(exp);

    std::mem::drop(lock);

    ret
}

fn get_cached_mag_arbitrary(sig: u64, base: u16) -> u64 {
    let lock = basedata_cache_lock!(base);

    let ret = basedata_val!(lock, base)
        .powers
        .iter()
        .enumerate()
        .find(|(_, &v)| sig < v)
        .unwrap_or_else(|| panic!("Unable to find base-{} magnitude of value {}", base, sig))
        .0
        .saturating_sub(1) as u64;

    std::mem::drop(lock);

    ret
}

fn get_cached_exp_range(base: u16) -> (u32, u32) {
    let lock = basedata_cache_lock!(base);

    let ret = basedata_val!(lock, base).exp_range;

    std::mem::drop(lock);

    ret
}

fn get_cached_sig_range(base: u16) -> (u64, u64) {
    let lock = basedata_cache_lock!(base);

    let ret = basedata_val!(lock, base).sig_range;

    std::mem::drop(lock);

    ret
}

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
#[derive(Debug)]
pub struct BaseData {
    base: u16,
    powers: Vec<u64>,
    sig_range: (u64, u64),
    /// These are `u32` to make `pow` calls more convenient
    exp_range: (u32, u32),
}
impl BaseData {
    pub fn new(base: u16) -> Self {
        match base {
            // The special bases have pre-defined const metadata so this struct should
            // never be constructed for those bases
            2 | 8 | 10 | 16 => panic!("Unable to create BaseData for base {}", base),
            _ => {
                let mut powers = vec![];

                let mut exp = 0u32;
                let mut sig: u128 = 1;

                while sig <= u64::MAX as u128 {
                    powers.push(sig as u64);

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

    pub fn sig_range(&self) -> (u64, u64) {
        self.sig_range
    }

    pub fn exp_range(&self) -> (u32, u32) {
        self.exp_range
    }

    pub fn pow(&self, exp: u32) -> u64 {
        self.powers[exp as usize]
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

#[derive(Clone, Copy, Debug)]
pub struct BigNum {
    sig: u64,
    exp: u64,
    base: u16,
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
            2 => Self::new_bin_spec(sig, exp),
            8 | 10 | 16 => Self::new_spec(sig, exp, base),
            _ => Self::new_arbitrary(sig, exp, base),
        }
    }
    pub fn new_bin(sig: u64, exp: u64) -> Self {
        Self::new(sig, exp, 2)
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
    fn new_bin_spec(sig: u64, exp: u64) -> Self {
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
            return Self::new_bin_spec(sig, exp);
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

    fn new_arbitrary(sig: u64, exp: u64, base: u16) -> Self {
        ensure_cached!(base);
        let (min_sig, max_sig) = get_cached_sig_range(base);
        let min_exp = get_cached_exp_range(base).0 as u64;

        let (sig, exp) = if sig > max_sig {
            (sig / base as u64, exp + 1)
        } else {
            (sig, exp)
        };

        if exp == 0 {
            Self::new_arbitrary_raw(sig, 0, base)
        } else if sig == 0 {
            panic!(
                "Unable to create a base-{} BigNum with exp of {} and sig of {}",
                base, exp, sig
            );
        } else if sig >= min_sig {
            Self::new_arbitrary_raw(sig, exp, base)
        } else {
            let mag = get_cached_mag_arbitrary(sig, base);

            if mag.saturating_add(exp) <= min_exp {
                Self::new_arbitrary_raw(sig * get_cached_pow(exp as u32, base), 0, base)
            } else {
                let adj = min_exp - mag;

                Self::new_arbitrary_raw(sig * get_cached_pow(adj as u32, base), exp - adj, base)
            }
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
            Self { sig, exp, base }
        }
    }

    fn new_bin_raw(sig: u64, exp: u64) -> Self {
        if exp != 0 && sig < BIN_SIG_RANGE.0 {
            panic!("Binary BigNum with sig {} and exp {} is invalid", sig, exp);
        } else {
            Self { sig, exp, base: 2 }
        }
    }

    fn new_arbitrary_raw(sig: u64, exp: u64, base: u16) -> Self {
        let (min_sig, max_sig) = get_cached_sig_range(base);
        if sig > max_sig || exp != 0 && sig < min_sig {
            panic!(
                "Base-{} BigNum with sig {} and exp {} is invalid. min_sig: {}, max_sig: {}",
                base, sig, exp, min_sig, max_sig
            );
        }

        Self { sig, exp, base }
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

    /// Helper function that gets the n-th power of self's base
    fn pow(&self, n: u32) -> u64 {
        match self.base {
            2 => 1 << n,
            8 => OCT_POWERS[n as usize],
            10 => DEC_POWERS[n as usize],
            16 => HEX_POWERS[n as usize],
            _ => get_cached_pow(n, self.base),
        }
    }

    fn min_sig(&self) -> u64 {
        match self.base {
            2 => BIN_SIG_RANGE.0,
            8 => OCT_SIG_RANGE.0,
            10 => DEC_SIG_RANGE.0,
            16 => HEX_SIG_RANGE.0,
            _ => get_cached_sig_range(self.base).0,
        }
    }
    fn max_sig(&self) -> u64 {
        match self.base {
            2 => BIN_SIG_RANGE.1,
            8 => OCT_SIG_RANGE.1,
            10 => DEC_SIG_RANGE.1,
            16 => HEX_SIG_RANGE.1,
            _ => get_cached_sig_range(self.base).1,
        }
    }
    fn min_exp(&self) -> u32 {
        match self.base {
            2 => BIN_EXP_RANGE.0,
            8 => OCT_EXP_RANGE.0,
            10 => DEC_EXP_RANGE.0,
            16 => HEX_EXP_RANGE.0,
            _ => get_cached_exp_range(self.base).0,
        }
    }
    fn max_exp(&self) -> u32 {
        match self.base {
            2 => BIN_EXP_RANGE.1,
            8 => OCT_EXP_RANGE.1,
            10 => DEC_EXP_RANGE.1,
            16 => HEX_EXP_RANGE.1,
            _ => get_cached_exp_range(self.base).1,
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
            return max;
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

        Self::new(sig, exp, self.base)
    }
}

//impl Sub for BigNum {
//    type Output = Self;
//
//    fn sub(self, rhs: Self) -> Self::Output {
//        check_bases!(self, rhs, "sub");
//
//        let (max, min) = if self >= rhs {
//            (self, rhs)
//        } else {
//            panic!("Unable to subtract {:?} from {:?}", rhs, self)
//        };
//        let shift = max.exp - min.exp;
//
//        if shift >= self.max_exp() as u64 {
//            // This shift is guaranteed to result in 0 on lhs, no need to compute
//            return self;
//        }
//
//        let result = max
//            .sig
//            .wrapping_add(min.sig.saturating_div(min.pow(shift as u32)));
//
//        let (sig, exp) = if result < max.sig {
//            // Wrapping occurred, handle it
//            (self.min_sig() + (result / self.base as u64), max.exp + 1)
//        } else {
//            (result, max.exp)
//        };
//
//        if !self.is_special_base() {
//            Self::new_with_template(sig, exp, &self)
//        } else {
//            Self::new(sig, exp, self.base)
//        }
//    }
//}

impl AddAssign for BigNum {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

#[cfg(test)]
mod tests {

    use std::u32;

    use crate::{
        alt_base::{
            get_cached_exp_range, get_cached_pow, get_cached_sig_range, BaseData, BigNum,
            IntoWithBase,
        },
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
        const BASE: u16 = 7;

        assert_eq!(BigNum::new(0, 0, 7), BigNum::new_arbitrary_raw(0, 0, BASE));
        assert_eq!(
            BigNum::new(0x15, 0, 7),
            BigNum::new_arbitrary_raw(0x15, 0, BASE)
        );
        assert_eq!(
            BigNum::new(0xFFFF_FFFF, 14, 7),
            BigNum::new_arbitrary_raw(0xFFFF_FFFF * (BASE as u64).pow(10), 4, BASE),
        );
        assert_eq!(
            BigNum::new(124092, 2, 7),
            BigNum::new_arbitrary_raw(124092 * 49, 0, BASE),
        );
        assert_eq!(
            BigNum::new(12342124098u64, 10, 7),
            BigNum::new_arbitrary_raw(12342124098u64 * get_cached_pow(10, BASE), 0, BASE)
        );
        assert_eq!(
            BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 0, 7),
            BigNum::new_arbitrary_raw(0xFFFF_FFFF_FFFF_FFFF / BASE as u64, 1, BASE)
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
        const BASE: u16 = 61;
        let _ = BigNum::new_arbitrary(1, 0, BASE);
        let (min_sig, max_sig) = get_cached_sig_range(BASE);

        assert_eq!(
            0xFFFF_FFFF_FFFF_FFFEu64.into_with_base(BASE) + 1.into_with_base(BASE),
            BigNum::new_arbitrary_raw(((u64::MAX as u128 + 1) / BASE as u128) as u64, 1, BASE)
        );
        assert_eq!(
            1u64.into_with_base(BASE) + 1.into_with_base(BASE),
            BigNum::new_arbitrary_raw(2, 0, BASE)
        );
        assert_eq!(
            BigNum::new(max_sig, 10, BASE) + BigNum::new(1, 10, BASE),
            BigNum::new_arbitrary_raw(min_sig, 11, BASE)
        );
        assert_eq!(
            BigNum::new(max_sig, 10, BASE) + BigNum::new(BASE as u64, 9, BASE),
            BigNum::new_arbitrary_raw(min_sig, 11, BASE)
        );

        Ok(())
    }
}

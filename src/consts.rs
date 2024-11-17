use std::{
    collections::HashMap,
    sync::{LazyLock, Mutex},
};

/// Holds runtime data for a base. This includes a table of valid powers, and ranges of
/// the significand. This type is Copy but since it does have a non-trivial amount of data
/// we still try to use references where it is convenient.
#[derive(Debug)]
pub struct BaseData {
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
                    powers,
                    exp_range: (exp - 2, exp - 1),
                    sig_range: (min as u64, (max - 1) as u64),
                }
            }
        }
    }

    pub fn pow(&self, exp: u32) -> u64 {
        self.powers[exp as usize]
    }
}
pub(crate) static BASEDATA_CACHE: LazyLock<Mutex<HashMap<u16, BaseData>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

#[macro_export]
macro_rules! basedata_cache_lock {
    ($base: expr) => {
        $crate::cache::BASEDATA_CACHE
            .lock()
            .expect("Unable to obtain lock on BASEDATA_CACHE")
    };
}

#[macro_export]
macro_rules! basedata_val {
    ($lock: ident, $base: expr) => {
        $lock
            .get(&$base)
            .unwrap_or_else(|| panic!("Unable to access metadata for base {}", $base))
    };
}

#[macro_export]
macro_rules! ensure_cached {
    ($base: expr) => {{
        let mut cache = $crate::cache::BASEDATA_CACHE
            .lock()
            .expect("Unable to obtain lock on BASEDATA_CACHE");

        cache
            .entry($base)
            .or_insert($crate::cache::BaseData::new($base));
        std::mem::drop(cache);
    }};
}

pub fn get_cached_pow(exp: u32, base: u16) -> u64 {
    let lock = basedata_cache_lock!(base);

    let ret = basedata_val!(lock, base).pow(exp);

    std::mem::drop(lock);

    ret
}

pub fn get_cached_mag_arbitrary(sig: u64, base: u16) -> u64 {
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

pub fn get_cached_exp_range(base: u16) -> (u32, u32) {
    let lock = basedata_cache_lock!(base);

    let ret = basedata_val!(lock, base).exp_range;

    std::mem::drop(lock);

    ret
}

pub fn get_cached_sig_range(base: u16) -> (u64, u64) {
    let lock = basedata_cache_lock!(base);

    let ret = basedata_val!(lock, base).sig_range;

    std::mem::drop(lock);

    ret
}

// Below are the tables/ranges for the special types. These are not part of the caching
// system but putting them here made the most sense.

pub const BIN_EXP_RANGE: (u32, u32) = (63, 64);
pub const OCT_EXP_RANGE: (u32, u32) = (20, 21);
pub const DEC_EXP_RANGE: (u32, u32) = (18, 19);
pub const HEX_EXP_RANGE: (u32, u32) = (15, 16);

/// Because of the edge cases noted above, this range *is* inclusive.
pub const BIN_SIG_RANGE: (u64, u64) = (1 << BIN_EXP_RANGE.0, u64::MAX);
pub const OCT_SIG_RANGE: (u64, u64) = (8u64.pow(OCT_EXP_RANGE.0), 8u64.pow(OCT_EXP_RANGE.1) - 1);
pub const DEC_SIG_RANGE: (u64, u64) = (10u64.pow(DEC_EXP_RANGE.0), 10u64.pow(DEC_EXP_RANGE.1) - 1);
pub const HEX_SIG_RANGE: (u64, u64) = (16u64.pow(HEX_EXP_RANGE.0), u64::MAX);

pub const OCT_POWERS: [u64; 22] = [
    0o1,
    0o10,
    0o100,
    0o1000,
    0o10000,
    0o100000,
    0o1000000,
    0o10000000,
    0o100000000,
    0o1000000000,
    0o10000000000,
    0o100000000000,
    0o1000000000000,
    0o10000000000000,
    0o100000000000000,
    0o1000000000000000,
    0o10000000000000000,
    0o100000000000000000,
    0o1000000000000000000,
    0o10000000000000000000,
    0o100000000000000000000,
    0o1000000000000000000000,
];

pub const DEC_POWERS: [u64; 20] = [
    1,
    10,
    100,
    1000,
    10000,
    100000,
    1000000,
    10000000,
    100000000,
    1000000000,
    10000000000,
    100000000000,
    1000000000000,
    10000000000000,
    100000000000000,
    1000000000000000,
    10000000000000000,
    100000000000000000,
    1000000000000000000,
    10000000000000000000,
];

pub const HEX_POWERS: [u64; 16] = [
    0x1,
    0x10,
    0x100,
    0x1000,
    0x10000,
    0x100000,
    0x1000000,
    0x10000000,
    0x100000000,
    0x1000000000,
    0x10000000000,
    0x100000000000,
    0x1000000000000,
    0x10000000000000,
    0x100000000000000,
    0x1000000000000000,
];

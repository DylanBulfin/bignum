use std::{
    cmp::Ordering,
    iter::{Product, Sum},
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Shl, Shr, Sub, SubAssign},
    u64,
};

use rand::distributions::uniform::{SampleBorrow, SampleUniform, UniformInt, UniformSampler};
use utils::get_exp_u64;

mod macros;
pub mod myu128;
pub mod old_methods;
mod utils;

/// Equal to `2^63`, minimum allowed value for base in non-compact `BigNum`
const MIN_BASE_VAL: u64 = 0x8000_0000_0000_0000;

/// Representation of large number. Formula to get true value is `base * 2^exp`
///
/// You should probably use `BigNum::from(n)` or `n.into()` instead of manually constructing this
///
/// # Examples
/// ```
/// use bignum::BigNum;
///
/// let a = BigNum::from(0x10000u64); // 2^16
/// let b = BigNum::from(0xFFFF_FFFF_FFFF_FFFFu64);
/// let c = a * b;
/// let d = c / BigNum::from(2);
/// let e = (a - a) + (b - b) + (c - c) + (d - d);
///
/// assert_eq!(c, BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 16));
/// assert_eq!(d, BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 15));
/// assert_eq!(e, BigNum::ZERO);
/// ```
#[derive(Debug, Clone, Copy, Eq)]
pub struct BigNum {
    base: u64,
    exp: u64,
    // This field keeps me from accidentally constructing this struct manually
    invalidate: bool,
}

impl BigNum {
    pub const ZERO: Self = Self {
        base: 0,
        exp: 0,
        invalidate: true,
    };

    pub const ONE: Self = Self {
        base: 1,
        exp: 0,
        invalidate: true,
    };

    pub const MAX: Self = Self {
        base: u64::MAX,
        exp: u64::MAX,
        invalidate: true,
    };

    pub const MIN: Self = Self::ZERO;

    /// Create a `BigNum` instance directly (e.g. not through the `From` trait)
    pub fn new(base: u64, exp: u64) -> Self {
        if base == 0 && exp != 0 {
            panic!("Invalid BigNum: base is 0 but exp is {}", exp)
        }
        if base < MIN_BASE_VAL && exp != 0 {
            panic!("Invalid BigNum: exp is {} but base is {:#x}", exp, base)
        }
        Self {
            base,
            exp,
            invalidate: false,
        }
    }
}

impl PartialEq for BigNum {
    fn eq(&self, other: &Self) -> bool {
        self.base == other.base && self.exp == other.exp
    }
}

impl Ord for BigNum {
    fn cmp(&self, other: &Self) -> Ordering {
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

/// Struct that implements uniform random generation for BigNum
///
/// Not uniform in the integer sense. This is because we generate a random u64 for the exp and then
/// a base (and then validate). E.g. it is basically just as likely to generate a number between
/// `2^10000000000` and `2^10000000100` as it is to generate numbers between `0` and `2^100 (~10^30)`
///
/// This means it is almost certainly only useful for testing
pub struct UniformBigNum {
    low: BigNum,
    high: BigNum,
    inclusive: bool,
}

impl UniformSampler for UniformBigNum {
    type X = BigNum;

    fn new<B1, B2>(low: B1, high: B2) -> Self
    where
        B1: SampleBorrow<Self::X> + Sized,
        B2: SampleBorrow<Self::X> + Sized,
    {
        let (&low, &high) = (low.borrow(), high.borrow());
        assert!(high > low);

        UniformBigNum {
            low,
            high,
            inclusive: false,
        }
    }

    fn new_inclusive<B1, B2>(low: B1, high: B2) -> Self
    where
        B1: SampleBorrow<Self::X> + Sized,
        B2: SampleBorrow<Self::X> + Sized,
    {
        let (&low, &high) = (low.borrow(), high.borrow());
        assert!(high > low);

        UniformBigNum {
            low,
            high,
            inclusive: true,
        }
    }

    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> Self::X {
        // Exponent should be inclusive even when self isn't since you can have two numbers with
        // equal exp but are different. You need non-inclusive only when high.base is MIN_BASE_VAL,
        // self is not inclusive, and high.exp is non-zero. This is because there is no valid
        // number with exp > 0 where base < MIN_BASE_VAL
        let exp_samp: UniformInt<u64> =
            if !self.inclusive && self.high.exp != 0 && self.high.base == MIN_BASE_VAL {
                UniformInt::new(self.low.exp, self.high.exp)
            } else {
                UniformInt::new_inclusive(self.low.exp, self.high.exp)
            };

        // Is this too gross
        let (low_base, high_base) = (
            self.low.base.min(self.high.base),
            self.low.base.max(self.high.base),
        );

        let base_samp: UniformInt<u64> = if self.inclusive {
            UniformInt::new_inclusive(low_base, high_base)
        } else {
            UniformInt::new(low_base, high_base)
        };

        let valid_base_samp: UniformInt<u64> = if self.inclusive && high_base >= MIN_BASE_VAL {
            UniformInt::new_inclusive(low_base.max(MIN_BASE_VAL), high_base)
        } else if high_base > MIN_BASE_VAL {
            UniformInt::new(low_base.max(MIN_BASE_VAL), high_base)
        } else {
            base_samp
        };

        let mut generate = || {
            let exp_sample = exp_samp.sample(rng);
            let base_sample = base_samp.sample(rng);
            let valid_base_sample = valid_base_samp.sample(rng);

            // When I get an invalid base I chose to handle it by shifting it. This skews results
            // towards numbers with 0s at the end. In a way the number of trailing zeroes is
            // almost uniformly distributed
            //
            // TODO Change this, probably create an alternative sampler that has a low of
            // BASE_MIN_VAL
            //
            // ABOVE IS OUTDATED, LEFT FOR LATER REFERENCE

            if exp_sample == 0 {
                BigNum::new(base_sample, exp_sample)
            } else {
                BigNum::new(valid_base_sample, exp_sample)
            }
        };
        let mut sample = generate();

        while sample < self.low || sample > self.high || (sample == self.high && !self.inclusive) {
            sample = generate();
        }

        sample
    }
}

impl SampleUniform for BigNum {
    type Sampler = UniformBigNum;
}

impl Add for BigNum {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let (max, min) = if self > rhs { (self, rhs) } else { (rhs, self) };
        let shift = max.exp - min.exp;

        if shift >= 64 {
            // minimum number is too small to make a difference in the sum
            return max;
        }

        let result = max.base.wrapping_add(min.base >> shift);

        if result < max.base {
            // Wrap occurred, need to normalize value and exp
            BigNum::new((result >> 1) + MIN_BASE_VAL, max.exp + 1)
        } else {
            // No wrap, easy
            BigNum::new(result, max.exp)
        }
    }
}

impl Sub for BigNum {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        if rhs > self {
            panic!("Attempt to subtract BigNum with underflow");
        }

        if rhs == self {
            return Self::ZERO;
        }

        let (max, min) = (self, rhs);
        let shift = max.exp - min.exp;

        if shift >= 64 {
            // minimum number is too small to make difference
            return max;
        }

        let result = max.base - (min.base >> shift);
        // How far we would need to shift result left to get into correct format
        // First term is how long self's base is, second is how long result is
        let adj = get_exp_u64(max.base) - get_exp_u64(result);

        if adj > max.exp {
            // Result fits in compact form, need to normalize
            // Imagine max = (MIN_BASE_VAL, 1) - (0x10, 0)
            // adj = 1, shift should be 1. So we get adj - max.exp + 1
            Self::new(result << (adj - max.exp - 1), 0)
        } else {
            Self::new(result << adj, max.exp - adj)
        }
    }
}

impl Mul for BigNum {
    type Output = Self;

    fn mul(self, rhs: BigNum) -> Self::Output {
        let result: u128 = self.base as u128 * rhs.base as u128;
        let max_pow = utils::get_exp_u128(result) as u64;

        if max_pow < 64 {
            // Result is compact
            BigNum::new(result as u64, self.exp + rhs.exp)
        } else {
            // Result is expanded
            let adj = max_pow - 63;

            BigNum::new((result >> adj) as u64, self.exp + rhs.exp + adj)
        }
    }
}

impl Div for BigNum {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        if rhs == BigNum::ZERO {
            panic!("Attempt to divide BigNum by zero")
        }
        if rhs > self {
            // Division will result in 0
            return BigNum::ZERO;
        }
        if rhs == self {
            // Division will result in 1
            return BigNum::ONE;
        }

        let lhs_n = (self.base as u128) << 64;
        let rhs_n = rhs.base as u128;

        let result = lhs_n / rhs_n;
        let max_pow = utils::get_exp_u128(result) as u64;

        if self.exp != 0 {
            // Since self is in expanded form, when dividing by 1 we expect result's max_pow
            // to be 127 (64 + 63), if not we adjust self.exp (or base if res is compact)
            let adj = 127 - max_pow;
            // Since you normally adjust by 64, shift is 64 with adjustment
            let shift = 64 - adj;

            if adj >= self.exp {
                // Result can be made compact
                // If we want to adjust by 3 but self.exp is 1, we subtract 1 from adj
                // and then shift result by 2 (3 - 1)
                BigNum::new((result >> (64 - self.exp + rhs.exp)) as u64, 0)
            } else {
                // Result is expanded
                BigNum::new((result >> shift) as u64, self.exp - rhs.exp - adj)
            }
        } else {
            // self is compact so result must be compact
            BigNum::new((result >> 64) as u64, 0)
        }
    }
}

impl Sum for BigNum {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(BigNum::ZERO, |acc, x| acc + x)
    }
}

impl Product for BigNum {
    fn product<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(BigNum::ONE, |acc, x| acc * x)
    }
}

impl<I, T> MinMax for I
where
    I: Iterator<Item = T>,
    T: Copy + Into<BigNum> + Ord,
{
    type Output = T;

    fn maximum_by_checked<F>(mut self, f: &mut F) -> Option<Self::Output>
    where
        F: FnMut(Self::Output, Self::Output) -> Ordering,
    {
        self.next().map(|start| {
            self.fold(
                start,
                |acc, x| if f(acc, x) == Ordering::Less { x } else { acc },
            )
        })
    }
    fn minimum_by_checked<F>(mut self, f: &mut F) -> Option<Self::Output>
    where
        F: FnMut(Self::Output, Self::Output) -> Ordering,
    {
        self.next().map(|start| {
            self.fold(start, |acc, x| {
                if f(acc, x) == Ordering::Greater {
                    x
                } else {
                    acc
                }
            })
        })
    }
}

bignum_math_impl!(u64);
bignum_math_impl!(u32);
bignum_math_impl!(u16);
bignum_math_impl!(u8);
bignum_math_impl!(i64);
bignum_math_impl!(i32);
bignum_math_impl!(i16);
bignum_math_impl![i8];

pub trait MinMax: Sized {
    type Output: Ord;

    fn maximum_by_checked<F>(self, f: &mut F) -> Option<Self::Output>
    where
        F: FnMut(Self::Output, Self::Output) -> Ordering;

    fn minimum_by_checked<F>(self, f: &mut F) -> Option<Self::Output>
    where
        F: FnMut(Self::Output, Self::Output) -> Ordering;

    fn maximum_checked(self) -> Option<Self::Output> {
        self.maximum_by_checked(&mut |a, b| a.cmp(&b))
    }

    fn maximum_by<F>(self, f: &mut F) -> Self::Output
    where
        F: FnMut(Self::Output, Self::Output) -> Ordering,
    {
        self.maximum_by_checked(f)
            .expect("Failed to find maximum value")
    }

    fn maximum(self) -> Self::Output {
        self.maximum_checked()
            .expect("Failed to find maximum value")
    }

    fn minimum_checked(self) -> Option<Self::Output> {
        self.minimum_by_checked(&mut |a, b| a.cmp(&b))
    }

    fn minimum_by<F>(self, f: &mut F) -> Self::Output
    where
        F: FnMut(Self::Output, Self::Output) -> Ordering,
    {
        self.minimum_by_checked(f)
            .expect("Failed to find minimum value")
    }

    fn minimum(self) -> Self::Output {
        self.minimum_checked()
            .expect("Failed to find minimum value")
    }
}

// Technically possible to have valid shift outside of u64 range
// TODO implement this
impl Shl<u128> for BigNum {
    type Output = Self;

    fn shl(self, rhs: u128) -> Self::Output {
        unimplemented!()
    }
}

impl Shr<u64> for BigNum {
    type Output = Self;

    fn shr(self, rhs: u64) -> Self::Output {
        if self.exp == 0 {
            if rhs > get_exp_u64(self.base) {
                panic!("Attempt to right shift BigNum with overflow")
            } else {
                Self::new(self.base >> rhs, self.exp)
            }
        } else if self.exp >= rhs {
            Self::new(self.base, self.exp - rhs)
        } else {
            let full_pow = self.exp.wrapping_add(63);

            if full_pow >= 63 && full_pow >= rhs {
                Self::new(self.base >> (full_pow - rhs), 0)
            } else if full_pow < 63 {
                // Overflow occurred, meaning full_pow >= u64::MAX, e.g. this is guaranteed to be
                // a valid shift
                Self::new(self.base >> (rhs - self.exp), 0)
            } else {
                // No overflow, and rhs > full_pow, so we panic
                panic!("Attempt to right shift BigNum with overflow")
            }
        }
    }
}

impl Shl<u64> for BigNum {
    type Output = Self;

    fn shl(self, rhs: u64) -> Self::Output {
        if self.exp == 0 {
            // Guaranteed not to overflow since it has at least 2^64 of wiggle room
            let adj = 63 - get_exp_u64(self.base);

            if adj > rhs {
                Self::new(self.base << rhs, 0)
            } else {
                Self::new(self.base << adj, rhs - adj)
            }
        } else if rhs <= u64::MAX - self.exp {
            // There is enough room for growth in the exponent
            Self::new(self.base, self.exp + rhs)
        } else {
            // There is not enough room for growth, panic
            panic!("Attempt to left shift BigNum with overflow")
        }
    }
}

/// Struct holding a custom ratio object, for more efficient but less flexible multiplication/division
pub struct Ratio {
    numerator: u16,
    denominator: u16,
}

impl Ratio {
    pub fn new(numerator: u16, denominator: u16) -> Self {
        if denominator.count_ones() != 1 {
            // Denominator is not a power of 2, panic
            panic!("Ratio does not support a denominator of {}", denominator);
        }
        if numerator == 0 && denominator != 1 {
            // To avoid having multiple equivalent Ratios we ensure that (0, 1) is the only valid
            // reprensentation of 0
            panic!(
                "Ration does not support a denominator of {} with a 0 numerator",
                denominator
            );
        }

        Self {
            numerator,
            denominator,
        }
    }
}

impl Mul<Ratio> for BigNum {
    type Output = BigNum;

    fn mul(self, rhs: Ratio) -> Self::Output {
        // Need to consider whether we should multiply or divide first
        // For now we do multiplication first
        (self * rhs.numerator) >> get_exp_u64(rhs.denominator as u64)
    }
}

#[cfg(test)]
mod tests {
    use rand::{distributions::Uniform, prelude::Distribution, thread_rng};

    use super::*;

    const A1: BigNum = BigNum::ZERO;

    const B1: BigNum = BigNum::ONE;
    const B2: BigNum = BigNum {
        base: 0x8000_0000_0000_0000,
        exp: 0,
        invalidate: false,
    };
    const B3: BigNum = BigNum {
        base: 0xFFFF_FFFF_FFFF_FFFF,
        exp: 0,
        invalidate: false,
    };

    const C1: BigNum = BigNum {
        base: 0x8000_0000_0000_0000,
        exp: 10,
        invalidate: false,
    };
    const C2: BigNum = BigNum {
        base: 0x8000_0000_0000_0000,
        exp: 73,
        invalidate: false,
    };
    const C3: BigNum = BigNum {
        base: 0xFFFF_FFFF_FFFF_FFFF,
        exp: 120,
        invalidate: false,
    };
    const C4: BigNum = BigNum {
        base: 0xFFFF_FFFF_FFFF_FFFF,
        exp: 127000,
        invalidate: false,
    };
    const C5: BigNum = BigNum {
        base: u64::MAX,
        exp: u64::MAX,
        invalidate: false,
    };

    #[test]
    fn add() {
        // A
        assert_eq!(A1 + A1, BigNum::ZERO);

        // A + B -> B
        assert_eq!(A1 + B1, BigNum::ONE);

        // B
        assert_eq!(B1 + B2, BigNum::new(0x8000_0000_0000_0001, 0));

        // B + B -> C
        assert_eq!(B1 + B3, BigNum::new(0x8000_0000_0000_0000, 1));
        assert_eq!(B2 + B3, BigNum::new(0xBFFF_FFFF_FFFF_FFFF, 1));

        // C
        assert_eq!(C1 + C2, BigNum::new(0x8000_0000_0000_0001, 73));
        assert_eq!(C2 + C3, BigNum::new(0x8000_0000_0000_7FFF, 121));
        assert_eq!(C3 + C4, C4); // Too small to make a difference
        assert_eq!(C1 + C3, C3);
    }

    #[should_panic]
    #[test]
    fn add_panic() {
        // Should panic when adding numbers that cause overflow
        let _ = C5 + C5;
    }

    #[test]
    fn sub() {
        // A
        assert_eq!(A1 - A1, BigNum::ZERO);

        // B - B = A
        assert_eq!(B1 - B1, BigNum::ZERO);
        assert_eq!(B2 - B2, BigNum::ZERO);
        assert_eq!(B3 - B3, BigNum::ZERO);

        // B - B = B
        assert_eq!(B2 - B1, BigNum::new(0x7FFF_FFFF_FFFF_FFFF, 0));
        assert_eq!(B3 - B1, BigNum::new(0xFFFF_FFFF_FFFF_FFFE, 0));
        assert_eq!(B3 - B2, BigNum::new(0x7FFF_FFFF_FFFF_FFFF, 0));

        // C - C = A
        assert_eq!(C1 - C1, BigNum::ZERO);
        assert_eq!(C2 - C2, BigNum::ZERO);
        assert_eq!(C3 - C3, BigNum::ZERO);
        assert_eq!(C4 - C4, BigNum::ZERO);
        assert_eq!(C5 - C5, BigNum::ZERO);

        // C - B = B
        assert_eq!(C1 - B2, BigNum::new(0xFFC0_0000_0000_0000, 9));
        assert_eq!(C1 - B3, BigNum::new(0xFF80_0000_0000_0002, 9));

        // C - A = C
        assert_eq!(C1 - A1, C1);
        assert_eq!(C2 - A1, C2);
        assert_eq!(C3 - A1, C3);
        assert_eq!(C4 - A1, C4);
        assert_eq!(C5 - A1, C5);

        // C - B = C
        assert_eq!(C2 - B3, C2);
        assert_eq!(C3 - B3, C3);
        assert_eq!(C4 - B3, C4);
        assert_eq!(C5 - B3, C5);
    }

    #[should_panic]
    #[test]
    fn sub_overflow() {
        let _ = A1 - B1;
    }

    #[should_panic]
    #[test]
    fn sub_expanded_overflow() {
        let _ = C1 - C2;
    }

    #[test]
    fn mul_u64() {
        let a = 1u64;
        let b = 0xFFFF_FFFF_FFFF_FFFFu64;
        let c = BigNum::new(MIN_BASE_VAL, 1);

        assert_eq!(c * a, c);
        assert_eq!(c * b, BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 64));

        let d = BigNum::from(0x8000_1000_1000_1000u64);
        let e = 2u64;
        assert_eq!(d * e, BigNum::new(0x8000_1000_1000_1000, 1));

        let f = 3u64;
        assert_eq!(d * f, BigNum::new(0xC000_1800_1800_1800, 1));

        let g = BigNum::new(0x8000_1000_1000_1000, 100);
        assert_eq!(g * f, BigNum::new(0xC000_1800_1800_1800, 101));
    }

    #[test]
    fn div_u64() {
        let a = 1u64;
        let b = 0x8000u64;
        let c = BigNum::new(MIN_BASE_VAL, 1);

        assert_eq!(c / a, c);
        assert_eq!(c / b, BigNum::new(0x0002_0000_0000_0000, 0));

        let d = BigNum::from(0x8000_1000_1000_1000u64);
        let e = 2u64;
        assert_eq!(d / e, BigNum::new(0x4000_0800_0800_0800, 0));

        //let d = BigNum::from(1e19)
        // d is right above lower limit for base
        let f = BigNum::new(10_000_000_000_000_000_000u64, 1);
        let g = 5u64;
        assert_eq!(f / g, BigNum::from(4_000_000_000_000_000_000u64));

        let h = BigNum::new(MIN_BASE_VAL, 10000);
        let i = BigNum::new(MIN_BASE_VAL, 9937);
        let j = BigNum::new(MIN_BASE_VAL, 9936);
        assert_eq!(h / i, BigNum::new(MIN_BASE_VAL, 0));
        assert_eq!(h / j, BigNum::new(MIN_BASE_VAL, 1));

        assert_eq!(c / c, BigNum::ONE);
        assert_eq!(d / d, BigNum::ONE);
        assert_eq!(f / f, BigNum::ONE);
        assert_eq!(h / h, BigNum::ONE);
        assert_eq!(i / i, BigNum::ONE);
        assert_eq!(j / j, BigNum::ONE);
    }

    #[should_panic]
    #[test]
    fn div_zero() {
        let _ = BigNum::ONE / BigNum::ZERO;
    }

    fn test_rand(iterations: usize, confidence_width: usize, exp_confidence_width: usize) {
        // I'm deliberately using occasionally-failing tests here to test the distribution, if this
        // test fails it will probably work by re-running it
        let full_rand = Uniform::new(BigNum::ZERO, BigNum::MAX);
        let small_rand = Uniform::new_inclusive(BigNum::ONE, BigNum::from(1000));

        // Holds number of samples that were in the range [1,100) for each sampler
        let mut full_count = 0;
        let mut small_count = 0;

        // The number of times the sample from the full range has exp >2^63 (e.g. top half of input
        // space)
        let mut exp_count = 0;

        for _ in 0..iterations {
            let full_samp = full_rand.sample(&mut thread_rng());
            let small_samp = small_rand.sample(&mut thread_rng());

            if full_samp >= BigNum::from(10) && full_samp <= BigNum::from(100) {
                full_count += 1;
            }
            if small_samp >= BigNum::from(10) && small_samp <= BigNum::from(100) {
                small_count += 1;
            }
            if full_samp >= BigNum::new(MIN_BASE_VAL, 1 << 63) {
                exp_count += 1;
            }
        }

        // The likelihood of getting a single value in this range is astronomical, something around 1 / 2^(57 + 64 - log2(iterations)).
        // This means that getting 2 or more, regardless of iteration count, is either a once-in-a-lifetime event, an issue with
        // the rng generator, or an issue with the sampling algorithm I wrote
        assert!(full_count < 2);

        // These intervals should be fairly forgiving
        let (confidence_low, confidence_high) = (
            (iterations / 10) - confidence_width,
            (iterations / 10) + confidence_width,
        );
        assert!(small_count > confidence_low);
        assert!(small_count < confidence_high);

        let (exp_confidence_low, exp_confidence_high) = (
            (iterations / 2) - exp_confidence_width,
            (iterations / 2) + exp_confidence_width,
        );
        assert!(exp_count > exp_confidence_low);
        assert!(exp_count < exp_confidence_high);
    }

    #[test]
    fn rand_short() {
        // May fail once, always re-run (unlikely but possible)
        test_rand(100000, 5000, 1000)
    }

    #[ignore]
    #[test]
    fn rand_full() {
        // Fairly slow to run, but completed in <1m on my computer
        // May fail once, always re-run (unlikely but possible)
        test_rand(10_000_000, 100_000, 10_000)
    }

    #[test]
    fn test_min_max_trait() {
        let elems = Vec::from([A1, B1, B2, B3, C1, C2, C3, C4, C5]);
        let empty: Vec<u64> = Vec::new();

        assert_eq!(*elems.iter().maximum(), C5);
        assert_eq!(
            *elems.iter().maximum_by(&mut |a, b| match a.cmp(b) {
                Ordering::Less => Ordering::Greater,
                Ordering::Greater => Ordering::Less,
                _ => Ordering::Equal,
            }),
            A1
        );
        assert_eq!(*elems.iter().minimum(), A1);
        assert_eq!(
            *elems.iter().minimum_by(&mut |a, b| match a.cmp(b) {
                Ordering::Less => Ordering::Greater,
                Ordering::Greater => Ordering::Less,
                _ => Ordering::Equal,
            }),
            C5
        );

        assert_eq!(elems.iter().maximum_checked().copied(), Some(C5));
        assert_eq!(
            elems
                .iter()
                .maximum_by_checked(&mut |a, b| match a.cmp(b) {
                    Ordering::Less => Ordering::Greater,
                    Ordering::Greater => Ordering::Less,
                    _ => Ordering::Equal,
                })
                .copied(),
            Some(A1)
        );
        assert_eq!(elems.iter().minimum_checked().copied(), Some(A1));
        assert_eq!(
            elems
                .iter()
                .minimum_by_checked(&mut |a, b| match a.cmp(b) {
                    Ordering::Less => Ordering::Greater,
                    Ordering::Greater => Ordering::Less,
                    _ => Ordering::Equal,
                })
                .copied(),
            Some(C5)
        );

        assert_eq!(empty.iter().maximum_checked(), None);
        assert_eq!(
            empty
                .iter()
                .maximum_by_checked(&mut |_, _| Ordering::Greater),
            None
        );
        assert_eq!(empty.iter().minimum_checked(), None);
        assert_eq!(
            empty
                .iter()
                .minimum_by_checked(&mut |_, _| Ordering::Greater),
            None
        );
    }

    #[should_panic(expected = "Failed to find maximum value")]
    #[test]
    fn maximum_empty_panic() {
        let empty: Vec<u64> = Vec::new();
        let _ = empty.iter().maximum();
    }
    #[should_panic(expected = "Failed to find maximum value")]
    #[test]
    fn maximum_by_empty_panic() {
        let empty: Vec<u64> = Vec::new();
        let _ = empty.iter().maximum_by(&mut |_, _| Ordering::Greater);
    }
    #[should_panic(expected = "Failed to find minimum value")]
    #[test]
    fn minimum_empty_panic() {
        let empty: Vec<u64> = Vec::new();
        let _ = empty.iter().minimum();
    }
    #[should_panic(expected = "Failed to find minimum value")]
    #[test]
    fn minimum_by_empty_panic() {
        let empty: Vec<u64> = Vec::new();
        let _ = empty.iter().minimum_by(&mut |_, _| Ordering::Greater);
    }

    #[test]
    fn shift_test() {
        // TODO add this test
    }
}

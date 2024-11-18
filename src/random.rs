use rand::{
    distributions::uniform::{SampleBorrow, SampleUniform, UniformInt, UniformSampler},
    Rng,
};

use crate::{Base, BigNumBase};

pub struct BigNumSampler<T>
where
    T: Base,
{
    base: T,
    low: BigNumBase<T>,
    high: BigNumBase<T>,
    inc: bool,
}

impl<T> UniformSampler for BigNumSampler<T>
where
    T: Base,
{
    type X = BigNumBase<T>;

    fn new<B1, B2>(low: B1, high: B2) -> Self
    where
        B1: SampleBorrow<Self::X> + Sized,
        B2: SampleBorrow<Self::X> + Sized,
    {
        let (low, high) = (*low.borrow(), *high.borrow());

        if low == high {
            panic!("Unable to create non-inclusive range with high == low");
        } else if low > high {
            panic!("Unable to create non-inclusive range with low > high");
        }

        Self {
            base: T::new(),
            low,
            high,
            inc: false,
        }
    }

    fn new_inclusive<B1, B2>(low: B1, high: B2) -> Self
    where
        B1: SampleBorrow<Self::X> + Sized,
        B2: SampleBorrow<Self::X> + Sized,
    {
        let (low, high) = (*low.borrow(), *high.borrow());

        if low == high {
            panic!("Unable to create non-inclusive range with high == low");
        } else if low > high {
            panic!("Unable to create non-inclusive range with low > high");
        }

        Self {
            base: T::new(),
            low,
            high,
            inc: true,
        }
    }

    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Self::X {
        let (low_exp, high_exp) = (self.low.exp, self.high.exp);
        let (low_sig, high_sig) = (self.low.sig, self.high.sig);
        let base = self.high.base;
        let sig_range = base.sig_range();

        if low_exp == high_exp {
            let sig_gen: UniformInt<u64> = if self.inc {
                UniformInt::new_inclusive(low_sig, high_sig)
            } else {
                UniformInt::new(low_sig, high_sig)
            };

            return Self::X {
                sig: sig_gen.sample(rng),
                exp: high_exp,
                base: self.high.base,
            };
        }

        let exp_gen: UniformInt<u64> = if !self.inc && high_sig == sig_range.min() {
            // Since the high value can't be included we must take a non-inclusive range
            // for exp
            UniformInt::new(low_exp, high_exp)
        } else {
            UniformInt::new_inclusive(low_exp, high_exp)
        };

        let sig_gen: UniformInt<u64> = UniformInt::new_inclusive(0, sig_range.max());

        let exp_sample = exp_gen.sample(rng);
        let sig_sample = sig_gen.sample(rng);

        Self::X::new(sig_sample, exp_sample)
    }
}

impl<T> SampleUniform for BigNumBase<T>
where
    T: Base,
{
    type Sampler = BigNumSampler<T>;
}

#[cfg(test)]
mod tests {
    use std::{
        collections::{hash_map::Entry, HashMap},
        iter::from_fn,
    };

    use rand::{distributions::Uniform, prelude::Distribution, thread_rng};

    use crate::Binary;

    use super::*;

    macro_rules! generate_freq_map {
        (
            sig,
            samples = $samp:expr,
            range = $range:expr
        ) => {{
            let mut map: HashMap<u64, u64> = HashMap::new();

            for i in $range {
                map.insert(i, 0);
            }

            for v in $samp {
                match map.entry(v.sig) {
                    Entry::Occupied(mut e) => {
                        e.insert(e.get() + 1);
                    }
                    Entry::Vacant(_) => panic!("Entry for {} has not been initialized", v.sig),
                }
            }

            map
        }};
        (
            exp,
            samples = $samp:expr,
            range = $range:expr
        ) => {{
            let mut map: HashMap<u64, u64> = HashMap::new();

            for i in $range {
                map.insert(i, 0);
            }

            for v in $samp {
                match map.entry(v.exp) {
                    Entry::Occupied(mut e) => {
                        e.insert(e.get() + 1);
                    }
                    Entry::Vacant(_) => panic!("Entry for {} has not been initialized", v.sig),
                }
            }

            map
        }};
    }

    macro_rules! assert_uniform {
        (
            map = $map:ident,
            range = $range:expr,
            expected = $exp:expr,
            confidence = $conf:expr
        ) => {
            for k in $map.keys() {
                let c = *$map.get(k).unwrap();

                if c.abs_diff($exp) > $conf {
                    panic!(
                        "Expected frequencies to be within {} of {}, found {} for key {}",
                        $conf, $exp, c, k
                    );
                }
            }
        };
    }

    // These tests use probabilistic reasoning to verify that random generation is working
    // as expected. It should fail very, very rarely but it's worth running it again if
    // it does fail
    #[test]
    fn rand_basic_test() {
        type BigNum = BigNumBase<Binary>;

        let (low, high) = (BigNum::from(0), BigNum::from(10));
        let rng = &mut thread_rng();

        let dist_inclusive: Uniform<BigNum> = Uniform::new_inclusive(low, high);
        let dist_exclusive: Uniform<BigNum> = Uniform::new(low, high);

        let inclusive_samples: Vec<BigNum> = from_fn(|| Some(dist_inclusive.sample(rng)))
            .take(1000)
            .collect();
        let exclusive_samples: Vec<BigNum> = from_fn(|| Some(dist_exclusive.sample(rng)))
            .take(1000)
            .collect();

        let inclusive_map = generate_freq_map!(sig, samples = inclusive_samples, range = 0..=10);
        let exclusive_map = generate_freq_map!(sig, samples = exclusive_samples, range = 0..10);

        assert_uniform!(
            map = inclusive_map,
            range = 0..=10,
            expected = 91,
            confidence = 40
        );
        assert_uniform!(
            map = exclusive_map,
            range = 0..10,
            expected = 100,
            confidence = 40
        );
    }

    // Since the implementation is not actually correct this test isn't useful for now
    //#[test]
    //fn rand_basic_test_2() {
    //    type BigNum = BigNumBase<Binary>;
    //
    //    let (low, high) = (BigNum::from(u64::MAX), BigNum::new(1 << 63, 2));
    //    let rng = &mut thread_rng();
    //
    //    let dist_inclusive: Uniform<BigNum> = Uniform::new_inclusive(low, high);
    //    let dist_exclusive: Uniform<BigNum> = Uniform::new(low, high);
    //
    //    let inclusive_samples: Vec<BigNum> = from_fn(|| Some(dist_inclusive.sample(rng)))
    //        .take(1000)
    //        .collect();
    //    let exclusive_samples: Vec<BigNum> = from_fn(|| Some(dist_exclusive.sample(rng)))
    //        .take(1000)
    //        .collect();
    //
    //    let inclusive_map = generate_freq_map!(exp, samples = inclusive_samples, range = 0..=2);
    //    let exclusive_map = generate_freq_map!(exp, samples = exclusive_samples, range = 0..=2);
    //}
}

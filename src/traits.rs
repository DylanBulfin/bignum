//! This module contains additional traits I thought may be useful for BigNum usage.

use crate::{Base, BigNumBase, SigRange};

/// This trait is meant to get the very next value of the type. E.g. for BigNumBase<T>,
/// adding 1 results in no change for numbers with `exp != 0`. So this provides the value
/// that is greater than the current value, but less than any other
pub trait Succ {
    fn succ(self) -> Self;
}

pub trait Pred {
    fn pred(self) -> Self;
}

impl<T> Succ for BigNumBase<T>
where
    T: Base,
{
    fn succ(self) -> Self {
        let SigRange(min_sig, max_sig) = self.base.sig_range();

        if self.sig == max_sig {
            Self {
                sig: min_sig,
                exp: self.exp + 1,
                base: self.base,
            }
        } else {
            Self {
                sig: self.sig + 1,
                ..self
            }
        }
    }
}

impl<T> Pred for BigNumBase<T>
where
    T: Base,
{
    fn pred(self) -> Self {
        let SigRange(min_sig, max_sig) = self.base.sig_range();

        if self.exp == 0 {
            if self.sig == 0 {
                panic!("Cannot get the predecessor of 0");
            }

            Self {
                sig: self.sig - 1,
                ..self
            }
        } else if self.sig == min_sig {
            Self {
                sig: max_sig,
                exp: self.exp - 1,
                base: self.base,
            }
        } else {
            Self {
                sig: self.sig - 1,
                ..self
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{assert_eq_bignum, create_default_base, BigNumBase, Binary};

    // Other tests are in the normal macro so we can test it with many different bases

    #[test]
    fn test_succ() {
        type BigNum = BigNumBase<Binary>;
        let SigRange(min_sig, max_sig) = Binary::calculate_ranges().1;

        assert_eq_bignum!(BigNum::new(0, 0).succ(), BigNum::new(1, 0));
        assert_eq_bignum!(BigNum::new(max_sig, 0).succ(), BigNum::new(min_sig, 1));
        assert_eq_bignum!(BigNum::new(max_sig, 1).succ(), BigNum::new(min_sig, 2));
    }

    #[test]
    fn test_pred() {
        type BigNum = BigNumBase<Binary>;
        let SigRange(min_sig, max_sig) = Binary::calculate_ranges().1;

        assert_eq_bignum!(BigNum::new(1, 0).pred(), BigNum::new(0, 0));
        assert_eq_bignum!(BigNum::new(min_sig, 1).pred(), BigNum::new(max_sig, 0));
        assert_eq_bignum!(BigNum::new(min_sig, 2).pred(), BigNum::new(max_sig, 1));

    }
}

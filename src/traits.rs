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

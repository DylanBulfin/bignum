use crate::traits::{Pred, Succ};
use crate::{Base, ExpRange, SigRange};

// Overriding it for better error messages
#[macro_export]
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

#[macro_export]
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

            impl<T> Sub<$ty> for BigNumBase<T> where T: Base {
                type Output = Self;

                fn sub(self, rhs: $ty) -> Self::Output {
                    self - BigNumBase::from(rhs)
                }
            }

            impl<T> Sub<BigNumBase<T>> for $ty where T: Base {
                type Output = BigNumBase<T>;

                fn sub(self, rhs: BigNumBase<T>) -> Self::Output {
                    BigNumBase::from(self) - rhs
                }
            }

            impl<T> SubAssign<$ty> for BigNumBase<T> where T: Base {
                fn sub_assign(&mut self, rhs: $ty) {
                    *self = *self - BigNumBase::from(rhs);
                }
            }

            impl<T> Mul<$ty> for BigNumBase<T> where T:Base {
                type Output = Self;

                fn mul(self, rhs: $ty) -> Self::Output {
                    self * BigNumBase::from(rhs)
                }
            }

            impl<T> Mul<BigNumBase<T>> for $ty where T:Base{
                type Output = BigNumBase<T>;

                fn mul(self, rhs: BigNumBase<T>) -> Self::Output {
                    BigNumBase::from(self) * rhs
                }
            }

            impl<T> MulAssign<$ty> for BigNumBase<T> where T:Base {
                fn mul_assign(&mut self, rhs: $ty){
                    *self = *self * BigNumBase::from(rhs);
                }
            }
        )+
    };
}

#[macro_export]
macro_rules! create_default_base {
    ($name:ident, $num:literal) => {
        #[derive(Debug, Clone, Copy)]
        pub struct $name {
            pub exp_range: $crate::ExpRange,
            pub sig_range: $crate::SigRange,
        }

        impl $crate::Base for $name {
            const NUMBER: u16 = $num;

            fn new() -> Self {
                let (exp_range, sig_range) = Self::calculate_ranges();
                Self {
                    exp_range,
                    sig_range,
                }
            }

            fn exp_range(&self) -> $crate::ExpRange {
                self.exp_range
            }

            fn sig_range(&self) -> $crate::SigRange {
                self.sig_range
            }
        }
    };
}

// Runs some non-base specific tests
#[macro_export]
macro_rules! test_base {
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
            BigNum::new(min_sig + $base::rshift(max_sig, 4), 143)
        );
    }};
    (mul $base:ident) => {{
        type BigNum = BigNumBase<$base>;
        let SigRange(min_sig, max_sig) = $base::calculate_ranges().1;
        let ExpRange(min_exp, max_exp) = $base::calculate_ranges().0;

        assert_eq_bignum!(
            BigNum::new(min_sig, 1) * $base::NUMBER as u64,
            BigNum::new(min_sig, 2)
        );
        assert_eq_bignum!(
            BigNum::new(max_sig, 1) * max_sig,
            BigNum::new_raw(max_sig - 1, max_exp as u64 + 1 + 0)
        );
        assert_eq_bignum!(
            BigNum::new(max_sig, 112341234) * BigNum::new(max_sig, 12341),
            BigNum::new_raw(max_sig - 1, max_exp as u64 + 112341234 + 12341)
        );
        assert_eq_bignum!(
            BigNum::new(min_sig, 1) * BigNum::new(min_sig + 1, 1241234),
            BigNum::new(min_sig + 1, min_exp as u64 + 1 + 1241234)
        );
        assert_eq_bignum!(
            BigNum::new(min_sig, 1) * BigNum::new(max_sig , 1241234),
            BigNum::new(max_sig, min_exp as u64 + 1 + 1241234)
        );
    }};
    (div $base:ident) => {{
        type BigNum = BigNumBase<$base>;
        let SigRange(min_sig, max_sig) = $base::calculate_ranges().1;
        let ExpRange(min_exp, max_exp) = $base::calculate_ranges().0;

        assert_eq_bignum!(
            BigNum::new(min_sig, 2) / BigNum::new(min_sig, 1),
            BigNum::new($base::NUMBER as u64, 0)
        );
        assert_eq_bignum!(
            BigNum::new_raw(max_sig - 1, max_exp as u64 + 1 + 0)
                / BigNum::new(max_sig, 1),
            BigNum::from(max_sig)
        );
        assert_eq_bignum!(
            BigNum::new_raw(max_sig - 1, max_exp as u64 + 112341234 + 12341)
                / BigNum::new(max_sig, 112341234),
            BigNum::new(max_sig, 12341)

        );
        assert_eq_bignum!(
            BigNum::new(min_sig, 1) * BigNum::new(min_sig + 1, 1241234),
            BigNum::new(min_sig + 1, min_exp as u64 + 1 + 1241234)
        );
        assert_eq_bignum!(
            BigNum::new(min_sig, 1) * BigNum::new(max_sig , 1241234),
            BigNum::new(max_sig, min_exp as u64 + 1 + 1241234)
        );
    }};
    (succ $base:ident) => {{
        use $crate::traits::Succ;
        type BigNum = BigNumBase<$base>;
        let SigRange(min_sig, max_sig) = $base::calculate_ranges().1;

        assert_eq_bignum!(BigNum::new(0, 0).succ(), BigNum::new(1, 0));
        assert_eq_bignum!(BigNum::new(max_sig, 0).succ(), BigNum::new(min_sig, 1));
        assert_eq_bignum!(BigNum::new(max_sig, 1).succ(), BigNum::new(min_sig, 2));
        assert_eq_bignum!(
            BigNum::new(max_sig, 2143124).succ(),
            BigNum::new(min_sig, 2143125)
        );
    }};
    (pred $base:ident) => {{
        use $crate::traits::Pred;
        type BigNum = BigNumBase<$base>;
        let SigRange(min_sig, max_sig) = $base::calculate_ranges().1;

        assert_eq_bignum!(BigNum::new(1, 0).pred(), BigNum::new(0, 0));
        assert_eq_bignum!(BigNum::new(min_sig, 1).pred(), BigNum::new(max_sig, 0));
        assert_eq_bignum!(BigNum::new(min_sig, 2).pred(), BigNum::new(max_sig, 1));
    }};
    (shl $base:ident) => {{
        type BigNum = BigNumBase<$base>;
        let SigRange(min_sig, max_sig) = $base::calculate_ranges().1;

        assert_eq_bignum!(
            BigNum::new(124, 0) << 1,
            BigNum::new(124 * $base::NUMBER as u64, 0)
        );
        assert_eq_bignum!(
            BigNum::new(124, 0) << 2,
            BigNum::new(124 * ($base::NUMBER as u64).pow(2), 0)
        );
        assert_eq_bignum!(BigNum::new(min_sig, 1) << 1, BigNum::new(min_sig, 2));
        assert_eq_bignum!(BigNum::new(min_sig, 2) << 1, BigNum::new(min_sig, 3));
        assert_eq_bignum!(BigNum::new(min_sig, 100) << 100, BigNum::new(min_sig, 200));
        assert_eq_bignum!(BigNum::new(max_sig, 3) << 4, BigNum::new(max_sig, 7));
        assert_eq_bignum!(BigNum::new(max_sig, 23) << 1243, BigNum::new(max_sig, 1266));
    }};
    (shr $base:ident) => {{
        type BigNum = BigNumBase<$base>;
        let SigRange(min_sig, max_sig) = $base::calculate_ranges().1;

        assert_eq_bignum!(
            BigNum::new(12412341324, 0) >> 1,
            BigNum::new(12412341324 / $base::NUMBER as u64, 0)
        );
        assert_eq_bignum!(
            BigNum::new(12412341324, 0) >> 2,
            BigNum::new(12412341324 / ($base::NUMBER as u64).pow(2), 0)
        );
        assert_eq_bignum!(BigNum::new(min_sig, 1) >> 1, BigNum::new(min_sig, 0));
        assert_eq_bignum!(BigNum::new(min_sig, 2) >> 1, BigNum::new(min_sig, 1));
        assert_eq_bignum!(
            BigNum::new(min_sig, 100) >> 102,
            BigNum::new(min_sig / ($base::NUMBER as u64).pow(2), 0)
        );
        assert_eq_bignum!(
            BigNum::new(max_sig, 3) >> 4,
            BigNum::new(max_sig / $base::NUMBER as u64, 0)
        );
        assert_eq_bignum!(BigNum::new(max_sig, 1266) >> 1243, BigNum::new(max_sig, 23));
    }};

    ($base:ident) => {{
        test_base!(add $base);
        test_base!(sub $base);
        test_base!(mul $base);
        test_base!(pred $base);
        test_base!(succ $base);
        test_base!(shl $base);
        test_base!(shr $base);
        //test_base!(div $base);
    }};
}

#[macro_export]
macro_rules! create_and_test_base {
    ($base:ident, $num:literal) => {
        create_default_base!($base, $num);
        test_base!($base);
    };
}

#[cfg(test)]
mod tests {
    use crate::Base;
    use crate::BigNumBase;

    #[test]
    fn default_base_test() {
        create_default_base!(Base7, 7);

        type BigNum = BigNumBase<Base7>;

        assert_eq!(BigNum::new(150, 2), BigNum::new_raw(150 * 49, 0));
        assert_eq!(
            BigNum::new(Base7::calculate_ranges().1 .1, 2) + BigNum::new(1, 2),
            BigNum::new_raw(Base7::calculate_ranges().1 .0, 3)
        );

        assert_eq!(
            BigNum::new(Base7::calculate_ranges().1 .1, 3) + BigNum::new(1, 2),
            BigNum::new_raw(Base7::calculate_ranges().1 .1, 3)
        );
        assert_eq!(BigNum::new(u64::MAX, 0), BigNum::new(u64::MAX / 7, 1))
    }
}

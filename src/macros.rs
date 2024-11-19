use crate::{Base, ExpRange, SigRange};

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
    (*; $base:ident) => {{
        test_base!(add $base);
        test_base!(sub $base);
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

#[macro_export]
macro_rules! create_and_test_base {
    (*; $base:ident, $num:literal) => {
        create_default_base!($base, $num);
        test_base!(sub $base);
        test_base!(add $base);
    };
    (sub $base:ident, $num:literal) => {
        create_default_base!($base, $num);
        test_base!(sub $base);
    };
    (add $base:ident, $num:literal) => {
        create_default_base!($base, $num);
        test_base!(add $base);
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

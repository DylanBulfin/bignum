pub mod test_macros {
    #![cfg(test)]

    macro_rules! assert_close_bignum {
        ($lhs:expr, $rhs:expr) => {{
            let (max, min) = if $rhs > $lhs {($rhs, $lhs)} else {($lhs, $rhs)};

            if !max.fuzzy_eq(min, max.base.as_number() as u64) {
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
        }};
    }

    // For better error messages
    macro_rules! assert_eq_bignum {
        ($lhs:expr, $rhs:expr) => {{
            let (lhs, rhs) = ($lhs, $rhs);

            if lhs != rhs {
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
            , lhs.sig,
            rhs.sig, lhs.exp, rhs.exp, lhs.base.as_number(), lhs.base.sig_range().min(), lhs.base.sig_range().max(), lhs.base.exp_range().min(), lhs.base.exp_range().max());
            }
        }};
    }

    macro_rules! test_add {
        ($base:ident) => {{
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
            // If the math is confusing look at the example for Decimal:
            // `9_999_999_999_999_999_999 + 9_999_999_999_999_999_999` is
            // `19_999_999_999_999_999_998`, which normalizes to
            // `BigNum{sig: 1_999_999_999_999_999_999, exp: 1}`
            assert_eq_bignum!(
                BigNum::new(max_sig, 0) + BigNum::new(max_sig, 0),
                BigNum::new(2 * min_sig - 1, 1)
            );
        }};
    }

    macro_rules! test_sub {
        ($base:ident) => {{
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
    }

    macro_rules! test_mul {
        ($base:ident) => {{
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
                BigNum::new(min_sig, 1) * BigNum::new(max_sig, 1241234),
                BigNum::new(max_sig, min_exp as u64 + 1 + 1241234)
            );
        }};
    }

    macro_rules! test_div {
        ($base:ident) => {{
            type BigNum = BigNumBase<$base>;
            let SigRange(min_sig, max_sig) = $base::calculate_ranges().1;
            let ExpRange(min_exp, _) = $base::calculate_ranges().0;

            assert_eq_bignum!(
                BigNum::new(min_sig, 2) / BigNum::new(min_sig, 1),
                BigNum::new($base::NUMBER as u64, 0)
            );
            assert_eq_bignum!(
                BigNum::new_raw(max_sig, 1) / BigNum::new(max_sig, 1),
                BigNum::from(1)
            );
            assert_eq_bignum!(
                BigNum::new_raw(max_sig, 1) / BigNum::new(max_sig, 2),
                BigNum::from(0)
            );
            assert_eq_bignum!(
                BigNum::new_raw(min_sig, 1) / BigNum::new(min_sig + 1, 1),
                BigNum::from(0)
            );
            assert_eq_bignum!(
                BigNum::new_raw(max_sig, 5) / BigNum::new(max_sig, 1),
                BigNum::new($base::pow(4), 0)
            );
            // The total magnitude for the lhs is (min_exp + min_exp + 112341234 + 12341), and
            // the total magnitude for the rhs is (min_exp + 112341234), so we expect that the
            // resulting magnitude will be mag(lhs) - mag(rhs) = min_exp + 12341, as shown
            assert_eq_bignum!(
                BigNum::new_raw(max_sig, min_exp as u64 + 112341234 + 12341)
                    / BigNum::new(min_sig, 112341234),
                BigNum::new(max_sig, 12341)
            );
            assert_eq_bignum!(
                BigNum::new(min_sig, 12341234) / BigNum::new(min_sig, 1241234),
                BigNum::new(min_sig, 12341234 - 1241234 - min_exp as u64)
            );
            assert_eq_bignum!(
                BigNum::new(min_sig, 1) * BigNum::new(max_sig, 1241234),
                BigNum::new(max_sig, min_exp as u64 + 1 + 1241234)
            );
        }};
    }

    macro_rules! test_succ {
        ($base:ident) => {{
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
    }

    macro_rules! test_pred {
        ($base:ident) => {{
            use $crate::traits::Pred;
            type BigNum = BigNumBase<$base>;
            let SigRange(min_sig, max_sig) = $base::calculate_ranges().1;

            assert_eq_bignum!(BigNum::new(1, 0).pred(), BigNum::new(0, 0));
            assert_eq_bignum!(BigNum::new(min_sig, 1).pred(), BigNum::new(max_sig, 0));
            assert_eq_bignum!(BigNum::new(min_sig, 2).pred(), BigNum::new(max_sig, 1));
        }};
    }

    macro_rules! test_shl {
        ($base:ident) => {{
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
    }

    macro_rules! test_shr {
        ($base:ident) => {{
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
    }

    macro_rules! test_relations {
        ($base:ident) => {{
            use rand::{
                distributions::{Distribution, Uniform},
                thread_rng,
            };
            use $crate::traits::{Pred, Succ};

            type BigNum = BigNumBase<$base>;
            let base = $base::NUMBER as u64;

            let dist: Uniform<BigNum> = Uniform::new(BigNum::from(0), BigNum::new(1, u64::MAX / 2));
            let rng = &mut thread_rng();
            let nums = dist.sample_iter(rng).take(100);

            for n in nums {
                assert_eq_bignum!(n + n, n * 2);
                assert_close_bignum!(
                    (0u64..base).fold(BigNum::from(0), |acc, _| acc + n),
                    n * base
                );
                assert_eq_bignum!(base * n + base * n, 2 * base * n);
                assert_eq_bignum!(n / base / base, n / (base * base));
                assert_eq_bignum!(n * 0, n / (n.succ()));
                assert_eq_bignum!(n + 0, n / 1);
                assert_eq_bignum!(n / n, BigNum::from(1));
                assert_eq_bignum!(n << 3, n * base.pow(3));
                assert_eq_bignum!(n >> 3, n / base.pow(3));
                assert_eq_bignum!(n.succ().pred(), n);
                assert_eq_bignum!(n.pred().succ(), n);
            }
        }};
    }

    macro_rules! test_sum {
        ($base:ident) => {{
            type BigNum = BigNumBase<$base>;
            let base = $base::NUMBER as u64;

            let a: [BigNum; 10] = [BigNum::from(base); 10];
            let b: [BigNum; 10] = [BigNum::from(1); 10];

            assert_eq_bignum!(BigNum::from(base * 10), a.into_iter().sum::<BigNum>());
            assert_eq_bignum!(BigNum::from(10), b.into_iter().sum::<BigNum>());
        }};
    }

    macro_rules! test_prod {
        ($base:ident) => {{
            type BigNum = BigNumBase<$base>;
            let base = $base::NUMBER as u64;
            let min_sig = $base::calculate_ranges().1.min();
            let min_exp = $base::calculate_ranges().0.min();

            let a: [BigNum; 10] = [BigNum::from(base); 10];
            let b: [BigNum; 10] = [BigNum::from(1); 10];
            let c: [BigNum; 10] = [BigNum::from(min_sig); 10];

            assert_eq_bignum!(BigNum::new(1, 10), a.into_iter().product::<BigNum>());
            assert_eq_bignum!(BigNum::from(1), b.into_iter().product::<BigNum>());
            assert_eq_bignum!(
                BigNum::new(1, min_exp as u64 * 10),
                c.into_iter().product::<BigNum>()
            );
        }};
    }

    // Runs some non-base specific tests
    macro_rules! test_base {
        ($base:ident) => {{
            test_add!($base);
            test_sub!($base);
            test_mul!($base);
            test_div!($base);
            test_succ!($base);
            test_pred!($base);
            test_shl!($base);
            test_shr!($base);
            test_relations!($base);
            test_sum!($base);
            test_prod!($base);
        }};
    }

    macro_rules! create_and_test_base {
        ($base:ident, $num:literal) => {
            $crate::create_default_base!($base, $num);
            test_base!($base);
        };
    }

    #[test]
    fn test_many_bases() {
        use crate::{Base, BigNumBase, Decimal, ExpRange, Octal, SigRange};
        // Not doing Binary or Hex since these tests assume max_sig + 1 fits in u64
        create_and_test_base!(Base61, 61);
        create_and_test_base!(Base11142, 11142);
        create_and_test_base!(Base942, 942);
        create_and_test_base!(Base3292, 3292);
        create_and_test_base!(Base1234, 1234);
        create_and_test_base!(Base5678, 5678);
        create_and_test_base!(Base9101, 9101);
        create_and_test_base!(Base2345, 2345);
        create_and_test_base!(Base6789, 6789);
        create_and_test_base!(Base1112, 1112);
        create_and_test_base!(Base3456, 3456);
        create_and_test_base!(Base7890, 7890);
        create_and_test_base!(Base1357, 1357);
        create_and_test_base!(Base2468, 2468);
        create_and_test_base!(Base65535, 65535);
        create_and_test_base!(Ternary, 3);

        test_base!(Octal);
        test_base!(Decimal);
    }

    pub(crate) use assert_close_bignum;
    pub(crate) use assert_eq_bignum;
}

macro_rules! impl_for_types {
    ($($ty:ty),+) => {
        $(
            impl<T> From<$ty> for $crate::BigNumBase<T> where T: $crate::Base {
                fn from(value: $ty) -> Self {
                    Self::new(value as u64, 0)
                }
            }

            impl<T> std::ops::Add<$ty> for $crate::BigNumBase<T> where T: $crate::Base {
                type Output = Self;

                fn add(self, rhs: $ty) -> Self::Output {
                    self + $crate::BigNumBase::from(rhs)
                }
            }

            impl<T> std::ops::Add<$crate::BigNumBase<T>> for $ty where T: $crate::Base {
                type Output = $crate::BigNumBase<T>;

                fn add(self, rhs: $crate::BigNumBase<T>) -> Self::Output {
                    rhs + $crate::BigNumBase::from(self)
                }
            }

            impl<T> std::ops::AddAssign<$ty> for $crate::BigNumBase<T> where T: $crate::Base {
                fn add_assign(&mut self, rhs: $ty) {
                    *self = *self + $crate::BigNumBase::from(rhs);
                }
            }

            impl<T> std::ops::Sub<$ty> for $crate::BigNumBase<T> where T: $crate::Base {
                type Output = Self;

                fn sub(self, rhs: $ty) -> Self::Output {
                    self - $crate::BigNumBase::from(rhs)
                }
            }

            impl<T> std::ops::Sub<$crate::BigNumBase<T>> for $ty where T: $crate::Base {
                type Output = $crate::BigNumBase<T>;

                fn sub(self, rhs: $crate::BigNumBase<T>) -> Self::Output {
                    $crate::BigNumBase::from(self) - rhs
                }
            }

            impl<T> std::ops::SubAssign<$ty> for $crate::BigNumBase<T> where T: $crate::Base {
                fn sub_assign(&mut self, rhs: $ty) {
                    *self = *self - $crate::BigNumBase::from(rhs);
                }
            }

            impl<T> std::ops::Mul<$ty> for $crate::BigNumBase<T> where T: $crate::Base {
                type Output = Self;

                fn mul(self, rhs: $ty) -> Self::Output {
                    self * $crate::BigNumBase::from(rhs)
                }
            }

            impl<T> std::ops::Mul<$crate::BigNumBase<T>> for $ty where T: $crate::Base{
                type Output = $crate::BigNumBase<T>;

                fn mul(self, rhs: $crate::BigNumBase<T>) -> Self::Output {
                    $crate::BigNumBase::from(self) * rhs
                }
            }

            impl<T> std::ops::MulAssign<$ty> for $crate::BigNumBase<T> where T: $crate::Base {
                fn mul_assign(&mut self, rhs: $ty){
                    *self = *self * $crate::BigNumBase::from(rhs);
                }
            }

            impl<T> std::ops::Div<$ty> for $crate::BigNumBase<T> where T: $crate::Base {
                type Output = Self;

                fn div(self, rhs: $ty) -> Self::Output {
                    self / $crate::BigNumBase::from(rhs)
                }
            }

            impl<T> std::ops::Div<$crate::BigNumBase<T>> for $ty where T: $crate::Base{
                type Output = $crate::BigNumBase<T>;

                fn div(self, rhs: $crate::BigNumBase<T>) -> Self::Output {
                    $crate::BigNumBase::from(self) / rhs
                }
            }

            impl<T> std::ops::DivAssign<$ty> for $crate::BigNumBase<T> where T: $crate::Base {
                fn div_assign(&mut self, rhs: $ty){
                    *self = *self / $crate::BigNumBase::from(rhs);
                }
            }
        )+
    };
}
impl_for_types!(u64);

/// This macro creates a default `Base` implementation with a given name and number.
///
/// # Examples
/// ```
/// use bignumbe_rs::{create_default_base, BigNumBase};
///
/// create_default_base!(Base83, 83);
/// type BigNum = BigNumBase<Base83>;
///
/// let bn1 = BigNum::from(83);
///
/// assert_eq!(bn1 >> 1, BigNum::from(1));
/// ```
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

#[cfg(test)]
mod tests {
    use crate::{Base, BigNumBase};

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

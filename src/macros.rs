// Macro to generate impls for each int type
#[macro_export]
macro_rules! bignum_math_impl {
    ( $t:ty ) => {
        impl From<$t> for BigNumOld {
            fn from(n: $t) -> Self {
                Self::new(n as u64, 0)
            }
        }

        impl Add<$t> for BigNumOld {
            type Output = Self;

            fn add(self, rhs: $t) -> Self::Output {
                self + BigNumOld::from(rhs)
            }
        }

        impl Add<BigNumOld> for $t {
            type Output = BigNumOld;

            fn add(self, rhs: BigNumOld) -> Self::Output {
                BigNumOld::from(self) + rhs
            }
        }

        impl AddAssign<$t> for BigNumOld {
            fn add_assign(&mut self, rhs: $t) {
                *self = *self + BigNumOld::from(rhs)
            }
        }

        impl Sub<$t> for BigNumOld {
            type Output = Self;

            fn sub(self, rhs: $t) -> Self::Output {
                self - BigNumOld::from(rhs)
            }
        }

        impl Sub<BigNumOld> for $t {
            type Output = BigNumOld;

            fn sub(self, rhs: BigNumOld) -> Self::Output {
                BigNumOld::from(self) - rhs
            }
        }

        impl SubAssign<$t> for BigNumOld {
            fn sub_assign(&mut self, rhs: $t) {
                *self = *self - BigNumOld::from(rhs)
            }
        }

        impl Mul<$t> for BigNumOld {
            type Output = Self;

            fn mul(self, rhs: $t) -> Self::Output {
                self * BigNumOld::from(rhs)
            }
        }

        impl Mul<BigNumOld> for $t {
            type Output = BigNumOld;

            fn mul(self, rhs: BigNumOld) -> Self::Output {
                BigNumOld::from(self) * rhs
            }
        }

        impl MulAssign<$t> for BigNumOld {
            fn mul_assign(&mut self, rhs: $t) {
                *self = *self * BigNumOld::from(rhs)
            }
        }

        impl Div<$t> for BigNumOld {
            type Output = Self;

            fn div(self, rhs: $t) -> Self::Output {
                self / BigNumOld::from(rhs)
            }
        }

        impl Div<BigNumOld> for $t {
            type Output = BigNumOld;

            fn div(self, rhs: BigNumOld) -> Self::Output {
                BigNumOld::from(self) / rhs
            }
        }

        impl DivAssign<$t> for BigNumOld {
            fn div_assign(&mut self, rhs: $t) {
                *self = *self / BigNumOld::from(rhs)
            }
        }
    };
}

#[macro_export]
macro_rules! check_bases {
    ($lhs:ident, $rhs:ident, $mess:expr) => {
        if $lhs.base != $rhs.base {
            panic!("Attempt to {} BigNum values with different bases", $mess);
        }
    };
}

#[macro_export]
macro_rules! new_bignum_math_impl {
    ( $t:ty ) => {
        //impl Add<$t> for BigNumOld {
        //    type Output = Self;
        //
        //    fn add(self, rhs: $t) -> Self::Output {
        //        self + BigNumOld::from(rhs)
        //    }
        //}
        //
        //impl Add<BigNumOld> for $t {
        //    type Output = BigNumOld;
        //
        //    fn add(self, rhs: BigNumOld) -> Self::Output {
        //        BigNumOld::from(self) + rhs
        //    }
        //}
        //
        //impl AddAssign<$t> for BigNumOld {
        //    fn add_assign(&mut self, rhs: $t) {
        //        *self = *self + BigNumOld::from(rhs)
        //    }
        //}

        impl Sub<$t> for BigNumOld {
            type Output = Self;

            fn sub(self, rhs: $t) -> Self::Output {
                self - BigNumOld::from(rhs)
            }
        }

        impl Sub<BigNumOld> for $t {
            type Output = BigNumOld;

            fn sub(self, rhs: BigNumOld) -> Self::Output {
                BigNumOld::from(self) - rhs
            }
        }

        impl SubAssign<$t> for BigNumOld {
            fn sub_assign(&mut self, rhs: $t) {
                *self = *self - BigNumOld::from(rhs)
            }
        }

        impl Mul<$t> for BigNumOld {
            type Output = Self;

            fn mul(self, rhs: $t) -> Self::Output {
                self * BigNumOld::from(rhs)
            }
        }

        impl Mul<BigNumOld> for $t {
            type Output = BigNumOld;

            fn mul(self, rhs: BigNumOld) -> Self::Output {
                BigNumOld::from(self) * rhs
            }
        }

        impl MulAssign<$t> for BigNumOld {
            fn mul_assign(&mut self, rhs: $t) {
                *self = *self * BigNumOld::from(rhs)
            }
        }

        impl Div<$t> for BigNumOld {
            type Output = Self;

            fn div(self, rhs: $t) -> Self::Output {
                self / BigNumOld::from(rhs)
            }
        }

        impl Div<BigNumOld> for $t {
            type Output = BigNumOld;

            fn div(self, rhs: BigNumOld) -> Self::Output {
                BigNumOld::from(self) / rhs
            }
        }

        impl DivAssign<$t> for BigNumOld {
            fn div_assign(&mut self, rhs: $t) {
                *self = *self / BigNumOld::from(rhs)
            }
        }
    };
}

#[macro_export]
macro_rules! impl_for_type {
    (*, $base:literal) => {
        impl_for_type!([u8, u16, u32, u64, i8, i16, i32, i64], $base);
    };
    ([$($ty:ty),+], $base:literal) => {
        use std::ops::{Add, AddAssign};

        $(
            impl From<$ty> for BigNum {
                fn from(sig: $ty) -> Self {
                    #[allow(unused_comparisons)]
                    if sig < 0 {
                        panic!("Unable to convert negative integer to BigNum");
                    }

                    Self::new(sig as u64, 0, $base)
                }
            }

            impl Add<$ty> for BigNum {
                type Output = Self;

                fn add(self, rhs: $ty) -> Self::Output {
                    self + BigNum::from(rhs)
                }
            }

            impl Add<BigNum> for $ty {
                type Output = BigNum;

                fn add(self, rhs: BigNum) -> Self::Output {
                    BigNum::from(self) + rhs
                }
            }

            impl AddAssign<$ty> for BigNum {
                fn add_assign(&mut self, rhs: $ty) {
                    *self = *self + BigNum::from(rhs)
                }
            }
        )+
    };
    ($base:literal, [$($ty:ty),+]) => {
        impl_for_type!([$($ty)+], $base)
    };
}

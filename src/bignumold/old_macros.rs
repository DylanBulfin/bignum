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

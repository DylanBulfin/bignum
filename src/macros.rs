// Macro to generate impls for each int type
#[macro_export]
macro_rules! bignum_math_impl {
    ( $t:ty ) => {
        impl From<$t> for BigNum {
            fn from(n: $t) -> Self {
                Self::new(n as u64, 0)
            }
        }

        impl Add<$t> for BigNum {
            type Output = Self;

            fn add(self, rhs: $t) -> Self::Output {
                self + BigNum::from(rhs)
            }
        }

        impl Add<BigNum> for $t {
            type Output = BigNum;

            fn add(self, rhs: BigNum) -> Self::Output {
                BigNum::from(self) + rhs
            }
        }

        impl AddAssign<$t> for BigNum {
            fn add_assign(&mut self, rhs: $t) {
                *self = *self + BigNum::from(rhs)
            }
        }

        impl Sub<$t> for BigNum {
            type Output = Self;

            fn sub(self, rhs: $t) -> Self::Output {
                self - BigNum::from(rhs)
            }
        }

        impl Sub<BigNum> for $t {
            type Output = BigNum;

            fn sub(self, rhs: BigNum) -> Self::Output {
                BigNum::from(self) - rhs
            }
        }

        impl SubAssign<$t> for BigNum {
            fn sub_assign(&mut self, rhs: $t) {
                *self = *self - BigNum::from(rhs)
            }
        }

        impl Mul<$t> for BigNum {
            type Output = Self;

            fn mul(self, rhs: $t) -> Self::Output {
                self * BigNum::from(rhs)
            }
        }

        impl Mul<BigNum> for $t {
            type Output = BigNum;

            fn mul(self, rhs: BigNum) -> Self::Output {
                BigNum::from(self) * rhs
            }
        }

        impl MulAssign<$t> for BigNum {
            fn mul_assign(&mut self, rhs: $t) {
                *self = *self * BigNum::from(rhs)
            }
        }

        impl Div<$t> for BigNum {
            type Output = Self;

            fn div(self, rhs: $t) -> Self::Output {
                self / BigNum::from(rhs)
            }
        }

        impl Div<BigNum> for $t {
            type Output = BigNum;

            fn div(self, rhs: BigNum) -> Self::Output {
                BigNum::from(self) / rhs
            }
        }

        impl DivAssign<$t> for BigNum {
            fn div_assign(&mut self, rhs: $t) {
                *self = *self / BigNum::from(rhs)
            }
        }

    };
}

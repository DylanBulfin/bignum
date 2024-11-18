use crate::{Base, ExpRange, SigRange};


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

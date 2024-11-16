use crate::{utils, BigNumOld, MIN_BASE_VAL};

/// The exponent x such that self = c * 2^x for some c between 0 and 1
pub fn get_full_exp(bn: BigNumOld) -> u64 {
    if bn.exp == 0 {
        utils::get_exp_u64(bn.base)
    } else {
        // Panics when bn.exp + 63 > u64::MAX
        bn.exp + 63
    }
}

/// Initial try implementing addition
pub fn add_old(lhs: BigNumOld, rhs: BigNumOld) -> BigNumOld {
    if lhs.exp == 0 && rhs.exp == 0 {
        // Both numbers are in compact form, first try normal addition
        let result = lhs.base.wrapping_add(rhs.base);

        // If remainder is less than either base, overflow occurred
        if result < lhs.base || result < rhs.base {
            BigNumOld::new(MIN_BASE_VAL + (result >> 1), 1)
        } else {
            BigNumOld::new(result, 0)
        }
    } else {
        // At least one of the numbers is in expanded form, first find which is bigger
        let (min, max) = if lhs > rhs { (rhs, lhs) } else { (lhs, rhs) };

        // Calculate how much we need to shift the smaller number to align
        let shift = if min.exp == 0 {
            max.exp
        } else {
            get_full_exp(max) - get_full_exp(min)
        };

        if shift >= 64 || shift > get_full_exp(min) {
            // Shifting will leave us with 0 so don't bother, return max
            max
        } else {
            // Now we can add them, and handle any overflow
            let res = max.base.wrapping_add(min.base >> shift);

            // If result is less than either base, overflow occurred
            if res < max.base {
                // Wrapping occurred, need to fix things up
                BigNumOld::new(MIN_BASE_VAL + (res >> 1), max.exp + 1)
            } else {
                BigNumOld::new(res, max.exp)
            }
        }
    }
}

/// Attempt to implement addition using u128 strategy
pub fn add_u128(lhs: BigNumOld, rhs: BigNumOld) -> BigNumOld {
    let (max, min) = if lhs > rhs { (lhs, rhs) } else { (rhs, lhs) };
    let shift = max.exp - min.exp;

    if shift >= 64 {
        // minimum number is too small to make a difference in the sum
        return max;
    }

    let result: u128 = (max.base as u128) + ((min.base >> shift) as u128);

    if utils::get_exp_u128(result) >= 64 {
        if max.exp == u64::MAX {
            panic!("Attempt to add BigNum with overflow");
        }
        BigNumOld::new((result >> 1) as u64, 1 + max.exp)
    } else {
        //(result as u64).into()
        BigNumOld::new((result) as u64, max.exp)
    }
}

pub fn sub_old(lhs: BigNumOld, rhs: BigNumOld) -> BigNumOld {
    if rhs > lhs {
        // We can't have negative numbers
        panic!("Attempt to subtract with overflow")
    }

    if rhs.exp == 0 && lhs.exp == 0 {
        // Both are in compact form, and since lhs > rhs we know we won't underflow
        BigNumOld::new(lhs.base - rhs.base, 0)
    } else {
        // Find how much we need to shift the smaller number to align
        let shift = if rhs.exp == 0 {
            lhs.exp
        } else {
            get_full_exp(lhs) - get_full_exp(rhs)
        };

        if shift >= 64 || shift > get_full_exp(rhs) {
            if lhs.base == MIN_BASE_VAL && (shift == 64 || shift == get_full_exp(rhs) + 1) {
                // Base is at the minimum value so we need to handle edge case
                // E.g. BigNum::new(0x8000_0000_0000_0000, 1) - BigNum::from(1)
                // shift = 1, get_full_exp = 0, so normally we would skip
                // But since base is at min value we need to decrease exp and normalize
                BigNumOld::new(u64::MAX, lhs.exp - 1)
            } else {
                // Shifting will leave us with 0 so don't bother, return lhs
                lhs
            }
        } else {
            let res = lhs.base - (rhs.base >> shift);

            // We know that underflow won't happen, but if numbers are equal
            // we need to handle 0 case
            if res == 0 {
                BigNumOld::new(0, 0)
            } else {
                // If new resulting base is not in range we need to fix
                let adjustment = 63 - utils::get_exp_u64(res);

                BigNumOld::new(res << adjustment, lhs.exp - adjustment)
            }
        }
    }
}

fn get_pow_sum_u16(n: u16) -> Vec<u16> {
    (0..16).filter(|i| n & 1 << i != 0).collect()
}

/// Attempt to do multiplication manually
pub fn multi_manual(lhs: BigNumOld, rhs: u16) -> BigNumOld {
    let pows = get_pow_sum_u16(rhs);
    if lhs.exp > 0 {
        // Number already in expanded form, easier
        pows.into_iter()
            .map(|p| BigNumOld::new(lhs.base, lhs.exp + p as u64))
            .sum()
    } else {
        let max_pow = utils::get_exp_u64(lhs.base);
        pows.into_iter()
            .map(|p| {
                if p as u64 + max_pow > 63 {
                    let new_exp = p as u64 + max_pow - 63;
                    let new_base = lhs.base << (new_exp - 1);
                    BigNumOld::new(new_base, new_exp)
                } else {
                    BigNumOld::new(lhs.base << p, 0)
                }
            })
            .sum()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const A1: BigNumOld = BigNumOld::ZERO;

    const B1: BigNumOld = BigNumOld::ONE;
    const B2: BigNumOld = BigNumOld {
        base: 0x8000_0000_0000_0000,
        exp: 0,
        invalidate: false,
    };
    const B3: BigNumOld = BigNumOld {
        base: 0xFFFF_FFFF_FFFF_FFFF,
        exp: 0,
        invalidate: false,
    };

    const C1: BigNumOld = BigNumOld {
        base: 0x8000_0000_0000_0000,
        exp: 10,
        invalidate: false,
    };
    const C2: BigNumOld = BigNumOld {
        base: 0x8000_0000_0000_0000,
        exp: 73,
        invalidate: false,
    };
    const C3: BigNumOld = BigNumOld {
        base: 0xFFFF_FFFF_FFFF_FFFF,
        exp: 120,
        invalidate: false,
    };
    const C4: BigNumOld = BigNumOld {
        base: 0xFFFF_FFFF_FFFF_FFFF,
        exp: 127000,
        invalidate: false,
    };
    const C5: BigNumOld = BigNumOld {
        base: u64::MAX,
        exp: u64::MAX,
        invalidate: false,
    };

    fn test_add(f: fn(BigNumOld, BigNumOld) -> BigNumOld) {
        // A
        assert_eq!(f(A1, A1), BigNumOld::ZERO);

        // A + B -> B
        assert_eq!(f(A1, B1), BigNumOld::ONE);

        // B
        assert_eq!(f(B1, B2), BigNumOld::new(0x8000_0000_0000_0001, 0));

        // B + B -> C
        assert_eq!(f(B1, B3), BigNumOld::new(0x8000_0000_0000_0000, 1));
        assert_eq!(f(B2, B3), BigNumOld::new(0xBFFF_FFFF_FFFF_FFFF, 1));

        // C
        assert_eq!(f(C1, C2), BigNumOld::new(0x8000_0000_0000_0001, 73));
        assert_eq!(f(C2, C3), BigNumOld::new(0x8000_0000_0000_7FFF, 121));
        assert_eq!(f(C3, C4), C4); // Too small to make a difference
        assert_eq!(f(C1, C3), C3);
    }

    #[test]
    fn test_add_old() {
        test_add(add_old);
    }

    #[test]
    fn test_add_u128() {
        test_add(add_u128)
    }

    #[test]
    fn test_multi_manual() {}
}

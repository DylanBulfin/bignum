use std::ops::Add;

/// Here I try to implement `u128` in software on my own. I was impressed by how fast their
/// implementation of multiplication was so I'd like to see how close I can get.

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct MyU128(u64, u64);

impl PartialOrd for MyU128 {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for MyU128 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.0 < other.0 {
            std::cmp::Ordering::Less
        } else if self.0 > other.0 {
            std::cmp::Ordering::Greater
        } else if self.1 < other.1 {
            std::cmp::Ordering::Less
        } else if self.1 > other.1 {
            std::cmp::Ordering::Greater
        } else {
            std::cmp::Ordering::Equal
        }
    }
}

// Add ideas:
// ----------
// Idea 1:
// For each half:
// - Shift both sides right by 1
// - Add
// - Save overflow status somewhere
// - Shift left one
//
// Idea 2:
// For each half:
// - wrapping_add()
// - Overflow occurred iff result less than both operands
//
//
// Idea 1 requires 6 total bit shifts, 2 64 bit adds, plus one 64 bit add for overflow and one for
// the last bit. Pretty sure it's awful now that I start but should be instructive
//
// Idea 2 requires 2 comparisons/potential jumps and one 64 bit add for bottom overflow
impl Add for MyU128 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        self.add1(rhs)
    }
}

impl MyU128 {
    // Idea 1
    pub fn add1(self, rhs: Self) -> Self {
        let bot_last_bit = self.1 & rhs.1 & 0x1;
        let bottom = (self.1 >> 1) + (rhs.1 >> 1);

       let carry = bottom & 0x8000_0000_0000_0000;

        let top_last_bit = self.0 & rhs.0 & 0x1;
        let top = (self.0 >> 1) + (rhs.0 >> 1);

        if top & 0x8000_0000_0000_0000 != 0 {
            panic!("Attempt to add MyU128 with overflow");
        }

        // Could cause hidden overflow maybe? But don't think I'm going forward to this so w/e
        Self(
            (bottom << 1) + bot_last_bit,
            (top << 1) + top_last_bit + carry,
        )
    }

    // Idea 2
    pub fn add2(self, rhs: Self) -> Self {
        let top = self.0.wrapping_add(rhs.0);
        let bot = self.1.wrapping_add(rhs.1);

        if top < self.0 && top < rhs.0 {
            panic!("Attempt to add MyU128 with overflow")
        }
        let carry = if bot < self.1 && bot < rhs.1 { 1 } else { 0 };

        match top.checked_add(carry) {
            Some(norm_top) => Self(norm_top, bot),
            None => panic!("Attempt to add MyU128 with overflow"),
        }
    }

    pub fn from_u128(n: u128) -> Self {
        Self((n >> 64) as u64, n as u64)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_u128() {
        // Testing this specifically since it allows me to generate uniformly distributed MyU128
        // values for testing and benchmarking purposes
        let a = MyU128::from_u128(1);
        let b = MyU128::from_u128(0x8000_0000_0000_0000);
        let c = MyU128::from_u128(0x1_8000_0000_0000_0000);
        let d = MyU128::from_u128(0x8000_0000_0000_0000_0000_0000_0000_0000);
        let e = MyU128::from_u128(u64::MAX as u128);
        let f = MyU128::from_u128(u128::MAX);

        assert_eq!(a, MyU128(0, 1));
        assert_eq!(b, MyU128(0, 0x8000_0000_0000_0000));
        assert_eq!(c, MyU128(0x1, 0x8000_0000_0000_0000));
        assert_eq!(d, MyU128(0x8000_0000_0000_0000, 0));
        assert_eq!(e, MyU128(0, u64::MAX));
        assert_eq!(f, MyU128(u64::MAX, u64::MAX));
    }
}

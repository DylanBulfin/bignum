// Below are the tables/ranges for the special types. These are not part of the caching
// system but putting them here made the most sense.

pub const BIN_EXP_RANGE: (u32, u32) = (63, 64);
pub const OCT_EXP_RANGE: (u32, u32) = (20, 21);
pub const DEC_EXP_RANGE: (u32, u32) = (18, 19);
pub const HEX_EXP_RANGE: (u32, u32) = (15, 16);

/// Because of the edge cases noted above, this range *is* inclusive.
pub const BIN_SIG_RANGE: (u64, u64) = (1 << BIN_EXP_RANGE.0, u64::MAX);
pub const OCT_SIG_RANGE: (u64, u64) = (8u64.pow(OCT_EXP_RANGE.0), 8u64.pow(OCT_EXP_RANGE.1) - 1);
pub const DEC_SIG_RANGE: (u64, u64) = (10u64.pow(DEC_EXP_RANGE.0), 10u64.pow(DEC_EXP_RANGE.1) - 1);
pub const HEX_SIG_RANGE: (u64, u64) = (16u64.pow(HEX_EXP_RANGE.0), u64::MAX);

pub const OCT_POWERS: [u64; 22] = [
    0o1,
    0o10,
    0o100,
    0o1000,
    0o10000,
    0o100000,
    0o1000000,
    0o10000000,
    0o100000000,
    0o1000000000,
    0o10000000000,
    0o100000000000,
    0o1000000000000,
    0o10000000000000,
    0o100000000000000,
    0o1000000000000000,
    0o10000000000000000,
    0o100000000000000000,
    0o1000000000000000000,
    0o10000000000000000000,
    0o100000000000000000000,
    0o1000000000000000000000,
];

pub const DEC_POWERS: [u64; 20] = [
    1,
    10,
    100,
    1000,
    10000,
    100000,
    1000000,
    10000000,
    100000000,
    1000000000,
    10000000000,
    100000000000,
    1000000000000,
    10000000000000,
    100000000000000,
    1000000000000000,
    10000000000000000,
    100000000000000000,
    1000000000000000000,
    10000000000000000000,
];

pub const HEX_POWERS: [u64; 16] = [
    0x1,
    0x10,
    0x100,
    0x1000,
    0x10000,
    0x100000,
    0x1000000,
    0x10000000,
    0x100000000,
    0x1000000000,
    0x10000000000,
    0x100000000000,
    0x1000000000000,
    0x10000000000000,
    0x100000000000000,
    0x1000000000000000,
];

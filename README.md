# BigNum custom library
This is mostly just a notes pags for now

## Preface
With two u64 values representing a base and an exponent we can implement a more flexible representation that can handle small and large numbers (integers)

I want it to have high precision with low-ish numbers and medium precision with high numbers, and reasonably performant on both

## Idea
Basically an extension of floating point numbers with relaxed rules to allow for much higher numbers.

If `a` is a `BigNum` value with base `b` and exponent `x`, we have `a = b * 2^x`. 

We have two different possible representation styles based on the magnitude of a number. 
For `n <= u64::MAX`, we have `base = n` and `exp = 0`

For `n > u64::MAX` it is a little more complicated:
We want as much information in the base as possible so we have to restrict it to the range `[2^63, 2^64)` (e.g. the top bit is `1`)




### Limitations
Since we only store 64 bits of actual information higher numbers are inherently imprecise
Edge cases:
- Subtract 1 from large number in form 2^x where x > 64
    - Consider 2^1000 - 1:
    - 2^1000, base = MIN_BASE_VAL = 2^63, exp = 1000 - 63 = 937
    - a = BigNum::new(2^63, 937)
    - b = BigNum::from(1)
    - Should result be
        - BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 936)
        - a
    - I have gone with the strategy of ignoring such small subtractions
    - I do, however, consider the specific edge case where the real exp difference is exactly 64, and base = 2^63
        - This handles the specific edge case 2^64 - 1 or similar
            - This result is fully representable as u64::MAX (0xFFFF_FFFF_FFFF_FFFF)
            - So we handle it specially



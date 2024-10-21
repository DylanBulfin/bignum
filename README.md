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

## Why custom
- Making an idle game, need something that can handle absurdly large numbers with decent precision, but is also reasonably performant
- Alternatives considered:
    - `f128` has a higher range but most extra space goes to the significand so the actual range doesn't change that much
        - Also implemented in software so loses a lot of benefits of `f64`
    - `BigInt` from `num_bigint` doesn't implement `Copy` which makes things slightly more tedious
        - Also seems like generally overkill for my use case
        - Will still use it if my library ends up being less performant 
        
## Multiplication/Division
- It's probably not useful to multiply the big numbers together (?)
    - They will mostly be used in an idle game, e.g.
        - Need to be multiplied by smaller value (unit count) to find total profits
        - Need to be multiplied by ratio when a unit is bought, since price rises by ratio
        - Neither case needs to multiply large numbers by each other
    - Would be inefficient and I would need to handle edge cases
- Division is really difficult and arbitrary division is probably not useful either
    - Division by powers of 2 is very easy
- Alternative: Ratios
    - Special type Ratio with u8/u16 numerator/denominator
    - Limit denominator to powers of 2 (turns division into bitshift)
        - Up to 2^15 = 32768
    - Limit numerator to 
        - Up to u16::MAX = 65535
    - Gives effective range from 1/32768 up to 65535/1
    - Multiplication strat:
        - Represent n as sum of powers of 2: `n = 2^a + 2^b + ... + 2^z`
        - Multiplication by power of 2 = bit shift
        - Max of 16 powers of 2,
        - E.g. needs up to 16 bit shift + sum operations, much less than arbitrary multiplication
- TODO: consider using above strategy to enable arbitrary (if inefficient) multiplication
- Other thoughts:
    - For multiplication we could use u128. E.g. cast a and b to u128, multiply, parse back to BigNum format
        - This is simple but possibly inefficient?
        - TODO: Test against the strat described above


### Limitations
Since we only store 64 bits of actual information higher numbers are inherently imprecise
Edge cases:
- Subtract 1 from large number in form 2^x where x > 64
    - Consider `2^1000 - 1`:
    - `2^1000, base = MIN_BASE_VAL = 2^63, exp = 1000 - 63 = 937`
    - `a = BigNum::new(2^63, 937)`
    - `b = BigNum::from(1)`
    - Should result be
        - `BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 936)`
        - `a`
    - I have gone with the strategy of ignoring such small subtractions
    - I do, however, consider the specific edge case where the real exp difference is exactly 64, and base = 2^63
        - This handles the specific edge case `2^64 - 1` or similar
            - This result is fully representable as `u64::MAX (0xFFFF_FFFF_FFFF_FFFF)`
            - So we handle it specially



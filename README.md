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
After some benchmarking it seems that instead of a custom multiplication solution, the easiest thing to do is lift everything into `u128`s, multiply, and then normalize.
- Same probably works with division

For division use the following steps:
- lift lhs to the top half of u128 by `(lhs as u128) << 64`
- lift rhs to bottom half of u128 by `rhs as u128`
- Normalize the result 

## Testing Notes
- Want to be reasonably sure everything works so I don't have to touch this ever again
- Unit tests should cover all important edge cases
- Let's define 3 'regions' of the `BigNum` space
    - Region A: `n = 0`
    - Region B: `1 <= n < 2^64`
    - Region C: `n >= 2^64`
- Biggest boundaries in input space:
    - A -> B
    - B -> C
    - C -> overflow
    - underflow -> A
- Each unit test should test the functionality on each boundary, and within each region
- Example test cases for Add, start letter is each number's region
    - `a1 + b1 = b2` `A -> B`
    - `b1 + b2 = c1` `B -> C`
    - `b1 + c1 = c2` `B -> C`
    - `c1 + c2 = overflow`
- Subtract is the same but with reversed boundaries and underflow instead of overflow
- With similarities like this it's nice to abstract away some of the logic

## Efficiency Notes
- Tested get_exp_u64 and it seems to account for only ~5% of runtime of addition and less for multiplication, so not refactoring for now
- For addition tested the following ways:
    - Initial implementation
        - Fastest
        - Spaghetti
    - Lift to `u128` like multiplication
        - Slowest
        - A bit janky
    - New clean version of initial implementation
        - Slightly slower than original for some reason? 
        - Best looking
    - Using the third option for now

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
- As of now panics when you do anything with a number with `exp > (u64::MAX - 63)`
    - It does this because it needs to get the "full exponent" e.g. `63 + exp` since the significand holds up to `2^63`



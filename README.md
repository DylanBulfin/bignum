# Custom Big Numbers in Rust
## Inspiration
Recently I was laying out the basics for an idle game that was starting to involve math with increasingly large numbers. 
It made me realize how limiting `u64` and `f64`, or even their plus-sized counterparts `u128` and `f128`, can be. `u64` 
has a range of `[0, 2^64)` and `f64` can represent numbers between `~(-1.8e308, 1.8e308)`. While a well-designed idle 
game can certainly work around that and many deliberately do (Leaf Blower Revolution's cap of resource count to `1e300` 
comes to mind), I thought it was a bit limiting and also added in a lot of verification I would have to do to ensure I
never hit that limit. 

## Alternatives Considered
- `f128`/`u128` are available as mentioned above, but
    - They're implemented in software, very efficiently certainly, but still they miss out on some of the performance
    benefits of using primitive types
    - As mentioned above the range isn't *that* much bigger. For `f128` a lot of extra bits go to significand so don't 
    affect the max/min value. For `u128` it raises the range to `[0, 2^128)` but when working with exponential functions
    this is not that much larger
- `num_bigint` is another project with superficially similar goals, but
    - `BigUInt` isn't `Copy` which makes things slightly more annoying
    - `BigUInt`, if I'm reading the code right, has arbitrary precision
        - My implementation would use the same value to represent the below numbers:
            - `0xFFFF_FFFF_FFFF_FFFF_FFFF`
            - `0xFFFF_FFFF_FFFF_FFFF_0000`
            - They would both be stored as `BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 16)`
        - `BigUInt` stores this internally as `Vec::from[0x0000_0000_0000_FFFF, 0xFFFF_FFFF_FFFF_FFFF]` for a 64-bit 
        system, keeping all bits of information around
            - Basically they do base-`2^64` arithmetic, using a `u64` as a digit, much more elegant than my solution
    - This level of precision is admirable and it's very important in a lot of contexts, but TUI(?) idle games are 
    probably not one of them
    - A bunch of other libraries like `num_bigint`, for example, `bnum` exist and I would probably get good results by 
    just using them, but I really wanted something with low overhead and you can't get much lower overhead (memory wise) 
    than 2 `u64` values

## Approach
The general way the library stores a large number is by storing a 64-bit `base` and 64-bit `exp`, where 
`BigNum::new(base, exp) = base * 2^exp`. This means for any number, no matter the magnitude, we store 64 significant 
bits. This is fairly similar to the way floating point numbers are stored but with far more bits for the exponent since 
arbitrarily large numbers are an explicit goal of this project. It also stores more significant bits than the 
traditional 32-bit and 64-bit floating point specifications (sometimes called `float` and `double`, e.g. 
double-precision). Basically it uses far less memory, and is hopefully far more performant, for large numbers than an 
arbitrary-precision library, but is still precise enough for the majority of basic applications (though you should 
definitely use `f64` or `u64` if their limit is not an issue).

### Notes/Explanations
- In this section I make further use of the constructor syntax to represent a `BigNum`. This syntax is: 
`BigNum::new(base, exp)`. 
- `u64::MAX = 0xFFFF_FFFF_FFFF_FFFF` and `2^63 = 0x8000_0000_0000_0000` 
- Magnitude is the highest exponent `x` such that `n >= 2^x`
- All math operations are defined on all signed and unsigned integer types up to 64 bits. So I often leave out the 
`BigNum` syntax for one of the operands for brevity

### Addition
- Addition is fairly simple with the representation I've chosen, you can check the code if interested.
- The basic idea is to first align the bases based on the difference between their `exp` values (or length if they are 
in the `u64` range). Then you add the normalized values, check the overflow, and adjust the `exp` of the output as
needed.
- One important note about the behavior is that if the difference in magnitudes  is too large the addition will be 
ignored (think `BigNum::new(u64:MAX, 10) + 1`). This is due to the number of significant bits we're limited to.

### Subtraction
- Subtraction is similar except instead of overflow you need to be worried about the base becoming unnormalized, e.g. 
`BigNum::new(u64::MAX, 100) - BigNum::new(2^63, 100)`. You would get `0x0FFF_FFFF_FFFF_FFFF` and then need to fix that
and get `BigNum::new(u64::MAX - 1, 99)`.
- The same is true about large differences in magnitude resulting in a no-op

### Multiplication
- At first I custom-wrote a basic multiplication algorithm to compute `BigNum * u16`. It wrote the `u16` as a sum of 
powers of 2, multiplied each by the `BigNum`, and added them all together.
    - This was supposed to be efficient since multiplying `n * 2^x` is the same as `n << x` and adding is pretty fast
- After that I had the thought that the max size of a multiplication result (bit-wise) is limited to around (maybe 
exactly?) twice the input's length, e.g. the max result of `u8 * u8` is `0xff * 0xff = 0xfe01`, which fits perfectly in
a `u16`
- So, now what I do is move both numbers' bases into `u128s` and multiply them. Then depending on the magnitude of the
result I adjust the `exp` of the output and normalize.
- This resulted in a massive speedup according to my basic benchmarking.
    - I actually tried to do this for addition and it caused a dramatic slowdown. This code, along with that for the old
    multplication method, is in the `old_methods` module

### Division
- The strategy above lends itself to division as well with modifications.
- If you just directly cast them to `u128` and divide that will destroy a lot of information.
    - With this you would get `BigNum::new(0xFFFF_FFFF_FFFF_FFFF, 0) / 0x8000_0000_0000_0000 = 1` which is not ideal.
- Instead, you first move the larger number to the top half, e.g. `(num as u128) << 64`, then divide
- You then just have to normalize and adjust the `exp` of the output as normal

### Bonus Stuff
- `MinMax` trait which has `maximum`, `minimum`, `maximum_by`, `maximum_by_checked` and the other variations
    - Is defined for all `I: Iterator` where `I::Item: Ord`, for custom types you must implement at least 
    `maximum_by_checked` and `minimum_by_checked`
    - Like most methods designed for iterators these take ownership of `self`. So don't implement it for long-lived data
    types, e.g. directly for `Vec<T>`
        - You can't actually do this anyway, 


# EVERYTHING BELOW IS OLD, LEAVING FOR REFERENCE FOR NOW

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



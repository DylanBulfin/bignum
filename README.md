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

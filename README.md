# BigNum
A library that allows the creation of incredibly large numbers, but with a low
memory/runtime footprint (relative to arbitrary-precision libraries). It accomplishes this
by only storing up to 64 bits in the significand (similar to the floating point
standard).

## Links
[docs.rs page](https://docs.rs/bignumbe-rs/latest/bignumbe_rs/)

[crates.io page](https://crates.io/crates/bignumbe-rs)

## Inspiration/Why Did I Make This?
The inspiration for this library was looking into idle/incremental games. These games
almost always have some sort of exponential growth function and as a result often have to
limit the number of items/buildings you can own to avoid overflowing the `f64` they store
the value in. I want to be able to ignore this limit, hence this library. It's perfect for
situations where you need to store an extremely large number but without the overhead of
an arbitrary-precision library.

## Goals
My ultimate goal is to make this as close to a `u64` stand-in as possible. The user should
be able to create them at the beginning of the program (or whenever they're needed), and
then use them as if they were a standard `u64` price/score/etc. value. 

As an addendum to the above I want it to be lightweight and performant. Users should be
able to modify, create, and delete them at will without notable performance hits (to the
extent that's possible). If they need to employ special management techniques (passing by
reference, etc) they're hardly better than a heavier type.

## Installation
Add a dependecy to `bignumbe-rs` to `Cargo.toml` either directly or via `cargo add`. 

Optionally, enabling the feature `random` adds a dependency on `rand` and some support for
`Uniform` random generation of BigNums. The algorithm is very imperfect and was mainly
meant for testing purposes, so it's not recommended to use it.

## Usage
Bases 2, 8, 10, and 16 are all pre-defined, and aliased to
`BigNumBin, BigNumOct, BigNumDec, BigNumHex`. As an example, to create a binary `BigNum`
to represent the formula `1234123223468 * 2^123422235`, do
`BigNumBin::new(1234123223468, 123422235)`. Then you can freely apply any of the 4 standard
math operations between this value and a `u64` or another `BigNumBin`. For more examples
check the page on `docs.rs` and the test code.

## Debugging
Since the math is a little odd some of the behaviors may not be obvious. Below are some of
the more odd aspects that may prove useful to know when debugging:
- When the base you're using is not a power of a power of 2 (such as Binary or
Hexadecimal), wrapping of the significand will not occur at the bounds of `u64`. So,
`BigNumBase<Decimal>::new(u64::MAX, 0).exp != 0`. The reason for this is explained in the
math section below. Basically, avoid relying on the specific way a value is represented.
- All math operations have the potential to result in loss of data. That is, `(a * b) / 2` 
does not necessarily evaluate the same as `a/2 + b/2`. You should not rely on the exact
equality of chained operations like this. If you must compare them do it fuzzily, checking
whether the difference between the two is within a certain threshold.
- An addendum to the above: differences between large `BigNum` values are imprecise.
    - E.g. `BigNumDec::new(10.pow(18) + 1, 100) - BigNumDec::new(10.pow(18), 100) = 
    BigNum::new(1, 100)`. So if you want to check closeness see the section below.


### Drifting
When applying sequences of functions that should result in the same value, there is some
inherent loss of precision in this design. If I'm correct, though, the significand should
drift from the "correct" answer by at most 1 on any given operation. So we define a
special function `eq_fuzzy(self, other: BigNum, margin: u64) -> bool` which checks if 
the  difference between the significands of the input numbers is greater than `margin`.
To get an estimate for the margin, count the max number of operations applied to its
arguments (see docs for this function for more details).

## The Math
The main restriction we make in order to enable efficient arithmetic of any base is that,
for any number where `exp != 0`, the significand is restricted to a single order of 
magnitude. That is for a base `b`, unless `b` is a power of a power of 2 
`(2, 4, 16, 256, ...)`, we find the highest power `x` of `b` such that `b^x <= u64::MAX`.
We then restrict the significand to `[b^(x - 1), b^x - 1]`. For example:
- Decimal: `[1_000_000_000_000_000_000, 9_999_999_999_999_999_999]`
- Octal: `[0o1_0000_0000_0000_0000_0000, 0o7_7777_7777_7777_7777_7777]`
If the significand goes above this range we divide by `b` and add one to the `exp` field.
Similarly, if the significand goes below this range we similarly multiply by `b` and subtract one
from the `exp` field

If at any point the number is less than the smallest value of the range calculated above,
we treat it as 'compact'. We just store the value as-is with an `exp` of 0. In the
examples below assume `type BigNum = BigNumBase<Decimal>`
- E.g. `BigNum::new(9_999_999_999_999_999_999, 0) + 1 =
BigNum::new(1_000_000_000_000_000_000, 1)`
- Also `BigNum::new(1_000_000_000_000_000_000, 1) - 1 =
BigNum::new(9_999_999_999_999_999_999, 0)`
- And `BigNum::new(1_000_000_000_000_000_000, 0) - 1 =
BigNum::new(999_999_999_999_999_999, 0)` 

## Printing
For now only decimal BigNum values have a default print method defined, and it works as 
follows:
- For numbers less than 1000, the number is printed as-is
- For numbers between 1000 and 1 quadrillion they are printed as a float multiple of the 
corresponding suffix. (`k` for thousands, followed by `m`, `b`, `t` for million, billion,
trillion respectively). Up to 5 significant figures will be shown
    - As an example, 999_999_999 is represented as `999.9m`, and 1_000_000 will be
    represented as `1m`
- For numbers 1 quadrillion and greater they are printed in standard (normalized) 
scientific notation
    - E.g. `999_999_999_999_999_999 = 9.999e17`

## Performance
Here are a couple of results from benchmarking the current version. TL;DR it's probably
fast enough for anything but performance-critical applications. 

Each test involved running 10k of the listed operations, code is in `benches/multi.rs`
| Test        | Time(us) |
|-------------|----------|
| Binary Add  | 21.540   |
| Binary Sub  | 17.002   |
| Binary Mul  | 35.191   |
| Binary Div  | 43.534   |
| Base300 Add | 26.034   |
| Base300 Sub | 68.320   |
| Base300 Mul | 107.33   |
| Base300 Div | 180.44   |
The results are not perfectly descriptive but I think give a general sense of efficiency.

## Features

### Random
I've added an implementation to generate random-ish BigNum values for testing. It is not
actually correct (values won't appear at the frequency you'd expect and the bounds are
not expected). But it will do for peformance testing and whatnot.

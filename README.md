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
then use them as if it was a standard `u64` price/score/etc. value. 

As an addendum to the above I want it to be lightweight and performant. Users should be
able to modify, create, and delete them at will without notable performance hits (to the
extent that's possible).

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

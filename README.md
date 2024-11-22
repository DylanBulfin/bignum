# BigNum
A library that allows the creation of incredibly large numbers, but with a low
memory/runtime footprint (relative to arbitrary-precision libraries). It accomplishes this
by only storing up to 64 bits in the significand (similar to the floating point standard).

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

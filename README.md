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

### String Representation
The interesting part about this is converting to base-10 notation in an efficient way.
Waaaay more difficult than I thought it would be 

The main thing to worry about is that any calculation we do is likely to have some imprecision, and we want to avoid a
situation where the exponent of the output jumps due to this. 

```
// We want to solve for 1 <= c: f64 < 10, d: u64 in
a * 2^b = c * 10^d

log10(a * 2^b) = log10(c * 10^d)
=> log10(a) + b * log10(2) = log10(c) + d
=> d = log10(a) + b * log10(2) - log10(c)
=> d = log10(a / c) + b * log10(2)
=> d ~ log10(a / c) + b * 1233 / 4096
=> d ~ log10(a) - log10(c) + b * 1233 / 4096
```

The approximation of `log10(2) ~ 1233 / 4096` is from [this article on efficient bitwise math on integers]
(https://graphics.stanford.edu/%7Eseander/bithacks.html#IntegerLog). 

With this we need to recognize that `d` is an integer. We can compute the value of `log10(a)` and `b * 1233 / 4096` very
quickly. And since we know that `1 <= c < 10`, `0 <= log10(c) < 1`. So we need to find the `log10(c)` such that `d`
is an integer, and use this to find `c`. 

We must be careful, though, because the approximation I used means that there might be some error in `d`. The error of 
this estimation is `(log10(2) - 1233 / 4096) / log10(2) ~ 0.0000153`, e.g. the estimate can have around `0.0015%` error.
At a certain magnitude this probably doesn't matter; for `1.1 * 10^(10^10)` being off by this amount is meaningless


#### Improvements
- Maybe I should compute the above when creating a new `BigNum` and store it in the struct?
    - On the other hand, since many/most `BigNum` values will be intermediaries that aren't ever printed, this may 
    add more overhead than it's worth. 
    - Could do it conditionally? Have a `cache_decimal` field that is set when the user wants to create a 
    longer-lived value? E.g. the cost of the next unit in a game, which will be written to the screen 60 times per
    second and likely only updated every few seconds at the very most
    - On the other other hand this algorithm may calculate the value many times faster than the value can be printed, 
    so this would be a waste

### Bonus Stuff

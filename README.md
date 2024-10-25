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
```
a * 2^b = c * 10^d
=> 10^d = a/c * 2^b
=> d = log10(10^d) = log10(a/c * 2^b) = log2(a/c * 2^b) / log2(10)
=> d = (log2(a) - log2(c) + log2(2^b)) / log2(10)
=> d ~ (log2(a) - log2(c) + b) * 1233 / 4096
=> c ~ (4096 * d / 1233) / (log2(a) - log2(c))
```
The approximation of 
`log2(10) ~ 4096 / 1233 is from [this article about bit hacks](https://graphics.stanford.edu/%7Eseander/bithacks.html#IntegerLog)`. 
Computing `log2` on integers is very efficient for reasons the above resource also goes into. The only thing left to do 
is normalize `c` to a value in the range `[0, 10)`. For this:
- First, find the highest exponent `x` such that `c >= 10 ^ x`, this can be done with an integer `log10`
    - Since `log10(n)` is a constant multiple of `log2(n)` for all `n` it can be quickly computed as well
- Then convert `c` to an `f64`, divide by `10^x`, and add `x` to `d` to compensate
This algorithm should really be pretty efficient, especially compared to I/O, which is very important since this will
primarily be used to print the value to the screen. 




### Bonus Stuff

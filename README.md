# BigNum
This is a complete rewrite of the BigNum program so I'm redoing the README as well. While
I'm working on it the original complete version is still around at
`src/bignumold/OLD_README.md`

## Performance
The rewrite has caused a bit of a performance hit on binary numbers. I may eventually
start handling binary in the same way as the old version, since early benchmarking
indicates a 50% increase in runtime for additions. That being said I will mostly be using
it with decimal numbers which are unavoidably less performant. So to avoid changing the
logic too much I'll leave it alone for now. 

### Benchmarks
These are the results of the benchmark tests in `benches/multi.rs`:

| Test            | Average(ms) |
|-----------------|-------------|
| Binary Add      | 14.402      | 
| Octal Add       | 15.122      |
| Decimal Add     | 15.771      |
| Hexadecimal Add | 16.792      |
| Arbitrary Add   | 230.98      |

From the above results we can make the following observations:
- Decimal takes barely longer than binary
    - This indicates that division/multiplication by powers of the base is not a major
    choke point for non-arbitrary bases. Need to focus on other parts of the code to
    improve binary performance
- The locking mechanism results in gigantic overhead
    - Alternatives we can try:
        - Start including metadata on the object again
        - Stop pre-calculating at all and rely on `pow`
            - With this approach we'd need to include ranges on the object
        - Drop arbitrary bases since the special bases cover 99.99% of use cases
            - This way we could include base as part of the type:
            ```
            pub struct BigNum<T> where T: Base {}
            pub trait Base {
                const exp_range: (u32, u32);
                const sig_range: (u64, u64);

                fn get_pow(&self, exp: u32) -> u64;
                fn divide(lhs: u64, rhs: u64) -> u64;
            }
            pub struct BaseTwo {}
            pub struct BaseEight {}
            ...
            // impl Base for each struct
            ```
            Or something like the above. This would allow for more efficient dispatching 
            of methods during performance-critical code, but also allow users to extend
            the default bases as they see fit. I could even provide a macro that does it.

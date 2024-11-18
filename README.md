# BigNum
In the middle of another rewrite.

## Performance
The rewrite has caused a bit of a performance hit on binary numbers. I may eventually
start handling binary in the same way as the old version, since early benchmarking
indicates a 50% increase in runtime for additions. That being said I will mostly be using
it with decimal numbers which are unavoidably less performant. So to avoid changing the
logic too much I'll leave it alone for now. 

### Benchmarks
These are the results of the old implementation benchmark tests in `benches/multi.rs`:

| Test            | Average(ms) |
|-----------------|-------------|
| Binary Add      | 14.402      | 
| Octal Add       | 15.122      |
| Decimal Add     | 15.771      |
| Hexadecimal Add | 16.792      |
| Arbitrary Add   | 230.98      |

After this I decided to rework the architecture again.

These are the results of the new implementation benchmark tests in `benches/multi.rs`:
| Test             | Average(ms) |
|------------------|-------------|
| Binary Add       | 11.300      | 
| Octal Add        | 11.236      |
| Decimal Add      | see below   |
| Hexadecimal Add  | 11.421      |
| Arbitrary Add    | see below   |
| Arbitrary Add  2 | see below   |

The results to this section are odd. The 3 with listed times were very consistent across
benchmark runs. The others seemed to depend on each other:
- When it was just the first 4, the `Decimal Add` test took around 5ms per time after an
initial slow run.
- When it was the first 5, the `Decimal Add` test took around 15ms and the `Arbitrary Add`
test took around 6ms
- When all are defined the `Decimal Add` test takes around 15ms and the others take around
10ms each.

I can only assume that this has to do with the processor cache. I need to switch to random
testing maybe? 

But overall it seems clear that this is a good direction performance-wise. 

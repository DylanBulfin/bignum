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

| Test            | Average(ns) |
|-----------------|-------------|
| Binary Add      | 14.402      | 
| Octal Add       | 15.122      |
| Decimal Add     | 15.771      |
| Hexadecimal Add | 16.792      |
| Arbitrary Add   | 230.98      |

After this I decided to rework the architecture again.

These are the results of the new implementation benchmark tests in `benches/multi.rs`:
| Test             | Average(ns) |
|------------------|-------------|
| Binary Add       | 11.300      | 
| Octal Add        | 11.236      |
| Decimal Add      | see below   |
| Hexadecimal Add  | 11.421      |
| Arbitrary Add    | see below   |
| Arbitrary Add  2 | see below   |

The results to this section are odd. The 3 with listed times were very consistent across
benchmark runs. The others seemed to depend on each other:
- When it was just the first 4, the `Decimal Add` test took around 5ns per time after an
initial slow run.
- When it was the first 5, the `Decimal Add` test took around 15ns and the `Arbitrary Add`
test took around 6ns
- When all are defined the `Decimal Add` test takes around 15ns and the others take around
10ns each.

I can only assume that this has to do with the processor cache. I need to switch to random
testing maybe? 

But overall it seems clear that this is a good direction performance-wise. 

#### Notes on caching
When I replaced the `pow` method in `Hexadecimal` with a table lookup and removed all 
other tests, it similarly jumped down to ~5-6 ns. I think this confirms two things:
- Using table lookup for powers improves performance even for nice, clean bases.
- This performance gain is lessened when you use BigNum instances with multiple bases
at once (e.g. if your code uses one decimal BigNum and a custom base-61 BigNum)
    - This is presumably because the cache can't hold all the useful values for all
    bases

### Binary/Hexadecimal performance
There was something very weird going on with the Decimal test. I made a macro that does
tests and called it with an operation that normally overflows (`10u64.pow(21)` because I
misremembered the `sig_max` value). I've confirmed that tests do indeed abort on panic.
So there's something about calling a macro within a criterion test function that
implicitly ignores numeric overflow which is bizarre. I'm unable to reproduce it in 
`main.rs` so it must be something about either the criterion harness itself (e.g.
module-wide `#[allow(arithmetic_overflow)]`) or the way it calls the test function. I've
updated the results that were affected by this.

To test using a const table for Binary exponentiation I added some new tests that involve 
generating 1000 random numbers and adding them. The results for Binary, Decimal, and Hex,
and involved running them all separately to avoid cache collisions, etc.
| Test             | Average(us) |
|------------------|-------------|
| Binary Add       | 22.152      | 
| Decimal Add      | 36.69       |
| Hexadecimal Add  | 18.514      |

Binary should not be this much slower than hexadecimal. It makes me think that the issue
is something like the below:
- Hex has a wider valid sig range, meaning that `new` is more likely to be called with
valid arguments and thus skip the normalization logic. 
- Hex has fewer valid powers meaning more efficient caching of all valid powers. Binary
needs a length 64 array while hex needs length 16

I realized that the previous test benchmarks were done via test code that didn't ensure 
the randomly-generated values were close enough to add. This means the vast majority
of calls to add were exited almost immediately since the magnitude differential was too
high. Following this I rewrote the code to generate the second number in a special way
that limited the magnitude differential. The results below (tests were run together):
| Test             | Average(us) |
|------------------|-------------|
| Binary Add       | 19.172      | 
| Decimal Add      | 30.403      |
| Hexadecimal Add  | 16.338      |

Following this I also wrote up some tests that limited the random range to compact
numbers, e.g. those where `exp == 0`
| Test             | Average(us) |
|------------------|-------------|
| Binary Add       | 13.629      | 
| Decimal Add      | 26.015      |
| Hexadecimal Add  | 13.471      |
The above indicates that I was right about the cause of the disparity, as removing calls
to `new` gets rid of it almost entirely. Additionally, the results were the same whether
the tests were run together or separately which suggests that the processor cache is not
as much of an issue. This is expected since with compact values we mostly sidestep the
type's logic.


## Features

### Random
I've added an implementation to generate random-ish BigNum values for testing. It is not
actually correct (values won't appear at the frequency you'd expect and the bounds are
not expected). But it will do for peformance testing and whatnot.

use bignumbe_rs::{Base, ExpRange, SigRange};
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse, parse_macro_input, Ident, Lit};

struct BaseData {
    exp_range: ExpRange,
    sig_range: SigRange,
    powers: Vec<u64>,
    powers_u128: Vec<u128>,
}

#[proc_macro]
pub fn create_efficient_base(input: TokenStream) -> TokenStream {
    let number: u64 = if let Lit::Int(li) = parse_macro_input!(input as Lit) {
        li.base10_parse()
            .expect("Input must be a valid base-10 number")
    } else {
        panic!("Input must be a valid u16 value greater than 2");
    };
    if number > u16::MAX as u64 {
        panic!("Input must be a valid u16 value greater than 2");
    }

    let base_ident = format_ident!("Base{}", number);

    let BaseData {
        exp_range,
        sig_range,
        powers,
        powers_u128,
    } = get_base_data(number as u16);

    let power_tables = generate_power_tables(number, powers, powers_u128);
    let impl_code = generate_impl(number, &base_ident, exp_range, sig_range);

    quote! {
        #[derive(Clone, Copy, Debug)]
        pub struct #base_ident();

        #power_tables
        #impl_code
    }
    .into()
}

fn generate_impl(
    number: u64,
    base_ident: &Ident,
    exp_range: ExpRange,
    sig_range: SigRange,
) -> proc_macro2::TokenStream {
    let powers_ident = format_ident!("BASE_{}_POWERS", number);
    let powers_u128_ident = format_ident!("BASE_{}_U128_POWERS", number);

    let ExpRange(min_exp, max_exp) = exp_range;
    let SigRange(min_sig, max_sig) = sig_range;

    let shared = quote! {
        const NUMBER: u16 = #number as u16;

        fn new() -> Self {
            Self()
        }

        fn exp_range(&self) -> bignumbe_rs::ExpRange {
            bignumbe_rs::ExpRange(#min_exp, #max_exp)
        }

        fn sig_range(&self) -> bignumbe_rs::SigRange {
            bignumbe_rs::SigRange(#min_sig, #max_sig)
        }

        fn pow(exp: u32) -> u64 {
            #powers_ident[exp as usize]
        }

        fn pow_u128(exp: u32) -> u128 {
            #powers_u128_ident[exp as usize]
        }
    };

    if number.is_power_of_two() {
        let log = number.ilog2();

        quote! {
            impl bignumbe_rs::Base for #base_ident {
                #shared

                fn rshift(lhs: u64, exp: u32) -> u64 {
                    lhs >> (#log * exp)
                }

                fn rshift_u128(lhs: u128, exp: u32) -> u128 {
                    lhs >> (#log * exp)
                }

                fn lshift(lhs: u64, exp: u32) -> u64 {
                    lhs << (#log * exp)
                }

                fn lshift_u128(lhs: u128, exp: u32) -> u128 {
                    lhs << (#log * exp)
                }
            }
        }
    } else {
        quote! {
            impl bignumbe_rs::Base for #base_ident {
                #shared

                fn rshift(lhs: u64, exp: u32) -> u64 {
                    lhs / Self::pow(exp)
                }

                fn rshift_u128(lhs: u128, exp: u32) -> u128 {
                    lhs / Self::pow_u128(exp)
                }

                fn lshift(lhs: u64, exp: u32) -> u64 {
                    lhs * Self::pow(exp)
                }

                fn lshift_u128(lhs: u128, exp: u32) -> u128 {
                    lhs * Self::pow_u128(exp)
                }
            }
        }
    }
}

fn generate_power_tables(
    number: u64,
    powers: Vec<u64>,
    powers_u128: Vec<u128>,
) -> proc_macro2::TokenStream {
    let powers_len = powers.len();
    let powers_u128_len = powers_u128.len();

    let table_ident = format_ident!("BASE_{}_POWERS", number);
    let table_u128_ident = format_ident!("BASE_{}_U128_POWERS", number);
    quote! {
        pub const #table_ident: [u64; #powers_len] = [
            #(
                #powers
            ),*
        ];

        pub const #table_u128_ident: [u128; #powers_u128_len] = [
            #(
                #powers_u128
            ),*
        ];
    }
}

fn get_base_data(number: u16) -> BaseData {
    let mut curr = 1u128;

    let mut powers = Vec::new();
    let mut powers_u128 = Vec::new();

    loop {
        if curr <= u64::MAX as u128 {
            powers.push(curr as u64);
        }

        powers_u128.push(curr);

        match curr.checked_mul(number as u128) {
            Some(res) => curr = res,
            None => break,
        }
    }

    let number = number as u64;
    // TODO consider rewriting this to use the length and content of powers array instead
    let (exp_range, sig_range) = if number.is_power_of_two() && number.ilog2().is_power_of_two() {
        // This is a special case where sig_max = u64::MAX. We have to handle it
        // specially to avoid overflowing the u64
        let pow = number.ilog2();
        let exp = 64 / pow;
        let sig = number.pow(exp - 1);

        (ExpRange(exp - 1, exp), SigRange(sig, u64::MAX))
    } else {
        let exp = u64::MAX.ilog(number);
        (
            ExpRange(exp - 1, exp),
            SigRange(number.pow(exp - 1), number.pow(exp) - 1),
        )
    };

    BaseData {
        powers,
        powers_u128,
        exp_range,
        sig_range,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_base {
        (spec $num:expr, $min_exp:expr) => {{
            let num = $num as u64;
            let data = get_base_data($num);
            let min_exp = $min_exp;

            assert_eq!(data.exp_range, ExpRange(min_exp, min_exp + 1));
            assert_eq!(data.sig_range, SigRange(num.pow(min_exp as u32), u64::MAX));
            assert_eq!(data.powers.len(), data.exp_range.1 as usize);

            assert_eq!(data.powers_u128.len(), data.exp_range.1 as usize * 2);

            for (i, n) in data.powers.iter().enumerate() {
                assert_eq!(*n, num.pow(i as u32));
            }
        }};
        // By default it treats the base as not a power of two
        ($num:expr, $min_exp:expr) => {{
            let num = $num as u64;
            let data = get_base_data($num);
            let min_exp = $min_exp;

            assert_eq!(data.exp_range, ExpRange(min_exp, min_exp + 1));
            assert_eq!(
                data.sig_range,
                SigRange(num.pow(min_exp as u32), num.pow(min_exp as u32 + 1) - 1)
            );
            assert_eq!(data.powers.len(), data.exp_range.1 as usize + 1);

            assert_eq!(data.powers_u128.len(), data.exp_range.1 as usize * 2 + 1);

            for (i, n) in data.powers.iter().enumerate() {
                assert_eq!(*n, num.pow(i as u32));
            }
        }};
    }

    #[test]
    fn get_base_data_test() {
        test_base!(10, 18);
        test_base!(8, 20);
        test_base!(spec 256, 7);
        test_base!(spec 16, 15);
        test_base!(spec 2, 63);
    }
}

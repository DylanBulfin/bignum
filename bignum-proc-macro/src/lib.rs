use std::num::{NonZeroU32, NonZeroU64};

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse::{self, Parse},
    parse_macro_input, Ident, Lit, Token, Visibility,
};

struct BaseData {
    exp_range: (u32, u32),
    sig_range: (u64, u64),
    powers: Vec<u64>,
    powers_u128: Vec<u128>,
}

struct BaseInput {
    num: Lit,
    vis: Visibility,
    name: Ident,
}

impl Parse for BaseInput {
    fn parse(input: parse::ParseStream) -> syn::Result<Self> {
        let num = input.parse()?;
        let _com: Token![,] = input.parse()?;
        let vis = input.parse()?;
        let name = input.parse()?;

        Ok(Self { num, vis, name })
    }
}

/// Called like create_efficient_base(n, (pub) IntName), where n is the number of the base
/// and IntName is the name of the type you want to create, with optional visibility
/// qualifiers
#[proc_macro]
pub fn make_bignum(input: TokenStream) -> TokenStream {
    let BaseInput { num, vis, name } = parse_macro_input!(input as BaseInput);
    let (core, base_ident) = create_efficient_base_core(num);

    quote! {
        #core

        #vis type #name = bignumbe_rs::BigNumBase<#base_ident>;
    }
    .into()
}

/// Called like create_efficient_base_bare(n), where n is the number of the base
#[proc_macro]
pub fn create_efficient_base(input: TokenStream) -> TokenStream {
    create_efficient_base_core(parse_macro_input!(input as Lit))
        .0
        .into()
}

fn create_efficient_base_core(lit: Lit) -> (proc_macro2::TokenStream, Ident) {
    let number: u16 = if let Lit::Int(li) = lit {
        li.base10_parse()
            .expect("Input must be a valid base-10 number")
    } else {
        panic!("Input must be a valid u16 value greater than 2");
    };
    let number = number as u64;

    let base_ident = format_ident!("__Base{}", number);

    let BaseData {
        exp_range,
        sig_range,
        powers,
        powers_u128,
    } = get_base_data(number as u16);

    let power_tables = generate_power_tables(number, powers, powers_u128);
    let impl_code = generate_impl(number, &base_ident, exp_range, sig_range);

    // Create a default

    (
        quote! {
            #[derive(Clone, Copy, Debug)]
            struct #base_ident();

            #power_tables
            #impl_code
        },
        base_ident,
    )
}

fn generate_impl(
    number: u64,
    base_ident: &Ident,
    exp_range: (u32, u32),
    sig_range: (u64, u64),
) -> proc_macro2::TokenStream {
    let powers_ident = format_ident!("__BASE_{}_POWERS", number);
    let powers_u128_ident = format_ident!("__BASE_{}_U128_POWERS", number);

    let (min_exp, max_exp) = exp_range;
    let (min_sig, max_sig) = sig_range;

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

    let table_ident = format_ident!("__BASE_{}_POWERS", number);
    let table_u128_ident = format_ident!("__BASE_{}_U128_POWERS", number);
    quote! {
        const #table_ident: [u64; #powers_len] = [
            #(
                #powers
            ),*
        ];

        const #table_u128_ident: [u128; #powers_u128_len] = [
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

        ((exp - 1, exp), (sig, u64::MAX))
    } else {
        let exp = u64::MAX.ilog(number);
        ((exp - 1, exp), (number.pow(exp - 1), number.pow(exp) - 1))
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

            assert_eq!(data.exp_range, (min_exp, min_exp + 1));
            assert_eq!(data.sig_range, (num.pow(min_exp as u32), u64::MAX));
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

            assert_eq!(data.exp_range, (min_exp, min_exp + 1));
            assert_eq!(
                data.sig_range,
                (num.pow(min_exp as u32), num.pow(min_exp as u32 + 1) - 1)
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

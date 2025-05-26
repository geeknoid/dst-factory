use proc_macro2::TokenStream;
use quote::format_ident;
use syn::{
    Ident, Result as SynResult, Token, Visibility,
    parse::{Parse, ParseStream, discouraged::Speculative},
};

pub struct MacroArgs {
    pub visibility: Visibility,
    pub base_method_name: Ident,
    pub no_std: bool,
}

impl Parse for MacroArgs {
    fn parse(input: ParseStream) -> SynResult<Self> {
        let mut result = Self {
            visibility: Visibility::Inherited,
            base_method_name: Ident::new("build", input.span()),
            no_std: false,
        };

        // Check for method name
        if input.peek(syn::Ident) {
            let ahead = input.fork();
            let ident = ahead.parse::<Ident>()?;
            if ident != "no_std" && ident != "pub" {
                result.base_method_name = ident;

                input.advance_to(&ahead);
                if !input.is_empty() {
                    input
                        .parse::<Token![,]>()
                        .map_err(|_| input.error("Expected comma after method name"))?;
                }
            }
        }

        // Check for visibility
        if input.peek(Token![pub]) {
            result.visibility = input
                .parse()
                .map_err(|_| input.error("Failed to parse visibility"))?;

            if !input.is_empty() {
                input
                    .parse::<Token![,]>()
                    .map_err(|_| input.error("Expected comma after visibility"))?;
            }
        }

        // Check for no_std
        if input.peek(syn::Ident) {
            let ahead = input.fork();
            let ident = ahead.parse::<Ident>()?;
            if ident == "no_std" {
                result.no_std = true;
                input.advance_to(&ahead);
            }
        }

        if input.is_empty() {
            Ok(result)
        } else {
            Err(input.error("Unexpected input"))
        }
    }
}

impl MacroArgs {
    pub fn parse(attr_args_ts: TokenStream) -> SynResult<Self> {
        if attr_args_ts.is_empty() {
            Ok(Self {
                visibility: Visibility::Inherited,
                base_method_name: format_ident!("build"),
                no_std: false,
            })
        } else {
            syn::parse2(attr_args_ts)
        }
    }
}

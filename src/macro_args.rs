use proc_macro2::TokenStream;
use syn::{
    Ident, Result as SynResult, Token, Visibility,
    parse::{Parse, ParseStream, discouraged::Speculative},
};

pub struct MacroArgs {
    pub base_factory_name: Ident,
    pub visibility: Visibility,
    pub no_std: bool,
    pub generic_name: Ident,
}

impl Default for MacroArgs {
    fn default() -> Self {
        Self {
            base_factory_name: Ident::new("build", proc_macro2::Span::call_site()),
            visibility: Visibility::Inherited,
            no_std: false,
            generic_name: Ident::new("G", proc_macro2::Span::call_site()),
        }
    }
}

impl Parse for MacroArgs {
    fn parse(input: ParseStream) -> SynResult<Self> {
        let mut result = Self::default();

        // Check for factory name
        if input.peek(syn::Ident) {
            let ahead = input.fork();
            let ident = ahead.parse::<Ident>()?;
            if ident != "no_std" && ident != "pub" && ident != "generic" {
                result.base_factory_name = ident;

                input.advance_to(&ahead);
                if !input.is_empty() {
                    _ = input
                        .parse::<Token![,]>()
                        .map_err(|_ignored| input.error("Expected comma after factory name"))?;
                }
            }
        }

        // Check for visibility
        if input.peek(Token![pub]) {
            result.visibility = input.parse().map_err(|_ignored| input.error("Failed to parse visibility"))?;

            if !input.is_empty() {
                _ = input
                    .parse::<Token![,]>()
                    .map_err(|_ignored| input.error("Expected comma after visibility"))?;
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

        // Check for generic = <ident>
        if input.peek(syn::Ident) {
            let ahead = input.fork();
            let ident = ahead.parse::<Ident>()?;
            if ident == "generic" {
                input.advance_to(&ahead);
                _ = input.parse::<Token![=]>()?;
                result.generic_name = input
                    .parse::<Ident>()
                    .map_err(|_ignored| input.error("Expected identifier after `generic =`"))?;
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
    pub fn parse(attr_args: TokenStream) -> SynResult<Self> {
        if attr_args.is_empty() {
            Ok(Self::default())
        } else {
            syn::parse2(attr_args)
        }
    }
}

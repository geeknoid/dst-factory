use proc_macro2::TokenStream;
use syn::{
    Ident, Result as SynResult, Token, Visibility,
    parse::{Parse, ParseStream, discouraged::Speculative},
};

pub struct MacroArgs {
    pub base_factory_name: Ident,
    pub base_destructor_name: Ident,
    /// When `None`, defaults to appending "Iter" to the type's name.
    pub iterator_name: Option<Ident>,
    pub visibility: Visibility,
    pub no_std: bool,
    pub generic_name: Ident,
}

impl Default for MacroArgs {
    fn default() -> Self {
        Self {
            base_factory_name: Ident::new("build", proc_macro2::Span::call_site()),
            base_destructor_name: Ident::new("destroy", proc_macro2::Span::call_site()),
            iterator_name: None,
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
            if ident != "no_std" && ident != "pub" && ident != "generic" && ident != "destructor" && ident != "iterator" {
                result.base_factory_name = ident;

                input.advance_to(&ahead);
                if !input.is_empty() {
                    _ = input
                        .parse::<Token![,]>()
                        .map_err(|_ignored| input.error("Expected comma after factory name"))?;
                }
            }
        }

        // Check for destructor = <ident>
        if input.peek(syn::Ident) {
            let ahead = input.fork();
            let ident = ahead.parse::<Ident>()?;
            if ident == "destructor" {
                input.advance_to(&ahead);
                _ = input.parse::<Token![=]>()?;
                result.base_destructor_name = input
                    .parse::<Ident>()
                    .map_err(|_ignored| input.error("Expected identifier after `destructor=`"))?;
                if !input.is_empty() {
                    _ = input
                        .parse::<Token![,]>()
                        .map_err(|_ignored| input.error("Expected comma after destructor name"))?;
                }
            }
        }

        // Check for iterator = <ident>
        if input.peek(syn::Ident) {
            let ahead = input.fork();
            let ident = ahead.parse::<Ident>()?;
            if ident == "iterator" {
                input.advance_to(&ahead);
                _ = input.parse::<Token![=]>()?;
                result.iterator_name = Some(
                    input
                        .parse::<Ident>()
                        .map_err(|_ignored| input.error("Expected identifier after `iterator=`"))?,
                );
                if !input.is_empty() {
                    _ = input
                        .parse::<Token![,]>()
                        .map_err(|_ignored| input.error("Expected comma after iterator name"))?;
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

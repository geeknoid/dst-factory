use proc_macro2::TokenStream;
use syn::{
    Ident, Result as SynResult, Token, Visibility,
    parse::{Parse, ParseStream, discouraged::Speculative},
};

pub struct MacroArgs {
    pub base_factory_name: Ident,
    pub base_destructurer_name: Ident,
    /// When `None`, defaults to appending "Iter" to the type's name.
    pub iterator_name: Option<Ident>,
    pub visibility: Visibility,
    pub no_std: bool,
    pub deserialize: bool,
    pub generic_name: Ident,
}

impl Default for MacroArgs {
    fn default() -> Self {
        Self {
            base_factory_name: Ident::new("build", proc_macro2::Span::call_site()),
            base_destructurer_name: Ident::new("destructure", proc_macro2::Span::call_site()),
            iterator_name: None,
            visibility: Visibility::Inherited,
            no_std: false,
            deserialize: false,
            generic_name: Ident::new("G", proc_macro2::Span::call_site()),
        }
    }
}

/// Try to consume a keyword identifier from the input, returning `true` if matched.
fn try_consume_keyword(input: ParseStream, keyword: &str) -> SynResult<bool> {
    if input.peek(Ident) {
        let ahead = input.fork();
        let ident = ahead.parse::<Ident>()?;
        if ident == keyword {
            input.advance_to(&ahead);
            return Ok(true);
        }
    }
    Ok(false)
}

/// Consume a trailing comma if the input is not empty.
fn consume_trailing_comma(input: ParseStream, after: &str) -> SynResult<()> {
    if !input.is_empty() {
        _ = input
            .parse::<Token![,]>()
            .map_err(|_ignored| input.error(format!("Expected comma after {after}")))?;
    }
    Ok(())
}

/// Parse `keyword = <ident>`, returning the value identifier.
fn parse_keyword_value(input: ParseStream, keyword: &str) -> SynResult<Option<Ident>> {
    if try_consume_keyword(input, keyword)? {
        _ = input.parse::<Token![=]>()?;
        let value = input
            .parse::<Ident>()
            .map_err(|_ignored| input.error(format!("Expected identifier after `{keyword}=`")))?;
        consume_trailing_comma(input, keyword)?;
        Ok(Some(value))
    } else {
        Ok(None)
    }
}

impl Parse for MacroArgs {
    fn parse(input: ParseStream) -> SynResult<Self> {
        let mut result = Self::default();

        // Check for factory name
        if input.peek(Ident) {
            let ahead = input.fork();
            let ident = ahead.parse::<Ident>()?;
            if ident != "no_std"
                && ident != "deserialize"
                && ident != "pub"
                && ident != "generic"
                && ident != "destructurer"
                && ident != "iterator"
            {
                result.base_factory_name = ident;
                input.advance_to(&ahead);
                consume_trailing_comma(input, "factory name")?;
            }
        }

        if let Some(name) = parse_keyword_value(input, "destructurer")? {
            result.base_destructurer_name = name;
        }

        if let Some(name) = parse_keyword_value(input, "iterator")? {
            result.iterator_name = Some(name);
        }

        // Check for visibility
        if input.peek(Token![pub]) {
            result.visibility = input.parse().map_err(|_ignored| input.error("Failed to parse visibility"))?;
            consume_trailing_comma(input, "visibility")?;
        }

        if try_consume_keyword(input, "no_std")? {
            result.no_std = true;
            consume_trailing_comma(input, "no_std")?;
        }

        if try_consume_keyword(input, "deserialize")? {
            result.deserialize = true;
            consume_trailing_comma(input, "deserialize")?;
        }

        if let Some(name) = parse_keyword_value(input, "generic")? {
            result.generic_name = name;
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

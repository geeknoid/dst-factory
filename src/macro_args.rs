use proc_macro2::TokenStream;
use syn::{
    Ident, Result as SynResult, Token, Visibility,
    parse::{Parse, ParseStream, discouraged::Speculative},
};

#[expect(
    clippy::struct_excessive_bools,
    reason = "macro options are naturally represented as independent feature flags"
)]
pub struct MacroArgs {
    pub base_factory_name: Ident,
    pub base_destructurer_name: Ident,
    /// When `None`, defaults to appending "Iter" to the type's name.
    pub iterator_name: Option<Ident>,
    pub visibility: Visibility,
    pub no_std: bool,
    pub deserialize: bool,
    pub clone: bool,
    pub debug: bool,
    pub eq: bool,
    pub ord: bool,
    pub hash: bool,
    pub zeroable: bool,
    pub arena: bool,
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
            clone: false,
            debug: false,
            eq: false,
            ord: false,
            hash: false,
            zeroable: false,
            arena: false,
            generic_name: Ident::new("G", proc_macro2::Span::call_site()),
        }
    }
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

/// Parse `keyword = <ident>`, returning the value identifier. The (already
/// matched) keyword token is consumed here.
fn parse_keyword_value(input: ParseStream, keyword: &str) -> SynResult<Ident> {
    _ = input.parse::<Ident>()?;
    _ = input.parse::<Token![=]>()?;
    let value = input
        .parse::<Ident>()
        .map_err(|_ignored| input.error(format!("Expected identifier after `{keyword}=`")))?;
    consume_trailing_comma(input, keyword)?;
    Ok(value)
}

/// The reserved identifiers that name a flag or `keyword = value` option and so
/// cannot be used as a custom factory name.
fn is_reserved_keyword(ident: &Ident) -> bool {
    matches!(
        ident.to_string().as_str(),
        "no_std"
            | "deserialize"
            | "clone"
            | "debug"
            | "eq"
            | "ord"
            | "hash"
            | "zeroable"
            | "arena"
            | "pub"
            | "generic"
            | "destructurer"
            | "iterator"
    )
}

/// Consume a boolean flag keyword, setting `slot` and rejecting duplicates. The
/// (already matched) keyword token is consumed here.
fn parse_flag(input: ParseStream, keyword: &str, slot: &mut bool, ident: &Ident) -> SynResult<()> {
    if *slot {
        return Err(syn::Error::new(ident.span(), format!("`{keyword}` specified more than once")));
    }
    _ = input.parse::<Ident>()?;
    *slot = true;
    consume_trailing_comma(input, keyword)?;
    Ok(())
}

impl Parse for MacroArgs {
    fn parse(input: ParseStream) -> SynResult<Self> {
        let mut result = Self::default();

        // The optional factory name is positional: when present it must be the
        // first argument. Every other option may appear in any order.
        if input.peek(Ident) {
            let ahead = input.fork();
            let ident = ahead.parse::<Ident>()?;
            if !is_reserved_keyword(&ident) {
                result.base_factory_name = ident;
                input.advance_to(&ahead);
                consume_trailing_comma(input, "factory name")?;
            }
        }

        let mut seen_visibility = false;
        let mut seen_destructurer = false;
        let mut seen_iterator = false;
        let mut seen_generic = false;

        // Remaining arguments are order-independent.
        while !input.is_empty() {
            if input.peek(Token![pub]) {
                if seen_visibility {
                    return Err(input.error("`pub` specified more than once"));
                }
                result.visibility = input.parse().map_err(|_ignored| input.error("Failed to parse visibility"))?;
                seen_visibility = true;
                consume_trailing_comma(input, "visibility")?;
                continue;
            }

            if input.peek(Ident) {
                let ident = input.fork().parse::<Ident>()?;
                match ident.to_string().as_str() {
                    "destructurer" => {
                        if seen_destructurer {
                            return Err(syn::Error::new(ident.span(), "`destructurer` specified more than once"));
                        }
                        result.base_destructurer_name = parse_keyword_value(input, "destructurer")?;
                        seen_destructurer = true;
                    }
                    "iterator" => {
                        if seen_iterator {
                            return Err(syn::Error::new(ident.span(), "`iterator` specified more than once"));
                        }
                        result.iterator_name = Some(parse_keyword_value(input, "iterator")?);
                        seen_iterator = true;
                    }
                    "generic" => {
                        if seen_generic {
                            return Err(syn::Error::new(ident.span(), "`generic` specified more than once"));
                        }
                        result.generic_name = parse_keyword_value(input, "generic")?;
                        seen_generic = true;
                    }
                    "no_std" => parse_flag(input, "no_std", &mut result.no_std, &ident)?,
                    "deserialize" => parse_flag(input, "deserialize", &mut result.deserialize, &ident)?,
                    "clone" => parse_flag(input, "clone", &mut result.clone, &ident)?,
                    "debug" => parse_flag(input, "debug", &mut result.debug, &ident)?,
                    "eq" => parse_flag(input, "eq", &mut result.eq, &ident)?,
                    "ord" => parse_flag(input, "ord", &mut result.ord, &ident)?,
                    "hash" => parse_flag(input, "hash", &mut result.hash, &ident)?,
                    "zeroable" => parse_flag(input, "zeroable", &mut result.zeroable, &ident)?,
                    "arena" => parse_flag(input, "arena", &mut result.arena, &ident)?,
                    _ => return Err(input.error("Unexpected input")),
                }
                continue;
            }

            return Err(input.error("Unexpected input"));
        }

        Ok(result)
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

#[cfg(test)]
mod tests {
    use super::MacroArgs;
    use proc_macro2::TokenStream;
    use quote::quote;

    // `MacroArgs` cannot derive `Debug` (its `syn::Visibility` field only
    // implements it under syn's `extra-traits` feature), so these helpers assert
    // on a bound bool rather than via `unwrap` or `assert!(.is_ok())`.
    fn parse_ok(args: TokenStream) {
        let parsed = MacroArgs::parse(args).is_ok();
        assert!(parsed, "expected the arguments to parse");
    }

    fn parse_err(args: TokenStream) {
        let rejected = MacroArgs::parse(args).is_err();
        assert!(rejected, "expected the arguments to be rejected");
    }

    #[test]
    fn flags_parse_in_any_order() {
        // Orderings the old fixed-sequence parser rejected.
        parse_ok(quote! { pub, arena, clone, debug });
        parse_ok(quote! { make, debug, clone, generic = T, pub });
    }

    #[test]
    fn duplicate_flag_is_rejected() {
        parse_err(quote! { clone, clone });
    }

    #[test]
    fn duplicate_visibility_is_rejected() {
        parse_err(quote! { pub, pub });
    }

    #[test]
    fn duplicate_destructurer_is_rejected() {
        parse_err(quote! { destructurer = a, destructurer = b });
    }

    #[test]
    fn duplicate_iterator_is_rejected() {
        parse_err(quote! { iterator = a, iterator = b });
    }

    #[test]
    fn duplicate_generic_is_rejected() {
        parse_err(quote! { generic = A, generic = B });
    }
}

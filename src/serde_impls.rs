use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{Fields, ItemStruct, Type};

use crate::{MacroArgs, StructInfo, TailKind};

/// Generates a `Deserialize` implementation for `Box<StructName>` using a shadow
/// intermediate struct that mirrors the original but replaces the DST tail with
/// an owned type (`Vec<T>` for `[T]`, `String` for `str`).
///
/// All `#[serde(...)]` attributes on fields are preserved on the shadow struct,
/// so serde's full attribute vocabulary works naturally.
///
/// The caller must reject trait object tails before invoking this function.
pub fn gen_deserialize(macro_args: &MacroArgs, struct_info: &StructInfo, input_struct: &ItemStruct) -> TokenStream {
    let (owned_tail_type, factory_name) = match &struct_info.tail_kind {
        TailKind::Slice(elem_type) => (
            syn::parse_quote! { ::std::vec::Vec<#elem_type> },
            format_ident!("{}_from_slice", &macro_args.base_factory_name),
        ),
        TailKind::Str => (syn::parse_quote! { ::std::string::String }, macro_args.base_factory_name.clone()),
        TailKind::TraitObject(_) => {
            return quote! {
                ::core::compile_error!("deserialize is not supported for DSTs with trait object tails");
            };
        }
    };

    let struct_name = struct_info.struct_name;
    let shadow_name = format_ident!("__{}Intermediate", struct_name);
    let (_impl_generics, ty_generics, where_clause) = struct_info.struct_generics.split_for_impl();

    let shadow_struct = build_shadow_struct(input_struct, &shadow_name, &owned_tail_type);
    let conversion = build_conversion(struct_info, &factory_name);

    quote! {
        const _: () = {
            #shadow_struct

            impl<'de> ::serde::Deserialize<'de> for ::std::boxed::Box<#struct_name #ty_generics> #where_clause {
                fn deserialize<D>(deserializer: D) -> ::core::result::Result<Self, D::Error>
                where
                    D: ::serde::Deserializer<'de>,
                {
                    let intermediate = <#shadow_name as ::serde::Deserialize>::deserialize(deserializer)?;
                    ::core::result::Result::Ok(#conversion)
                }
            }
        };
    }
}

/// Builds the shadow struct: same fields and attributes, but the tail field
/// is replaced with the provided owned type.
fn build_shadow_struct(input_struct: &ItemStruct, shadow_name: &proc_macro2::Ident, owned_tail_type: &Type) -> TokenStream {
    let mut shadow = input_struct.clone();
    shadow.ident = shadow_name.clone();
    shadow.vis = syn::Visibility::Inherited;

    // Remove non-serde derives and attributes from the shadow, keep only serde-related ones
    shadow.attrs.retain(|attr| attr.path().is_ident("serde"));

    // Replace the tail field type
    let fields = match &mut shadow.fields {
        Fields::Named(named) => &mut named.named,
        Fields::Unnamed(unnamed) => &mut unnamed.unnamed,
        Fields::Unit => &mut syn::punctuated::Punctuated::new(),
    };

    if let Some(tail) = fields.last_mut() {
        tail.ty = owned_tail_type.clone();
    }

    quote! {
        #[derive(::serde::Deserialize)]
        #shadow
    }
}

/// Generates the expression that converts the shadow struct into `Box<StructName>`
/// by calling the appropriate factory method.
fn build_conversion(struct_info: &StructInfo, factory_name: &proc_macro2::Ident) -> TokenStream {
    let struct_name = struct_info.struct_name;

    let header_accesses: Vec<TokenStream> = struct_info
        .header_field_idents
        .iter()
        .map(|ident| quote! { intermediate.#ident })
        .collect();

    let tail_ident = &struct_info.tail_field_ident;

    quote! {
        #struct_name::#factory_name(#( #header_accesses, )* &intermediate.#tail_ident)
    }
}

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{Ident, ItemStruct, Type};

use crate::common::StructInfo;

/// Generates a `Deserialize` implementation for `Box<StructName>` using a shadow
/// intermediate struct that mirrors the original but replaces the DST tail with
/// an owned type (`Vec<T>` for `[T]`, `String` for `str`).
///
/// All `#[serde(...)]` attributes on fields are preserved on the shadow struct,
/// so serde's full attribute vocabulary works naturally.
///
/// The caller must reject trait object tails before invoking this function.
pub fn gen_deserialize(struct_info: &StructInfo, input_struct: &ItemStruct, owned_tail_type: &Type, factory_name: &Ident) -> TokenStream {
    let struct_name = struct_info.struct_name;
    let shadow_name = format_ident!("__{}Intermediate", struct_name);
    let (_impl_generics, ty_generics, where_clause) = struct_info.struct_generics.split_for_impl();

    let mut shadow = input_struct.clone();
    shadow.ident = shadow_name.clone();
    shadow.vis = syn::Visibility::Inherited;
    shadow.attrs.retain(|attr| attr.path().is_ident("serde"));

    if let Some(tail) = shadow.fields.iter_mut().last() {
        tail.ty = owned_tail_type.clone();
    }

    let header_accesses: Vec<TokenStream> = struct_info
        .header_field_idents
        .iter()
        .map(|ident| quote! { intermediate.#ident })
        .collect();
    let tail_ident = &struct_info.tail_field_ident;

    quote! {
        const _: () = {
            #[derive(::serde::Deserialize)]
            #shadow

            impl<'de> ::serde::Deserialize<'de> for ::std::boxed::Box<#struct_name #ty_generics> #where_clause {
                fn deserialize<D>(deserializer: D) -> ::core::result::Result<Self, D::Error>
                where
                    D: ::serde::Deserializer<'de>,
                {
                    let intermediate = <#shadow_name as ::serde::Deserialize>::deserialize(deserializer)?;
                    ::core::result::Result::Ok(#struct_name::#factory_name(#( #header_accesses, )* &intermediate.#tail_ident))
                }
            }
        };
    }
}

/// Generates a standalone associated function that deserializes into a given smart
/// pointer type (e.g. `Arc<T>` or `Rc<T>`).
///
/// Users reference it via `#[serde(deserialize_with = "MyStruct::deserialize_arc")]`.
#[expect(
    clippy::too_many_arguments,
    reason = "proc-macro codegen helper needs all these context parameters"
)]
pub fn gen_deserialize_fn(
    struct_info: &StructInfo,
    input_struct: &ItemStruct,
    owned_tail_type: &Type,
    factory_name: &Ident,
    fn_name: &Ident,
    shadow_suffix: &str,
    wrapper_path: &TokenStream,
    visibility: &syn::Visibility,
) -> TokenStream {
    let struct_name = struct_info.struct_name;
    let shadow_name = format_ident!("__{struct_name}{shadow_suffix}");
    let (_impl_generics, _ty_generics, _where_clause) = struct_info.struct_generics.split_for_impl();

    let mut shadow = input_struct.clone();
    shadow.ident = shadow_name.clone();
    shadow.vis = syn::Visibility::Inherited;
    shadow.attrs.retain(|attr| attr.path().is_ident("serde"));

    if let Some(tail) = shadow.fields.iter_mut().last() {
        tail.ty = owned_tail_type.clone();
    }

    let header_accesses: Vec<TokenStream> = struct_info
        .header_field_idents
        .iter()
        .map(|ident| quote! { intermediate.#ident })
        .collect();
    let tail_ident = &struct_info.tail_field_ident;

    let factory_doc = format!(
        "Deserializes into `{wrapper_path}<{struct_name}>`. Use with `#[serde(deserialize_with = \"{struct_name}::{fn_name}\")]`.",
    );

    quote! {
        #[doc = #factory_doc]
        #visibility fn #fn_name<'de, D>(deserializer: D) -> ::core::result::Result<#wrapper_path<Self>, D::Error>
        where
            D: ::serde::Deserializer<'de>,
        {
            #[derive(::serde::Deserialize)]
            #shadow

            let intermediate = <#shadow_name as ::serde::Deserialize>::deserialize(deserializer)?;
            ::core::result::Result::Ok(Self::#factory_name(#( #header_accesses, )* &intermediate.#tail_ident))
        }
    }
}

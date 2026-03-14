use proc_macro2::TokenStream;
use quote::quote;
use syn::{Ident, Type};

use crate::common::{StructInfo, box_path, dealloc_path};
use crate::macro_args::MacroArgs;

pub fn destructurer_iterator_type(macro_args: &MacroArgs, struct_info: &StructInfo, tail_type: &Type) -> TokenStream {
    let dealloc_path = dealloc_path(macro_args.no_std);

    let visibility = &macro_args.visibility;
    let iterator_name = macro_args
        .iterator_name
        .clone()
        .unwrap_or_else(|| Ident::new(&format!("{}Iter", struct_info.struct_name), proc_macro2::Span::call_site()));

    let struct_name = &struct_info.struct_name;
    let struct_generics = &struct_info.struct_generics;
    let (impl_generics, ty_generics, where_clause) = struct_info.struct_generics.split_for_impl();

    let factory_doc = format!("Iterator type for a destructured `Box<{struct_name}>`");

    quote! {
        #[doc = #factory_doc]
        #visibility struct #iterator_name #struct_generics #where_clause {
            ptr: *mut #tail_type,
            index: usize,
            len: usize,
            free_ptr: *mut u8,
            layout: ::core::alloc::Layout,
        }

        impl #impl_generics ::core::iter::Iterator for #iterator_name #ty_generics #where_clause {
            type Item = #tail_type;
            fn next(&mut self) -> Option<Self::Item> {
                if self.index >= self.len { return None; }
                #[allow(clippy::zst_offset)]
                let value = unsafe { self.ptr.add(self.index).read() };
                self.index += 1;
                Some(value)
            }
        }

        impl #impl_generics ::core::iter::ExactSizeIterator for #iterator_name #ty_generics #where_clause {
            fn len(&self) -> usize { self.len - self.index }
        }

        impl #impl_generics ::core::fmt::Debug for #iterator_name #ty_generics #where_clause {
            fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                f.debug_struct(::core::stringify!(#iterator_name))
                    .field("index", &self.index)
                    .field("len", &self.len)
                    .finish()
            }
        }

        impl #impl_generics ::core::ops::Drop for #iterator_name #ty_generics #where_clause {
            fn drop(&mut self) {
                unsafe {
                    while self.index < self.len {
                        self.ptr.add(self.index).drop_in_place();
                        self.index += 1;
                    }
                    #dealloc_path(self.free_ptr, self.layout)
                }
            }
        }
    }
}

pub fn destructurer_with_iter(macro_args: &MacroArgs, struct_info: &StructInfo) -> TokenStream {
    let box_path = box_path(macro_args.no_std);

    let header_fields = &struct_info.header_field_idents;
    let header_types = &struct_info.header_types;

    let visibility = &macro_args.visibility;
    let destructurer_name = &macro_args.base_destructurer_name;

    let tail_field = &struct_info.tail_field_ident;
    let struct_name = &struct_info.struct_name;
    let iterator_name = macro_args
        .iterator_name
        .clone()
        .unwrap_or_else(|| Ident::new(&format!("{}Iter", struct_info.struct_name), proc_macro2::Span::call_site()));
    let (_impl_generics, ty_generics, _where_clause) = struct_info.struct_generics.split_for_impl();

    let factory_doc = format!("Destructures an instance of `Box<{struct_name}>`, returning the tail slice as an iterator.");

    quote! {
        #[doc = #factory_doc]
        #visibility fn #destructurer_name(
            this: #box_path<Self>,
        ) -> ( #( #header_types, )* #iterator_name #ty_generics)
        {
            let layout = ::core::alloc::Layout::for_value(&*this);
            let len = this.#tail_field.len();
            let this = #box_path::into_raw(this);
            unsafe {
                (
                    #( (&raw mut (*this).#header_fields).read(), )*
                    #iterator_name {
                        ptr: (*this).#tail_field.as_mut_ptr(),
                        index: 0,
                        len,
                        free_ptr: this.cast(),
                        layout,
                    }
                )
            }
        }
    }
}

pub enum CloneTailKind<'a> {
    Slice(&'a Type),
    Str,
}

pub fn gen_clone(macro_args: &MacroArgs, struct_info: &StructInfo, clone_tail: &CloneTailKind) -> TokenStream {
    let struct_name = struct_info.struct_name;
    let box_path = box_path(macro_args.no_std);
    let factory_name = &macro_args.base_factory_name;

    let header_accesses: Vec<TokenStream> = struct_info
        .header_field_idents
        .iter()
        .map(|ident| quote! { self.#ident.clone() })
        .collect();

    let tail_ident = &struct_info.tail_field_ident;

    let (factory_call, extra_bounds) = match clone_tail {
        CloneTailKind::Slice(elem_type) => (
            quote! { #struct_name::#factory_name(#( #header_accesses, )* self.#tail_ident.iter().cloned()) },
            vec![quote! { #elem_type: ::core::clone::Clone }],
        ),
        CloneTailKind::Str => (
            quote! { #struct_name::#factory_name(#( #header_accesses, )* &self.#tail_ident) },
            vec![],
        ),
    };

    let mut clone_bounds: Vec<TokenStream> = struct_info
        .header_fields
        .iter()
        .map(|field| {
            let ty = &field.ty;
            quote! { #ty: ::core::clone::Clone }
        })
        .collect();
    clone_bounds.extend(extra_bounds);

    let existing_predicates: Vec<TokenStream> = struct_info
        .struct_generics
        .where_clause
        .as_ref()
        .map_or_else(Vec::new, |wc| wc.predicates.iter().map(|p| quote! { #p }).collect());

    let all_bounds: Vec<TokenStream> = existing_predicates.into_iter().chain(clone_bounds).collect();

    let (impl_generics, ty_generics, _) = struct_info.struct_generics.split_for_impl();

    let where_clause = if all_bounds.is_empty() {
        quote! {}
    } else {
        quote! { where #( #all_bounds ),* }
    };

    quote! {
        impl #impl_generics ::core::clone::Clone for #box_path<#struct_name #ty_generics> #where_clause {
            fn clone(&self) -> Self {
                #factory_call
            }
        }
    }
}

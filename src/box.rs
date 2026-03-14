use proc_macro2::TokenStream;
use quote::quote;
use syn::{Ident, Type};

use crate::common::{StructInfo, TailKind, box_path, dealloc_path};
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

fn merged_where_clause(struct_info: &StructInfo, extra_bounds: Vec<TokenStream>) -> TokenStream {
    let existing_predicates: Vec<TokenStream> = struct_info
        .struct_generics
        .where_clause
        .as_ref()
        .map_or_else(Vec::new, |wc| wc.predicates.iter().map(|predicate| quote! { #predicate }).collect());

    let all_bounds: Vec<TokenStream> = existing_predicates.into_iter().chain(extra_bounds).collect();

    if all_bounds.is_empty() {
        quote! {}
    } else {
        quote! { where #( #all_bounds ),* }
    }
}

fn header_bounds(struct_info: &StructInfo, bound: &TokenStream) -> Vec<TokenStream> {
    struct_info
        .header_fields
        .iter()
        .map(|field| {
            let ty = &field.ty;
            quote! { #ty: #bound }
        })
        .collect()
}

fn tail_bound(struct_info: &StructInfo, bound: &TokenStream) -> Option<TokenStream> {
    match &struct_info.tail_kind {
        TailKind::Slice(elem_type) => Some(quote! { #elem_type: #bound }),
        TailKind::TraitObject(_) => {
            let tail_ty = &struct_info.tail_field.ty;
            Some(quote! { #tail_ty: #bound })
        }
        TailKind::Str => None, // str always implements Debug/PartialEq/Eq/Hash
    }
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

    let (factory_call, mut clone_bounds) = match clone_tail {
        CloneTailKind::Slice(elem_type) => (
            quote! { #struct_name::#factory_name(#( #header_accesses, )* self.#tail_ident.iter().cloned()) },
            vec![quote! { #elem_type: ::core::clone::Clone }],
        ),
        CloneTailKind::Str => (
            quote! { #struct_name::#factory_name(#( #header_accesses, )* &self.#tail_ident) },
            vec![],
        ),
    };

    let mut prefixed_bounds = header_bounds(struct_info, &quote! { ::core::clone::Clone });
    prefixed_bounds.append(&mut clone_bounds);
    clone_bounds = prefixed_bounds;

    let (impl_generics, ty_generics, _) = struct_info.struct_generics.split_for_impl();
    let where_clause = merged_where_clause(struct_info, clone_bounds);

    quote! {
        impl #impl_generics ::core::clone::Clone for #box_path<#struct_name #ty_generics> #where_clause {
            fn clone(&self) -> Self {
                #factory_call
            }
        }
    }
}

pub fn gen_debug(struct_info: &StructInfo) -> TokenStream {
    let struct_name = struct_info.struct_name;
    let (impl_generics, ty_generics, _) = struct_info.struct_generics.split_for_impl();

    let mut debug_bounds = header_bounds(struct_info, &quote! { ::core::fmt::Debug });
    debug_bounds.extend(tail_bound(struct_info, &quote! { ::core::fmt::Debug }));
    let where_clause = merged_where_clause(struct_info, debug_bounds);

    struct_info.tail_field.ident.as_ref().map_or_else(
        || {
            let header_fields: Vec<TokenStream> = struct_info
                .header_field_idents
                .iter()
                .map(|ident| quote! { .field(&self.#ident) })
                .collect();
            let tail_ident = &struct_info.tail_field_ident;

            quote! {
                impl #impl_generics ::core::fmt::Debug for #struct_name #ty_generics #where_clause {
                    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                        f.debug_tuple(::core::stringify!(#struct_name))
                            #( #header_fields )*
                            .field(&&self.#tail_ident)
                            .finish()
                    }
                }
            }
        },
        |tail_ident| {
            let header_fields: Vec<TokenStream> = struct_info
                .header_fields
                .iter()
                .filter_map(|field| field.ident.as_ref())
                .map(|field_ident| {
                    let field_name = field_ident.to_string();
                    quote! { .field(#field_name, &self.#field_ident) }
                })
                .collect();
            let tail_name = tail_ident.to_string();

            quote! {
                impl #impl_generics ::core::fmt::Debug for #struct_name #ty_generics #where_clause {
                    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                        f.debug_struct(::core::stringify!(#struct_name))
                            #( #header_fields )*
                            .field(#tail_name, &&self.#tail_ident)
                            .finish()
                    }
                }
            }
        },
    )
}

pub fn gen_partial_eq(struct_info: &StructInfo) -> TokenStream {
    let struct_name = struct_info.struct_name;
    let (impl_generics, ty_generics, _) = struct_info.struct_generics.split_for_impl();

    let mut partial_eq_bounds = header_bounds(struct_info, &quote! { ::core::cmp::PartialEq });
    partial_eq_bounds.extend(tail_bound(struct_info, &quote! { ::core::cmp::PartialEq }));
    let where_clause = merged_where_clause(struct_info, partial_eq_bounds);

    let comparisons: Vec<TokenStream> = struct_info
        .header_field_idents
        .iter()
        .map(|ident| quote! { self.#ident == other.#ident })
        .chain(::core::iter::once({
            let tail_ident = &struct_info.tail_field_ident;
            quote! { self.#tail_ident == other.#tail_ident }
        }))
        .collect();

    quote! {
        impl #impl_generics ::core::cmp::PartialEq for #struct_name #ty_generics #where_clause {
            fn eq(&self, other: &Self) -> bool {
                #( #comparisons )&&*
            }
        }
    }
}

pub fn gen_eq(struct_info: &StructInfo) -> TokenStream {
    let struct_name = struct_info.struct_name;
    let (impl_generics, ty_generics, _) = struct_info.struct_generics.split_for_impl();

    let mut eq_bounds = header_bounds(struct_info, &quote! { ::core::cmp::Eq });
    eq_bounds.extend(tail_bound(struct_info, &quote! { ::core::cmp::Eq }));
    let where_clause = merged_where_clause(struct_info, eq_bounds);

    quote! {
        impl #impl_generics ::core::cmp::Eq for #struct_name #ty_generics #where_clause {}
    }
}

pub fn gen_hash(struct_info: &StructInfo) -> TokenStream {
    let struct_name = struct_info.struct_name;
    let (impl_generics, ty_generics, _) = struct_info.struct_generics.split_for_impl();

    let mut hash_bounds = header_bounds(struct_info, &quote! { ::core::hash::Hash });
    hash_bounds.extend(tail_bound(struct_info, &quote! { ::core::hash::Hash }));
    let where_clause = merged_where_clause(struct_info, hash_bounds);

    let header_hashes: Vec<TokenStream> = struct_info
        .header_field_idents
        .iter()
        .map(|ident| quote! { ::core::hash::Hash::hash(&self.#ident, state); })
        .collect();
    let tail_ident = &struct_info.tail_field_ident;

    quote! {
        impl #impl_generics ::core::hash::Hash for #struct_name #ty_generics #where_clause {
            fn hash<H>(&self, state: &mut H)
            where
                H: ::core::hash::Hasher,
            {
                #( #header_hashes )*
                ::core::hash::Hash::hash(&self.#tail_ident, state);
            }
        }
    }
}

pub fn gen_partial_ord(struct_info: &StructInfo) -> TokenStream {
    let struct_name = struct_info.struct_name;
    let (impl_generics, ty_generics, _) = struct_info.struct_generics.split_for_impl();

    let mut bounds = header_bounds(struct_info, &quote! { ::core::cmp::PartialOrd });
    bounds.extend(tail_bound(struct_info, &quote! { ::core::cmp::PartialOrd }));
    let where_clause = merged_where_clause(struct_info, bounds);

    let field_chains: Vec<TokenStream> = struct_info
        .header_field_idents
        .iter()
        .map(|ident| {
            quote! {
                match ::core::cmp::PartialOrd::partial_cmp(&self.#ident, &other.#ident) {
                    ::core::option::Option::Some(::core::cmp::Ordering::Equal) => {}
                    ord => return ord,
                }
            }
        })
        .collect();
    let tail_ident = &struct_info.tail_field_ident;

    quote! {
        impl #impl_generics ::core::cmp::PartialOrd for #struct_name #ty_generics #where_clause {
            fn partial_cmp(&self, other: &Self) -> ::core::option::Option<::core::cmp::Ordering> {
                #( #field_chains )*
                ::core::cmp::PartialOrd::partial_cmp(&self.#tail_ident, &other.#tail_ident)
            }
        }
    }
}

pub fn gen_ord(struct_info: &StructInfo) -> TokenStream {
    let struct_name = struct_info.struct_name;
    let (impl_generics, ty_generics, _) = struct_info.struct_generics.split_for_impl();

    let mut bounds = header_bounds(struct_info, &quote! { ::core::cmp::Ord });
    bounds.extend(tail_bound(struct_info, &quote! { ::core::cmp::Ord }));
    let where_clause = merged_where_clause(struct_info, bounds);

    let field_chains: Vec<TokenStream> = struct_info
        .header_field_idents
        .iter()
        .map(|ident| {
            quote! {
                match ::core::cmp::Ord::cmp(&self.#ident, &other.#ident) {
                    ::core::cmp::Ordering::Equal => {}
                    ord => return ord,
                }
            }
        })
        .collect();
    let tail_ident = &struct_info.tail_field_ident;

    quote! {
        impl #impl_generics ::core::cmp::Ord for #struct_name #ty_generics #where_clause {
            fn cmp(&self, other: &Self) -> ::core::cmp::Ordering {
                #( #field_chains )*
                ::core::cmp::Ord::cmp(&self.#tail_ident, &other.#tail_ident)
            }
        }
    }
}

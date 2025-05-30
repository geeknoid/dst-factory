//! C-like [flexible array members](https://en.wikipedia.org/wiki/Flexible_array_member) for Rust.
//!
//! This crate lets you allocate variable data inline at the end of a struct. If you have a
//! struct that gets allocated on the heap and has some variable-length data associated with it
//! (like a string or an array), then you can allocate this data directly inline with the struct.
//! This saves memory by avoiding the need for a pointer and a separate allocation, and saves CPU
//! cycles by eliminating the need for indirection when accessing the data.
//!
//! Rust supports the notion of [Dynamically Sized Types](https://doc.rust-lang.org/reference/dynamically-sized-types.html), known as DSTs,
//! which are types that have a size not known at compile time. DSTs are perfect to implement
//! flexible array members. But unfortunately, Rust doesn't provide an out-of-the-box way to allocate
//! instances of such types. This is where this crate comes in.
//!
//! You can apply the #[[`macro@make_dst_factory`]] attribute to your DST struct which causes factory
//! functions to be produced that let you easily and safely create instances of your DST.
//!
//! # Why Should You Care?
//!
//! Dynamically sized types aren't for everyone. You can't put hold them as local variables
//! or put them in arrays or vectors, so they can be inconvenient to use. However, their value
//! lies in situations where you have a lot of heap-allocated objects, as they can substantially
//! reduce the memory footprint of your application. If you're building graphs, tress, or other
//! dynamic data structures, you can often leverage DSTs to keep your individual nodes smaller
//! and more efficient.
//!
//! # Examples
//!
//! Here's an example using an array as the last field of a struct:
//!
//! ```rust
//! use dst_factory::make_dst_factory;
//!
//! #[make_dst_factory]
//! struct User {
//!     age: u32,
//!     signing_key: [u8],
//! }
//!
//! // allocate one user with a 4 byte key
//! let a = User::build(33, &[0, 1, 2, 3]);
//!
//! // allocate another user with a 5 byte key
//! let b = User::build(33, &[0, 1, 2, 3, 4]);
//!
//! // allocate another user, this time using an iterator
//! let v = vec![0u8, 1, 2, 3, 4];
//! let iter = v.into_iter();
//! let c = User::build_from_iter(33, iter);
//! ```
//! Here's another example, this time using a string as the last field of a struct:
//!
//! ```rust
//! use dst_factory::make_dst_factory;
//!
//! #[make_dst_factory]
//! struct User {
//!     age: u32,
//!     name: str,
//! }
//!
//! // allocate one user with a 5-character string
//! let a = User::build(33, "Alice");
//!
//! // allocate another user with a 3-character string
//! let b = User::build(33, "Bob");
//! ```
//! And finally, here's an example using a trait object as the last field of a struct:
//! ```rust
//! use dst_factory::make_dst_factory;
//!
//! // a trait we'll use in our DST
//! trait NumberProducer {
//!    fn get_number(&self) -> u32;
//! }
//!
//! // an implementation of the trait we're going to use
//! struct FortyTwoProducer {}
//! impl NumberProducer for FortyTwoProducer {
//!    fn get_number(&self) -> u32 {
//!        42
//!    }
//! }
//!
//! // another implementation of the trait we're going to use
//! struct TenProducer {}
//! impl NumberProducer for TenProducer {
//!    fn get_number(&self) -> u32 {
//!        10
//!    }
//! }
//!
//! #[make_dst_factory]
//! struct Node {
//!     count: u32,
//!     producer: dyn NumberProducer,
//! }
//!
//! // allocate an instance with one implementation of the trait
//! let a = Node::build(33, FortyTwoProducer{});
//! assert_eq!(42, a.producer.get_number());
//!
//! // allocate an instance with another implementation of the trait
//! let b = Node::build(33, TenProducer{});
//! assert_eq!(10, b.producer.get_number());
//! ```
//!
//! Because DSTs don't have a known size at compile time, you can't store them on the stack,
//! and you can't pass them by value. As a result of these constraints, the factory functions
//! always return boxed instances of the structs.
//!
//! # Attribute Features
//!
//! The common use case for the `#[make_dst_factory]` attribute is to not pass any arguments.
//! This results in factory functions called `build` when using a string or dynamic trait as the
//! last field of the struct, and `build` and `build_from_iter` when using an array as the last
//! field of the struct.
//!
//! The generated functions are private by default and have the following signatures:
//!
//! ```ignore
//! // for arrays
//! fn build(field1, field2, ..., last_field: &[last_field_type]) -> Box<Self>
//! where
//!     last_field_type: Clone;
//!
//! fn build_from_iter<G>(field1, field2, ..., last_field: G) -> Box<Self>
//! where
//!     G: IntoIterator<Item = last_field_type>,
//!     <G as IntoIterator>::IntoIter: ExactSizeIterator,
//!
//! // for strings
//! fn build(field1, field2, ..., last_field: impl AsRef<str>) -> Box<Self>;
//!
//! // for traits
//! fn build(field1, field2, ..., last_field: G) -> Box<Self>
//! where
//!     G: TraitName + Sized;
//! ```
//!
//! The attribute lets you control the name of the generated functions, their
//! visibility, and whether to generate code for the `no_std` environment. The general
//! grammar is:
//!
//! ```ignore
//! #[make_dst_factory(<base_factory_name>[, <visibility>] [, no_std] [, generic=<generic_name>])]
//! ```
//!
//! Some examples:
//!
//! ```ignore
//! // The factory functions will be private and called `create` and `create_from_iter`
//! #[make_dst_factory(create)]
//!
//! // The factory functions will be public and called `create` and `create_from_iter`
//! #[make_dst_factory(create, pub)]
//!
//! // The factory functions will be private, called `create` and `create_from_iter`, and support the `no_std` environment
//! #[make_dst_factory(create, no_std)]
//!
//! // The factory functions will be private, called `create` and `create_from_iter`,
//! // support the `no_std` environment, and will have generic types called `X`.
//! #[make_dst_factory(create, no_std, generic=X)]
//! ```
//!
//! # Error Conditions
//!
//! The #[[`macro@make_dst_factory`]] attribute produces a compile-time error if:
//!
//! - It's applied to anything other than a struct with named fields.
//! - Its arguments are malformed (e.g., incorrect visibility keyword, too many arguments).
//! - The struct has no fields.
//! - The last field of the struct is not a slice (`[T]`), a string (`str`), or a trait object (`dyn Trait`).
//! - The resulting struct exceeds the maximum size allowed of `isize::MAX`.

extern crate proc_macro;
mod macro_args;

use macro_args::MacroArgs;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use syn::token::Where;
use syn::{
    Field, Fields, Generics, Ident, Index, ItemStruct, Type, TypePath, TypeSlice, TypeTraitObject,
    parse::Result as SynResult, punctuated::Punctuated, spanned::Spanned,
};

enum TailKind {
    Slice(TypeSlice),
    Str,
    TraitObject(TypeTraitObject),
}

struct StructInfo<'a> {
    struct_name_ident: &'a Ident,
    struct_generics: &'a Generics,
    header_fields: Box<[&'a Field]>,
    header_field_idents: Box<[&'a Ident]>,
    tail_field: &'a Field,
    tail_field_ident: &'a Ident,
    tail_kind: TailKind,
}

impl StructInfo<'_> {
    fn new(input_struct: &ItemStruct) -> SynResult<StructInfo> {
        match &input_struct.fields {
            Fields::Named(named_fields) => {
                if named_fields.named.is_empty() {
                    return Err(syn::Error::new_spanned(
                        named_fields,
                        "Struct must have at least one field",
                    ));
                }

                // Split fields into header fields (all but last) and the tail field (last)
                let mut fields_iter = named_fields.named.iter();
                let last_field = fields_iter.next_back().unwrap(); // Safe because we checked for emptiness above
                let mut tail_kind = TailKind::Str;

                // Verify the last field is a DST with unsized type [T], str, or dyn Trait
                match &last_field.ty {
                    Type::Path(type_path) if type_path.path.is_ident("str") => {}
                    Type::Path(TypePath { path, .. })
                        if path.segments.last().is_some_and(|s| s.ident == "str") => {}

                    Type::TraitObject(trait_object) => {
                        tail_kind = TailKind::TraitObject(trait_object.clone());
                    }
                    Type::Slice(type_slice) => tail_kind = TailKind::Slice(type_slice.clone()),

                    _ => {
                        return Err(syn::Error::new_spanned(
                            &last_field.ty,
                            "Last field must be a dynamically sized type like [T], str, or dyn Trait",
                        ));
                    }
                }

                let mut header_fields = Vec::new();
                let mut header_field_idents = Vec::new();

                let num_fields = named_fields.named.len();
                for (i, field) in named_fields.named.iter().enumerate() {
                    if i < num_fields - 1 {
                        header_fields.push(field);
                        header_field_idents.push(field.ident.as_ref().unwrap());
                    }
                }

                let tail_field = named_fields.named.last().unwrap();

                Ok(StructInfo {
                    struct_name_ident: &input_struct.ident,
                    struct_generics: &input_struct.generics,
                    header_fields: header_fields.into_boxed_slice(),
                    header_field_idents: header_field_idents.into_boxed_slice(),
                    tail_field,
                    tail_field_ident: tail_field.ident.as_ref().unwrap(),
                    tail_kind,
                })
            }

            Fields::Unnamed(_) | Fields::Unit => Err(syn::Error::new_spanned(
                &input_struct.fields,
                "Structs with unnamed fields are not supported",
            )),
        }
    }
}

fn generate_header_layout_tokens(header_fields: &[&Field]) -> TokenStream {
    let mut header_layout_tokens = quote! {
        let layout = ::core::alloc::Layout::from_size_align(0, 1).unwrap();
    };

    // Extend layout for each field.
    for field in header_fields {
        let field_ty = &field.ty;
        let field_span = field_ty.span();
        header_layout_tokens.extend(quote_spanned! {field_span =>
            let layout = layout.extend(::core::alloc::Layout::new::<#field_ty>()).unwrap().0;
        });
    }

    header_layout_tokens
}

fn generate_tail_layout_tokens<T: quote::ToTokens>(tail_type: &T, span: Span) -> TokenStream {
    quote_spanned! { span => ::core::alloc::Layout::array::<#tail_type>(len).expect("Array exceeds maximum size allowed of isize::MAX") }
}

fn generate_guard_type(macro_args: &MacroArgs) -> TokenStream {
    let dealloc_path = if macro_args.no_std {
        quote! { ::alloc::alloc::dealloc }
    } else {
        quote! { ::std::alloc::dealloc }
    };

    quote! {
        struct Guard<T> {
            mem_ptr: *mut u8,
            tail_ptr: *mut T,
            initialized: usize,
            layout: ::core::alloc::Layout,
        }

        impl<T> Drop for Guard<T> {
            fn drop(&mut self) {
                unsafe {
                    let slice_ptr = ::core::ptr::slice_from_raw_parts_mut(self.tail_ptr, self.initialized);
                    ::core::ptr::drop_in_place(slice_ptr);
                    #dealloc_path(self.mem_ptr, self.layout);
                }
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn generate_factory_common(
    macro_args: &MacroArgs,
    struct_info: &StructInfo<'_>,
    factory_name_ident: &Ident,
    factory_doc_string: &String,
    factory_generics_tokens: Option<&TokenStream>,
    factory_where_clause_tokens: Option<&TokenStream>,
    guard_type_tokens: Option<&TokenStream>,
    args_tuple_ident: &Ident,
    header_layout_tokens: &TokenStream,
    tail_param_tokens: &TokenStream,
    tail_layout_tokens: &TokenStream,
    tail_processing_tokens: &TokenStream,
    tail_write_tokens: &TokenStream,
    fat_ptr_tokens: &TokenStream,
) -> TokenStream {
    let MacroArgs {
        visibility: factory_visibility,
        no_std,
        ..
    } = macro_args;

    let StructInfo {
        header_field_idents,
        tail_field_ident,
        ..
    } = struct_info;

    let (box_path, alloc_path, handle_alloc_error) = if *no_std {
        (
            quote! { ::alloc::boxed::Box },
            quote! { ::alloc::alloc::alloc },
            quote! { panic!("out of memory") },
        )
    } else {
        (
            quote! { ::std::boxed::Box },
            quote! { ::std::alloc::alloc },
            quote! { ::std::alloc::handle_alloc_error(layout) },
        )
    };

    let tuple_elements_for_assignment = header_field_idents
        .iter()
        .map(|ident| quote! { #ident })
        .chain(std::iter::once(quote! { #tail_field_ident }))
        .collect::<Vec<_>>();

    let tuple_assignment = quote! {
        let #args_tuple_ident = ( #( #tuple_elements_for_assignment, )* );
    };

    let header_field_writes =
        header_field_idents
            .iter()
            .enumerate()
            .map(|(idx, field_ident_on_struct)| {
                let tuple_idx = syn::Index::from(idx);
                quote! { ::core::ptr::write(&mut ((*fat_ptr).#field_ident_on_struct), #args_tuple_ident.#tuple_idx);}
            });

    let mut header_params_tokens = Vec::new();
    for field in &struct_info.header_fields {
        let field_ident = field.ident.as_ref().unwrap();
        let field_ty = &field.ty;
        header_params_tokens.push(quote! { #field_ident: #field_ty });
    }

    quote! {
        #[doc = #factory_doc_string]
        #[allow(clippy::let_unit_value)]
        #[allow(clippy::zst_offset)]
        #[allow(clippy::transmute_undefined_repr)]
        #factory_visibility fn #factory_name_ident #factory_generics_tokens (
            #( #header_params_tokens, )*
            #tail_param_tokens
        ) -> #box_path<Self> #factory_where_clause_tokens {
            #guard_type_tokens

            // Assign the args to a tuple with a unique name to avoid conflicts with arg names in the
            // rest of this function
            #tuple_assignment

            #tail_processing_tokens

            // Calculate the total size of the struct, including the header and tail fields
            #header_layout_tokens
            let layout = layout.extend(#tail_layout_tokens).expect("Struct exceeds maximum size allowed of isize::MAX").0;
            let layout = layout.pad_to_align();

            unsafe {
                if layout.size() == 0 {
                    // Handle ZST case
                    let mem_ptr = ::core::ptr::NonNull::<()>::dangling().as_ptr();
                    let fat_ptr = ::core::mem::transmute::<(*mut (), usize), *mut Self>((mem_ptr, 0));
                    #box_path::from_raw(fat_ptr)
                } else {
                    let mem_ptr = #alloc_path(layout);
                    if mem_ptr.is_null() {
                        #handle_alloc_error
                    }

                    #fat_ptr_tokens

                    #( #header_field_writes )*

                    #tail_write_tokens

                    ::core::debug_assert_eq!(::core::alloc::Layout::for_value(&*fat_ptr), layout);
                    #box_path::from_raw(fat_ptr)
                }
            }
        }
    }
}

fn generate_factory_for_slice_arg(
    macro_args: &MacroArgs,
    struct_info: &StructInfo,
    header_layout_tokens: &TokenStream,
    args_tuple_ident: &Ident,
    tail_param_tuple_idx: &Index,
    tail_type: &Type,
) -> TokenStream {
    let struct_name_ident = &struct_info.struct_name_ident;
    let factory_name_ident = &macro_args.base_factory_name;
    let factory_doc_string =
        format!("Creates an instance of `Box<{struct_name_ident}>` from a slice.");

    let tail_field_ident = &struct_info.tail_field_ident;
    let tail_param_tokens = quote! { #tail_field_ident: &[#tail_type] };

    let clone_predicate_tokens: syn::WherePredicate = syn::parse_quote_spanned! {tail_type.span()=>
        #tail_type: ::core::clone::Clone
    };

    let mut final_where_clause = struct_info
        .struct_generics
        .where_clause
        .clone()
        .unwrap_or_else(|| syn::WhereClause {
            where_token: Where::default(),
            predicates: Punctuated::new(),
        });

    final_where_clause.predicates.push(clone_predicate_tokens);

    let factory_where_clause_tokens = Some(quote! { #final_where_clause });

    let tail_processing_tokens = quote! {
        let len = #args_tuple_ident.#tail_param_tuple_idx.len();
    };

    let tail_layout_tokens =
        generate_tail_layout_tokens(tail_type, struct_info.tail_field.ty.span());

    let tail_write_tokens = quote_spanned! {tail_type.span()=>
        // Clone the slice content into the tail field
        let tail_ptr = ::core::ptr::addr_of_mut!((*fat_ptr).#tail_field_ident).cast::<#tail_type>();
        let mut guard = Guard { mem_ptr, tail_ptr, layout, initialized: 0 };
        for idx in 0..len {
            let src_val_ref = #args_tuple_ident.#tail_param_tuple_idx.get_unchecked(idx);
            #[allow(clippy::clone_on_copy)]
            let cloned_val = src_val_ref.clone();
            ::core::ptr::write(tail_ptr.add(idx), cloned_val);
            guard.initialized += 1;
        }
        ::std::mem::forget(guard);
    };

    let fat_ptr_tokens = quote! {
        let fat_ptr = ::core::mem::transmute::<(*mut u8, usize), *mut Self>((mem_ptr, len));
    };

    generate_factory_common(
        macro_args,
        struct_info,
        factory_name_ident,
        &factory_doc_string,
        None, // factory_generics_tokens
        factory_where_clause_tokens.as_ref(),
        Some(&generate_guard_type(macro_args)),
        args_tuple_ident,
        header_layout_tokens,
        &tail_param_tokens,
        &tail_layout_tokens,
        &tail_processing_tokens,
        &tail_write_tokens,
        &fat_ptr_tokens,
    )
}

fn generate_factory_for_iter_arg(
    macro_args: &MacroArgs,
    struct_info: &StructInfo,
    header_layout_tokens: &TokenStream,
    args_tuple_ident: &Ident,
    tail_param_tuple_idx: &Index,
    tail_type: &Type,
) -> TokenStream {
    let struct_name_ident = &struct_info.struct_name_ident;
    let factory_name_ident = format_ident!("{}_from_iter", &macro_args.base_factory_name);
    let factory_doc_string =
        format!("Creates an instance of `Box<{struct_name_ident}>` from an iterator.");

    let iter_generic_param_ident = &macro_args.generic_name;
    let tail_field_ident = &struct_info.tail_field_ident;
    let tail_param_tokens = quote! { #tail_field_ident: #iter_generic_param_ident };
    let factory_generics_tokens = Some(quote! { <#iter_generic_param_ident> });
    let factory_where_clause_tokens = Some(quote! {
        where
            #iter_generic_param_ident: ::core::iter::IntoIterator<Item = #tail_type>,
            <#iter_generic_param_ident as ::core::iter::IntoIterator>::IntoIter: ::core::iter::ExactSizeIterator
    });

    let tail_processing_tokens = quote! {
        let iter = #args_tuple_ident.#tail_param_tuple_idx.into_iter();
        let len = iter.len();
    };

    let tail_layout_tokens =
        generate_tail_layout_tokens(tail_type, struct_info.tail_field.ty.span());

    let tail_write_tokens = quote! {
        // Write each element from the iterator into the tail field
        let tail_ptr = ::core::ptr::addr_of_mut!((*fat_ptr).#tail_field_ident).cast::<#tail_type>();
        let mut guard = Guard { mem_ptr, tail_ptr, layout, initialized: 0 };
        iter.for_each(|element| {
            if guard.initialized == len {
                panic!("Mismatch between iterator-reported length and the number of items produced by the iterator");
            }

            ::core::ptr::write(tail_ptr.add(guard.initialized), element);
            guard.initialized += 1;
        });

        if guard.initialized != len {
            panic!("Mismatch between iterator-reported length and the number of items produced by the iterator");
        }

        ::std::mem::forget(guard);
    };

    let fat_ptr_tokens = quote! {
        let fat_ptr = ::core::mem::transmute::<(*mut u8, usize), *mut Self>((mem_ptr, len));
    };

    generate_factory_common(
        macro_args,
        struct_info,
        &factory_name_ident,
        &factory_doc_string,
        factory_generics_tokens.as_ref(),
        factory_where_clause_tokens.as_ref(),
        Some(&generate_guard_type(macro_args)),
        args_tuple_ident,
        header_layout_tokens,
        &tail_param_tokens,
        &tail_layout_tokens,
        &tail_processing_tokens,
        &tail_write_tokens,
        &fat_ptr_tokens,
    )
}

fn generate_factory_for_str_arg(
    macro_args: &MacroArgs,
    struct_info: &StructInfo,
    header_layout_tokens: &TokenStream,
    args_tuple_ident: &Ident,
    tail_param_tuple_idx: &Index,
) -> TokenStream {
    let struct_name_ident = &struct_info.struct_name_ident;
    let factory_name_ident = &macro_args.base_factory_name;
    let factory_doc_string =
        format!("Creates an instance of `Box<{struct_name_ident}>` from a string slice.");

    let tail_field_ident = &struct_info.tail_field_ident;
    let tail_param_tokens = quote! { #tail_field_ident: impl ::core::convert::AsRef<str> };

    let tail_type = &struct_info.tail_field.ty;
    let tail_processing_tokens = quote! {
        // Ensure the tail type is an actual string
        ::core::assert_eq!(::core::any::TypeId::of::<#tail_type>(), ::core::any::TypeId::of::<str>());
        let s = #args_tuple_ident.#tail_param_tuple_idx.as_ref();
        let len = s.len();
    };

    let tail_layout_tokens =
        generate_tail_layout_tokens(&quote! { u8 }, struct_info.tail_field.ty.span());

    let tail_write_tokens = quote! {
        // copy the string data into the tail field
        let tail_ptr = (&raw mut (*fat_ptr).#tail_field_ident).cast::<u8>();
        ::core::ptr::copy_nonoverlapping(s.as_ptr(), tail_ptr, len);
    };

    let fat_ptr_tokens = quote! {
        let fat_ptr = ::core::mem::transmute::<(*mut u8, usize), *mut Self>((mem_ptr, len));
    };

    generate_factory_common(
        macro_args,
        struct_info,
        factory_name_ident,
        &factory_doc_string,
        None, // factory_generics_tokens
        None, // factory_where_clause_tokens
        None, // guard_type_tokens
        args_tuple_ident,
        header_layout_tokens,
        &tail_param_tokens,
        &tail_layout_tokens, // factory_where_clause_tokens
        &tail_processing_tokens,
        &tail_write_tokens,
        &fat_ptr_tokens,
    )
}

fn generate_factory_for_trait_arg(
    macro_args: &MacroArgs,
    struct_info: &StructInfo,
    type_trait_object: &TypeTraitObject,
    args_tuple_ident: &Ident,
    header_layout_tokens: &TokenStream,
    tail_param_tuple_idx: &Index,
) -> SynResult<TokenStream> {
    // Verify that the trait object is not a higher-rank trait bound
    for bound in &type_trait_object.bounds {
        if let syn::TypeParamBound::Trait(trait_bound) = bound {
            if trait_bound.lifetimes.is_some() {
                return Err(syn::Error::new_spanned(
                    trait_bound,
                    "Higher-rank trait bounds (e.g., `for<'a> dyn Trait<'a>`) are not supported for the tail field.",
                ));
            }
        }
    }

    // Extract the primary trait path from the bounds.
    let trait_path = type_trait_object
        .bounds
        .iter()
        .find_map(|bound| {
            if let syn::TypeParamBound::Trait(trait_bound) = bound {
                Some(&trait_bound.path)
            } else {
                None // Not a trait bound (e.g., a lifetime bound like `'a`).
            }
        })
        .unwrap();

    let factory_name_ident = &macro_args.base_factory_name;
    let factory_doc_string = format!(
        "Builds an instance of `Box<{}>`.",
        struct_info.struct_name_ident,
    );

    let trait_generic = &macro_args.generic_name;
    let factory_generics_tokens = Some(quote! { <#trait_generic> });
    let factory_where_clause_tokens = Some(quote! { where #trait_generic: #trait_path + Sized });
    let tail_field_ident = &struct_info.tail_field_ident;
    let tail_param_tokens = quote! { #tail_field_ident: #trait_generic };
    let tail_layout_tokens = quote! { ::core::alloc::Layout::new::<#trait_generic>() };

    let tail_processing_tokens = quote! {
        let s = #args_tuple_ident.#tail_param_tuple_idx;
        let trait_object: &dyn #trait_path = &s;
        let (_, vtable): (*const #trait_generic, *const ()) = unsafe { ::core::mem::transmute(trait_object) };
    };

    let tail_write_tokens = quote! {
        // copy the instance data into the tail field
        let tail_ptr = (&raw mut (*fat_ptr).#tail_field_ident).cast::<#trait_generic>();
        ::core::ptr::copy_nonoverlapping(::core::ptr::addr_of!(s), tail_ptr, 1);
    };

    let fat_ptr_tokens = quote! {
        let fat_ptr = ::core::mem::transmute::<(*mut u8, *const ()), *mut Self>((mem_ptr, vtable));
    };

    Ok(generate_factory_common(
        macro_args,
        struct_info,
        factory_name_ident, // This is the Ident for the factory function name
        &factory_doc_string,
        factory_generics_tokens.as_ref(),
        factory_where_clause_tokens.as_ref(),
        None, // guard_type_tokens
        args_tuple_ident,
        header_layout_tokens,
        &tail_param_tokens,
        &tail_layout_tokens,
        &tail_processing_tokens,
        &tail_write_tokens,
        &fat_ptr_tokens,
    ))
}

fn make_dst_factory_impl(attr_args: TokenStream, item: TokenStream) -> SynResult<TokenStream> {
    let macro_args = MacroArgs::parse(attr_args)?;
    let input_struct: ItemStruct = syn::parse2(item)?;

    let struct_info = StructInfo::new(&input_struct)?;
    let (impl_generics, ty_generics, where_clause) = struct_info.struct_generics.split_for_impl();
    let header_layout_tokens = generate_header_layout_tokens(&struct_info.header_fields);
    let mut generated_factories = Vec::new();

    let args_tuple_ident = format_ident!("args");
    let num_header_fields = struct_info.header_field_idents.len();
    let tail_param_tuple_idx = syn::Index::from(num_header_fields);

    match &struct_info.tail_kind {
        TailKind::Slice(type_slice) => {
            generated_factories.push(generate_factory_for_slice_arg(
                &macro_args,
                &struct_info,
                &header_layout_tokens,
                &args_tuple_ident,
                &tail_param_tuple_idx,
                &type_slice.elem,
            ));

            generated_factories.push(generate_factory_for_iter_arg(
                &macro_args,
                &struct_info,
                &header_layout_tokens,
                &args_tuple_ident,
                &tail_param_tuple_idx,
                &type_slice.elem,
            ));
        }

        TailKind::Str => {
            generated_factories.push(generate_factory_for_str_arg(
                &macro_args,
                &struct_info,
                &header_layout_tokens,
                &args_tuple_ident,
                &tail_param_tuple_idx,
            ));
        }

        TailKind::TraitObject(type_trait_object) => {
            generated_factories.push(generate_factory_for_trait_arg(
                &macro_args,
                &struct_info,
                type_trait_object,
                &args_tuple_ident,
                &header_layout_tokens,
                &tail_param_tuple_idx,
            )?);
        }
    }

    let struct_name_ident = struct_info.struct_name_ident;
    Ok(quote! {
        #[repr(C)]
        #input_struct

        impl #impl_generics #struct_name_ident #ty_generics #where_clause {
            #( #generated_factories )*
        }
    })
}

/// Generate factory functions for dynamically sized types (DST) structs.
///
/// This macro, when applied to a struct whose last field is a slice (`[T]`), `str`, or `dyn Trait`,
/// generates an `impl` block with functions to construct instances of the structs with
/// dynamically sized tail data.
///
/// This attribute is generally used without any arguments, but more advanced
/// uses can pass arguments with the following grammar:
///
/// ```ignore
/// #[make_dst_factory(<base_factory_name>[, <visibility>] [, no_std] [, generic=<generic_name>])]
/// ```
/// Refer to the [crate-level documentation](crate) for more details and example uses.
///
/// # Usage
///
/// ```rust
/// use dst_factory::make_dst_factory;
///
/// #[make_dst_factory]
/// struct MyStruct {
///     id: u32,
///     data: [u8],
/// }
///
/// // With custom factory name and public visibility
/// #[make_dst_factory(create, pub)]
/// struct PublicStruct {
///     id: u32,
///     data: str,
/// }
///
/// trait MyTrait {};
///
/// // With a trait object as the tail field
/// #[make_dst_factory]
/// struct TraitStruct {
///     id: u32,
///     handler: dyn MyTrait,
/// }
/// ```
#[proc_macro_attribute]
pub fn make_dst_factory(
    attr_args: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let result = make_dst_factory_impl(attr_args.into(), item.into());
    result.unwrap_or_else(|err| err.to_compile_error()).into()
}

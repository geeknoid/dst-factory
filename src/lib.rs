//! Rich support to safely create instances of [Dynamically Sized Types](https://doc.rust-lang.org/reference/dynamically-sized-types.html).
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
//! You can apply the #[[`macro@make_dst_factory`]] attribute to your DST structs, which causes factory
//! functions to be produced that let you easily and safely create instances of your DSTs.
//!
//! # Why Should You Care?
//!
//! Dynamically sized types aren't for everyone. You can't use them as local variables
//! or put them in arrays or vectors, so they can be inconvenient to use. However, their value
//! lies in situations where you have a lot of heap-allocated objects, as they can substantially
//! reduce the memory footprint of your application. If you're building graphs, trees, or other
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
//! // allocate one user with a 4-byte key
//! let a = User::build(33, [0, 1, 2, 3]);
//!
//! // allocate another user with a 5-byte key
//! let b = User::build_from_slice(33, &[0, 1, 2, 3, 4]);
//!
//! // allocate another user, this time using an iterator
//! let v = vec![0, 1, 2, 3, 4];
//! let c = User::build(33, v.iter().copied());
//!
//! // destructure this user and compare its key to the vector
//! // this has the advantage of iterating over u8, not &u8 or &mut u8.
//! let (_age, signing_key) = User::destructure(c);
//! assert!(signing_key.eq(v.into_iter()));
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
//!     fn get_number(&self) -> u32;
//! }
//!
//! // an implementation of the trait we're going to use
//! struct FortyTwoProducer;
//! impl NumberProducer for FortyTwoProducer {
//!     fn get_number(&self) -> u32 {
//!         42
//!     }
//! }
//!
//! // another implementation of the trait we're going to use
//! struct TenProducer;
//! impl NumberProducer for TenProducer {
//!     fn get_number(&self) -> u32 {
//!         10
//!     }
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
//! The common use case for the #[[`macro@make_dst_factory`]] attribute is to not pass any arguments.
//! This results in a function called `build` when using a string or dynamic trait as the
//! last field of the struct, and the functions `build`, `build_from_slice`, and `destructure` when using an array as the last
//! field of the struct.
//!
//! The generated functions are private by default and have the following signatures:
//!
//! ```ignore
//! // for arrays
//! fn build<G>(field1, field2, ..., last_field: G) -> Box<Self>
//! where
//!     G: IntoIterator<Item = last_field_type>,
//!     <G as IntoIterator>::IntoIter: ExactSizeIterator,
//!
//! fn build_from_slice(field1, field2, ..., last_field: &[last_field_type]) -> Box<Self>
//! where
//!     last_field_type: Copy + Sized;
//!
//! fn destructure(this: Box<Self>) -> (Type1, Type2, ..., SelfIter);
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
//! #[make_dst_factory(<base_factory_name> [, destructurer=<destructurer_name>] [, iterator=<iterator_name>] [, <visibility>] [, no_std] [, deserialize] [, clone] [, generic=<generic_name>])]
//! ```
//!
//! Some examples:
//!
//! ```ignore
//! // The generated functions will be public and called `build`, `build_from_slice`, and `destructure`.
//! #[make_dst_factory(pub)]
//!
//! // The generated functions will be private and called `create`, `create_from_slice`, and `destructure`.
//! #[make_dst_factory(create)]
//!
//! // The generated functions will be private and called `create`, `create_from_slice`, and `destroy`.
//! #[make_dst_factory(create, destructurer = destroy)]
//!
//! // The generated functions will be public and called `create`, `create_from_slice`, and `destructure`
//! #[make_dst_factory(create, pub)]
//!
//! // The generated functions will be private, called `create`, `create_from_slice`, and `destructure`, and support the `no_std` environment
//! #[make_dst_factory(create, no_std)]
//!
//! // The generated functions will be private, called `create`, `create_from_slice`, and `destructure`,
//! // support the `no_std` environment, and will have generic types called `X`.
//! #[make_dst_factory(create, no_std, generic=X)]
//! ```
//!
//! # Other Features
//!
//! You can use the #[[`macro@make_dst_factory`]] attribute on structs with the normal Rust
//! representation or C representation (`#[repr(C)]`), with any padding and alignment
//! specification. See the Rust reference on [Type Layout](https://doc.rust-lang.org/reference/type-layout.html)
//! for more details.
//!
//! # Serde Support
//!
//! DST structs work naturally with `#[derive(Serialize)]` from serde, since serialization
//! only requires a reference. However, `#[derive(Deserialize)]` cannot work because the
//! standard derive tries to construct the struct directly, which is impossible for unsized types.
//!
//! Passing the `deserialize` flag in the attribute generates a
//! [`Deserialize`](https://docs.rs/serde/latest/serde/trait.Deserialize.html) implementation
//! for `Box<T>` that uses the macro-generated factory functions to construct the struct.
//! All standard `#[serde(...)]` field attributes (such as `rename`, `default`, `skip`, etc.)
//! are fully supported.
//!
//! ```ignore
//! use dst_factory::make_dst_factory;
//! use serde::Serialize;
//!
//! #[derive(Serialize)]
//! #[make_dst_factory(deserialize)]
//! struct Message {
//!     id: u32,
//!     text: str,
//! }
//!
//! // Serialize
//! let msg = Message::build(1, "hello");
//! let json = serde_json::to_string(&*msg).unwrap();
//!
//! // Deserialize
//! let restored: Box<Message> = serde_json::from_str(&json).unwrap();
//! assert_eq!(restored.id, 1);
//! assert_eq!(&restored.text, "hello");
//! ```
//!
//! Serde support works with both named and tuple structs, and with both `str` and `[T]`
//! slice tails. It is not supported for `dyn Trait` tails, since there is no way to
//! reconstruct the concrete type from serialized data.
//!
//! # Error Conditions
//!
//! The #[[`macro@make_dst_factory`]] attribute produces a compile-time error if:
//!
//! - It's applied to anything other than a regular struct or a tuple struct.
//! - Its arguments are malformed (e.g., incorrect visibility keyword, too many arguments).
//! - The struct has no fields.
//! - The last field of the struct is not a slice (`[T]`), a string (`str`), or a trait object (`dyn Trait`).
//! - The resulting struct exceeds the maximum size allowed of `isize::MAX`.
//! - The `deserialize` flag is used on a struct with a `dyn Trait` tail.
//! - The `clone` flag is used on a struct with a `dyn Trait` tail.
//!
//! # Clone Support
//!
//! Because DST structs are unsized, `#[derive(Clone)]` cannot work for `Box<T>` since the
//! standard derive doesn't know how to reconstruct the struct. Passing the `clone` flag in
//! the attribute generates a `Clone` implementation for `Box<T>` that uses the macro-generated
//! factory functions to create a deep copy.
//!
//! ```ignore
//! use dst_factory::make_dst_factory;
//!
//! #[make_dst_factory(clone)]
//! struct Message {
//!     id: u32,
//!     text: str,
//! }
//!
//! let msg = Message::build(1, "hello");
//! let cloned = msg.clone();
//! assert_eq!(cloned.id, 1);
//! assert_eq!(&cloned.text, "hello");
//! ```
//!
//! Clone support works with both named and tuple structs, and with both `str` and `[T]`
//! slice tails. It is not supported for `dyn Trait` tails, since there is no way to
//! clone the concrete type through a trait object reference.
//!
//! # Acknowledgements
//!
//! Many thanks to <https://github.com/scottmcm> for his invaluable help getting the factory methods
//! in top shape.

mod macro_args;
mod serde_impls;

use macro_args::MacroArgs;
use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote, quote_spanned};
use syn::token::{Comma, Where};
use syn::{
    Field, Fields, Generics, Ident, Index, ItemStruct, Path, Type, TypePath, parse::Result as SynResult, punctuated::Punctuated,
    spanned::Spanned,
};

enum TailKind {
    Slice(Box<Type>),
    Str,
    TraitObject(Path),
}

enum FieldIdent {
    Named(Ident),
    Unnamed(Index),
}

impl ToTokens for FieldIdent {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Named(ident) => ident.to_tokens(tokens),
            Self::Unnamed(index) => index.to_tokens(tokens),
        }
    }
}

struct StructInfo<'a> {
    struct_name: &'a Ident,
    struct_generics: &'a Generics,
    header_fields: Box<[&'a Field]>,
    header_field_idents: Box<[FieldIdent]>,
    header_param_idents: Box<[Ident]>,
    header_types: Vec<Type>,
    tail_field: &'a Field,
    tail_field_ident: FieldIdent,
    tail_param_ident: Ident,
    tail_kind: TailKind,
}

impl<'a> StructInfo<'a> {
    fn new(input_struct: &'a ItemStruct) -> SynResult<Self> {
        match &input_struct.fields {
            Fields::Named(named_fields) => Self::process_fields(input_struct, &named_fields.named),
            Fields::Unnamed(unnamed_fields) => Self::process_fields(input_struct, &unnamed_fields.unnamed),
            Fields::Unit => Err(syn::Error::new_spanned(input_struct, "Unit structs are not supported")),
        }
    }

    #[expect(
        clippy::unwrap_used,
        reason = "fields validated non-empty above; trait object always has a trait bound"
    )]
    fn process_fields(input_struct: &'a ItemStruct, fields: &'a Punctuated<Field, Comma>) -> SynResult<Self> {
        if fields.is_empty() {
            return Err(syn::Error::new_spanned(input_struct, "Struct must have at least one field"));
        }

        let header_fields: Vec<_> = fields.iter().take(fields.len() - 1).collect();
        let tail_field = fields.last().unwrap();

        let tail_kind = match &tail_field.ty {
            Type::Path(type_path) if type_path.path.is_ident("str") => TailKind::Str,
            Type::Path(TypePath { path, .. }) if path.segments.last().is_some_and(|s| s.ident == "str") => TailKind::Str,
            Type::Slice(type_slice) => TailKind::Slice(type_slice.elem.clone()),

            Type::TraitObject(type_trait_object) => {
                // Verify that the trait object is not a higher-rank trait bound
                for bound in &type_trait_object.bounds {
                    if let syn::TypeParamBound::Trait(trait_bound) = bound
                        && trait_bound.lifetimes.is_some()
                    {
                        return Err(syn::Error::new_spanned(
                            trait_bound,
                            "Higher-rank trait bounds (e.g., `for<'a> dyn Trait<'a>`) are not supported for the tail field.",
                        ));
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

                TailKind::TraitObject(trait_path.clone())
            }

            _ => {
                return Err(syn::Error::new_spanned(
                    &tail_field.ty,
                    "Last field must be a dynamically sized type like [T], str, or dyn Trait",
                ));
            }
        };

        let header_param_idents: Vec<_> = header_fields
            .iter()
            .enumerate()
            .map(|(i, field)| field.ident.as_ref().map_or_else(|| format_ident!("f{i}"), Clone::clone))
            .collect();

        let header_field_idents: Vec<_> = header_fields
            .iter()
            .enumerate()
            .map(|(i, field)| {
                field
                    .ident
                    .as_ref()
                    .map_or_else(|| FieldIdent::Unnamed(Index::from(i)), |ident| FieldIdent::Named(ident.clone()))
            })
            .collect();

        let header_types: Vec<_> = header_fields.iter().map(|field| field.ty.clone()).collect();

        let tail_param_ident = tail_field
            .ident
            .as_ref()
            .map_or_else(|| format_ident!("f{}", header_fields.len()), Clone::clone);

        let tail_field_ident = tail_field.ident.as_ref().map_or_else(
            || FieldIdent::Unnamed(Index::from(header_fields.len())),
            |ident| FieldIdent::Named(ident.clone()),
        );

        Ok(Self {
            struct_name: &input_struct.ident,
            struct_generics: &input_struct.generics,
            header_fields: header_fields.into_boxed_slice(),
            header_field_idents: header_field_idents.into_boxed_slice(),
            header_param_idents: header_param_idents.into_boxed_slice(),
            header_types,
            tail_field,
            tail_field_ident,
            tail_param_ident,
            tail_kind,
        })
    }
}

fn header_layout(macro_args: &MacroArgs, struct_info: &StructInfo, for_trait: bool) -> TokenStream {
    let tail_field_ident = &struct_info.tail_field_ident;

    let header_field_types: Vec<_> = struct_info.header_fields.iter().map(|field| &field.ty).collect();

    let fat_payload = if for_trait {
        quote! { vtable }
    } else {
        quote! { 0_usize }
    };

    let tail_type = match &struct_info.tail_kind {
        TailKind::Slice(elem_type) => quote! { #elem_type },
        TailKind::Str => quote! { u8 },
        TailKind::TraitObject(_) => {
            let generic_name = &macro_args.generic_name;
            quote! { #generic_name }
        }
    };

    if header_field_types.is_empty() {
        return quote! {
            let layout = unsafe {
                let buffer = ::core::mem::MaybeUninit::<(#tail_type,)>::uninit();
                let head_ptr: *const Self = ::core::mem::transmute((&raw const buffer, #fat_payload));
                let align = ::core::mem::align_of_val::<Self>(&*head_ptr);
                ::core::alloc::Layout::from_size_align_unchecked(0, align)
            };
        };
    }

    quote! {
        let buffer = ::core::mem::MaybeUninit::<(#( #header_field_types, )* #tail_type)>::uninit();
        let (offset, align) = unsafe {
            let head_ptr: *const Self = ::core::mem::transmute((&raw const buffer, #fat_payload));
            let tail_ptr = &raw const (*head_ptr).#tail_field_ident;
            (
                (tail_ptr as *const u8).offset_from_unsigned(head_ptr as *const u8),
                ::core::mem::align_of_val::<Self>(&*head_ptr)
            )
        };

        let layout = ::core::alloc::Layout::from_size_align(offset, align).unwrap();
    }
}

fn tail_layout<T: ToTokens>(tail_type: &T, span: Span) -> TokenStream {
    quote_spanned! { span => ::core::alloc::Layout::array::<#tail_type>(len).expect("Array exceeds maximum size allowed of isize::MAX") }
}

fn dealloc_path(no_std: bool) -> TokenStream {
    if no_std {
        quote! { ::alloc::alloc::dealloc }
    } else {
        quote! { ::std::alloc::dealloc }
    }
}

fn box_path(no_std: bool) -> TokenStream {
    if no_std {
        quote! { ::alloc::boxed::Box }
    } else {
        quote! { ::std::boxed::Box }
    }
}

fn guard_type(macro_args: &MacroArgs) -> TokenStream {
    let dealloc_path = dealloc_path(macro_args.no_std);

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

fn header_params(struct_info: &StructInfo) -> Vec<TokenStream> {
    struct_info
        .header_fields
        .iter()
        .enumerate()
        .map(|(i, field)| {
            let param_ident = &struct_info.header_param_idents[i];
            let field_ty = &field.ty;
            quote! { #param_ident: #field_ty }
        })
        .collect()
}

fn header_field_writes(struct_info: &StructInfo) -> Vec<TokenStream> {
    struct_info
        .header_field_idents
        .iter()
        .enumerate()
        .map(|(i, field_ident)| {
            let tuple_idx = Index::from(i);
            quote! { ::core::ptr::write_unaligned(&raw mut ((*fat_ptr).#field_ident), args.#tuple_idx);}
        })
        .collect()
}

fn args_tuple_assignment(struct_info: &StructInfo) -> TokenStream {
    let header_param_idents = &struct_info.header_param_idents;
    let tail_param_ident = &struct_info.tail_param_ident;

    quote! {
        let args = ( #( #header_param_idents, )* #tail_param_ident, );
    }
}

fn alloc(no_std: bool) -> TokenStream {
    let (alloc_path, handle_alloc_error) = if no_std {
        (quote! { ::alloc::alloc::alloc }, quote! { panic!("out of memory") })
    } else {
        (quote! { ::std::alloc::alloc }, quote! { ::std::alloc::handle_alloc_error(layout) })
    };

    quote! {
        let mem_ptr = #alloc_path(layout);
        if mem_ptr.is_null() {
            #handle_alloc_error
        }
    }
}

fn alloc_zst(box_path: &TokenStream, for_trait: bool) -> TokenStream {
    let mem_ptr = quote! { let mem_ptr = ::core::ptr::without_provenance_mut::<u8>(layout.align()); };

    let fat_ptr = if for_trait {
        quote! { let fat_ptr = ::core::mem::transmute::<(*mut u8, *const ()), *mut Self>((mem_ptr, vtable)); }
    } else {
        quote! { let fat_ptr = ::core::mem::transmute::<(*mut u8, usize), *mut Self>((mem_ptr, 0_usize)); }
    };

    let box_from_raw = quote! { #box_path::from_raw(fat_ptr) };

    quote! {
        #mem_ptr
        #fat_ptr
        #box_from_raw
    }
}

fn factory_for_slice_arg(macro_args: &MacroArgs, struct_info: &StructInfo, tail_elem_type: &Type) -> TokenStream {
    let copy_bound_tokens: syn::WherePredicate = syn::parse_quote_spanned! {tail_elem_type.span()=>
        #tail_elem_type: ::core::marker::Copy
    };

    let mut factory_where_clause = struct_info.struct_generics.where_clause.as_ref().map_or_else(
        || syn::WhereClause {
            where_token: Where::default(),
            predicates: Punctuated::new(),
        },
        Clone::clone,
    );

    factory_where_clause.predicates.push(copy_bound_tokens);

    let alloc = alloc(macro_args.no_std);
    let box_path = box_path(macro_args.no_std);
    let make_zst = alloc_zst(&box_path, false);

    let tail_layout = tail_layout(tail_elem_type, struct_info.tail_field.ty.span());
    let header_layout = header_layout(macro_args, struct_info, false);
    let tuple_assignment = args_tuple_assignment(struct_info);
    let header_field_writes = header_field_writes(struct_info);
    let header_params = header_params(struct_info);

    let factory_name = format_ident!("{}_from_slice", &macro_args.base_factory_name);
    let visibility = &macro_args.visibility;

    let tail_param = &struct_info.tail_param_ident;
    let tail_field = &struct_info.tail_field_ident;
    let struct_name = &struct_info.struct_name;
    let tail_args_tuple_idx = Index::from(struct_info.header_fields.len());

    let factory_doc = format!("Creates an instance of `Box<{struct_name}>`.");

    quote! {
        #[doc = #factory_doc]
        #[allow(clippy::let_unit_value)]
        #[allow(clippy::zst_offset)]
        #[allow(clippy::transmute_undefined_repr)]
        #visibility fn #factory_name (
            #( #header_params, )*
            #tail_param: &[#tail_elem_type]
        ) -> #box_path<Self> #factory_where_clause {
            #tuple_assignment

            let s = args.#tail_args_tuple_idx.as_ref();
            let len = s.len();

            #header_layout
            let layout = layout.extend(#tail_layout).expect("Struct exceeds maximum size allowed of isize::MAX").0;
            let layout = layout.pad_to_align();

            unsafe {
                if layout.size() == 0 {
                    #make_zst
                } else {
                    #alloc

                    let fat_ptr = ::core::mem::transmute::<(*mut u8, usize), *mut Self>((mem_ptr, len));
                    ::core::debug_assert_eq!(::core::alloc::Layout::for_value(&*fat_ptr), layout);

                    #( #header_field_writes )*

                    // Copy the slice content into the tail field
                    let tail_ptr = (&raw mut (*fat_ptr).#tail_field).cast::<#tail_elem_type>();
                    ::core::ptr::copy_nonoverlapping(s.as_ptr(), tail_ptr, len);

                    #box_path::from_raw(fat_ptr)
                }
            }
        }
    }
}

fn factory_for_iter_arg(macro_args: &MacroArgs, struct_info: &StructInfo, tail_type: &Type) -> TokenStream {
    let guard_type_tokens = guard_type(macro_args);
    let alloc = alloc(macro_args.no_std);
    let box_path = box_path(macro_args.no_std);
    let make_zst = alloc_zst(&box_path, false);

    let tail_layout = tail_layout(tail_type, struct_info.tail_field.ty.span());
    let header_layout = header_layout(macro_args, struct_info, false);
    let tuple_assignment = args_tuple_assignment(struct_info);
    let header_field_writes = header_field_writes(struct_info);
    let header_params = header_params(struct_info);

    let visibility = &macro_args.visibility;
    let factory_name = &macro_args.base_factory_name;
    let iter_generic_param = &macro_args.generic_name;

    let tail_param = &struct_info.tail_param_ident;
    let tail_field = &struct_info.tail_field_ident;
    let struct_name = &struct_info.struct_name;
    let tail_args_tuple_idx = Index::from(struct_info.header_fields.len());

    let factory_doc = format!("Creates an instance of `Box<{struct_name}>`.");

    quote! {
        #[doc = #factory_doc]
        #[allow(clippy::let_unit_value)]
        #[allow(clippy::zst_offset)]
        #[allow(clippy::transmute_undefined_repr)]
        #visibility fn #factory_name <#iter_generic_param> (
            #( #header_params, )*
            #tail_param: #iter_generic_param
        ) -> #box_path<Self>
        where
            #iter_generic_param: ::core::iter::IntoIterator<Item = #tail_type>,
            <#iter_generic_param as ::core::iter::IntoIterator>::IntoIter: ::core::iter::ExactSizeIterator
        {
            #guard_type_tokens
            #tuple_assignment

            let iter = args.#tail_args_tuple_idx.into_iter();
            let len = iter.len();

            #header_layout
            let layout = layout.extend(#tail_layout).expect("Struct exceeds maximum size allowed of isize::MAX").0;
            let layout = layout.pad_to_align();

            unsafe {
                if layout.size() == 0 {
                    #make_zst
                } else {
                    #alloc

                    let fat_ptr = ::core::mem::transmute::<(*mut u8, usize), *mut Self>((mem_ptr, len));
                    ::core::debug_assert_eq!(::core::alloc::Layout::for_value(&*fat_ptr), layout);

                    #( #header_field_writes )*

                    // Write each element from the iterator into the tail field
                    let tail_ptr = (&raw mut (*fat_ptr).#tail_field).cast::<#tail_type>();
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

                    ::core::mem::forget(guard);

                    #box_path::from_raw(fat_ptr)
                }
            }
        }
    }
}

fn destructurer_iterator_type(macro_args: &MacroArgs, struct_info: &StructInfo, tail_type: &Type) -> TokenStream {
    let dealloc_path = dealloc_path(macro_args.no_std);

    let visibility = &macro_args.visibility;
    let iterator_name = macro_args
        .iterator_name
        .clone()
        .unwrap_or_else(|| Ident::new(&format!("{}Iter", struct_info.struct_name), Span::call_site()));

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
                if self.index >= self.len {
                    return None;
                }
                #[allow(clippy::zst_offset)]
                let value = unsafe { self.ptr.add(self.index).read() };
                self.index += 1;
                Some(value)
            }
        }

        impl #impl_generics ::core::iter::ExactSizeIterator for #iterator_name #ty_generics #where_clause {
            fn len(&self) -> usize {
                self.len - self.index
            }
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
                    // Drop any remaining un-yielded elements
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

fn destructurer_with_iter(macro_args: &MacroArgs, struct_info: &StructInfo) -> TokenStream {
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
        .unwrap_or_else(|| Ident::new(&format!("{}Iter", struct_info.struct_name), Span::call_site()));
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

fn factory_for_str_arg(macro_args: &MacroArgs, struct_info: &StructInfo) -> TokenStream {
    let alloc = alloc(macro_args.no_std);
    let box_path = box_path(macro_args.no_std);
    let make_zst = alloc_zst(&box_path, false);

    let tail_layout = tail_layout(&quote! { u8 }, struct_info.tail_field.ty.span());
    let header_layout = header_layout(macro_args, struct_info, false);
    let tuple_assignment = args_tuple_assignment(struct_info);
    let header_field_writes = header_field_writes(struct_info);
    let header_params = header_params(struct_info);

    let factory_name = &macro_args.base_factory_name;
    let visibility = &macro_args.visibility;

    let struct_name = &struct_info.struct_name;
    let tail_param = &struct_info.tail_param_ident;
    let tail_field = &struct_info.tail_field_ident;
    let tail_type = &struct_info.tail_field.ty;
    let tail_args_tuple_idx = Index::from(struct_info.header_fields.len());

    let factory_doc = format!("Creates an instance of `Box<{struct_name}>`.");

    quote! {
        #[doc = #factory_doc]
        #[allow(clippy::let_unit_value)]
        #[allow(clippy::zst_offset)]
        #[allow(clippy::transmute_undefined_repr)]
        #visibility fn #factory_name(
            #( #header_params, )*
            #tail_param: impl ::core::convert::AsRef<str>
        ) -> #box_path<Self> {
            #tuple_assignment

            ::core::assert_eq!(::core::any::TypeId::of::<#tail_type>(), ::core::any::TypeId::of::<str>());
            let s = args.#tail_args_tuple_idx.as_ref();
            let len = s.len();

            #header_layout
            let layout = layout.extend(#tail_layout).expect("Struct exceeds maximum size allowed of isize::MAX").0;
            let layout = layout.pad_to_align();

            unsafe {
                if layout.size() == 0 {
                    #make_zst
                } else {
                    #alloc

                    let fat_ptr = ::core::mem::transmute::<(*mut u8, usize), *mut Self>((mem_ptr, len));
                    ::core::debug_assert_eq!(::core::alloc::Layout::for_value(&*fat_ptr), layout);

                    #( #header_field_writes )*

                    // copy the string data into the tail field
                    let tail_ptr = (&raw mut (*fat_ptr).#tail_field).cast::<u8>();
                    ::core::ptr::copy_nonoverlapping(s.as_ptr(), tail_ptr, len);

                    #box_path::from_raw(fat_ptr)
                }
            }
        }
    }
}

fn factory_for_trait_arg(macro_args: &MacroArgs, struct_info: &StructInfo, trait_path: &Path) -> TokenStream {
    let alloc = alloc(macro_args.no_std);
    let box_path = box_path(macro_args.no_std);
    let make_zst = alloc_zst(&box_path, true);

    let header_layout = header_layout(macro_args, struct_info, true);
    let tuple_assignment = args_tuple_assignment(struct_info);
    let header_field_writes = header_field_writes(struct_info);
    let header_params = header_params(struct_info);

    let factory_name = &macro_args.base_factory_name;
    let trait_generic = &macro_args.generic_name;
    let visibility = &macro_args.visibility;

    let struct_name = &struct_info.struct_name;
    let tail_param = &struct_info.tail_param_ident;
    let tail_field = &struct_info.tail_field_ident;
    let tail_args_tuple_idx = Index::from(struct_info.header_fields.len());

    let factory_doc = format!("Builds an instance of `Box<{struct_name}>`.");

    quote! {
        #[doc = #factory_doc]
        #[allow(clippy::let_unit_value)]
        #[allow(clippy::zst_offset)]
        #[allow(clippy::transmute_undefined_repr)]
        #visibility fn #factory_name <#trait_generic> (
            #( #header_params, )*
            #tail_param: #trait_generic
        ) -> #box_path<Self>
        where
            #trait_generic: #trait_path + Sized
        {
            #tuple_assignment

            let s = args.#tail_args_tuple_idx;
            let trait_object: &dyn #trait_path = &s;
            let (_, vtable): (*const #trait_generic, *const ()) = unsafe { ::core::mem::transmute(trait_object) };

            #header_layout
            let layout = layout.extend(::core::alloc::Layout::new::<#trait_generic>()).expect("Struct exceeds maximum size allowed of isize::MAX").0;
            let layout = layout.pad_to_align();

            unsafe {
                if layout.size() == 0 {
                    #make_zst
                } else {
                    #alloc

                    let fat_ptr = ::core::mem::transmute::<(*mut u8, *const ()), *mut Self>((mem_ptr, vtable));
                    ::core::debug_assert_eq!(::core::alloc::Layout::for_value(&*fat_ptr), layout);

                    #( #header_field_writes )*

                    let tail_ptr = (&raw mut (*fat_ptr).#tail_field).cast::<#trait_generic>();
                    ::core::ptr::copy_nonoverlapping(::core::ptr::addr_of!(s), tail_ptr, 1);
                    ::core::mem::forget(s);

                    #box_path::from_raw(fat_ptr)
                }
            }
        }
    }
}

enum CloneTailKind<'a> {
    Slice(&'a Type),
    Str,
}

fn gen_clone(macro_args: &MacroArgs, struct_info: &StructInfo, clone_tail: &CloneTailKind) -> TokenStream {
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

    // Build where clause with Clone bounds for header fields
    let mut clone_bounds: Vec<TokenStream> = struct_info
        .header_fields
        .iter()
        .map(|field| {
            let ty = &field.ty;
            quote! { #ty: ::core::clone::Clone }
        })
        .collect();
    clone_bounds.extend(extra_bounds);

    // Merge with existing where clause
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

fn make_dst_factory_impl(attr_args: TokenStream, item: TokenStream) -> SynResult<TokenStream> {
    let macro_args = MacroArgs::parse(attr_args)?;
    let input_struct: ItemStruct = syn::parse2(item)?;
    let struct_info = StructInfo::new(&input_struct)?;

    let mut factories = Vec::new();
    let mut iterator_type = None;
    match &struct_info.tail_kind {
        TailKind::Slice(elem_type) => {
            factories.push(factory_for_iter_arg(&macro_args, &struct_info, elem_type));
            factories.push(factory_for_slice_arg(&macro_args, &struct_info, elem_type));
            factories.push(destructurer_with_iter(&macro_args, &struct_info));
            iterator_type = Some(destructurer_iterator_type(&macro_args, &struct_info, elem_type));
        }

        TailKind::Str => {
            factories.push(factory_for_str_arg(&macro_args, &struct_info));
        }

        TailKind::TraitObject(trait_path) => {
            factories.push(factory_for_trait_arg(&macro_args, &struct_info, trait_path));
        }
    }

    let mut serde_tokens: Option<TokenStream> = None;
    let mut clone_tokens: Option<TokenStream> = None;

    if macro_args.deserialize {
        match &struct_info.tail_kind {
            TailKind::TraitObject(_) => {
                return Err(syn::Error::new_spanned(
                    &input_struct,
                    "deserialize is not supported for DSTs with trait object tails",
                ));
            }
            TailKind::Slice(elem_type) => {
                let owned_tail_type: Type = syn::parse_quote! { ::std::vec::Vec<#elem_type> };
                let factory_name = format_ident!("{}_from_slice", &macro_args.base_factory_name);
                serde_tokens = Some(serde_impls::gen_deserialize(
                    &struct_info,
                    &input_struct,
                    &owned_tail_type,
                    &factory_name,
                ));
            }
            TailKind::Str => {
                let owned_tail_type: Type = syn::parse_quote! { ::std::string::String };
                serde_tokens = Some(serde_impls::gen_deserialize(
                    &struct_info,
                    &input_struct,
                    &owned_tail_type,
                    &macro_args.base_factory_name,
                ));
            }
        }
    }

    if macro_args.clone {
        match &struct_info.tail_kind {
            TailKind::TraitObject(_) => {
                return Err(syn::Error::new_spanned(
                    &input_struct,
                    "clone is not supported for DSTs with trait object tails",
                ));
            }
            TailKind::Slice(elem_type) => {
                clone_tokens = Some(gen_clone(&macro_args, &struct_info, &CloneTailKind::Slice(elem_type)));
            }
            TailKind::Str => {
                clone_tokens = Some(gen_clone(&macro_args, &struct_info, &CloneTailKind::Str));
            }
        }
    }

    let (impl_generics, ty_generics, where_clause) = struct_info.struct_generics.split_for_impl();
    let struct_name_ident = struct_info.struct_name;

    Ok(quote! {
        #input_struct

        impl #impl_generics #struct_name_ident #ty_generics #where_clause {
            #( #factories )*
        }

        #iterator_type
        #serde_tokens
        #clone_tokens
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
/// #[make_dst_factory(<base_factory_name>[, <visibility>] [, no_std] [, deserialize] [, clone] [, generic=<generic_name>])]
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
/// trait MyTrait {}
///
/// // With a trait object as the tail field
/// #[make_dst_factory]
/// struct TraitStruct {
///     id: u32,
///     handler: dyn MyTrait,
/// }
/// ```
#[proc_macro_attribute]
pub fn make_dst_factory(attr_args: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let result = make_dst_factory_impl(attr_args.into(), item.into());
    result.unwrap_or_else(|err| err.to_compile_error()).into()
}

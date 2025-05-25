//! C-like [flexible array members](https://en.wikipedia.org/wiki/Flexible_array_member) for Rust.
//!
//! This crate lets you allocate variable data inline at the end of a struct. If you have a
//! struct that gets allocated on the heap and has some variable-length data associated with it
//! (like a string or an array), then you can allocate this data directly inline with the struct.
//! This saves memory by avoiding the need for a pointer and a separate allocation, and saves CPU
//! cycles by eliminating the need for an indirection when accessing the data.
//!
//! Rust supports the notion of [Dynamically Sized Types](https://doc.rust-lang.org/reference/dynamically-sized-types.html), known as DSTs,
//! which are types that have a size
//! not known at compile time. DSTs are perfect to implement flexible array members. But
//! unfortunately, Rust doesn't provide an out-of-the-box way to allocate instances of such types.
//! This is where this crate comes in.
//!
//! You can apply the #[[`macro@make_dst_builder`]] attribute to your DST struct which causes factory
//! methods to be produced that let you easily and safely create instances of your DST.
//!
//! ## Example
//!
//! Here's an example using an array as the last field of a struct:
//!
//! ```rust
//! use tail_extend::make_dst_builder;
//!
//! #[make_dst_builder]
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
//! use tail_extend::make_dst_builder;
//!
//! #[make_dst_builder]
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
//! Because DSTs don't have a known size at compile time, you can't store them on the stack,
//! and you can't pass them by value. As a result of these constraints, the `build` and
//! `build_from_iter` functions always return boxed instances of the structs.
//!
//! ## Attribute Features
//!
//! The common use case for the #[[`macro@make_dst_builder`]] attribute is to not pass any arguments.
//! This results in factory methods called `build` when using a string as the last field of the
//! struct, and `build` and `build_from_iter` when using an array as the last field of the struct.
//!
//! The generated methods are private by default and have the following signatures:
//!
//! ```ignore
//! // for arrays
//! fn build(field1, field2, ..., last_field: &[last_field_type]) -> Box<Self> where last_field_type: Clone;
//! fn build_from_iter<I>(field1, field2, ..., last_field: I) -> Box<Self>
//! where
//!     I: IntoIterator<Item = last_field_type>,
//!     <I as IntoIterator>::IntoIter: ExactSizeIterator,
//!
//! // for strings
//! fn build(field1, field2, ..., last_field: impl AsRef<str>) -> Box<Self>;
//! ```
//!
//! The attribute lets you control the name of the generated factory methods, their
//! visibility, and whether to generate for the `no_std` environment. The general
//! grammar is:
//!
//! ```ignore
//! #[make_dst_builder(<base_method_name>[, <visibility>] [, no_std])]
//! ```
//!
//! Some examples:
//!
//! ```ignore
//! // The factory methods will be private and called `create` and `create_from_iter`
//! #[make_dst_builder(create)]
//!
//! // The factory methods will be public and called `create` and `create_from_iter`
//! #[make_dst_builder(create, pub)]
//!
//! // The factory methods will be private, called `create` and `create_from_iter`, and support the `no_std` environment
//! #[make_dst_builder(create, no_std)]
//! ```
//!
//! ## Error Conditions
//!
//! The #[[`macro@make_dst_builder`]] macro produces a compile-time error if:
//!
//! - It's applied to anything other than a struct with named fields.
//! - The struct has no fields (a tail field is essential for a DST).
//! - The last field of the struct is not a slice (`[T]`) or a string (`str`).
//! - The arguments are malformed (e.g., incorrect visibility keyword, too many arguments).

#![allow(clippy::too_many_arguments)]

extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use std::collections::HashSet;
use syn::parse::discouraged::Speculative;
use syn::token::Where;
use syn::{
    Field, Fields, GenericParam, Generics, Ident, ItemStruct, Lifetime, Token, Type, TypePath,
    TypeSlice, Visibility,
    parse::{Parse, ParseStream, Result as SynResult},
    punctuated::Punctuated,
    spanned::Spanned,
    visit::{self, Visit},
};

struct MakeDstBuilderArgs {
    visibility: Visibility,
    base_method_name: Ident,
    no_std: bool,
}

impl Parse for MakeDstBuilderArgs {
    fn parse(input: ParseStream) -> SynResult<Self> {
        let mut result = Self {
            visibility: Visibility::Inherited,
            base_method_name: Ident::new("build", input.span()),
            no_std: false,
        };

        // Check for method name
        if input.peek(syn::Ident) {
            let ahead = input.fork();
            let ident = ahead.parse::<Ident>()?;
            if ident != "no_std" && ident != "pub" {
                result.base_method_name = ident;

                input.advance_to(&ahead);
                if !input.is_empty() {
                    input
                        .parse::<Token![,]>()
                        .map_err(|_| input.error("Expected comma after method name"))?;
                }
            }
        }

        // Check for visibility
        if input.peek(Token![pub]) {
            result.visibility = input
                .parse()
                .map_err(|_| input.error("Failed to parse visibility"))?;

            if !input.is_empty() {
                input
                    .parse::<Token![,]>()
                    .map_err(|_| input.error("Expected comma after visibility"))?;
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

        if input.is_empty() {
            Ok(result)
        } else {
            Err(input.error("Unexpected input"))
        }
    }
}

// --- Helper Structs for Extracted Information ---
struct FieldInfo<'a> {
    header_field_structs: Vec<&'a Field>,
    header_field_idents: Vec<&'a Ident>,
    build_fn_header_params: Vec<TokenStream>,
    tail_field: &'a Field,
    tail_field_name_ident: &'a Ident,
}

// --- Helper Struct for Common Build Method Parameters ---
struct CommonBuildParams<'a> {
    builder_args: &'a MakeDstBuilderArgs,
    field_info: &'a FieldInfo<'a>,
    header_layout_calc_tokens: &'a TokenStream,
    offset_of_tail_payload_ident: &'a Ident,
    layout_header_full_ident: &'a Ident,
    unique_tuple_ident: &'a Ident, // Added field
    method_name_ident: Ident,
    build_fn_tail_param_tokens: TokenStream,
    build_fn_generics_tokens: Option<TokenStream>,
    build_fn_where_clause_tokens: Option<TokenStream>,
    build_fn_tail_processing_tokens: TokenStream,
    layout_calculation_for_tail_payload_tokens: TokenStream,
    write_tail_data_call_tokens: TokenStream,
    method_doc_string: String,
}

// --- Helper for array/slice layout calculations ---
fn create_array_layout_calculation<T: quote::ToTokens>(
    element_type: &T,
    span: proc_macro2::Span,
) -> TokenStream {
    quote_spanned! {span=>
        let layout_tail_payload =
            ::core::alloc::Layout::array::<#element_type>(len)
                .expect("Array exceeds maximum size allowed of isize::MAX");
    }
}

#[derive(Default)]
struct UsedGenericParams {
    lifetimes: HashSet<Lifetime>,
    types: HashSet<Ident>,
    consts: HashSet<Ident>,
}

struct GenericUsageVisitor<'a> {
    // Parameters defined on the original struct
    defined_lifetimes: &'a HashSet<Lifetime>,
    defined_type_param_idents: &'a HashSet<Ident>,
    defined_const_param_idents: &'a HashSet<Ident>,
    // Parameters found to be used
    used_params: UsedGenericParams,
}

impl<'a> GenericUsageVisitor<'a> {
    fn new(
        defined_lifetimes: &'a HashSet<Lifetime>,
        defined_type_param_idents: &'a HashSet<Ident>,
        defined_const_param_idents: &'a HashSet<Ident>,
    ) -> Self {
        Self {
            defined_lifetimes,
            defined_type_param_idents,
            defined_const_param_idents,
            used_params: UsedGenericParams::default(),
        }
    }
}

impl<'ast> Visit<'ast> for GenericUsageVisitor<'_> {
    fn visit_expr(&mut self, ex: &'ast syn::Expr) {
        if let syn::Expr::Path(expr_path) = ex {
            if expr_path.qself.is_none() && expr_path.path.segments.len() == 1 {
                let ident = &expr_path.path.segments[0].ident;
                if self.defined_const_param_idents.contains(ident) {
                    self.used_params.consts.insert(ident.clone());
                }
            }
        }
        visit::visit_expr(self, ex); // Default traversal
    }

    fn visit_lifetime(&mut self, l: &'ast Lifetime) {
        if self.defined_lifetimes.contains(l) {
            self.used_params.lifetimes.insert(l.clone());
        }
    }

    fn visit_path(&mut self, path: &'ast syn::Path) {
        // Check for type or const params used as path segments
        if path.leading_colon.is_none() && path.segments.len() == 1 {
            let ident = &path.segments[0].ident;
            if self.defined_type_param_idents.contains(ident) {
                self.used_params.types.insert(ident.clone());
            } else if self.defined_const_param_idents.contains(ident) {
                self.used_params.consts.insert(ident.clone());
            }
        }
        // Visit generic arguments within the path, like `Vec<T>`
        for segment in &path.segments {
            self.visit_path_arguments(&segment.arguments);
        }
    }
}

// --- Helper Functions ---

fn extract_field_info(input_struct: &ItemStruct) -> SynResult<FieldInfo> {
    let mut header_field_structs: Vec<&Field> = Vec::new();
    let mut header_field_idents: Vec<&Ident> = Vec::new();
    let mut build_fn_header_params: Vec<TokenStream> = Vec::new();

    let tail_field: &Field;
    let tail_field_name_ident: &Ident;

    match &input_struct.fields {
        Fields::Named(fields_named) => {
            if fields_named.named.is_empty() {
                return Err(syn::Error::new_spanned(
                    fields_named,
                    "Struct must have at least one field",
                ));
            }

            // Split fields into header fields (all but last) and the tail field (last)
            let mut fields_iter = fields_named.named.iter();
            let last_field = fields_iter.next_back().unwrap(); // Safe because we checked for emptiness above

            // Verify the last field is a DST with unsized type [T] or str
            match &last_field.ty {
                Type::Slice(_) => true,
                Type::Path(type_path) if type_path.path.is_ident("str") => true,
                Type::Path(TypePath { path, .. })
                    if path.segments.last().is_some_and(|s| s.ident == "str") =>
                {
                    true
                }
                _ => {
                    return Err(syn::Error::new_spanned(
                        &last_field.ty,
                        "Last field must be a dynamically sized type like [T] or str",
                    ));
                }
            };

            let num_fields = fields_named.named.len();
            for (i, field) in fields_named.named.iter().enumerate() {
                if i < num_fields - 1 {
                    header_field_structs.push(field);
                    let field_ident = field.ident.as_ref().unwrap();
                    header_field_idents.push(field_ident);
                    let field_ty = &field.ty;
                    build_fn_header_params.push(quote! { #field_ident: #field_ty });
                }
            }
            tail_field = fields_named.named.last().unwrap();
            tail_field_name_ident = tail_field.ident.as_ref().unwrap();

            Ok(FieldInfo {
                header_field_structs,
                header_field_idents,
                build_fn_header_params,
                tail_field,
                tail_field_name_ident,
            })
        }
        Fields::Unnamed(_) | Fields::Unit => Err(syn::Error::new_spanned(
            &input_struct.fields,
            "Structs with unnamed fields are not supported",
        )),
    }
}

#[expect(clippy::too_many_lines)]
fn generate_header_layout_code(
    header_field_structs: &[&Field],
    input_struct_generics: &Generics,
) -> TokenStream {
    let layout_header_full_ident = format_ident!("layout_header_full");
    let helper_struct_name = format_ident!("PrefixLayoutHelper");

    if header_field_structs.is_empty() {
        return quote! {
            let #layout_header_full_ident = ::core::alloc::Layout::new::<()>();
        };
    }

    // 1. Collect all defined generic parameters from the input struct
    let mut defined_lifetimes = HashSet::new();
    let mut defined_type_param_idents = HashSet::new();
    let mut defined_const_param_idents = HashSet::new();

    for param in &input_struct_generics.params {
        match param {
            GenericParam::Lifetime(lt) => {
                defined_lifetimes.insert(lt.lifetime.clone());
            }
            GenericParam::Type(tp) => {
                defined_type_param_idents.insert(tp.ident.clone());
            }
            GenericParam::Const(cp) => {
                defined_const_param_idents.insert(cp.ident.clone());
            }
        }
    }

    // 2. Determine which generic parameters are used by the header fields
    let mut overall_used_params = UsedGenericParams::default();
    for field in header_field_structs {
        let mut visitor = GenericUsageVisitor::new(
            &defined_lifetimes,
            &defined_type_param_idents,
            &defined_const_param_idents,
        );
        visitor.visit_type(&field.ty);
        overall_used_params
            .lifetimes
            .extend(visitor.used_params.lifetimes);
        overall_used_params.types.extend(visitor.used_params.types);
        overall_used_params
            .consts
            .extend(visitor.used_params.consts);
    }

    // 3. Construct generics for the helper struct (only used params)
    let mut helper_generics = Generics::default();
    for param in &input_struct_generics.params {
        match param {
            GenericParam::Lifetime(lt_def)
                if overall_used_params.lifetimes.contains(&lt_def.lifetime) =>
            {
                helper_generics.params.push(param.clone());
            }
            GenericParam::Type(ty_param) if overall_used_params.types.contains(&ty_param.ident) => {
                helper_generics.params.push(param.clone());
            }
            GenericParam::Const(cn_param)
                if overall_used_params.consts.contains(&cn_param.ident) =>
            {
                helper_generics.params.push(param.clone());
            }
            _ => {} // Param not used in header
        }
    }

    // 4. Filter the where clause for the helper struct
    if let Some(input_where_clause) = &input_struct_generics.where_clause {
        let mut helper_where_clause = syn::WhereClause {
            where_token: input_where_clause.where_token,
            predicates: Punctuated::new(),
        };
        for predicate in &input_where_clause.predicates {
            let mut predicate_visitor = GenericUsageVisitor::new(
                &defined_lifetimes,
                &defined_type_param_idents,
                &defined_const_param_idents,
            );
            visit::visit_where_predicate(&mut predicate_visitor, predicate);

            let is_relevant = predicate_visitor
                .used_params
                .lifetimes
                .is_subset(&overall_used_params.lifetimes)
                && predicate_visitor
                    .used_params
                    .types
                    .is_subset(&overall_used_params.types)
                && predicate_visitor
                    .used_params
                    .consts
                    .is_subset(&overall_used_params.consts);

            if is_relevant {
                helper_where_clause.predicates.push(predicate.clone());
            }
        }
        if !helper_where_clause.predicates.is_empty() {
            helper_generics.where_clause = Some(helper_where_clause);
        }
    }

    let (helper_impl_generics, helper_ty_generics, helper_where_clause_for_impl) =
        helper_generics.split_for_impl();

    let mut helper_fields_definitions = Vec::new();
    for field in header_field_structs {
        let field_ident = field.ident.as_ref().unwrap();
        let field_ty = &field.ty;
        helper_fields_definitions.push(quote_spanned!(field.span()=> #field_ident: #field_ty));
    }

    quote_spanned! {helper_struct_name.span() =>
        #[allow(dead_code)]
        struct #helper_struct_name #helper_impl_generics #helper_where_clause_for_impl {
            #( #helper_fields_definitions ),*
        }
        let #layout_header_full_ident = ::core::alloc::Layout::new::<#helper_struct_name #helper_ty_generics>();
    }
}

// --- Unified Build Method Generation ---

fn generate_build_method_common(common_params: CommonBuildParams) -> TokenStream {
    let CommonBuildParams {
        builder_args:
            MakeDstBuilderArgs {
                visibility: method_visibility,
                no_std,
                ..
            },
        field_info:
            FieldInfo {
                build_fn_header_params,
                header_field_idents: header_param_idents_in_signature,
                tail_field_name_ident: tail_param_ident_in_signature,
                ..
            },
        header_layout_calc_tokens,
        offset_of_tail_payload_ident,
        layout_header_full_ident,
        unique_tuple_ident, // Destructure the new field
        method_name_ident,
        build_fn_tail_param_tokens,
        build_fn_generics_tokens,
        build_fn_where_clause_tokens,
        build_fn_tail_processing_tokens,
        layout_calculation_for_tail_payload_tokens,
        write_tail_data_call_tokens,
        method_doc_string,
    } = common_params;

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
            quote! { ::std::alloc::handle_alloc_error(final_layout) },
        )
    };

    let tuple_elements_for_assignment = header_param_idents_in_signature
        .iter()
        .map(|ident| quote! { #ident })
        .chain(std::iter::once(quote! { #tail_param_ident_in_signature }))
        .collect::<Vec<_>>();

    let tuple_assignment = quote! {
        let #unique_tuple_ident = ( #( #tuple_elements_for_assignment, )* );
    };

    let header_field_writes =
        header_param_idents_in_signature
            .iter()
            .enumerate()
            .map(|(idx, field_ident_on_struct)| {
                let tuple_idx = syn::Index::from(idx);
                quote! {
                    ::core::ptr::write(&mut ((*fat_ptr).#field_ident_on_struct), #unique_tuple_ident.#tuple_idx);
                }
            });

    quote! {
        #[doc = #method_doc_string]
        #[allow(unused_variables)]
        #[allow(clippy::let_unit_value)]
        #[allow(clippy::zst_offset)]
        #[allow(clippy::transmute_undefined_repr)]
        #method_visibility fn #method_name_ident #build_fn_generics_tokens (
            #( #build_fn_header_params, )*
            #build_fn_tail_param_tokens
        ) -> #box_path<Self> #build_fn_where_clause_tokens {
            #tuple_assignment

            #build_fn_tail_processing_tokens

            #header_layout_calc_tokens
            #layout_calculation_for_tail_payload_tokens

            let (final_layout, #offset_of_tail_payload_ident) = #layout_header_full_ident
                .extend(layout_tail_payload)
                .expect("Struct exceeds maximum size allowed of isize::MAX");

            unsafe {
                if final_layout.size() == 0 {
                    let data_ptr = ::core::ptr::NonNull::<u8>::dangling().as_ptr();
                    let fat_ptr_components = (data_ptr, len);
                    #box_path::from_raw(::core::mem::transmute::<(*mut u8, usize), *mut Self>(fat_ptr_components))
                } else {
                    let mem_ptr = #alloc_path(final_layout);
                    if mem_ptr.is_null() {
                        #handle_alloc_error
                    }

                    let fat_ptr_components = (mem_ptr, len);
                    let fat_ptr = ::core::mem::transmute::<(*mut u8, usize), *mut Self>(fat_ptr_components);

                    #( #header_field_writes )*

                    #write_tail_data_call_tokens

                    #box_path::from_raw(fat_ptr)
                }
            }
        }
    }
}

fn generate_build_method_for_slice(
    builder_args: &MakeDstBuilderArgs,
    struct_name_ident: &Ident,
    field_info: &FieldInfo,
    element_type: &Type,
    input_struct_generics: &Generics,
    header_layout_calc_tokens: &TokenStream,
    offset_of_tail_payload_ident: &Ident,
    layout_header_full_ident: &Ident,
    unique_tuple_ident: &Ident,
) -> TokenStream {
    let base_method_name = &builder_args.base_method_name;
    let tail_field_name_ident_for_access = field_info.tail_field_name_ident;

    let method_name_ident = base_method_name.clone();
    let method_doc_string =
        format!("Creates an instance of `Box<{struct_name_ident}>` from a slice.");

    let build_fn_tail_param_tokens = quote! { #tail_field_name_ident_for_access: &[#element_type] };

    let clone_predicate: syn::WherePredicate = syn::parse_quote_spanned! {element_type.span()=>
        #element_type: ::core::clone::Clone
    };

    let mut final_where_clause = input_struct_generics
        .where_clause
        .clone()
        .unwrap_or_else(|| syn::WhereClause {
            where_token: Where::default(),
            predicates: Punctuated::new(),
        });

    final_where_clause.predicates.push(clone_predicate);

    let build_fn_where_clause_tokens = Some(quote! { #final_where_clause });

    let num_header_fields = field_info.header_field_idents.len();
    let tail_param_tuple_idx = syn::Index::from(num_header_fields);

    let build_fn_tail_processing_tokens = quote! {
        let len = #unique_tuple_ident.#tail_param_tuple_idx.len();
    };

    let layout_calculation_for_tail_payload_tokens =
        create_array_layout_calculation(element_type, field_info.tail_field.ty.span());

    let write_tail_data_call_tokens = quote_spanned! {element_type.span()=>
        let dest_slice_data_ptr = ::core::ptr::addr_of_mut!((*fat_ptr).#tail_field_name_ident_for_access).cast::<#element_type>();
        for idx in 0..len {
            let src_val_ref = #unique_tuple_ident.#tail_param_tuple_idx.get_unchecked(idx);
            #[allow(clippy::clone_on_copy)]
            let cloned_val = src_val_ref.clone();
            ::core::ptr::write(dest_slice_data_ptr.add(idx), cloned_val);
        }
    };

    generate_build_method_common(CommonBuildParams {
        builder_args,
        field_info,
        header_layout_calc_tokens,
        offset_of_tail_payload_ident,
        layout_header_full_ident,
        unique_tuple_ident,
        method_name_ident,
        build_fn_tail_param_tokens,
        build_fn_generics_tokens: None,
        build_fn_where_clause_tokens,
        build_fn_tail_processing_tokens,
        layout_calculation_for_tail_payload_tokens,
        write_tail_data_call_tokens,
        method_doc_string,
    })
}

fn generate_build_method_for_iterator(
    builder_args: &MakeDstBuilderArgs,
    struct_name_ident: &Ident,
    field_info: &FieldInfo,
    element_type: &Type,
    header_layout_calc_tokens: &TokenStream,
    offset_of_tail_payload_ident: &Ident,
    layout_header_full_ident: &Ident,
    unique_tuple_ident: &Ident,
) -> TokenStream {
    let base_method_name = &builder_args.base_method_name;
    let tail_field_name_ident_for_access = field_info.tail_field_name_ident;

    let method_name_ident = format_ident!("{}_from_iter", base_method_name);
    let method_doc_string =
        format!("Creates an instance of `Box<{struct_name_ident}>` from an iterator.");

    let iter_generic_param = format_ident!("I");
    let build_fn_tail_param_tokens =
        quote! { #tail_field_name_ident_for_access: #iter_generic_param };
    let build_fn_generics_tokens = Some(quote! { <#iter_generic_param> });
    let build_fn_where_clause_tokens = Some(quote! {
        where #iter_generic_param: ::core::iter::IntoIterator<Item = #element_type>,
              <#iter_generic_param as ::core::iter::IntoIterator>::IntoIter: ::core::iter::ExactSizeIterator
    });

    let num_header_fields = field_info.header_field_idents.len();
    let tail_param_tuple_idx = syn::Index::from(num_header_fields);

    let build_fn_tail_processing_tokens = quote! {
        let iter = #unique_tuple_ident.#tail_param_tuple_idx.into_iter();
        let len = iter.len();
    };

    let layout_calculation_for_tail_payload_tokens =
        create_array_layout_calculation(element_type, field_info.tail_field.ty.span());

    let write_tail_data_call_tokens = quote! {
        let dest_slice_data_ptr = ::core::ptr::addr_of_mut!((*fat_ptr).#tail_field_name_ident_for_access).cast::<#element_type>();
        for (i, element) in iter.enumerate() {
             ::core::ptr::write(dest_slice_data_ptr.add(i), element);
        }
    };

    generate_build_method_common(CommonBuildParams {
        builder_args,
        field_info,
        header_layout_calc_tokens,
        offset_of_tail_payload_ident,
        layout_header_full_ident,
        unique_tuple_ident,
        method_name_ident,
        build_fn_tail_param_tokens,
        build_fn_generics_tokens,
        build_fn_where_clause_tokens,
        build_fn_tail_processing_tokens,
        layout_calculation_for_tail_payload_tokens,
        write_tail_data_call_tokens,
        method_doc_string,
    })
}

fn generate_build_method_for_str(
    builder_args: &MakeDstBuilderArgs,
    struct_name_ident: &Ident,
    field_info: &FieldInfo,
    header_layout_calc_tokens: &TokenStream,
    offset_of_tail_payload_ident: &Ident,
    layout_header_full_ident: &Ident,
    unique_tuple_ident: &Ident,
) -> TokenStream {
    let base_method_name = &builder_args.base_method_name;
    let tail_field_name_ident_for_access = field_info.tail_field_name_ident;

    let method_name_ident = base_method_name.clone();
    let method_doc_string =
        format!("Creates an instance of `Box<{struct_name_ident}>` from a string slice.");

    let build_fn_tail_param_tokens =
        quote! { #tail_field_name_ident_for_access: impl ::core::convert::AsRef<str> };

    let num_header_fields = field_info.header_field_idents.len();
    let tail_param_tuple_idx = syn::Index::from(num_header_fields);

    let build_fn_tail_processing_tokens = quote! {
        let s = #unique_tuple_ident.#tail_param_tuple_idx.as_ref();
        let len = s.len();
    };

    let layout_calculation_for_tail_payload_tokens =
        create_array_layout_calculation(&quote! { u8 }, field_info.tail_field.ty.span());

    let write_tail_data_call_tokens = quote! {
        let dest_str_data_ptr = (&raw mut (*fat_ptr).#tail_field_name_ident_for_access).cast::<u8>();
        ::core::ptr::copy_nonoverlapping(s.as_ptr(), dest_str_data_ptr, len);
    };

    generate_build_method_common(CommonBuildParams {
        builder_args,
        field_info,
        header_layout_calc_tokens,
        offset_of_tail_payload_ident,
        layout_header_full_ident,
        unique_tuple_ident,
        method_name_ident,
        build_fn_tail_param_tokens,
        build_fn_generics_tokens: None,
        build_fn_where_clause_tokens: None,
        build_fn_tail_processing_tokens,
        layout_calculation_for_tail_payload_tokens,
        write_tail_data_call_tokens,
        method_doc_string,
    })
}

/// Generate builder methods for dynamically sized types (DST) structs.
///
/// This macro, when applied to a struct whose last field is a slice (`[T]`) or `str`,
/// generates an `impl` block with methods to construct instances of the structs with
/// dynamically sized tail data.
///
/// This attribute is generally used without any arguments, but more advanced
/// uses can pass arguments with the following grammar:
///
/// ```ignore
/// #[make_dst_builder(<base_method_name>[, <visibility>] [, no_std])]
/// ```
/// Refer to the [crate-level documentation](crate) for more details and example uses.
///
/// # Usage
///
/// ```rust
/// use tail_extend::make_dst_builder;
///
/// #[make_dst_builder]
/// struct MyStruct {
///     id: u32,
///     data: [u8],
/// }
///
/// // With custom method name and public visibility
/// #[make_dst_builder(create, pub)]
/// struct PublicStruct {
///     id: u32,
///     data: str,
/// }
/// ```
#[proc_macro_attribute]
pub fn make_dst_builder(
    attr_args_ts: proc_macro::TokenStream,
    item_ts: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let result = make_dst_builder_impl(attr_args_ts.into(), item_ts.into());
    result.unwrap_or_else(|err| err.to_compile_error()).into()
}

fn make_dst_builder_impl(
    attr_args_ts: TokenStream,
    item_ts: TokenStream,
) -> SynResult<TokenStream> {
    let builder_args = if attr_args_ts.is_empty() {
        MakeDstBuilderArgs {
            visibility: Visibility::Inherited,
            base_method_name: format_ident!("build"),
            no_std: false,
        }
    } else {
        syn::parse2(attr_args_ts)?
    };
    let input_struct: ItemStruct = syn::parse2(item_ts)?;

    let struct_name_ident = &input_struct.ident;
    let struct_generics: &Generics = &input_struct.generics;
    let (impl_generics, ty_generics, where_clause) = struct_generics.split_for_impl();

    let field_info = extract_field_info(&input_struct)?;

    let header_layout_calc_tokens =
        generate_header_layout_code(&field_info.header_field_structs, struct_generics);

    let mut generated_build_methods: Vec<TokenStream> = Vec::new();
    let offset_of_tail_payload_ident =
        format_ident!("offset_of_{}_payload", field_info.tail_field_name_ident);
    let layout_header_full_ident = format_ident!("layout_header_full");

    // Collect incoming argument names from the signature to avoid collision for the tuple variable
    let mut arg_names_for_collision_check = field_info
        .header_field_idents
        .iter()
        .map(std::string::ToString::to_string)
        .collect::<HashSet<String>>();
    arg_names_for_collision_check.insert(field_info.tail_field_name_ident.to_string());

    // Generate a unique name for the tuple (e.g., a, aa, aaa)
    let mut generated_tuple_name_str = "a".to_string();
    while arg_names_for_collision_check.contains(&generated_tuple_name_str) {
        generated_tuple_name_str.push('a');
    }
    let unique_tuple_ident = format_ident!(
        "{}",
        generated_tuple_name_str,
        span = proc_macro2::Span::call_site()
    );

    match &field_info.tail_field.ty {
        Type::Slice(TypeSlice { elem, .. }) => {
            generated_build_methods.push(generate_build_method_for_slice(
                &builder_args,
                struct_name_ident,
                &field_info,
                elem,
                struct_generics,
                &header_layout_calc_tokens,
                &offset_of_tail_payload_ident,
                &layout_header_full_ident,
                &unique_tuple_ident, // Pass generated ident
            ));
            generated_build_methods.push(generate_build_method_for_iterator(
                &builder_args,
                struct_name_ident,
                &field_info,
                elem,
                &header_layout_calc_tokens,
                &offset_of_tail_payload_ident,
                &layout_header_full_ident,
                &unique_tuple_ident, // Pass generated ident
            ));
        }
        _ => {
            generated_build_methods.push(generate_build_method_for_str(
                &builder_args,
                struct_name_ident,
                &field_info,
                &header_layout_calc_tokens,
                &offset_of_tail_payload_ident,
                &layout_header_full_ident,
                &unique_tuple_ident, // Pass generated ident
            ));
        }
    }

    let build_methods_impl_block = quote! {
        impl #impl_generics #struct_name_ident #ty_generics #where_clause {
            #( #generated_build_methods )*
        }
    };

    let expanded = quote! {
        #input_struct
        #build_methods_impl_block
    };

    Ok(expanded)
}

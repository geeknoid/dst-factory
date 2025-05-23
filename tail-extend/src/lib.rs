//! # `tail-extend`: Build `Box<DST>` with Ease
//!
//! This crate provides the `#[make_dst_builder]` procedural attribute macro
//! for Rust structs. When applied to a struct definition that is a Dynamically
//! Sized Type (DST) – meaning its last field is a slice `[T]` or `str` –
//! this macro automatically generates an `impl` block. This block contains
//! constructor methods to easily and safely create `Box<Self>` instances
//! of the DST.
//!
//! The primary goal of `#[make_dst_builder]` is to abstract away the manual and
//! error-prone complexities of memory layout calculations and unsafe pointer
//! operations typically required for initializing DSTs where the size of the
//! final field is determined at runtime.
//!
//! ## Usage
//!
//! Apply the `#[make_dst_builder]` attribute directly above your struct definition.
//! You can customize the visibility and base name of the generated constructor methods.
//!
//! ### Syntax
//!
//! The attribute supports the following forms:
//!
//! 1.  **Default (private `build` method):**
//!     ```rust
//!     # use tail_extend::make_dst_builder;
//!     #[make_dst_builder]
//!     # struct MyStruct { field: u32, data: [u8] }
//!     ```
//!     Generates `fn build(...) -> Box<Self>`.
//!
//! 2.  **Custom method name (private):**
//!     ```rust
//!     # use tail_extend::make_dst_builder;
//!     #[make_dst_builder(create)]
//!     # struct MyStruct { field: u32, data: [u8] }
//!     ```
//!     Generates `fn create(...) -> Box<Self>`.
//!
//! 3.  **Public visibility (default `build` method name):**
//!     ```rust
//!     # use tail_extend::make_dst_builder;
//!     #[make_dst_builder(pub)]
//!     # struct MyStruct { field: u32, data: [u8] }
//!     ```
//!     Generates `pub fn build(...) -> Box<Self>`.
//!
//! 4.  **Public visibility and custom method name:**
//!     ```rust
//!     # use tail_extend::make_dst_builder;
//!     #[make_dst_builder(new, pub)]
//!     # struct MyStruct { field: u32, data: [u8] }
//!     ```
//!     Generates `pub fn new(...) -> Box<Self>`.
//!
//! 5. **With `no_std` support:**
//!     ```rust
//!     # use tail_extend::make_dst_builder;
//!     #[make_dst_builder(no_std)]
//!     # struct MyStruct { field: u32, data: [u8] }
//!     ```
//!     Generates methods using `::alloc::boxed::Box` instead of `::std::boxed::Box`.
//!
//! 6. **Combining options:**
//!     ```rust
//!     # use tail_extend::make_dst_builder;
//!     #[make_dst_builder(create, pub, no_std)]
//!     # struct MyStruct { field: u32, data: [u8] }
//!     ```
//!     Generates public methods with custom name and `no_std` support.
//!
//! You can use other visibilities like `pub(crate)` as well.
//!
//! ## Generated Methods
//!
//! Based on the type of the last field (the "tail" field), the macro generates
//! one or two constructor methods. Let `<base_method_name>` be the name you
//! specified (or `build` by default), and `HeaderFieldType1, HeaderFieldType2, ...`
//! be the types of the fields preceding the tail field.
//!
//! ### For `tail: [T]` (slice tail)
//!
//! 1.  **From a slice:**
//!     ```rust
//!     # use tail_extend::make_dst_builder;
//!     # #[make_dst_builder(example_build, pub)]
//!     # struct ExampleSlice<G> { id: u32, generic_field: G, data: [u8] }
//!     # fn signature_example<G>(id: u32, generic_field: G, data: &[u8]) -> Box<ExampleSlice<G>> {
//!     // Signature:
//!     // pub fn <base_method_name>(
//!     //     header_field1: HeaderFieldType1,
//!     //     header_field2: HeaderFieldType2,
//!     //     ...,
//!     //     tail_data: &[T]
//!     // ) -> Box<Self>;
//!     # todo!()
//!     # }
//!     ```
//!
//! 2.  **From an iterator (ExactSizeIterator):**
//!     A `<base_method_name>_from_iter` method is also generated.
//!     ```rust
//!     # use tail_extend::make_dst_builder;
//!     # #[make_dst_builder(example_build, pub)]
//!     # struct ExampleSliceIter<G> { id: u32, generic_field: G, data: [u8] }
//!     # fn signature_example_from_iter<G, I>(id: u32, generic_field: G, data_iter: I) -> Box<ExampleSliceIter<G>>
//!     # where I: IntoIterator<Item = u8>, <I as IntoIterator>::IntoIter: ExactSizeIterator
//!     # {
//!     // Signature:
//!     // pub fn <base_method_name>_from_iter<IterType>(
//!     //     header_field1: HeaderFieldType1,
//!     //     header_field2: HeaderFieldType2,
//!     //     ...,
//!     //     tail_data_iter: IterType
//!     // ) -> Box<Self>
//!     // where
//!     //     IterType: IntoIterator<Item = T>,
//!     //     <IterType as IntoIterator>::IntoIter: ExactSizeIterator;
//!     # todo!()
//!     # }
//!     ```
//!
//! ### For `tail: str` (string slice tail)
//!
//! 1.  **From a string slice:**
//!     ```rust
//!     # use tail_extend::make_dst_builder;
//!     # #[make_dst_builder(example_build_str, pub)]
//!     # struct ExampleStr { name: String, description: str }
//!     # fn signature_example_str(name: String, description: &str) -> Box<ExampleStr> {
//!     // Signature:
//!     // pub fn <base_method_name>(
//!     //     header_field1: HeaderFieldType1,
//!     //     header_field2: HeaderFieldType2,
//!     //     ...,
//!     //     tail_data: &str
//!     // ) -> Box<Self>;
//!     # todo!()
//!     # }
//!     ```
//!
//! ## Example
//!
//! Here's a comprehensive example demonstrating generics, lifetimes, and different
//! tail types:
//!
//! ```rust
//! use tail_extend::make_dst_builder;
//! use std::fmt::Debug;
//!
//! // Struct with a slice tail and generics
//! #[make_dst_builder(create_packet, pub)]
//! #[derive(Debug, PartialEq)] // For easy testing
//! pub struct DataPacket<'a, IdType: Copy + Debug + PartialEq> { // Renamed ID_TYPE to IdType
//!     packet_id: IdType,
//!     source_ip: &'a str,
//!     is_urgent: bool,
//!     payload: [u8], // The dynamically sized tail field
//! }
//!
//! // Struct with a str tail
//! #[make_dst_builder(create_log)] // Private method
//! #[derive(Debug, PartialEq)]
//! pub struct LogEntry {
//!     timestamp: u64,
//!     level: String,
//!     message: str, // The dynamically sized tail field
//! }
//!
//! // --- DataPacket Example ---
//! let packet1_payload = vec![0xDE, 0xAD, 0xBE, 0xEF];
//! let packet1 = DataPacket::create_packet(
//!     1001_u32,      // packet_id
//!     "192.168.1.1", // source_ip
//!     true,          // is_urgent
//!     &packet1_payload, // payload (slice)
//! );
//!
//! assert_eq!(packet1.packet_id, 1001_u32);
//! assert_eq!(packet1.source_ip, "192.168.1.1");
//! assert!(packet1.is_urgent);
//! assert_eq!(packet1.payload, [0xDE, 0xAD, 0xBE, 0xEF]);
//! println!("Packet 1: {:?}", packet1);
//!
//! // Using the _from_iter method for DataPacket (iterator)
//! let packet2_payload_iter = vec![0xFF, 0xEE, 0xDD];
//! let packet2 = DataPacket::create_packet_from_iter(
//!     2002_u16,      // packet_id (different generic type)
//!     "10.0.0.5",    // source_ip
//!     false,         // is_urgent
//!     packet2_payload_iter, // payload (iterator)
//! );
//! assert_eq!(packet2.packet_id, 2002_u16);
//! assert_eq!(packet2.payload, [0xFF, 0xEE, 0xDD]);
//! println!("Packet 2 (from iter): {:?}", packet2);
//!
//! // --- LogEntry Example ---
//! let log1 = LogEntry::create_log(
//!     1678886400,
//!     "INFO".to_string(),
//!     "System startup complete.",
//! );
//!
//! assert_eq!(log1.timestamp, 1678886400);
//! assert_eq!(log1.level, "INFO");
//! assert_eq!(&log1.message, "System startup complete."); // Compare &log1.message with &str
//! println!("Log 1: {:?}", log1);
//! ```
//!
//! ## Error Conditions
//!
//! The `#[make_dst_builder]` macro produces a compile-time error if:
//!
//! - It's applied to anything other than a struct with named fields.
//! - The struct has no fields (a tail field is essential for a DST).
//! - The last field of the struct is not a slice (`[T]`) or a string slice (`str`).
//! - The arguments provided to the `#[make_dst_builder(...)]` attribute are malformed
//!   (e.g., incorrect visibility keyword, too many arguments).

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use std::collections::HashSet;
use syn::{
    Field, Fields, GenericParam, Generics, Ident, ItemStruct, Lifetime, Type, TypePath, TypeSlice,
    Visibility,
    parse::{Parse, ParseStream, Result as SynResult},
    punctuated::Punctuated,
    spanned::Spanned,
    visit::{self, Visit},
};

// --- Helper Struct for Macro Arguments ---

struct MakeDstBuilderArgs {
    visibility: Visibility,
    base_method_name: Ident,
    no_std: bool,
}

impl Parse for MakeDstBuilderArgs {
    fn parse(input: ParseStream) -> SynResult<Self> {
        // Default values
        let mut visibility: Visibility = Visibility::Inherited;
        let mut base_method_name = Ident::new("build", input.span());
        let mut no_std = false;

        // Exit early if there are no arguments
        if input.is_empty() {
            return Ok(Self {
                visibility,
                base_method_name,
                no_std,
            });
        }

        // First parameter: Check for method name or no_std
        if input.peek(syn::Ident) {
            let ident: Ident = input.parse()?;
            if ident == "no_std" {
                no_std = true;
                return Ok(Self {
                    visibility,
                    base_method_name,
                    no_std,
                });
            }

            // It's a custom method name
            base_method_name = ident;

            // Must be followed by a comma if more arguments exist
            if input.is_empty() {
                return Ok(Self {
                    visibility,
                    base_method_name,
                    no_std,
                });
            }

            let _comma: syn::Token![,] = input.parse()?;
        }

        // Second parameter: Check for visibility
        if !input.is_empty() && input.peek(syn::Token![pub]) {
            visibility = input.parse()?;

            // Must be followed by a comma if more arguments exist
            if input.is_empty() {
                return Ok(Self {
                    visibility,
                    base_method_name,
                    no_std,
                });
            }

            let _comma: syn::Token![,] = input.parse()?;
        }

        // Third parameter: Check for no_std
        if !input.is_empty() && input.peek(syn::Ident) {
            let ident: Ident = input.parse()?;
            if ident == "no_std" {
                no_std = true;
            } else {
                return Err(input.error("Expected 'no_std' as the third argument"));
            }
        }

        // Ensure we've consumed all tokens
        if !input.is_empty() {
            return Err(input.error("Expected end of input, found additional tokens"));
        }

        Ok(Self {
            visibility,
            base_method_name,
            no_std,
        })
    }
}

// --- Helper Structs for Extracted Information ---
struct FieldInfo<'a> {
    header_field_structs: Vec<&'a Field>,
    header_field_idents: Vec<&'a Ident>,
    build_fn_header_params: Vec<proc_macro2::TokenStream>,
    tail_field: &'a Field,
    tail_field_name_ident: &'a Ident,
}

// --- Helper Struct for Common Build Method Parameters ---
struct CommonBuildParams<'a> {
    method_name_ident: &'a Ident,
    build_fn_tail_param_tokens: &'a proc_macro2::TokenStream,
    build_fn_generics_tokens: Option<&'a proc_macro2::TokenStream>,
    build_fn_where_clause_tokens: Option<&'a proc_macro2::TokenStream>,
    build_fn_tail_processing_tokens: &'a proc_macro2::TokenStream,
    layout_calculation_for_tail_payload_tokens: &'a proc_macro2::TokenStream,
    write_tail_data_call_tokens: &'a proc_macro2::TokenStream,
    create_node_raw_ptr_for_non_zst_tokens: &'a proc_macro2::TokenStream,
    create_zst_node_raw_ptr_tokens: &'a proc_macro2::TokenStream,
    method_doc_string: &'a str,
}

// --- Helper for array/slice layout calculations ---
fn create_array_layout_calculation<T: quote::ToTokens>(
    element_type: &T,
    span: proc_macro2::Span,
) -> proc_macro2::TokenStream {
    quote_spanned! {span=>
        let layout_tail_payload =
            ::core::alloc::Layout::array::<#element_type>(len)
                .expect("Overflow in memory layout calculation");
    }
}

// --- Helper for creating raw node pointers for slices ---
fn create_slice_raw_ptr_tokens<T: quote::ToTokens>(
    element_type: &T,
) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    let non_zst = quote! {
        ::core::ptr::slice_from_raw_parts_mut(mem_ptr as *mut #element_type, len) as *mut Self
    };

    let zst = quote! {
        {
            let data_ptr = ::core::ptr::NonNull::<#element_type>::dangling().as_ptr();
            ::core::ptr::slice_from_raw_parts_mut(data_ptr, len) as *mut Self
        }
    };

    (non_zst, zst)
}

// --- Helper for creating raw node pointers for string slices ---
fn create_str_raw_ptr_tokens() -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    let non_zst = quote! {
        let fat_pointer_components = (mem_ptr as *mut (), len);
        ::core::mem::transmute(fat_pointer_components)
    };

    let zst = quote! {
        {
            let data_ptr = ::core::ptr::NonNull::<u8>::dangling().as_ptr();
            let fat_pointer_components = (data_ptr as *mut (), len);
            ::core::mem::transmute(fat_pointer_components)
        }
    };

    (non_zst, zst)
}

// --- Helper for Generic Parameter Usage Analysis ---
#[derive(Debug, Default)]
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
                if self.defined_type_param_idents.contains(ident) {
                    self.used_params.types.insert(ident.clone());
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
    let mut build_fn_header_params: Vec<proc_macro2::TokenStream> = Vec::new();

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
            let is_dst = match &last_field.ty {
                Type::Slice(_) => true,
                Type::Path(type_path) if type_path.path.is_ident("str") => true,
                Type::Path(TypePath { path, .. })
                    if path.segments.last().is_some_and(|seg| seg.ident == "str") =>
                {
                    true
                }
                _ => false,
            };

            if !is_dst {
                return Err(syn::Error::new_spanned(
                    &last_field.ty,
                    "Last field must be a dynamically sized type like [T] or str",
                ));
            }

            let num_fields = fields_named.named.len();
            for (i, field) in fields_named.named.iter().enumerate() {
                if i < num_fields - 1 {
                    header_field_structs.push(field);
                    let field_ident = field.ident.as_ref().ok_or_else(|| {
                        syn::Error::new(
                            field.span(),
                            "Internal error: Named field is missing an identifier.",
                        )
                    })?;
                    header_field_idents.push(field_ident);
                    let field_ty = &field.ty;
                    build_fn_header_params.push(quote! { #field_ident: #field_ty });
                }
            }
            tail_field = fields_named.named.last().unwrap();
            tail_field_name_ident = tail_field.ident.as_ref().unwrap();
        }
        Fields::Unnamed(_) | Fields::Unit => {
            return Err(syn::Error::new_spanned(
                &input_struct.fields,
                "Structs with unnamed fields are not supported",
            ));
        }
    }
    Ok(FieldInfo {
        header_field_structs,
        header_field_idents,
        build_fn_header_params,
        tail_field,
        tail_field_name_ident,
    })
}

#[expect(clippy::too_many_lines)]
fn generate_header_layout_code(
    header_field_structs: &[&Field],
    input_struct_generics: &Generics,
) -> proc_macro2::TokenStream {
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
            syn::visit::visit_where_predicate(&mut predicate_visitor, predicate);

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

    quote_spanned! {helper_struct_name.span()=>
        #[allow(dead_code)]
        struct #helper_struct_name #helper_impl_generics #helper_where_clause_for_impl {
            #( #helper_fields_definitions ),*
        }
        let #layout_header_full_ident = ::core::alloc::Layout::new::<#helper_struct_name #helper_ty_generics>();
    }
}

// --- Unified Build Method Generation ---

fn generate_common_build_method(
    builder_args: &MakeDstBuilderArgs,
    field_info: &FieldInfo,
    header_layout_calc_tokens: &proc_macro2::TokenStream,
    offset_of_tail_payload_ident: &Ident,
    layout_header_full_ident: &Ident,
    common_params: &CommonBuildParams,
) -> proc_macro2::TokenStream {
    let method_visibility = &builder_args.visibility;
    let build_fn_header_params = &field_info.build_fn_header_params;
    let header_field_idents_for_write = &field_info.header_field_idents;
    let no_std = builder_args.no_std;

    // Box and allocation namespace paths based on no_std flag
    let box_path = if no_std {
        quote! { ::alloc::boxed::Box }
    } else {
        quote! { ::std::boxed::Box }
    };

    let alloc_path = if no_std {
        quote! { ::alloc::alloc::alloc }
    } else {
        quote! { ::std::alloc::alloc }
    };

    // Handle allocation error based on no_std flag
    let handle_alloc_error = if no_std {
        quote! { panic!("memory allocation failed: out of memory") }
    } else {
        quote! { ::std::alloc::handle_alloc_error(final_layout) }
    };

    // Destructure common_params for easier use in quote!
    let CommonBuildParams {
        method_name_ident,
        build_fn_tail_param_tokens,
        build_fn_generics_tokens,
        build_fn_where_clause_tokens,
        build_fn_tail_processing_tokens,
        layout_calculation_for_tail_payload_tokens,
        write_tail_data_call_tokens,
        create_node_raw_ptr_for_non_zst_tokens,
        create_zst_node_raw_ptr_tokens,
        method_doc_string,
    } = common_params;

    quote! {
        #[doc = #method_doc_string]
        #[allow(unused_variables)] // For len, iter etc. if not used by all paths
        #[expect(clippy::transmute_undefined_repr)] // For the transmutes
        #method_visibility fn #method_name_ident #build_fn_generics_tokens (
            #( #build_fn_header_params, )*
            #build_fn_tail_param_tokens
        ) -> #box_path<Self> #build_fn_where_clause_tokens {
            #build_fn_tail_processing_tokens // Defines `len`, maybe `iter`

            #header_layout_calc_tokens
            #layout_calculation_for_tail_payload_tokens // Defines `layout_tail_payload`

            let (full_node_layout_val, #offset_of_tail_payload_ident) = #layout_header_full_ident
                .extend(layout_tail_payload)
                .expect("Overflow in memory layout calculation");
            let final_layout = full_node_layout_val.pad_to_align();

            unsafe {
                let node_raw_ptr: *mut Self = if final_layout.size() == 0 {
                    #create_zst_node_raw_ptr_tokens
                } else {
                    let mem_ptr = #alloc_path(final_layout);
                    if mem_ptr.is_null() {
                        #handle_alloc_error;
                    }
                    #create_node_raw_ptr_for_non_zst_tokens
                };

                #(
                    ::core::ptr::addr_of_mut!((*node_raw_ptr).#header_field_idents_for_write)
                        .write(#header_field_idents_for_write);
                )*

                #write_tail_data_call_tokens

                #box_path::from_raw(node_raw_ptr)
            }
        }
    }
}

fn generate_build_method_for_slice(
    builder_args: &MakeDstBuilderArgs,
    struct_name_ident: &Ident,
    field_info: &FieldInfo,
    element_type: &Type,
    header_layout_calc_tokens: &proc_macro2::TokenStream,
    offset_of_tail_payload_ident: &Ident,
    layout_header_full_ident: &Ident,
) -> proc_macro2::TokenStream {
    let base_method_name = &builder_args.base_method_name;
    let tail_field_name_ident_for_access = field_info.tail_field_name_ident; // Used in quoted tokens

    let method_name_ident = base_method_name.clone();
    let method_doc_string =
        format!("Creates an instance of `Box<{struct_name_ident}>` from a slice.",);

    let build_fn_tail_param_tokens = quote! { #tail_field_name_ident_for_access: &[#element_type] };
    let build_fn_tail_processing_tokens =
        quote! { let len = #tail_field_name_ident_for_access.len(); };

    let layout_calculation_for_tail_payload_tokens =
        create_array_layout_calculation(element_type, field_info.tail_field.ty.span());

    let write_tail_data_call_tokens = quote! {
        if final_layout.size() > 0 && len > 0 { // final_layout and len are in scope in common method
            let dest_slice_data_ptr = ::core::ptr::addr_of_mut!((*node_raw_ptr).#tail_field_name_ident_for_access).cast::<#element_type>();
            ::core::ptr::copy_nonoverlapping(#tail_field_name_ident_for_access.as_ptr(), dest_slice_data_ptr, len);
        }
    };

    let (create_node_raw_ptr_for_non_zst_tokens, create_zst_node_raw_ptr_tokens) =
        create_slice_raw_ptr_tokens(element_type);

    let common_params = CommonBuildParams {
        method_name_ident: &method_name_ident,
        build_fn_tail_param_tokens: &build_fn_tail_param_tokens,
        build_fn_generics_tokens: None,
        build_fn_where_clause_tokens: None,
        build_fn_tail_processing_tokens: &build_fn_tail_processing_tokens,
        layout_calculation_for_tail_payload_tokens: &layout_calculation_for_tail_payload_tokens,
        write_tail_data_call_tokens: &write_tail_data_call_tokens,
        create_node_raw_ptr_for_non_zst_tokens: &create_node_raw_ptr_for_non_zst_tokens,
        create_zst_node_raw_ptr_tokens: &create_zst_node_raw_ptr_tokens, // New field
        method_doc_string: &method_doc_string,
    };

    generate_common_build_method(
        builder_args,
        field_info,
        header_layout_calc_tokens,
        offset_of_tail_payload_ident,
        layout_header_full_ident,
        &common_params,
    )
}

fn generate_build_from_iter_method_for_slice(
    builder_args: &MakeDstBuilderArgs,
    struct_name_ident: &Ident,
    field_info: &FieldInfo,
    element_type: &Type,
    header_layout_calc_tokens: &proc_macro2::TokenStream,
    offset_of_tail_payload_ident: &Ident,
    layout_header_full_ident: &Ident,
) -> proc_macro2::TokenStream {
    let base_method_name = &builder_args.base_method_name;
    let tail_field_name_ident_for_access = field_info.tail_field_name_ident; // Used in quoted tokens

    let method_name_ident = format_ident!("{}_from_iter", base_method_name);
    let method_doc_string =
        format!("Creates an instance of `Box<{struct_name_ident}>` from an iterator.",);

    let iter_generic_param = format_ident!("I");
    let build_fn_tail_param_tokens =
        quote! { #tail_field_name_ident_for_access: #iter_generic_param };
    let build_fn_generics_tokens = Some(quote! { <#iter_generic_param> });
    let build_fn_where_clause_tokens = Some(quote! {
        where #iter_generic_param: ::core::iter::IntoIterator<Item = #element_type>,
              <#iter_generic_param as ::core::iter::IntoIterator>::IntoIter: ::core::iter::ExactSizeIterator
    });

    let build_fn_tail_processing_tokens = quote! {
        let mut iter = #tail_field_name_ident_for_access.into_iter();
        let len = iter.len();
    };

    let layout_calculation_for_tail_payload_tokens =
        create_array_layout_calculation(element_type, field_info.tail_field.ty.span());

    let write_tail_data_call_tokens = quote! {
        if final_layout.size() > 0 && len > 0 { // final_layout, len, iter are in scope
            let dest_slice_data_ptr = ::core::ptr::addr_of_mut!((*node_raw_ptr).#tail_field_name_ident_for_access).cast::<#element_type>();
            for (i, element) in iter.enumerate() {
                 unsafe { ::core::ptr::write(dest_slice_data_ptr.add(i), element); }
            }
        }
    };

    let (create_node_raw_ptr_for_non_zst_tokens, create_zst_node_raw_ptr_tokens) =
        create_slice_raw_ptr_tokens(element_type);

    let common_params = CommonBuildParams {
        method_name_ident: &method_name_ident,
        build_fn_tail_param_tokens: &build_fn_tail_param_tokens,
        build_fn_generics_tokens: build_fn_generics_tokens.as_ref(),
        build_fn_where_clause_tokens: build_fn_where_clause_tokens.as_ref(),
        build_fn_tail_processing_tokens: &build_fn_tail_processing_tokens,
        layout_calculation_for_tail_payload_tokens: &layout_calculation_for_tail_payload_tokens,
        write_tail_data_call_tokens: &write_tail_data_call_tokens,
        create_node_raw_ptr_for_non_zst_tokens: &create_node_raw_ptr_for_non_zst_tokens,
        create_zst_node_raw_ptr_tokens: &create_zst_node_raw_ptr_tokens,
        method_doc_string: &method_doc_string,
    };

    generate_common_build_method(
        builder_args,
        field_info,
        header_layout_calc_tokens,
        offset_of_tail_payload_ident,
        layout_header_full_ident,
        &common_params,
    )
}

fn generate_build_method_for_str(
    builder_args: &MakeDstBuilderArgs,
    struct_name_ident: &Ident,
    field_info: &FieldInfo,
    header_layout_calc_tokens: &proc_macro2::TokenStream,
    offset_of_tail_payload_ident: &Ident,
    layout_header_full_ident: &Ident,
) -> proc_macro2::TokenStream {
    let base_method_name = &builder_args.base_method_name;
    let tail_field_name_ident_for_access = field_info.tail_field_name_ident; // Used in quoted tokens

    let method_name_ident = base_method_name.clone();
    let method_doc_string =
        format!("Creates an instance of `Box<{struct_name_ident}>` from a string slice.",);

    let build_fn_tail_param_tokens = quote! { #tail_field_name_ident_for_access: &str };
    let build_fn_tail_processing_tokens = quote! {
        let len = #tail_field_name_ident_for_access.len();
    };

    let layout_calculation_for_tail_payload_tokens =
        create_array_layout_calculation(&quote! { u8 }, field_info.tail_field.ty.span());

    let write_tail_data_call_tokens = quote! {
        if final_layout.size() > 0 && len > 0 { // final_layout and len are in scope
            let dest_str_data_ptr = ::core::ptr::addr_of_mut!((*node_raw_ptr).#tail_field_name_ident_for_access) as *mut u8;
            ::core::ptr::copy_nonoverlapping(#tail_field_name_ident_for_access.as_ptr(), dest_str_data_ptr, len);
        }
    };

    let (create_node_raw_ptr_for_non_zst_tokens, create_zst_node_raw_ptr_tokens) =
        create_str_raw_ptr_tokens();

    let common_params = CommonBuildParams {
        method_name_ident: &method_name_ident,
        build_fn_tail_param_tokens: &build_fn_tail_param_tokens,
        build_fn_generics_tokens: None,
        build_fn_where_clause_tokens: None,
        build_fn_tail_processing_tokens: &build_fn_tail_processing_tokens,
        layout_calculation_for_tail_payload_tokens: &layout_calculation_for_tail_payload_tokens,
        write_tail_data_call_tokens: &write_tail_data_call_tokens,
        create_node_raw_ptr_for_non_zst_tokens: &create_node_raw_ptr_for_non_zst_tokens,
        create_zst_node_raw_ptr_tokens: &create_zst_node_raw_ptr_tokens,
        method_doc_string: &method_doc_string,
    };

    generate_common_build_method(
        builder_args,
        field_info,
        header_layout_calc_tokens,
        offset_of_tail_payload_ident,
        layout_header_full_ident,
        &common_params,
    )
}

/// Generate builder methods for dynamically-sized types (DST) structs.
///
/// This macro, when applied to a struct whose last field is a slice (`[T]`) or `str`,
/// generates an `impl` block with methods to construct `Box<Self>` instances with
/// dynamically sized tail data.
///
/// # Usage
///
/// ```rust
/// use tail_extend::make_dst_builder;
///
/// #[make_dst_builder(new, )]
/// struct MyStruct {
///     id: u32,
///     data: [u8],
/// }
/// // Generates: pub fn new(id: u32, data: &[u8]) -> Box<MyStruct>
/// ```
///
/// # Attribute Arguments
///
/// - Optional visibility (e.g., `pub`, `pub(crate)`)
/// - Optional base method name (default: `build`)
///
/// # Generated Methods
///
/// - For `[T]` tail: `fn <base>(fields..., tail: &[T]) -> Box<Self>`
/// - For `[T]` tail: `fn <base>_from_iter<I>(fields..., tail: I) -> Box<Self>`
///   where `I: IntoIterator<Item = T>, I::IntoIter: ExactSizeIterator`
/// - For `str` tail: `fn <base>(fields..., tail: &str) -> Box<Self>`
///
/// # Errors
///
/// - Only structs with named fields are supported.
/// - The last field must be a slice or `str`.
/// - Malformed macro arguments will cause a compile error.
#[proc_macro_attribute]
pub fn make_dst_builder(attr_args_ts: TokenStream, item_ts: TokenStream) -> TokenStream {
    let result = make_dst_builder_impl(attr_args_ts.into(), item_ts.into());
    result.unwrap_or_else(|err| err.to_compile_error()).into()
}

fn make_dst_builder_impl(
    attr_args_ts: proc_macro2::TokenStream,
    item_ts: proc_macro2::TokenStream,
) -> SynResult<proc_macro2::TokenStream> {
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

    let mut generated_build_methods: Vec<proc_macro2::TokenStream> = Vec::new();
    let offset_of_tail_payload_ident =
        format_ident!("offset_of_{}_payload", field_info.tail_field_name_ident);
    let layout_header_full_ident = format_ident!("layout_header_full");

    match &field_info.tail_field.ty {
        Type::Slice(TypeSlice { elem, .. }) => {
            generated_build_methods.push(generate_build_method_for_slice(
                &builder_args,
                struct_name_ident,
                &field_info,
                elem,
                &header_layout_calc_tokens,
                &offset_of_tail_payload_ident,
                &layout_header_full_ident,
            ));
            generated_build_methods.push(generate_build_from_iter_method_for_slice(
                &builder_args,
                struct_name_ident,
                &field_info,
                elem,
                &header_layout_calc_tokens,
                &offset_of_tail_payload_ident,
                &layout_header_full_ident,
            ));
        }
        Type::Path(type_path) if type_path.path.is_ident("str") => {
            generated_build_methods.push(generate_build_method_for_str(
                &builder_args,
                struct_name_ident,
                &field_info,
                &header_layout_calc_tokens,
                &offset_of_tail_payload_ident,
                &layout_header_full_ident,
            ));
        }
        _ => {
            return Err(syn::Error::new_spanned(
                &field_info.tail_field.ty,
                "Last field must be a dynamically sized type like [T] or str",
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

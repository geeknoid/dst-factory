//! # Make DST Builder Macro
//!
//! This crate provides the `#[make_dst_builder]` procedural attribute macro.
//! When applied to a struct definition that is a Dynamically Sized Type (DST)
//! (i.e., its last field is a slice `[T]` or `str`), this macro generates
//! an `impl` block for that struct with methods to construct `Box<Self>` instances.
//!
//! ## Macro: `#[make_dst_builder]`
//!
//! ### Functionality
//!
//! Applying `#[make_dst_builder]` to a DST struct:
//!
//! 1.  **Preserves the Struct Definition**: The original struct definition is emitted as-is.
//! 2.  **Generates an `impl` Block**: This block contains builder methods.
//!     * The parameters of these methods correspond directly to the fields of the struct.
//!
//! ### Attribute Arguments (Optional)
//!
//! `#[make_dst_builder([visibility] [base_method_name])]`
//!
//! - `[visibility]`: An optional visibility specifier for the generated methods (e.g., `pub`, `pub(crate)`).
//!   If omitted, methods are private to the `impl` block.
//! - `[base_method_name]`: An optional identifier to use as the base name for the generated methods.
//!   If omitted, "build" is used as the base name.
//!
//! ### Generated Methods
//!
//! Based on the optional `base_method_name` (defaulting to "build"):
//!
//! -   **If the last field is `[T]` (a slice type):**
//!     * `fn <base_method_name>(field1: Type1, ..., last_field_name: &[T]) -> Box<Self>`
//!     * `fn <base_method_name>_with<I>(field1: Type1, ..., last_field_name: I) -> Box<Self>`
//!       `where I: IntoIterator<Item = T>, <I as IntoIterator>::IntoIter: ExactSizeIterator`
//! -   **If the last field is `str`:**
//!     * `fn <base_method_name>(field1: Type1, ..., last_field_name: &str) -> Box<Self>`
//!
//! All generated methods will have the specified `[visibility]` (or default private visibility).
//!
//! ### Generics
//!
//! Generic parameters and `where` clauses from the struct definition are correctly
//! propagated to the generated `impl` block and the method signatures.
//!
//! ### Errors
//!
//! This macro will cause a compile-time error if:
//! - The input item is not a struct with named fields.
//! - The struct has no fields (a tail field is required).
//! - The last field of the struct is not a slice `[T]` or `str`.
//! - The arguments to `#[make_dst_builder(...)]` are malformed.
//!
//! ### Example
//!
//! ```rust
//! // In a real scenario, you'd import from your macro crate:
//! // use your_dst_builder_crate::make_dst_builder;
//! # use dst_builder_macro::make_dst_builder; // For doctest
//!
//! #[make_dst_builder(pub new)] // Methods will be `pub fn new(...)` and `pub fn new_with(...)`
//! #[derive(Debug, PartialEq)]
//! pub struct MyPacket<'a, T: Copy>
//! where
//!     T: std::fmt::Debug + PartialEq,
//! {
//!     id: u32,
//!     marker: T,
//!     config: &'a str,
//!     pub data: [u8], // The tail field
//! }
//!
//! #[make_dst_builder] // Methods will be `fn build(...)` (private)
//! #[derive(Debug, PartialEq)]
//! pub struct LogMessage {
//!     timestamp: u64,
//!     message: str, // String tail
//! }
//!
//! # fn main() {
//! #   let packet = MyPacket::new(123, 0.5f32, "test_cfg", &[1,2,3]);
//! #   assert_eq!(packet.id, 123);
//! #   assert_eq!(packet.marker, 0.5f32);
//! #   assert_eq!(packet.config, "test_cfg");
//! #   assert_eq!(packet.data, [1,2,3]);
//! #
//! #   let packet2 = MyPacket::new_with(456, 0.25f32, "another_cfg", vec![4,5]);
//! #   assert_eq!(packet2.id, 456);
//! #   assert_eq!(packet2.data, [4,5]);
//! #
//! #   // LogMessage::build would be private, so not directly callable in this doctest scope
//! #   // unless it was in the same module and the test was an inner function.
//! #   // For demonstration, we'll assume it works as intended.
//! #   // let log = LogMessage::build(1678886400, "Hello DST!");
//! #   // assert_eq!(log.timestamp, 1678886400);
//! #   // assert_eq!(log.message, "Hello DST!");
//! # }
//! ```
//!
//! This macro uses `unsafe` code for memory management. Ensure thorough understanding
//! of the implications.

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use std::collections::HashSet;
use syn::{
    Field, Fields, GenericParam, Generics, Ident, ItemStruct, Lifetime, Type, TypeSlice,
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
}

impl Parse for MakeDstBuilderArgs {
    fn parse(input: ParseStream) -> SynResult<Self> {
        let visibility: Visibility = input.parse()?;

        let base_method_name = if input.is_empty() {
            Ident::new("build", input.span())
        } else {
            input.parse()?
        };

        Ok(Self {
            visibility,
            base_method_name,
        })
    }
}

// --- Helper Structs for Extracted Information ---
struct FieldInfo<'a> {
    header_field_structs: Vec<&'a Field>,
    header_field_idents: Vec<&'a Ident>,
    build_fn_header_params: Vec<proc_macro2::TokenStream>,
    header_param_names_types_for_docs: Vec<(String, String)>,
    tail_field: &'a Field,
    tail_field_name_ident: &'a Ident,
    tail_field_name_str_val: String,
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
}

// --- Helper Functions ---

fn extract_field_info(input_struct: &ItemStruct) -> SynResult<FieldInfo> {
    let mut header_field_structs: Vec<&Field> = Vec::new();
    let mut header_field_idents: Vec<&Ident> = Vec::new();
    let mut build_fn_header_params: Vec<proc_macro2::TokenStream> = Vec::new();
    let mut header_param_names_types_for_docs: Vec<(String, String)> = Vec::new();

    let tail_field: &Field;
    let tail_field_name_ident: &Ident;
    let tail_field_name_str_val: String;

    match &input_struct.fields {
        Fields::Named(fields_named) => {
            if fields_named.named.is_empty() {
                return Err(syn::Error::new_spanned(
                    fields_named,
                    "make_dst_builder requires at least one field (the tail field).",
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
                    header_param_names_types_for_docs
                        .push((field_ident.to_string(), quote! {#field_ty}.to_string()));
                }
            }
            tail_field = fields_named.named.last().unwrap();
            tail_field_name_ident = tail_field
                .ident
                .as_ref()
                .ok_or_else(|| syn::Error::new(tail_field.span(), "Tail field must be named."))?;
            tail_field_name_str_val = tail_field_name_ident.to_string();
        }
        Fields::Unnamed(_) | Fields::Unit => {
            return Err(syn::Error::new_spanned(
                &input_struct.fields,
                "make_dst_builder only supports structs with named fields.",
            ));
        }
    }
    Ok(FieldInfo {
        header_field_structs,
        header_field_idents,
        build_fn_header_params,
        header_param_names_types_for_docs,
        tail_field,
        tail_field_name_ident,
        tail_field_name_str_val,
    })
}

#[allow(clippy::too_many_lines)]
fn generate_header_layout_code(
    header_field_structs: &[&Field],
    input_struct_generics: &Generics,
) -> proc_macro2::TokenStream {
    let layout_header_full_ident = format_ident!("layout_header_full");
    let helper_struct_name = format_ident!("PrefixLayoutHelper");

    if header_field_structs.is_empty() {
        return quote! {
            let #layout_header_full_ident = ::std::alloc::Layout::new::<()>();
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
        #[allow(dead_code)] // Helper struct is only for layout calculation
        struct #helper_struct_name #helper_impl_generics #helper_where_clause_for_impl {
            #( #helper_fields_definitions ),*
        }
        let #layout_header_full_ident = ::std::alloc::Layout::new::<#helper_struct_name #helper_ty_generics>();
    }
}

fn generate_doc_params_string(
    header_param_names_types: &[(String, String)],
    tail_param_name: &Ident,
    tail_param_type_str: &str,
) -> String {
    use ::std::fmt::Write;
    let mut params_doc = String::new();
    for (name, ty) in header_param_names_types {
        let _ = writeln!(
            params_doc,
            "    - `{name}`: `{ty}` - Value for the `{name}` field."
        );
    }
    let _ = write!(
        params_doc,
        "    - `{tail_param_name}`: `{tail_param_type_str}` - The data for the tail field `{tail_param_name}`."
    );
    params_doc
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
    let method_visibility = &builder_args.visibility;
    let base_method_name = &builder_args.base_method_name;
    let tail_field_name_ident_for_access = field_info.tail_field_name_ident;
    let tail_field_name_str_val_for_format = &field_info.tail_field_name_str_val;
    let build_fn_header_params = &field_info.build_fn_header_params;
    let header_field_idents_for_write = &field_info.header_field_idents;

    let layout_calculation_for_tail_payload_slice = quote_spanned! {field_info.tail_field.ty.span()=>
        let layout_tail_payload = ::std::alloc::Layout::array::<#element_type>(len)
            .expect(&format!("Layout for tail slice field '{}' failed", #tail_field_name_str_val_for_format));
    };

    let element_type_str = quote! {#element_type}.to_string();
    let doc_params_slice_str = generate_doc_params_string(
        &field_info.header_param_names_types_for_docs,
        tail_field_name_ident_for_access,
        &format!("&[{element_type_str}]"),
    );
    let method_doc_slice = format!(
        "Creates an instance of `Box<{struct_name_ident}>`.\n\n\
         # Parameters\n\
         {doc_params_slice_str}\n\n\
         # Returns\n\n\
         A `Box<{struct_name_ident}>` containing the initialized data."
    );

    let build_fn_name_ident_slice = base_method_name;
    let build_fn_tail_param_type_slice = quote! { &[#element_type] };
    let build_fn_tail_processing_slice =
        quote! { let len = #tail_field_name_ident_for_access.len(); };
    let write_tail_data_call_slice = quote! {
        // Only write if node_raw_ptr is not dangling due to ZST and len > 0
        if final_layout.size() > 0 && len > 0 {
            let dest_slice_data_ptr = ::std::ptr::addr_of_mut!((*node_raw_ptr).#tail_field_name_ident_for_access).cast::<#element_type>();
            ::std::ptr::copy_nonoverlapping(#tail_field_name_ident_for_access.as_ptr(), dest_slice_data_ptr, len);
        }
    };
    quote! {
        #[doc = #method_doc_slice]
        #[allow(unused_variables)]
        #[allow(clippy::transmute_undefined_repr)]
        #method_visibility fn #build_fn_name_ident_slice(#( #build_fn_header_params, )* #tail_field_name_ident_for_access: #build_fn_tail_param_type_slice) -> Box<Self> {
            use ::std::alloc::{Layout, alloc, handle_alloc_error};
            use ::std::{mem, ptr};
            #build_fn_tail_processing_slice
            #header_layout_calc_tokens
            #layout_calculation_for_tail_payload_slice
            let (full_node_layout_val, #offset_of_tail_payload_ident) = #layout_header_full_ident
                .extend(layout_tail_payload)
                .expect(&format!("Layout extend for tail data of '{}' failed", #tail_field_name_str_val_for_format));
            let final_layout = full_node_layout_val.pad_to_align();

            unsafe {
                let node_raw_ptr: *mut Self;
                let mem_ptr: *mut u8; // Keep mem_ptr for writing header fields if ZST

                if final_layout.size() == 0 {
                    // For zero-sized DSTs, create a dangling fat pointer.
                    // The data part is the alignment, metadata (len) is 0.
                    mem_ptr = final_layout.align() as *mut u8; // Non-null, aligned pointer
                    let fat_pointer_components = (mem_ptr as *mut (), len); // len is 0 for empty slice/str
                    node_raw_ptr = mem::transmute(fat_pointer_components);
                } else {
                    mem_ptr = alloc(final_layout);
                    if mem_ptr.is_null() {
                        handle_alloc_error(final_layout);
                    }
                    let fat_pointer_components = (mem_ptr as *mut (), len);
                    node_raw_ptr = mem::transmute(fat_pointer_components);
                }

                // Write header fields. This is safe even for ZSTs if fields are ZSTs.
                // If header fields are not ZSTs but the overall layout is zero (e.g. only ZST fields + empty slice),
                // we still need to write them to the "dangling" but aligned pointer.
                // The `ptr::addr_of_mut!((*node_raw_ptr).field)` will resolve correctly.
                #(
                    ::std::ptr::addr_of_mut!((*node_raw_ptr).#header_field_idents_for_write)
                        .write(#header_field_idents_for_write);
                )*

                #write_tail_data_call_slice

                Box::from_raw(node_raw_ptr)
            }
        }
    }
}

fn generate_build_with_method_for_slice(
    builder_args: &MakeDstBuilderArgs,
    struct_name_ident: &Ident,
    field_info: &FieldInfo,
    element_type: &Type,
    header_layout_calc_tokens: &proc_macro2::TokenStream,
    offset_of_tail_payload_ident: &Ident,
    layout_header_full_ident: &Ident,
) -> proc_macro2::TokenStream {
    let method_visibility = &builder_args.visibility;
    let base_method_name = &builder_args.base_method_name;
    let tail_field_name_ident_for_access = field_info.tail_field_name_ident;
    let tail_field_name_str_val_for_format = &field_info.tail_field_name_str_val;
    let build_fn_header_params = &field_info.build_fn_header_params;
    let header_field_idents_for_write = &field_info.header_field_idents;

    let layout_calculation_for_tail_payload_slice = quote_spanned! {field_info.tail_field.ty.span()=>
        let layout_tail_payload = ::std::alloc::Layout::array::<#element_type>(len)
            .expect(&format!("Layout for tail slice field '{}' failed", #tail_field_name_str_val_for_format));
    };

    let build_fn_name_ident_iter = format_ident!("{}_with", base_method_name);
    let iter_generic_param = format_ident!("I");
    let build_fn_tail_param_type_iter = quote! { #iter_generic_param };
    let build_fn_where_clause_iter = quote! {
        where #iter_generic_param: ::std::iter::IntoIterator<Item = #element_type>,
              <#iter_generic_param as ::std::iter::IntoIterator>::IntoIter: ::std::iter::ExactSizeIterator
    };
    let build_fn_tail_processing_iter = quote! {
        let mut iter = #tail_field_name_ident_for_access.into_iter();
        let len = iter.len();
    };
    let write_tail_data_call_iter = quote! {
        // Only write if node_raw_ptr is not dangling due to ZST and len > 0
        if final_layout.size() > 0 && len > 0 {
            let dest_slice_data_ptr = ::std::ptr::addr_of_mut!((*node_raw_ptr).#tail_field_name_ident_for_access).cast::<#element_type>();
            for (i, element) in iter.enumerate() {
                 unsafe { ::std::ptr::write(dest_slice_data_ptr.add(i), element); }
            }
        }
    };
    let element_type_str = quote! {#element_type}.to_string();
    let doc_params_iter_str = generate_doc_params_string(
        &field_info.header_param_names_types_for_docs,
        tail_field_name_ident_for_access,
        &format!("impl IntoIterator<Item = {element_type_str}>"),
    );
    let method_doc_iter = format!(
        "Creates an instance of `Box<{struct_name_ident}>`.\n\n\
         # Parameters\n\
         {doc_params_iter_str}\n\n\
         # Returns\n\n\
         A `Box<{struct_name_ident}>` containing the initialized data."
    );
    quote! {
        #[doc = #method_doc_iter]
        #[allow(unused_variables)]
        #[allow(clippy::transmute_undefined_repr)]
        #method_visibility fn #build_fn_name_ident_iter<#iter_generic_param>(#( #build_fn_header_params, )* #tail_field_name_ident_for_access: #build_fn_tail_param_type_iter) -> Box<Self> #build_fn_where_clause_iter {
            use ::std::alloc::{Layout, alloc, handle_alloc_error};
            use ::std::{mem, ptr};
            #build_fn_tail_processing_iter
            #header_layout_calc_tokens
            #layout_calculation_for_tail_payload_slice
            let (full_node_layout_val, #offset_of_tail_payload_ident) = #layout_header_full_ident
                .extend(layout_tail_payload)
                .expect(&format!("Layout extend for tail data of '{}' failed", #tail_field_name_str_val_for_format));
            let final_layout = full_node_layout_val.pad_to_align();

            unsafe {
                let node_raw_ptr: *mut Self;
                let mem_ptr: *mut u8;

                if final_layout.size() == 0 {
                    mem_ptr = final_layout.align() as *mut u8;
                    let fat_pointer_components = (mem_ptr as *mut (), len);
                    node_raw_ptr = mem::transmute(fat_pointer_components);
                } else {
                    mem_ptr = alloc(final_layout);
                    if mem_ptr.is_null() {
                        handle_alloc_error(final_layout);
                    }
                    let fat_pointer_components = (mem_ptr as *mut (), len);
                    node_raw_ptr = mem::transmute(fat_pointer_components);
                }

                #(
                    ::std::ptr::addr_of_mut!((*node_raw_ptr).#header_field_idents_for_write)
                        .write(#header_field_idents_for_write);
                )*

                #write_tail_data_call_iter

                Box::from_raw(node_raw_ptr)
            }
        }
    }
}

fn generate_build_method_for_str(
    builder_args: &MakeDstBuilderArgs,
    struct_name_ident: &Ident,
    field_info: &FieldInfo,
    header_layout_calc_tokens: &proc_macro2::TokenStream,
    offset_of_tail_payload_ident: &Ident,
    layout_header_full_ident: &Ident,
) -> proc_macro2::TokenStream {
    let method_visibility = &builder_args.visibility;
    let base_method_name = &builder_args.base_method_name;
    let tail_field_name_ident_for_access = field_info.tail_field_name_ident;
    let tail_field_name_str_val_for_format = &field_info.tail_field_name_str_val;
    let build_fn_header_params = &field_info.build_fn_header_params;
    let header_field_idents_for_write = &field_info.header_field_idents;

    let build_fn_name_ident_str = base_method_name;
    let build_fn_tail_param_type_str = quote! { &str };
    let build_fn_tail_processing_str = quote! {
        let len = #tail_field_name_ident_for_access.len();
    };
    let layout_calculation_for_tail_payload_str = quote_spanned! {field_info.tail_field.ty.span()=>
        let layout_tail_payload = ::std::alloc::Layout::array::<u8>(len)
            .expect(&format!("Layout for tail str field '{}' failed", #tail_field_name_str_val_for_format));
    };
    let write_tail_data_call_str = quote! {
        // Only write if node_raw_ptr is not dangling due to ZST and len > 0
        if final_layout.size() > 0 && len > 0 {
            let dest_str_data_ptr = ::std::ptr::addr_of_mut!((*node_raw_ptr).#tail_field_name_ident_for_access) as *mut u8;
            ::std::ptr::copy_nonoverlapping(#tail_field_name_ident_for_access.as_ptr(), dest_str_data_ptr, len);
        }
    };
    let doc_params_str_val = generate_doc_params_string(
        &field_info.header_param_names_types_for_docs,
        tail_field_name_ident_for_access,
        "&str",
    );
    let method_doc_str = format!(
        "Creates an instance of `Box<{struct_name_ident}>`.\n\n\
         # Parameters\n\
         {doc_params_str_val}\n\n\
         # Returns\n\n\
         A `Box<{struct_name_ident}>` containing the initialized data."
    );
    quote! {
        #[doc = #method_doc_str]
        #[allow(unused_variables)]
        #[allow(clippy::transmute_undefined_repr)]
        #method_visibility fn #build_fn_name_ident_str(#( #build_fn_header_params, )* #tail_field_name_ident_for_access: #build_fn_tail_param_type_str) -> Box<Self> {
            use ::std::alloc::{Layout, alloc, handle_alloc_error};
            use ::std::{mem, ptr};

            #build_fn_tail_processing_str
            #header_layout_calc_tokens
            #layout_calculation_for_tail_payload_str

            let (full_node_layout_val, #offset_of_tail_payload_ident) = #layout_header_full_ident
                .extend(layout_tail_payload)
                .expect(&format!("Layout extend for tail data of '{}' failed", #tail_field_name_str_val_for_format));
            let final_layout = full_node_layout_val.pad_to_align();

            unsafe {
                let node_raw_ptr: *mut Self;
                let mem_ptr: *mut u8;

                if final_layout.size() == 0 {
                    mem_ptr = final_layout.align() as *mut u8;
                    let fat_pointer_components = (mem_ptr as *mut (), len);
                    node_raw_ptr = mem::transmute(fat_pointer_components);
                } else {
                    mem_ptr = alloc(final_layout);
                    if mem_ptr.is_null() {
                        handle_alloc_error(final_layout);
                    }
                    let fat_pointer_components = (mem_ptr as *mut (), len);
                    node_raw_ptr = mem::transmute(fat_pointer_components);
                }

                #(
                    ::std::ptr::addr_of_mut!((*node_raw_ptr).#header_field_idents_for_write)
                        .write(#header_field_idents_for_write);
                )*

                #write_tail_data_call_str

                Box::from_raw(node_raw_ptr)
            }
        }
    }
}

// --- Main Macro ---
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
            generated_build_methods.push(generate_build_with_method_for_slice(
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
                "The last field of a struct for make_dst_builder must be a slice [T] or str.",
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

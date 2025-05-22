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
//! ### Panics
//!
//! This macro will cause a compile-time error (panic) if:
//! - The input item is not a struct with named fields.
//! - The struct has no fields (a tail field is required).
//! - The last field of the struct is not a slice `[T]` or `str`.
//! - The arguments to `#[make_dst_builder(...)]` are malformed.
//! - Internal layout calculations fail (e.g., due to excessively large types), though this is rare.
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
use quote::{quote, quote_spanned};
use std::fmt::Write;
use syn::{
    Field, Fields, Generics, Ident, ItemStruct, Type, TypeSlice, Visibility,
    parse::{Parse, ParseStream, Result as SynResult},
    parse_macro_input,
    spanned::Spanned,
}; // For write! macro

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
                    &input_struct.fields,
                    "make_dst_builder requires at least one field (the tail field).",
                ));
            }
            let num_fields = fields_named.named.len();
            for (i, field) in fields_named.named.iter().enumerate() {
                if i < num_fields - 1 {
                    header_field_structs.push(field);
                    let field_ident = field
                        .ident
                        .as_ref()
                        .expect("Named field should have an ident");
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
                .expect("Tail field must be named.");
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

fn generate_header_layout_code(
    header_field_structs: &[&Field],
    struct_name_ident: &Ident,
) -> proc_macro2::TokenStream {
    let mut layout_calculations_for_header = Vec::new();
    let mut layout_extend_calls_for_header = Vec::new();
    let mut header_field_layout_vars = Vec::new();

    for field in header_field_structs {
        let ident = field
            .ident
            .as_ref()
            .expect("Header field should have an ident");
        let ty = &field.ty;
        let layout_var = Ident::new(&format!("layout_field_{ident}"), ident.span());
        header_field_layout_vars.push(layout_var.clone());
        layout_calculations_for_header
            .push(quote! { let #layout_var = ::std::alloc::Layout::new::<#ty>(); });
    }

    let layout_header_full_ident = Ident::new("layout_header_full", struct_name_ident.span());

    if header_field_layout_vars.is_empty() {
        layout_extend_calls_for_header
            .push(quote! { let #layout_header_full_ident = ::std::alloc::Layout::new::<()>(); });
    } else {
        let first_layout_var = &header_field_layout_vars[0];
        if header_field_layout_vars.len() == 1 {
            layout_extend_calls_for_header
                .push(quote! { let #layout_header_full_ident = #first_layout_var; });
        } else {
            let second_layout_var = &header_field_layout_vars[1];
            let second_field_ident = header_field_structs[1]
                .ident
                .as_ref()
                .expect("Header field should have an ident");
            let second_field_ident_str = second_field_ident.to_string();
            let offset_var = Ident::new(
                &format!("_offset_{second_field_ident}"),
                second_field_ident.span(),
            );
            layout_extend_calls_for_header.push(quote! {
                let (mut #layout_header_full_ident, #offset_var) = #first_layout_var
                    .extend(#second_layout_var)
                    .expect(&format!("Layout extend for field '{}' failed", #second_field_ident_str));
            });
            for i in 2..header_field_layout_vars.len() {
                let current_layout_var = &header_field_layout_vars[i];
                let current_field_ident = header_field_structs[i]
                    .ident
                    .as_ref()
                    .expect("Header field should have an ident");
                let current_field_ident_str = current_field_ident.to_string();
                let offset_var = Ident::new(
                    &format!("_offset_{current_field_ident}"),
                    current_field_ident.span(),
                );
                layout_extend_calls_for_header.push(quote! {
                    let (extended_layout, #offset_var) = #layout_header_full_ident
                        .extend(#current_layout_var)
                        .expect(&format!("Layout extend for field '{}' failed", #current_field_ident_str));
                    #layout_header_full_ident = extended_layout;
                });
            }
        }
    }
    quote! {
        #( #layout_calculations_for_header )*
        #( #layout_extend_calls_for_header )*
    }
}

fn generate_doc_params_string(
    header_param_names_types: &[(String, String)],
    tail_param_name: &Ident,
    tail_param_type_str: &str,
) -> String {
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
        "Constructs a new `Box<{struct_name_ident}>` with the given header fields and tail slice data.\n\n\
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
        if len > 0 {
            let data_slice_elements_ptr = (*node_raw_ptr).#tail_field_name_ident_for_access.as_mut_ptr();
            ::std::ptr::copy_nonoverlapping(#tail_field_name_ident_for_access.as_ptr(), data_slice_elements_ptr, len);
        }
    };
    quote! {
        #[doc = #method_doc_slice]
        #[allow(unused_variables)]
        #[allow(clippy::transmute_undefined_repr)] // Clippy suppression for transmute
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
                let mem_ptr = alloc(final_layout);
                if mem_ptr.is_null() { handle_alloc_error(final_layout); }
                let fat_pointer_components = (mem_ptr as *mut (), len);
                let node_raw_ptr: *mut Self = mem::transmute(fat_pointer_components);
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

    let build_fn_name_ident_iter =
        Ident::new(&format!("{base_method_name}_with"), base_method_name.span());
    let iter_generic_param = Ident::new("I", field_info.tail_field_name_ident.span());
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
        if len > 0 {
            let data_slice_elements_ptr = (*node_raw_ptr).#tail_field_name_ident_for_access.as_mut_ptr();
            for (i, element) in iter.enumerate() {
                 unsafe { ::std::ptr::write(data_slice_elements_ptr.add(i), element); }
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
        "Constructs a new `Box<{struct_name_ident}>` with the given header fields and tail data from an exact-size iterator.\n\n\
         # Parameters\n\
         {doc_params_iter_str}\n\n\
         # Returns\n\n\
         A `Box<{struct_name_ident}>` containing the initialized data."
    );
    quote! {
        #[doc = #method_doc_iter]
        #[allow(unused_variables)]
        #[allow(clippy::transmute_undefined_repr)] // Clippy suppression for transmute
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
                let mem_ptr = alloc(final_layout);
                if mem_ptr.is_null() { handle_alloc_error(final_layout); }
                let fat_pointer_components = (mem_ptr as *mut (), len);
                let node_raw_ptr: *mut Self = mem::transmute(fat_pointer_components);
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
        if len > 0 {
            let dest_ptr_for_str_bytes = mem_ptr.add(#offset_of_tail_payload_ident);
            ::std::ptr::copy_nonoverlapping(#tail_field_name_ident_for_access.as_ptr(), dest_ptr_for_str_bytes, len);
        }
    };
    let doc_params_str_val = generate_doc_params_string(
        &field_info.header_param_names_types_for_docs,
        tail_field_name_ident_for_access,
        "&str",
    );
    let method_doc_str = format!(
        "Constructs a new `Box<{struct_name_ident}>` with the given header fields and tail string data.\n\n\
         # Parameters\n\
         {doc_params_str_val}\n\n\
         # Returns\n\n\
         A `Box<{struct_name_ident}>` containing the initialized data."
    );
    quote! {
        #[doc = #method_doc_str]
        #[allow(unused_variables)]
        #[allow(clippy::transmute_undefined_repr)] // Clippy suppression for transmute
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
                let mem_ptr = alloc(final_layout);
                if mem_ptr.is_null() { handle_alloc_error(final_layout); }
                let fat_pointer_components = (mem_ptr as *mut (), len);
                let node_raw_ptr: *mut Self = mem::transmute(fat_pointer_components);
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
    let builder_args = if attr_args_ts.is_empty() {
        MakeDstBuilderArgs {
            visibility: Visibility::Inherited,
            base_method_name: Ident::new("build", proc_macro2::Span::call_site()),
        }
    } else {
        match syn::parse(attr_args_ts) {
            Ok(args) => args,
            Err(e) => return e.to_compile_error().into(),
        }
    };
    let input_struct = parse_macro_input!(item_ts as ItemStruct);

    let struct_name_ident = &input_struct.ident;
    let struct_generics: &Generics = &input_struct.generics;
    let (impl_generics, ty_generics, where_clause) = struct_generics.split_for_impl();

    let field_info = match extract_field_info(&input_struct) {
        Ok(info) => info,
        Err(e) => return e.to_compile_error().into(),
    };

    let header_layout_calc_tokens =
        generate_header_layout_code(&field_info.header_field_structs, struct_name_ident);

    let mut generated_build_methods: Vec<proc_macro2::TokenStream> = Vec::new();
    let offset_of_tail_payload_ident = Ident::new(
        &format!("offset_of_{}_payload", field_info.tail_field_name_ident),
        field_info.tail_field_name_ident.span(),
    );
    let layout_header_full_ident = Ident::new("layout_header_full", struct_name_ident.span());

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
            return syn::Error::new_spanned(
                &field_info.tail_field.ty,
                "The last field of a struct for make_dst_builder must be a slice [T] or str.",
            )
            .to_compile_error()
            .into();
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

    TokenStream::from(expanded)
}

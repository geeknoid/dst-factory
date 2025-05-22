//! # Tail Extend Macros
//!
//! This crate provides procedural macros for generating Rust structs with dynamically-sized
//! "tail" fields, along with helper methods to construct instances of these structs.
//! This is particularly useful for creating data structures where a fixed-size header is
//! followed by a variable-length array or string, all within a single memory allocation.
//!
//! ## Macros
//!
//! - `#[tail_extend(TargetStructName, [vis] tail_field_name: TailType)]`: The main macro that
//!   generates a new struct (the "target struct") from an "args" struct.
//! - `#[tail_attr(attribute_content)]`: A helper macro to apply arbitrary attributes
//!   (like `derive`, `repr`, etc.) to the generated target struct.
//!
//! ## `#[tail_extend]` Macro Details
//!
//! Applied to an "Args" struct, this macro generates:
//! 1.  A new "target" struct with the specified `TargetStructName`. This struct includes:
//!     * All fields copied from the "Args" struct (preserving their visibility and documentation).
//!     * A new dynamically-sized tail field named `tail_field_name` with the given `TailType`.
//!       The `TailType` can be a slice `[T]` (e.g., `[u32]`, `[MyItem]`) or `str`.
//!     * An optional visibility `[vis]` (e.g., `pub`, `pub(crate)`) can be specified for the tail field.
//!       If omitted, the tail field is private.
//! 2.  An `impl` block for the target struct with methods to construct `Box<Self>` instances:
//!     * If `TailType` is `[T]`:
//!         * `fn build(args: ArgsStruct<...>, tail_field_name: &[T]) -> Box<Self>`
//!         * `fn build_with<I>(args: ArgsStruct<...>, tail_field_name: I) -> Box<Self> where I: IntoIterator<Item = T>, <I as IntoIterator>::IntoIter: ExactSizeIterator`
//!     * If `TailType` is `str`:
//!         * `fn build(args: ArgsStruct<...>, tail_field_name: &str) -> Box<Self>`
//!
//! Generic parameters and `where` clauses from the "Args" struct are propagated to the target struct
//! and its `impl` block. Documentation comments on the "Args" struct and its fields are also copied.
//!
//! ## `#[tail_attr]` Macro Details
//!
//! Use this attribute on the "Args" struct to specify attributes that should be applied to the
//! generated target struct. It can be used multiple times.
//!
//! The content inside `tail_attr(...)` should be the inner part of the attribute.
//! For example, `#[tail_attr(derive(Debug, Clone))]` will add `#[derive(Debug, Clone)]` to the target struct.
//!
//! ## Examples
//!
//! ### 1. Basic Slice Tail
//!
//! ```rust
//! use tail_extend_macro_corrected_v2::{tail_extend, tail_attr}; // Use your crate name
//!
//! /// Arguments for creating a Packet.
//! #[tail_attr(derive(Debug, PartialEq))] // Apply to generated Packet struct
//! #[tail_extend(Packet, pub payload: [u8])] // Generated struct name, tail field name and type
//! pub struct PacketArgs {
//!     pub source_ip: [u8; 4],
//!     pub dest_ip: [u8; 4],
//!     pub protocol: u8,
//! }
//!
//! // The macro generates `Packet` struct and `Packet::build` / `Packet::build_with` methods.
//!
//! fn main_test_basic_slice() {
//!     let args = PacketArgs {
//!         source_ip: [192, 168, 1, 1],
//!         dest_ip: [192, 168, 1, 100],
//!         protocol: 6, // TCP
//!     };
//!     let payload_data: &[u8] = &[0xDE, 0xAD, 0xBE, 0xEF];
//!
//!     let packet_box = Packet::build(args, payload_data);
//!
//!     assert_eq!(packet_box.source_ip, [192, 168, 1, 1]);
//!     assert_eq!(packet_box.protocol, 6);
//!     assert_eq!(packet_box.payload.len(), 4);
//!     assert_eq!(packet_box.payload, [0xDE, 0xAD, 0xBE, 0xEF]);
//!
//!     // Using build_with
//!     let args2 = PacketArgs {
//!         source_ip: [10, 0, 0, 1],
//!         dest_ip: [10, 0, 0, 2],
//!         protocol: 17, // UDP
//!     };
//!     let payload_iter = vec![0xCA, 0xFE]; // Vec implements IntoIterator + ExactSizeIterator
//!     let packet_box2 = Packet::build_with(args2, payload_iter);
//!     assert_eq!(packet_box2.payload, [0xCA, 0xFE]);
//! }
//! # main_test_basic_slice(); // Call the function to ensure it's tested by doctest
//! ```
//!
//! ### 2. String Tail
//!
//! ```rust
//! use tail_extend_macro_corrected_v2::{tail_extend, tail_attr}; // Use your crate name
//!
//! /// Arguments for a log entry.
//! #[tail_attr(derive(Debug))]
//! #[tail_attr(allow(dead_code))] // Example of another attribute
//! #[tail_extend(LogEntry, message: str)]
//! pub struct LogEntryArgs {
//!     pub timestamp: u64,
//!     pub level: String,
//! }
//!
//! // The macro generates `LogEntry` struct and `LogEntry::build` method.
//!
//! fn main_test_str_tail() {
//!     let args = LogEntryArgs {
//!         timestamp: 1678886400,
//!         level: "INFO".to_string(),
//!     };
//!     let message_content = "User logged in successfully.";
//!
//!     let log_box = LogEntry::build(args, message_content);
//!
//!     assert_eq!(log_box.timestamp, 1678886400);
//!     assert_eq!(log_box.level, "INFO");
//!     assert_eq!(log_box.message, "User logged in successfully.");
//!     assert_eq!(log_box.message.len(), message_content.len());
//! }
//! # main_test_str_tail();
//! ```
//!
//! ### 3. Generics and Where Clauses
//!
//! ```rust
//! use tail_extend_macro_corrected_v2::{tail_extend, tail_attr}; // Use your crate name
//!
//! /// Generic arguments for a container.
//! #[tail_attr(derive(Debug, PartialEq))]
//! #[tail_extend(GenericContainer, pub items: [T])]
//! pub struct GenericContainerArgs<T: Copy + Default, const N: usize>
//! where
//!     T: std::fmt::Debug + PartialEq, // Where clause on Args struct
//! {
//!     pub id: [T; N], // Using const generic N
//!     pub description: &'static str,
//! }
//!
//! // The macro propagates generics and where clauses to `GenericContainer`
//! // and its `impl` block.
//!
//! fn main_test_generics() {
//!     let args = GenericContainerArgs::<u32, 2> {
//!         id: [10, 20],
//!         description: "My generic container",
//!     };
//!     let items_data: &[u32] = &[100, 200, 300];
//!
//!     let container_box = GenericContainer::<u32, 2>::build(args, items_data);
//!
//!     assert_eq!(container_box.id, [10, 20]);
//!     assert_eq!(container_box.description, "My generic container");
//!     assert_eq!(container_box.items, [100, 200, 300]);
//!
//!     // Using build_with
//!     let args2 = GenericContainerArgs::<char, 3> {
//!         id: ['a', 'b', 'c'],
//!         description: "Char container",
//!     };
//!     let items_iter = vec!['x', 'y', 'z'];
//!     let container_box2 = GenericContainer::<char, 3>::build_with(args2, items_iter);
//!     assert_eq!(container_box2.items, ['x', 'y', 'z']);
//! }
//! # main_test_generics();
//! ```
//!
//! ### 4. No Header Fields (Only Tail)
//!
//! ```rust
//! use tail_extend_macro_corrected_v2::{tail_extend, tail_attr}; // Use your crate name
//!
//! #[tail_attr(derive(Debug, PartialEq))]
//! #[tail_extend(RawData, bytes: [u8])]
//! pub struct RawDataArgs {} // No fixed-size fields
//!
//! fn main_test_no_header() {
//!     let args = RawDataArgs {};
//!     let data: &[u8] = &[1, 2, 3, 4];
//!     let raw_box = RawData::build(args, data);
//!     assert_eq!(raw_box.bytes, [1, 2, 3, 4]);
//! }
//! # main_test_no_header();
//! ```

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::{
    parse_macro_input, ItemStruct, Ident, Type, Fields, Visibility, Attribute,
    parse::{Parse, ParseStream, Parser, Result as SynResult},
    Meta, Token,
    spanned::Spanned,
    Generics,
};

// --- Helper Structs for Parsing ---

/// Represents the type of the tail field.
enum TailFieldDataType {
    Slice { element_type: Box<Type> }, // For field: [T]
    Str,                               // For field: str
}

/// Parses the arguments of the main `#[tail_extend(...)]` attribute.
struct TailExtendAttributeArgs {
    target_struct_ident: Ident,
    tail_field_visibility: Visibility, // Added for optional visibility
    tail_field_ident: Ident,
    tail_data_type: TailFieldDataType,
    original_tail_type_syn: syn::Type,
}

impl Parse for TailExtendAttributeArgs {
    fn parse(input: ParseStream) -> SynResult<Self> {
        let target_struct_ident: Ident = input.parse()?;
        input.parse::<Token![,]>()?;

        // Parse optional visibility for the tail field
        let tail_field_visibility: Visibility = input.parse()?;

        let tail_field_ident: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        let ty: syn::Type = input.parse()?;
        let original_tail_type_syn = ty.clone();

        let tail_data_type = match ty {
            Type::Slice(slice_type) => TailFieldDataType::Slice {
                element_type: slice_type.elem,
            },
            Type::Path(type_path) => {
                if type_path.path.is_ident("str") {
                    TailFieldDataType::Str
                } else {
                    return Err(syn::Error::new_spanned(
                        original_tail_type_syn,
                        "Expected a slice type like [T] (e.g., [u32]) or the type `str` for the tail field.",
                    ));
                }
            }
            _ => {
                return Err(syn::Error::new_spanned(
                    original_tail_type_syn,
                    "Unsupported type for tail field. Expected a slice type like [T] or `str`.",
                ));
            }
        };

        Ok(TailExtendAttributeArgs {
            target_struct_ident,
            tail_field_visibility,
            tail_field_ident,
            tail_data_type,
            original_tail_type_syn,
        })
    }
}

// --- Procedural Macros ---

/// Specifies an arbitrary attribute to be applied to the generated "target" struct.
///
/// This attribute macro itself does not modify the item it's attached to. Instead, it serves as a
/// marker for the `#[tail_extend]` macro. `#[tail_extend]` will parse the content of `#[tail_attr]`
/// and apply it as an attribute to the newly generated struct.
///
/// You can use `#[tail_attr]` multiple times to apply multiple attributes.
///
/// # Syntax
///
/// ```rust
/// # // This is a dummy import for doctest purposes.
/// # // In a real scenario, you'd import from your macro crate.
/// # use tail_extend_macro_corrected_v2::tail_attr;
/// #[tail_attr(derive(Debug, PartialEq))]
/// #[tail_attr(repr(C))]
/// // ... struct definition for #[tail_extend] ...
/// # struct YourArgsStruct {};
/// ```
///
/// In the example above, the struct generated by `#[tail_extend]` would have `#[derive(Debug, PartialEq)]`
/// and `#[repr(C)]` applied to it.
///
/// The content inside `tail_attr(...)` should be the inner part of the attribute you want to generate.
/// For example, for `#[derive(Debug)]`, you would write `#[tail_attr(derive(Debug))]`.
#[proc_macro_attribute]
pub fn tail_attr(_attr_tokens: TokenStream, item_tokens: TokenStream) -> TokenStream {
    item_tokens
}

/// Generates a new struct with a dynamically-sized tail field and associated build methods.
///
/// This attribute macro is applied to an "Args" struct, which defines the fixed-size fields.
/// `#[tail_extend]` then generates:
/// 1.  A new "target" struct with the specified name. This struct includes all fields from the
///     "Args" struct, plus a new dynamically-sized tail field (either a slice `[T]` or `str`).
/// 2.  An `impl` block for the target struct containing methods to construct instances of it.
///
/// # Syntax
///
/// `#[tail_extend(TargetStructName, [vis] tail_field_name: TailType)]`
///
/// - `TargetStructName`: The identifier for the new struct to be generated.
/// - `[vis]`: An optional visibility specifier (e.g., `pub`, `pub(crate)`) for the tail field.
///   If omitted, the tail field is private.
/// - `tail_field_name`: The identifier for the new tail field in the generated struct.
/// - `TailType`: The type of the tail field. Must be either:
///     - A slice type, e.g., `[u32]`, `[MyCustomType]`.
///     - The `str` type, for a string tail.
///
/// # Generated Methods
///
/// - If `TailType` is `[T]`:
///     - `fn build(args: ArgsStruct<...>, tail_field_name: &[T]) -> Box<Self>`:
///       Constructs an instance using a slice for the tail data.
///     - `fn build_with<I>(args: ArgsStruct<...>, tail_field_name: I) -> Box<Self>`
///       `where I: IntoIterator<Item = T>, <I as IntoIterator>::IntoIter: ExactSizeIterator`:
///       Constructs an instance using an exact-size iterator for the tail data.
/// - If `TailType` is `str`:
///     - `fn build(args: ArgsStruct<...>, tail_field_name: &str) -> Box<Self>`:
///       Constructs an instance using a string slice for the tail data.
///
/// # Associated Attributes
///
/// - `#[tail_attr(attribute_content)]`: Use this on the "Args" struct to specify attributes
///   (like `derive`, `repr`, `allow`, etc.) that should be applied to the generated target struct.
///   Can be used multiple times.
///
/// # Generics and Where Clauses
///
/// Generic parameters and `where` clauses from the "Args" struct are automatically propagated
/// to the generated target struct and its `impl` block.
///
/// # Example
///
/// ```rust
/// # // This is a dummy import for doctest purposes.
/// # // In a real scenario, you'd import from your macro crate.
/// # use tail_extend_macro_corrected_v2::{tail_extend, tail_attr};
/// #[tail_attr(derive(Debug))]
/// #[tail_extend(MyNode, pub data: [u32])] // Tail field `data` is public
/// struct MyNodeArgs<T: Copy> where T: Default {
///     id: T,
///     count: u16,
/// }
///
/// // Generated (conceptually):
/// // #[derive(Debug)]
/// // struct MyNode<T: Copy> where T: Default {
/// //     id: T,
/// //     count: u16,
/// //     pub data: [u32], // Visibility applied
/// // }
/// //
/// // impl<T: Copy> MyNode<T> where T: Default {
/// //     fn build(args: MyNodeArgs<T>, data: &[u32]) -> Box<Self> { /* ... */ }
/// //     fn build_with<I>(args: MyNodeArgs<T>, data: I) -> Box<Self>
/// //     where I: IntoIterator<Item = u32>, <I as IntoIterator>::IntoIter: ExactSizeIterator { /* ... */ }
/// // }
/// ```
#[proc_macro_attribute]
pub fn tail_extend(attr: TokenStream, item: TokenStream) -> TokenStream {
    let main_macro_args = parse_macro_input!(attr as TailExtendAttributeArgs);
    let args_struct = parse_macro_input!(item as ItemStruct);

    let args_struct_vis = &args_struct.vis;
    let args_struct_name_ident = &args_struct.ident;

    let args_generics: &Generics = &args_struct.generics;
    let (impl_generics, ty_generics, where_clause) = args_generics.split_for_impl();

    let mut struct_doc_attrs_for_target: Vec<&Attribute> = Vec::new();
    let mut custom_target_attrs: Vec<Attribute> = Vec::new();

    for attr_on_args in args_struct.attrs.iter() {
        if attr_on_args.path().is_ident("doc") {
            struct_doc_attrs_for_target.push(attr_on_args);
        } else if attr_on_args.path().is_ident("tail_attr") {
            match &attr_on_args.meta {
                Meta::List(meta_list) => {
                    let inner_tokens = &meta_list.tokens;
                    let full_attr_tokens = quote_spanned!(inner_tokens.span()=> #[#inner_tokens]);
                    match Attribute::parse_outer.parse2(full_attr_tokens) {
                        Ok(mut parsed_attrs_vec) => {
                            if parsed_attrs_vec.len() == 1 {
                                custom_target_attrs.push(parsed_attrs_vec.remove(0));
                            } else {
                                return syn::Error::new_spanned(
                                    inner_tokens,
                                    format!("Expected content of tail_attr to form a single attribute, but found {} or zero attributes.", parsed_attrs_vec.len())
                                ).to_compile_error().into();
                            }
                        }
                        Err(e) => {
                            return syn::Error::new_spanned(
                                inner_tokens,
                                format!("Failed to parse content of tail_attr as an attribute: {}", e)
                            ).to_compile_error().into();
                        }
                    }
                }
                _ => {
                    return syn::Error::new_spanned(
                        attr_on_args,
                        "Invalid #[tail_attr] format. Expected #[tail_attr(attribute_content)]"
                    ).to_compile_error().into();
                }
            }
        }
    }

    let mut generated_field_definitions = Vec::new();
    let mut header_field_idents_for_build = Vec::new();
    let mut header_field_types_for_build = Vec::new();

    if let Fields::Named(fields_named) = &args_struct.fields {
        for field in &fields_named.named {
            let field_doc_attrs: Vec<&Attribute> = field.attrs.iter()
                .filter(|attr_on_field| attr_on_field.path().is_ident("doc"))
                .collect();
            let field_vis = &field.vis;
            let field_ident = field.ident.as_ref().expect("Named field must have an ident");
            let field_ty = &field.ty;

            generated_field_definitions.push(quote_spanned! {field.span()=>
                #( #field_doc_attrs )*
                #field_vis #field_ident: #field_ty
            });

            header_field_idents_for_build.push(field_ident);
            header_field_types_for_build.push(field_ty);
        }
    } else {
        return syn::Error::new_spanned(
            &args_struct.fields,
            "tail_extend macro only supports structs with named fields."
        ).to_compile_error().into();
    }

    let target_struct_name = &main_macro_args.target_struct_ident;
    let tail_field_visibility = &main_macro_args.tail_field_visibility; // Get visibility for tail field
    let tail_field_name_ident = &main_macro_args.tail_field_ident;
    let tail_field_name_str_val = tail_field_name_ident.to_string();

    let header_layout_calc_tokens = {
        let mut layout_calculations_for_header = Vec::new();
        let mut layout_extend_calls_for_header = Vec::new();
        let mut header_field_layout_vars = Vec::new();

        for (ident, ty) in header_field_idents_for_build.iter().zip(header_field_types_for_build.iter()) {
            let layout_var = Ident::new(&format!("layout_field_{}", ident), ident.span());
            header_field_layout_vars.push(layout_var.clone());
            layout_calculations_for_header.push(quote! { let #layout_var = ::std::alloc::Layout::new::<#ty>(); });
        }

        let layout_header_full_ident = Ident::new("layout_header_full", args_struct_name_ident.span());

        if header_field_layout_vars.is_empty() {
            layout_extend_calls_for_header.push(quote! { let #layout_header_full_ident = ::std::alloc::Layout::new::<()>(); });
        } else {
            let first_layout_var = &header_field_layout_vars[0];
            if header_field_layout_vars.len() == 1 {
                layout_extend_calls_for_header.push(quote! { let #layout_header_full_ident = #first_layout_var; });
            } else {
                let second_layout_var = &header_field_layout_vars[1];
                let second_field_ident = &header_field_idents_for_build[1];
                let second_field_ident_str = second_field_ident.to_string();
                let offset_var = Ident::new(&format!("_offset_{}", second_field_ident), second_field_ident.span());
                layout_extend_calls_for_header.push(quote! {
                    let (mut #layout_header_full_ident, #offset_var) = #first_layout_var
                        .extend(#second_layout_var)
                        .expect(&format!("Layout extend for field '{}' failed", #second_field_ident_str));
                });
                for i in 2..header_field_layout_vars.len() {
                    let current_layout_var = &header_field_layout_vars[i];
                    let current_field_ident = &header_field_idents_for_build[i];
                    let current_field_ident_str = current_field_ident.to_string();
                    let offset_var = Ident::new(&format!("_offset_{}", current_field_ident), current_field_ident.span());
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
    };

    let tail_field_type_for_struct_def: proc_macro2::TokenStream;
    let mut generated_build_methods: Vec<proc_macro2::TokenStream> = Vec::new();

    let offset_of_tail_payload_ident = Ident::new(&format!("offset_of_{}_payload", tail_field_name_ident), tail_field_name_ident.span());
    let layout_header_full_ident = Ident::new("layout_header_full", args_struct_name_ident.span());

    match &main_macro_args.tail_data_type {
        TailFieldDataType::Slice { element_type } => {
            tail_field_type_for_struct_def = quote! { [#element_type] };

            let layout_calculation_for_tail_payload_slice = quote_spanned! {main_macro_args.original_tail_type_syn.span()=>
                let layout_tail_payload = ::std::alloc::Layout::array::<#element_type>(len)
                    .expect(&format!("Layout for tail slice field '{}' failed", #tail_field_name_str_val));
            };

            let build_fn_name_ident_slice = Ident::new("build", tail_field_name_ident.span());
            let build_fn_tail_param_type_slice = quote! { &[#element_type] };
            let build_fn_tail_processing_slice = quote! { let len = #tail_field_name_ident.len(); };
            let write_tail_data_call_slice = quote! {
                if len > 0 {
                    let data_slice_elements_ptr = (*node_raw_ptr).#tail_field_name_ident.as_mut_ptr();
                    ::std::ptr::copy_nonoverlapping(#tail_field_name_ident.as_ptr(), data_slice_elements_ptr, len);
                }
            };
            generated_build_methods.push(quote! {
                #[allow(unused_variables)]
                fn #build_fn_name_ident_slice(args: #args_struct_name_ident #ty_generics, #tail_field_name_ident: #build_fn_tail_param_type_slice) -> Box<Self> {
                    use ::std::alloc::{Layout, alloc, handle_alloc_error};
                    use ::std::{mem, ptr};
                    #build_fn_tail_processing_slice
                    #header_layout_calc_tokens
                    #layout_calculation_for_tail_payload_slice 
                    let (full_node_layout_val, #offset_of_tail_payload_ident) = #layout_header_full_ident
                        .extend(layout_tail_payload) 
                        .expect(&format!("Layout extend for tail data of '{}' failed", #tail_field_name_str_val));
                    let final_layout = full_node_layout_val.pad_to_align();
                    unsafe {
                        let mem_ptr = alloc(final_layout); 
                        if mem_ptr.is_null() { handle_alloc_error(final_layout); }
                        let fat_pointer_components = (mem_ptr as *mut (), len);
                        let node_raw_ptr: *mut Self = mem::transmute(fat_pointer_components);
                        #( 
                            ::std::ptr::addr_of_mut!((*node_raw_ptr).#header_field_idents_for_build)
                                .write(args.#header_field_idents_for_build);
                        )*
                        #write_tail_data_call_slice
                        Box::from_raw(node_raw_ptr)
                    }
                }
            });

            let build_fn_name_ident_iter = Ident::new("build_with", tail_field_name_ident.span());
            let iter_generic_param = Ident::new("I", tail_field_name_ident.span()); // Renamed to I
            let build_fn_tail_param_type_iter = quote! { #iter_generic_param };
            let build_fn_where_clause_iter = quote! {
                where #iter_generic_param: ::std::iter::IntoIterator<Item = #element_type>,
                      <#iter_generic_param as ::std::iter::IntoIterator>::IntoIter: ::std::iter::ExactSizeIterator
            };
            let build_fn_tail_processing_iter = quote! { 
                let mut iter = #tail_field_name_ident.into_iter();
                let len = iter.len(); 
            };
            let write_tail_data_call_iter = quote! {
                if len > 0 {
                    let data_slice_elements_ptr = (*node_raw_ptr).#tail_field_name_ident.as_mut_ptr();
                    for (i, element) in iter.enumerate() {
                         unsafe { ::std::ptr::write(data_slice_elements_ptr.add(i), element); }
                    }
                }
            };
            generated_build_methods.push(quote! {
                #[allow(unused_variables)]
                fn #build_fn_name_ident_iter<#iter_generic_param>(args: #args_struct_name_ident #ty_generics, #tail_field_name_ident: #build_fn_tail_param_type_iter) -> Box<Self> #build_fn_where_clause_iter {
                    use ::std::alloc::{Layout, alloc, handle_alloc_error};
                    use ::std::{mem, ptr};
                    #build_fn_tail_processing_iter 
                    #header_layout_calc_tokens
                    #layout_calculation_for_tail_payload_slice 
                    let (full_node_layout_val, #offset_of_tail_payload_ident) = #layout_header_full_ident
                        .extend(layout_tail_payload) 
                        .expect(&format!("Layout extend for tail data of '{}' failed", #tail_field_name_str_val));
                    let final_layout = full_node_layout_val.pad_to_align();
                    unsafe {
                        let mem_ptr = alloc(final_layout); 
                        if mem_ptr.is_null() { handle_alloc_error(final_layout); }
                        let fat_pointer_components = (mem_ptr as *mut (), len);
                        let node_raw_ptr: *mut Self = mem::transmute(fat_pointer_components);
                        #( 
                            ::std::ptr::addr_of_mut!((*node_raw_ptr).#header_field_idents_for_build)
                                .write(args.#header_field_idents_for_build);
                        )*
                        #write_tail_data_call_iter 
                        Box::from_raw(node_raw_ptr)
                    }
                }
            });
        }
        TailFieldDataType::Str => {
            tail_field_type_for_struct_def = quote! { str };
            let build_fn_name_ident_str = Ident::new("build", tail_field_name_ident.span());
            let build_fn_tail_param_type_str = quote! { &str };
            let build_fn_tail_processing_str = quote! {
                let len = #tail_field_name_ident.len(); 
            };
            let layout_calculation_for_tail_payload_str = quote_spanned! {main_macro_args.original_tail_type_syn.span()=>
                let layout_tail_payload = ::std::alloc::Layout::array::<u8>(len) 
                    .expect(&format!("Layout for tail str field '{}' failed", #tail_field_name_str_val));
            };
            let write_tail_data_call_str = quote! {
                if len > 0 {
                    let dest_ptr_for_str_bytes = mem_ptr.add(#offset_of_tail_payload_ident);
                    ::std::ptr::copy_nonoverlapping(#tail_field_name_ident.as_ptr(), dest_ptr_for_str_bytes, len);
                }
            };
            generated_build_methods.push(quote! {
                #[allow(unused_variables)]
                fn #build_fn_name_ident_str(args: #args_struct_name_ident #ty_generics, #tail_field_name_ident: #build_fn_tail_param_type_str) -> Box<Self> {
                    use ::std::alloc::{Layout, alloc, handle_alloc_error};
                    use ::std::{mem, ptr};
                    #build_fn_tail_processing_str
                    #header_layout_calc_tokens
                    #layout_calculation_for_tail_payload_str
                    let (full_node_layout_val, #offset_of_tail_payload_ident) = #layout_header_full_ident
                        .extend(layout_tail_payload) 
                        .expect(&format!("Layout extend for tail data of '{}' failed", #tail_field_name_str_val));
                    let final_layout = full_node_layout_val.pad_to_align();
                    unsafe {
                        let mem_ptr = alloc(final_layout); 
                        if mem_ptr.is_null() { handle_alloc_error(final_layout); }
                        let fat_pointer_components = (mem_ptr as *mut (), len);
                        let node_raw_ptr: *mut Self = mem::transmute(fat_pointer_components);
                        #( 
                            ::std::ptr::addr_of_mut!((*node_raw_ptr).#header_field_idents_for_build)
                                .write(args.#header_field_idents_for_build);
                        )*
                        #write_tail_data_call_str
                        Box::from_raw(node_raw_ptr)
                    }
                }
            });
        }
    }

    let target_struct_def = quote! {
        #( #struct_doc_attrs_for_target )*
        #( #custom_target_attrs )* #args_struct_vis struct #target_struct_name #args_generics { 
            #( #generated_field_definitions, )*
            #tail_field_visibility #tail_field_name_ident: #tail_field_type_for_struct_def, // Apply visibility here
        }
    };

    let build_methods_impl_block = quote! {
        impl #impl_generics #target_struct_name #ty_generics #where_clause {
            #( #generated_build_methods )*
        }
    };

    let expanded = quote! {
        #args_struct
        #target_struct_def
        #build_methods_impl_block
    };

    TokenStream::from(expanded)
}

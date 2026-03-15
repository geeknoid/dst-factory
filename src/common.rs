use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote, quote_spanned};
use syn::token::Comma;
use syn::{
    Field, Fields, Generics, Ident, Index, ItemStruct, Type, TypePath, parse::Result as SynResult, punctuated::Punctuated, spanned::Spanned,
};

use crate::{r#box, macro_args::MacroArgs, serde};

/// Encapsulates everything that varies between Box/Arc/Rc pointer types.
pub struct PtrKind {
    /// The suffix appended to factory names (empty for Box, "_arc" for Arc, "_rc" for Rc)  
    pub name_suffix: &'static str,
    /// Human-readable name for doc comments ("Box", "Arc", "Rc")
    pub display_name: &'static str,
    /// Token stream for the smart pointer path (e.g., `::std::boxed::Box`)
    pub ptr_path: TokenStream,
    /// Token stream for allocation and initialization. For Box this produces `mem_ptr`;
    /// for Arc/Rc this produces `mem_ptr` and `data_ptr` and refcount init.
    pub alloc_tokens: TokenStream,
    /// The identifier for the data pointer in the generated code (`mem_ptr` for Box, `data_ptr` for Arc/Rc)
    pub data_ptr_ident: Ident,
    /// The layout variable name for the Guard (`layout` for Box, `arc_layout`/`rc_layout` for Arc/Rc)
    pub guard_layout: TokenStream,
    /// Token stream for converting `fat_ptr` to the final return value
    pub from_raw: TokenStream,
}

impl PtrKind {
    pub fn new_box(no_std: bool) -> Self {
        let ptr_path = box_path(no_std);
        let alloc_tokens = alloc(no_std);

        Self {
            name_suffix: "",
            display_name: "Box",
            ptr_path: ptr_path.clone(),
            alloc_tokens,
            data_ptr_ident: format_ident!("mem_ptr"),
            guard_layout: quote! { layout },
            from_raw: quote! { #ptr_path::from_raw(fat_ptr) },
        }
    }

    pub fn new_arc(no_std: bool) -> Self {
        let ptr_path = arc_path(no_std);
        let alloc_tokens = arc_alloc_and_init(no_std);

        Self {
            name_suffix: "_arc",
            display_name: "Arc",
            ptr_path: ptr_path.clone(),
            alloc_tokens,
            data_ptr_ident: format_ident!("data_ptr"),
            guard_layout: quote! { arc_layout },
            from_raw: quote! { #ptr_path::from_raw(fat_ptr as *const Self) },
        }
    }

    pub fn new_rc(no_std: bool) -> Self {
        let ptr_path = rc_path(no_std);
        let alloc_tokens = rc_alloc_and_init(no_std);

        Self {
            name_suffix: "_rc",
            display_name: "Rc",
            ptr_path: ptr_path.clone(),
            alloc_tokens,
            data_ptr_ident: format_ident!("data_ptr"),
            guard_layout: quote! { rc_layout },
            from_raw: quote! { #ptr_path::from_raw(fat_ptr as *const Self) },
        }
    }
}

pub enum TailKind {
    Slice(Box<Type>),
    Str,
    /// Stores the primary trait path for the trait object tail.
    TraitObject(syn::Path),
}

pub enum FieldIdent {
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

pub struct StructInfo<'a> {
    pub struct_name: &'a Ident,
    pub struct_generics: &'a Generics,
    pub header_fields: Box<[&'a Field]>,
    pub header_field_idents: Box<[FieldIdent]>,
    pub header_param_idents: Box<[Ident]>,
    pub header_types: Vec<Type>,
    pub tail_field: &'a Field,
    pub tail_field_ident: FieldIdent,
    pub tail_param_ident: Ident,
    pub tail_kind: TailKind,
}

impl<'a> StructInfo<'a> {
    pub fn new(input_struct: &'a ItemStruct) -> SynResult<Self> {
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

                let trait_path = type_trait_object
                    .bounds
                    .iter()
                    .find_map(|bound| {
                        if let syn::TypeParamBound::Trait(trait_bound) = bound {
                            Some(&trait_bound.path)
                        } else {
                            None
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

// ---------------------------------------------------------------------------
// Shared code-generation helpers used by box, arc, and rc modules
// ---------------------------------------------------------------------------

pub fn header_layout(macro_args: &MacroArgs, struct_info: &StructInfo, for_trait: bool) -> TokenStream {
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

    let make_head_ptr = quote! { let head_ptr: *const Self = ::core::mem::transmute((&raw const buffer, #fat_payload)); };

    if header_field_types.is_empty() {
        return quote! {
            let layout = unsafe {
                let buffer = ::core::mem::MaybeUninit::<(#tail_type,)>::uninit();
                #make_head_ptr
                let align = ::core::mem::align_of_val::<Self>(&*head_ptr);
                ::core::alloc::Layout::from_size_align_unchecked(0, align)
            };
        };
    }

    quote! {
        let buffer = ::core::mem::MaybeUninit::<(#( #header_field_types, )* #tail_type)>::uninit();
        let (offset, align) = unsafe {
            #make_head_ptr
            let tail_ptr = &raw const (*head_ptr).#tail_field_ident;
            (
                (tail_ptr as *const u8).offset_from_unsigned(head_ptr as *const u8),
                ::core::mem::align_of_val::<Self>(&*head_ptr)
            )
        };

        let layout = ::core::alloc::Layout::from_size_align(offset, align).unwrap();
    }
}

pub fn tail_layout<T: ToTokens>(tail_type: &T, span: Span) -> TokenStream {
    quote_spanned! { span => ::core::alloc::Layout::array::<#tail_type>(len).expect("Array exceeds maximum size allowed of isize::MAX") }
}

pub fn dealloc_path(no_std: bool) -> TokenStream {
    if no_std {
        quote! { ::alloc::alloc::dealloc }
    } else {
        quote! { ::std::alloc::dealloc }
    }
}

pub fn box_path(no_std: bool) -> TokenStream {
    if no_std {
        quote! { ::alloc::boxed::Box }
    } else {
        quote! { ::std::boxed::Box }
    }
}

pub fn arc_path(no_std: bool) -> TokenStream {
    if no_std {
        quote! { ::alloc::sync::Arc }
    } else {
        quote! { ::std::sync::Arc }
    }
}

pub fn rc_path(no_std: bool) -> TokenStream {
    if no_std {
        quote! { ::alloc::rc::Rc }
    } else {
        quote! { ::std::rc::Rc }
    }
}

pub fn guard_type(macro_args: &MacroArgs) -> TokenStream {
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

pub fn header_params(struct_info: &StructInfo) -> Vec<TokenStream> {
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

pub fn header_field_writes(struct_info: &StructInfo) -> Vec<TokenStream> {
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

pub fn args_tuple_assignment(struct_info: &StructInfo) -> TokenStream {
    let header_param_idents = &struct_info.header_param_idents;
    let tail_param_ident = &struct_info.tail_param_ident;

    quote! {
        let args = ( #( #header_param_idents, )* #tail_param_ident, );
    }
}

pub fn alloc(no_std: bool) -> TokenStream {
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

pub fn alloc_zst(box_path: &TokenStream, for_trait: bool) -> TokenStream {
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

/// Generates the allocation and reference-count initialization for a single-allocation `Arc<T>`.
///
/// Expects `layout` (the DST's own layout) to already be defined. Produces:
/// - `arc_layout` / `mem_ptr` / `data_ptr`
pub fn arc_alloc_and_init(no_std: bool) -> TokenStream {
    let (alloc_path, handle_alloc_error) = if no_std {
        (quote! { ::alloc::alloc::alloc }, quote! { panic!("out of memory") })
    } else {
        (
            quote! { ::std::alloc::alloc },
            quote! { ::std::alloc::handle_alloc_error(arc_layout) },
        )
    };

    quote! {
        // SAFETY: We replicate the layout of Arc's internal `#[repr(C)] ArcInner<T>`:
        //   { strong: AtomicUsize, weak: AtomicUsize, data: T }
        let arc_header_layout = ::core::alloc::Layout::from_size_align_unchecked(
            2 * ::core::mem::size_of::<usize>(),
            ::core::mem::size_of::<usize>(),
        );
        let (arc_layout, data_offset) = arc_header_layout.extend(layout)
            .expect("Arc layout exceeds maximum size allowed of isize::MAX");
        let arc_layout = arc_layout.pad_to_align();

        let mem_ptr = #alloc_path(arc_layout);
        if mem_ptr.is_null() {
            #handle_alloc_error
        }

        let strong_ptr = mem_ptr.cast::<::core::sync::atomic::AtomicUsize>();
        ::core::ptr::write(strong_ptr, ::core::sync::atomic::AtomicUsize::new(1));
        ::core::ptr::write(strong_ptr.add(1), ::core::sync::atomic::AtomicUsize::new(1));

        let data_ptr = mem_ptr.add(data_offset);
    }
}

/// Generates the allocation and reference-count initialization for a single-allocation `Rc<T>`.
///
/// Expects `layout` (the DST's own layout) to already be defined. Produces:
/// - `rc_layout` / `mem_ptr` / `data_ptr`
pub fn rc_alloc_and_init(no_std: bool) -> TokenStream {
    let (alloc_path, handle_alloc_error) = if no_std {
        (quote! { ::alloc::alloc::alloc }, quote! { panic!("out of memory") })
    } else {
        (
            quote! { ::std::alloc::alloc },
            quote! { ::std::alloc::handle_alloc_error(rc_layout) },
        )
    };

    quote! {
        // SAFETY: We replicate the layout of Rc's internal `#[repr(C)] RcInner<T>`:
        //   { strong: Cell<usize>, weak: Cell<usize>, value: T }
        let rc_header_layout = ::core::alloc::Layout::from_size_align_unchecked(
            2 * ::core::mem::size_of::<usize>(),
            ::core::mem::size_of::<usize>(),
        );
        let (rc_layout, data_offset) = rc_header_layout.extend(layout)
            .expect("Rc layout exceeds maximum size allowed of isize::MAX");
        let rc_layout = rc_layout.pad_to_align();

        let mem_ptr = #alloc_path(rc_layout);
        if mem_ptr.is_null() {
            #handle_alloc_error
        }

        let strong_ptr = mem_ptr.cast::<::core::cell::Cell<usize>>();
        ::core::ptr::write(strong_ptr, ::core::cell::Cell::new(1));
        ::core::ptr::write(strong_ptr.add(1), ::core::cell::Cell::new(1));

        let data_ptr = mem_ptr.add(data_offset);
    }
}

fn rebuilt_fat_ptr_from_existing_metadata(struct_info: &StructInfo, data_ptr_ident: &Ident) -> TokenStream {
    match &struct_info.tail_kind {
        TailKind::Slice(_) | TailKind::Str => quote! {
            let (_, metadata): (*mut u8, usize) = ::core::mem::transmute(src_ptr);
            let fat_ptr = ::core::mem::transmute::<(*mut u8, usize), *mut Self>((#data_ptr_ident, metadata));
        },
        TailKind::TraitObject(_) => quote! {
            let (_, metadata): (*mut u8, *const ()) = ::core::mem::transmute(src_ptr);
            let fat_ptr = ::core::mem::transmute::<(*mut u8, *const ()), *mut Self>((#data_ptr_ident, metadata));
        },
    }
}

fn gen_into_ptr(macro_args: &MacroArgs, struct_info: &StructInfo, ptr: &PtrKind) -> TokenStream {
    let visibility = &macro_args.visibility;
    let box_path = box_path(macro_args.no_std);
    let ptr_path = &ptr.ptr_path;
    let alloc_tokens = &ptr.alloc_tokens;
    let data_ptr_ident = &ptr.data_ptr_ident;
    let from_raw = &ptr.from_raw;
    let dealloc_path = dealloc_path(macro_args.no_std);
    let fat_ptr_rebuild = rebuilt_fat_ptr_from_existing_metadata(struct_info, data_ptr_ident);
    let fn_name = format_ident!("into{}", ptr.name_suffix);
    let factory_doc = format!(
        "Converts a `Box<{name}>` into a single-allocation `{display}<{name}>`.",
        name = struct_info.struct_name,
        display = ptr.display_name
    );

    quote! {
        #[doc = #factory_doc]
        #[allow(clippy::transmute_undefined_repr)]
        #visibility fn #fn_name(this: #box_path<Self>) -> #ptr_path<Self> {
            let layout = ::core::alloc::Layout::for_value(&*this);
            let src_ptr = #box_path::into_raw(this);

            unsafe {
                #alloc_tokens
                #fat_ptr_rebuild

                ::core::ptr::copy_nonoverlapping(src_ptr as *const u8, fat_ptr as *mut u8, layout.size());
                if layout.size() != 0 {
                    #dealloc_path(src_ptr as *mut u8, layout);
                }

                #from_raw
            }
        }
    }
}

pub fn gen_into_arc(macro_args: &MacroArgs, struct_info: &StructInfo) -> TokenStream {
    gen_into_ptr(macro_args, struct_info, &PtrKind::new_arc(macro_args.no_std))
}

pub fn gen_into_rc(macro_args: &MacroArgs, struct_info: &StructInfo) -> TokenStream {
    gen_into_ptr(macro_args, struct_info, &PtrKind::new_rc(macro_args.no_std))
}

// ---------------------------------------------------------------------------
// Unified factory generation functions
// ---------------------------------------------------------------------------

pub fn factory_for_slice_arg(macro_args: &MacroArgs, struct_info: &StructInfo, tail_elem_type: &Type, ptr: &PtrKind) -> TokenStream {
    let copy_bound_tokens: syn::WherePredicate = syn::parse_quote_spanned! {tail_elem_type.span()=>
        #tail_elem_type: ::core::marker::Copy
    };

    let mut factory_where_clause = struct_info.struct_generics.where_clause.as_ref().map_or_else(
        || syn::WhereClause {
            where_token: syn::token::Where::default(),
            predicates: Punctuated::new(),
        },
        Clone::clone,
    );

    factory_where_clause.predicates.push(copy_bound_tokens);

    let alloc_tokens = &ptr.alloc_tokens;
    let ptr_path = &ptr.ptr_path;
    let data_ptr_ident = &ptr.data_ptr_ident;
    let from_raw = &ptr.from_raw;

    let tail_layout = tail_layout(tail_elem_type, struct_info.tail_field.ty.span());
    let header_layout = header_layout(macro_args, struct_info, false);
    let tuple_assignment = args_tuple_assignment(struct_info);
    let header_field_writes = header_field_writes(struct_info);
    let header_params = header_params(struct_info);

    let factory_name = if ptr.name_suffix.is_empty() {
        format_ident!("{}_from_slice", &macro_args.base_factory_name)
    } else {
        format_ident!("{}{}_from_slice", &macro_args.base_factory_name, ptr.name_suffix)
    };
    let visibility = &macro_args.visibility;

    let tail_param = &struct_info.tail_param_ident;
    let tail_field = &struct_info.tail_field_ident;
    let struct_name = &struct_info.struct_name;
    let tail_args_tuple_idx = Index::from(struct_info.header_fields.len());

    let factory_doc = format!("Creates an instance of `{}<{struct_name}>`.", ptr.display_name);

    let zst_or_alloc = if ptr.name_suffix.is_empty() {
        // Box case
        let zst_tokens = alloc_zst(&ptr.ptr_path, false);
        quote! {
            if layout.size() == 0 {
                #zst_tokens
            } else {
                #alloc_tokens

                let fat_ptr = ::core::mem::transmute::<(*mut u8, usize), *mut Self>((#data_ptr_ident, len));
                ::core::debug_assert_eq!(::core::alloc::Layout::for_value(&*fat_ptr), layout);

                #( #header_field_writes )*

                let tail_ptr = (&raw mut (*fat_ptr).#tail_field).cast::<#tail_elem_type>();
                ::core::ptr::copy_nonoverlapping(s.as_ptr(), tail_ptr, len);

                #from_raw
            }
        }
    } else {
        // Arc/Rc always allocate
        quote! {
            #alloc_tokens

            let fat_ptr = ::core::mem::transmute::<(*mut u8, usize), *mut Self>((#data_ptr_ident, len));
            ::core::debug_assert_eq!(::core::alloc::Layout::for_value(&*fat_ptr), layout);

            #( #header_field_writes )*

            let tail_ptr = (&raw mut (*fat_ptr).#tail_field).cast::<#tail_elem_type>();
            ::core::ptr::copy_nonoverlapping(s.as_ptr(), tail_ptr, len);

            #from_raw
        }
    };

    quote! {
        #[doc = #factory_doc]
        #[allow(clippy::let_unit_value)]
        #[allow(clippy::zst_offset)]
        #[allow(clippy::transmute_undefined_repr)]
        #visibility fn #factory_name (
            #( #header_params, )*
            #tail_param: &[#tail_elem_type]
        ) -> #ptr_path<Self> #factory_where_clause {
            #tuple_assignment

            let s = args.#tail_args_tuple_idx.as_ref();
            let len = s.len();

            #header_layout
            let layout = layout.extend(#tail_layout).expect("Struct exceeds maximum size allowed of isize::MAX").0;
            let layout = layout.pad_to_align();

            unsafe {
                #zst_or_alloc
            }
        }
    }
}

pub fn factory_for_iter_arg(macro_args: &MacroArgs, struct_info: &StructInfo, tail_type: &Type, ptr: &PtrKind) -> TokenStream {
    let guard_type_tokens = guard_type(macro_args);
    let alloc_tokens = &ptr.alloc_tokens;
    let ptr_path = &ptr.ptr_path;
    let data_ptr_ident = &ptr.data_ptr_ident;
    let guard_layout = &ptr.guard_layout;
    let from_raw = &ptr.from_raw;

    let tail_layout = tail_layout(tail_type, struct_info.tail_field.ty.span());
    let header_layout = header_layout(macro_args, struct_info, false);
    let tuple_assignment = args_tuple_assignment(struct_info);
    let header_field_writes = header_field_writes(struct_info);
    let header_params = header_params(struct_info);

    let visibility = &macro_args.visibility;
    let factory_name = if ptr.name_suffix.is_empty() {
        macro_args.base_factory_name.clone()
    } else {
        format_ident!("{}{}", &macro_args.base_factory_name, ptr.name_suffix)
    };
    let iter_generic_param = &macro_args.generic_name;

    let tail_param = &struct_info.tail_param_ident;
    let tail_field = &struct_info.tail_field_ident;
    let struct_name = &struct_info.struct_name;
    let tail_args_tuple_idx = Index::from(struct_info.header_fields.len());

    let factory_doc = format!(
        "Creates an instance of `{}<{struct_name}>`. \
         Prefer `_from_slice` for vectors or arrays instead for better performance.",
        ptr.display_name
    );

    let zst_or_alloc = if ptr.name_suffix.is_empty() {
        // Box case
        let zst_tokens = alloc_zst(&ptr.ptr_path, false);
        quote! {
            if layout.size() == 0 {
                #zst_tokens
            } else {
                #alloc_tokens

                let fat_ptr = ::core::mem::transmute::<(*mut u8, usize), *mut Self>((#data_ptr_ident, len));
                ::core::debug_assert_eq!(::core::alloc::Layout::for_value(&*fat_ptr), layout);

                #( #header_field_writes )*

                let tail_ptr = (&raw mut (*fat_ptr).#tail_field).cast::<#tail_type>();
                let mut guard = Guard { mem_ptr, tail_ptr, layout: #guard_layout, initialized: 0 };

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

                #from_raw
            }
        }
    } else {
        // Arc/Rc always allocate
        quote! {
            #alloc_tokens

            let fat_ptr = ::core::mem::transmute::<(*mut u8, usize), *mut Self>((#data_ptr_ident, len));
            ::core::debug_assert_eq!(::core::alloc::Layout::for_value(&*fat_ptr), layout);

            #( #header_field_writes )*

            let tail_ptr = (&raw mut (*fat_ptr).#tail_field).cast::<#tail_type>();
            let mut guard = Guard { mem_ptr, tail_ptr, layout: #guard_layout, initialized: 0 };

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

            #from_raw
        }
    };

    quote! {
        #[doc = #factory_doc]
        #[allow(clippy::let_unit_value)]
        #[allow(clippy::zst_offset)]
        #[allow(clippy::transmute_undefined_repr)]
        #visibility fn #factory_name <#iter_generic_param> (
            #( #header_params, )*
            #tail_param: #iter_generic_param
        ) -> #ptr_path<Self>
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
                #zst_or_alloc
            }
        }
    }
}

pub fn factory_for_str_arg(macro_args: &MacroArgs, struct_info: &StructInfo, ptr: &PtrKind) -> TokenStream {
    let alloc_tokens = &ptr.alloc_tokens;
    let ptr_path = &ptr.ptr_path;
    let data_ptr_ident = &ptr.data_ptr_ident;
    let from_raw = &ptr.from_raw;

    let tail_layout = tail_layout(&quote! { u8 }, struct_info.tail_field.ty.span());
    let header_layout = header_layout(macro_args, struct_info, false);
    let tuple_assignment = args_tuple_assignment(struct_info);
    let header_field_writes = header_field_writes(struct_info);
    let header_params = header_params(struct_info);

    let factory_name = if ptr.name_suffix.is_empty() {
        macro_args.base_factory_name.clone()
    } else {
        format_ident!("{}{}", &macro_args.base_factory_name, ptr.name_suffix)
    };
    let visibility = &macro_args.visibility;

    let struct_name = &struct_info.struct_name;
    let tail_param = &struct_info.tail_param_ident;
    let tail_field = &struct_info.tail_field_ident;
    let tail_type = &struct_info.tail_field.ty;
    let tail_args_tuple_idx = Index::from(struct_info.header_fields.len());

    let factory_doc = format!("Creates an instance of `{}<{struct_name}>`.", ptr.display_name);

    let zst_or_alloc = if ptr.name_suffix.is_empty() {
        // Box case
        let zst_tokens = alloc_zst(&ptr.ptr_path, false);
        quote! {
            if layout.size() == 0 {
                #zst_tokens
            } else {
                #alloc_tokens

                let fat_ptr = ::core::mem::transmute::<(*mut u8, usize), *mut Self>((#data_ptr_ident, len));
                ::core::debug_assert_eq!(::core::alloc::Layout::for_value(&*fat_ptr), layout);

                #( #header_field_writes )*

                let tail_ptr = (&raw mut (*fat_ptr).#tail_field).cast::<u8>();
                ::core::ptr::copy_nonoverlapping(s.as_ptr(), tail_ptr, len);

                #from_raw
            }
        }
    } else {
        // Arc/Rc always allocate
        quote! {
            #alloc_tokens

            let fat_ptr = ::core::mem::transmute::<(*mut u8, usize), *mut Self>((#data_ptr_ident, len));
            ::core::debug_assert_eq!(::core::alloc::Layout::for_value(&*fat_ptr), layout);

            #( #header_field_writes )*

            let tail_ptr = (&raw mut (*fat_ptr).#tail_field).cast::<u8>();
            ::core::ptr::copy_nonoverlapping(s.as_ptr(), tail_ptr, len);

            #from_raw
        }
    };

    quote! {
        #[doc = #factory_doc]
        #[allow(clippy::let_unit_value)]
        #[allow(clippy::zst_offset)]
        #[allow(clippy::transmute_undefined_repr)]
        #visibility fn #factory_name(
            #( #header_params, )*
            #tail_param: impl ::core::convert::AsRef<str>
        ) -> #ptr_path<Self> {
            #tuple_assignment

            ::core::assert_eq!(::core::any::TypeId::of::<#tail_type>(), ::core::any::TypeId::of::<str>());
            let s = args.#tail_args_tuple_idx.as_ref();
            let len = s.len();

            #header_layout
            let layout = layout.extend(#tail_layout).expect("Struct exceeds maximum size allowed of isize::MAX").0;
            let layout = layout.pad_to_align();

            unsafe {
                #zst_or_alloc
            }
        }
    }
}

pub fn factory_for_trait_arg(macro_args: &MacroArgs, struct_info: &StructInfo, trait_path: &syn::Path, ptr: &PtrKind) -> TokenStream {
    let alloc_tokens = &ptr.alloc_tokens;
    let ptr_path = &ptr.ptr_path;
    let data_ptr_ident = &ptr.data_ptr_ident;
    let from_raw = &ptr.from_raw;

    let header_layout = header_layout(macro_args, struct_info, true);
    let tuple_assignment = args_tuple_assignment(struct_info);
    let header_field_writes = header_field_writes(struct_info);
    let header_params = header_params(struct_info);

    let factory_name = if ptr.name_suffix.is_empty() {
        macro_args.base_factory_name.clone()
    } else {
        format_ident!("{}{}", &macro_args.base_factory_name, ptr.name_suffix)
    };
    let trait_generic = &macro_args.generic_name;
    let visibility = &macro_args.visibility;

    let struct_name = &struct_info.struct_name;
    let tail_param = &struct_info.tail_param_ident;
    let tail_field = &struct_info.tail_field_ident;
    let tail_args_tuple_idx = Index::from(struct_info.header_fields.len());

    let factory_doc = format!("Builds an instance of `{}<{struct_name}>`.", ptr.display_name);

    let zst_or_alloc = if ptr.name_suffix.is_empty() {
        // Box case
        let zst_tokens = alloc_zst(&ptr.ptr_path, true); // Note: true for trait objects
        quote! {
            if layout.size() == 0 {
                #zst_tokens
            } else {
                #alloc_tokens

                let fat_ptr = ::core::mem::transmute::<(*mut u8, *const ()), *mut Self>((#data_ptr_ident, vtable));
                ::core::debug_assert_eq!(::core::alloc::Layout::for_value(&*fat_ptr), layout);

                #( #header_field_writes )*

                let tail_ptr = (&raw mut (*fat_ptr).#tail_field).cast::<#trait_generic>();
                ::core::ptr::copy_nonoverlapping(::core::ptr::addr_of!(s), tail_ptr, 1);
                ::core::mem::forget(s);

                #from_raw
            }
        }
    } else {
        // Arc/Rc always allocate
        quote! {
            #alloc_tokens

            let fat_ptr = ::core::mem::transmute::<(*mut u8, *const ()), *mut Self>((#data_ptr_ident, vtable));
            ::core::debug_assert_eq!(::core::alloc::Layout::for_value(&*fat_ptr), layout);

            #( #header_field_writes )*

            let tail_ptr = (&raw mut (*fat_ptr).#tail_field).cast::<#trait_generic>();
            ::core::ptr::copy_nonoverlapping(::core::ptr::addr_of!(s), tail_ptr, 1);
            ::core::mem::forget(s);

            #from_raw
        }
    };

    quote! {
        #[doc = #factory_doc]
        #[allow(clippy::let_unit_value)]
        #[allow(clippy::zst_offset)]
        #[allow(clippy::transmute_undefined_repr)]
        #visibility fn #factory_name <#trait_generic> (
            #( #header_params, )*
            #tail_param: #trait_generic
        ) -> #ptr_path<Self>
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
                #zst_or_alloc
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Orchestration
// ---------------------------------------------------------------------------

fn gen_serde(
    macro_args: &MacroArgs,
    struct_info: &StructInfo,
    input_struct: &ItemStruct,
    factories: &mut Vec<TokenStream>,
) -> SynResult<Option<TokenStream>> {
    match &struct_info.tail_kind {
        TailKind::TraitObject(_) => Err(syn::Error::new_spanned(
            input_struct,
            "deserialize is not supported for DSTs with trait object tails",
        )),
        TailKind::Slice(elem_type) => {
            let owned_tail_type: Type = if macro_args.no_std {
                syn::parse_quote! { ::alloc::vec::Vec<#elem_type> }
            } else {
                syn::parse_quote! { ::std::vec::Vec<#elem_type> }
            };
            let bp = box_path(macro_args.no_std);
            let factory_name = format_ident!("{}_from_slice", &macro_args.base_factory_name);
            let box_tokens = serde::gen_deserialize(struct_info, input_struct, &owned_tail_type, &factory_name, &bp, false);

            for ptr in &[PtrKind::new_arc(macro_args.no_std), PtrKind::new_rc(macro_args.no_std)] {
                let factory = format_ident!("{}{}_from_slice", &macro_args.base_factory_name, ptr.name_suffix);
                let fn_name = format_ident!("deserialize{}", ptr.name_suffix);
                let intermediate_name = format!("{}Intermediate", ptr.display_name);
                factories.push(serde::gen_deserialize_fn(
                    struct_info,
                    input_struct,
                    &owned_tail_type,
                    &factory,
                    &fn_name,
                    &intermediate_name,
                    &ptr.ptr_path,
                    &macro_args.visibility,
                    false,
                ));
            }

            Ok(Some(box_tokens))
        }
        TailKind::Str => {
            let (tail_type, borrow_tail): (Type, bool) = if macro_args.no_std {
                // no_std: use alloc::string::String (Cow requires std serde internals)
                (syn::parse_quote! { ::alloc::string::String }, false)
            } else {
                // std: use Cow<'__borrow, str> to avoid intermediate String allocation
                (syn::parse_quote! { ::std::borrow::Cow<'__borrow, str> }, true)
            };
            let bp = box_path(macro_args.no_std);
            let box_tokens = serde::gen_deserialize(
                struct_info,
                input_struct,
                &tail_type,
                &macro_args.base_factory_name,
                &bp,
                borrow_tail,
            );

            for ptr in &[PtrKind::new_arc(macro_args.no_std), PtrKind::new_rc(macro_args.no_std)] {
                let factory = format_ident!("{}{}", &macro_args.base_factory_name, ptr.name_suffix);
                let fn_name = format_ident!("deserialize{}", ptr.name_suffix);
                let intermediate_name = format!("{}Intermediate", ptr.display_name);
                factories.push(serde::gen_deserialize_fn(
                    struct_info,
                    input_struct,
                    &tail_type,
                    &factory,
                    &fn_name,
                    &intermediate_name,
                    &ptr.ptr_path,
                    &macro_args.visibility,
                    borrow_tail,
                ));
            }

            Ok(Some(box_tokens))
        }
    }
}

pub fn make_dst_factory_impl(attr_args: TokenStream, item: TokenStream) -> SynResult<TokenStream> {
    let macro_args = MacroArgs::parse(attr_args)?;
    let input_struct: ItemStruct = syn::parse2(item)?;
    let struct_info = StructInfo::new(&input_struct)?;

    let mut factories = Vec::new();
    let mut iterator_type = None;

    factories.push(gen_into_arc(&macro_args, &struct_info));
    factories.push(gen_into_rc(&macro_args, &struct_info));

    // Create PtrKind instances for all three pointer types
    let ptr_kinds = [
        PtrKind::new_box(macro_args.no_std),
        PtrKind::new_arc(macro_args.no_std),
        PtrKind::new_rc(macro_args.no_std),
    ];

    match &struct_info.tail_kind {
        TailKind::Slice(elem_type) => {
            // Generate factories for all pointer types using unified approach
            for ptr in &ptr_kinds {
                factories.push(factory_for_iter_arg(&macro_args, &struct_info, elem_type, ptr));
                factories.push(factory_for_slice_arg(&macro_args, &struct_info, elem_type, ptr));
            }

            // Box-specific destructurer functionality (only for Box)
            factories.push(r#box::destructurer_with_iter(&macro_args, &struct_info));
            iterator_type = Some(r#box::destructurer_iterator_type(&macro_args, &struct_info, elem_type));
        }

        TailKind::Str => {
            // Generate factories for all pointer types using unified approach
            for ptr in &ptr_kinds {
                factories.push(factory_for_str_arg(&macro_args, &struct_info, ptr));
            }
        }

        TailKind::TraitObject(trait_path) => {
            // Generate factories for all pointer types using unified approach
            for ptr in &ptr_kinds {
                factories.push(factory_for_trait_arg(&macro_args, &struct_info, trait_path, ptr));
            }
        }
    }

    let mut serde_tokens: Option<TokenStream> = None;
    let mut clone_tokens: Option<TokenStream> = None;
    let mut debug_tokens: Option<TokenStream> = None;
    let mut partial_eq_tokens: Option<TokenStream> = None;
    let mut eq_tokens: Option<TokenStream> = None;
    let mut partial_ord_tokens: Option<TokenStream> = None;
    let mut ord_tokens: Option<TokenStream> = None;
    let mut hash_tokens: Option<TokenStream> = None;

    if macro_args.deserialize {
        serde_tokens = gen_serde(&macro_args, &struct_info, &input_struct, &mut factories)?;
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
                clone_tokens = Some(r#box::gen_clone(&macro_args, &struct_info, &r#box::CloneTailKind::Slice(elem_type)));
            }
            TailKind::Str => {
                clone_tokens = Some(r#box::gen_clone(&macro_args, &struct_info, &r#box::CloneTailKind::Str));
            }
        }
    }

    if macro_args.debug {
        debug_tokens = Some(r#box::gen_debug(&struct_info));
    }

    if macro_args.eq {
        partial_eq_tokens = Some(r#box::gen_partial_eq(&struct_info));
        eq_tokens = Some(r#box::gen_eq(&struct_info));
    }

    if macro_args.ord {
        partial_ord_tokens = Some(r#box::gen_partial_ord(&struct_info));
        ord_tokens = Some(r#box::gen_ord(&struct_info));
    }

    if macro_args.hash {
        hash_tokens = Some(r#box::gen_hash(&struct_info));
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
        #debug_tokens
        #partial_eq_tokens
        #eq_tokens
        #partial_ord_tokens
        #ord_tokens
        #hash_tokens
    })
}

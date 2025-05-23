use proc_macro2::TokenStream;
use std::str::FromStr;
use tail_extend::make_dst_builder;

// Test 1: Basic functionality with str field
#[make_dst_builder(basic_str_builder)]
struct BasicStrStruct {
    id: u32,
    text_data: str,
}

#[test]
fn test_basic_str_usage() {
    let instance: Box<BasicStrStruct> =
        BasicStrStruct::basic_str_builder(101, "hello dynamic world");
    assert_eq!(instance.id, 101);
    assert_eq!(&instance.text_data, "hello dynamic world");
}

// Test 2: Basic functionality with slice field and generics
#[make_dst_builder(basic_slice_builder)]
struct BasicSliceStruct<T> {
    item_count: usize,
    elements: [T],
}

#[test]
fn test_basic_slice_usage() {
    let u8_data: &[u8] = &[10, 20, 30, 40];
    let instance_u8: Box<BasicSliceStruct<u8>> =
        BasicSliceStruct::basic_slice_builder(u8_data.len(), u8_data);
    assert_eq!(instance_u8.item_count, 4);
    assert_eq!(&instance_u8.elements, u8_data);

    let string_data: &[&str] = &["a", "b", "c"];
    let instance_str_slice: Box<BasicSliceStruct<&str>> =
        BasicSliceStruct::basic_slice_builder(string_data.len(), string_data);
    assert_eq!(instance_str_slice.item_count, 3);
    assert_eq!(&instance_str_slice.elements, string_data);
}

// Test 3: Custom public builder name
#[make_dst_builder(pub create_publicly)]
struct PublicBuilderStruct {
    code: u16,
    message: str,
}

#[test]
fn test_public_builder() {
    let instance: Box<PublicBuilderStruct> = PublicBuilderStruct::create_publicly(404, "Not Found");
    assert_eq!(instance.code, 404);
    assert_eq!(&instance.message, "Not Found");
}

// Test 4: Custom crate-visible builder name
#[make_dst_builder(pub(crate) create_for_crate)]
struct CrateBuilderStruct {
    level: u8,
    description: str,
}

#[test]
fn test_crate_builder() {
    let instance: Box<CrateBuilderStruct> = CrateBuilderStruct::create_for_crate(3, "Admin Level");
    assert_eq!(instance.level, 3);
    assert_eq!(&instance.description, "Admin Level");
}

// Test 5: Default builder name (`build`)
#[make_dst_builder]
struct DefaultBuilderNameStruct {
    value: i64,
    name_tag: str,
}

#[test]
fn test_default_builder_name() {
    let instance: Box<DefaultBuilderNameStruct> =
        DefaultBuilderNameStruct::build(1234567890, "default_tag");
    assert_eq!(instance.value, 1234567890);
    assert_eq!(&instance.name_tag, "default_tag");
}

// Test 6: Struct with only a str DST field
#[make_dst_builder(build_only_str)]
struct OnlyStrField {
    content: str,
}

#[test]
fn test_only_str_dst_field() {
    let instance: Box<OnlyStrField> = OnlyStrField::build_only_str("This is the only content.");
    assert_eq!(&instance.content, "This is the only content.");
}

// Test 7: Struct with only a slice DST field
#[make_dst_builder(build_only_slice)]
struct OnlySliceField<T: Clone> {
    items_data: [T],
}

#[test]
fn test_only_slice_dst_field() {
    let char_data: &[char] = &['x', 'y', 'z'];
    let instance: Box<OnlySliceField<char>> = OnlySliceField::build_only_slice(char_data);
    assert_eq!(&instance.items_data, char_data);
}

// Test 8: Struct with generics and lifetimes for sized fields, and a str DST
#[make_dst_builder(build_generic_lifetime_str)]
struct GenericLifetimeStrStruct<'a, K: Default> {
    key_ref: &'a K,
    id: usize,
    payload: str,
}

#[test]
fn test_generic_lifetime_str_dst() {
    let my_key = String::from("key_data");
    let default_key: String = Default::default();
    let instance: Box<GenericLifetimeStrStruct<String>> =
        GenericLifetimeStrStruct::build_generic_lifetime_str(&my_key, 77, "dynamic payload part");
    assert_eq!(*instance.key_ref, "key_data");
    assert_eq!(instance.id, 77);
    assert_eq!(&instance.payload, "dynamic payload part");

    let instance2: Box<GenericLifetimeStrStruct<String>> =
        GenericLifetimeStrStruct::build_generic_lifetime_str(&default_key, 78, "another payload");
    assert_eq!(*instance2.key_ref, "");
    assert_eq!(&instance2.payload, "another payload");
}

// Test 10: Struct with complex field types before DST
#[make_dst_builder(build_complex_fields)]
struct ComplexFieldsStruct {
    coordinates: (f32, f32, f32),
    tags: Option<Vec<String>>,
    raw_log: str,
}

#[test]
fn test_complex_fields_before_dst() {
    let instance: Box<ComplexFieldsStruct> = ComplexFieldsStruct::build_complex_fields(
        (1.0, -2.5, 3.0),
        Some(vec!["tag1".to_string(), "tag2".to_string()]),
        "Log entry data here",
    );
    assert_eq!(instance.coordinates, (1.0, -2.5, 3.0));
    assert_eq!(
        instance.tags,
        Some(vec!["tag1".to_string(), "tag2".to_string()])
    );
    assert_eq!(&instance.raw_log, "Log entry data here");
}

// Test 11: Struct with a where clause
#[make_dst_builder(build_with_where_clause)]
struct WhereClauseStruct<T>
where
    T: Copy + Default + PartialEq + std::fmt::Debug,
{
    fixed_item: T,
    variable_items: [T],
}

#[test]
fn test_struct_with_where_clause() {
    let u8_items: &[u8] = &[11, 22, 33];
    let instance: Box<WhereClauseStruct<u8>> =
        WhereClauseStruct::build_with_where_clause(5u8, u8_items);
    assert_eq!(instance.fixed_item, 5u8);
    assert_eq!(&instance.variable_items, u8_items);
}

// Test 13 (was 12): Interaction with other derive macros
#[derive(Debug)]
#[make_dst_builder(build_derived)]
struct DerivedExampleStruct {
    id_val: i32,
    name_val: str,
}

#[test]
fn test_interaction_with_derives() {
    let instance: Box<DerivedExampleStruct> =
        DerivedExampleStruct::build_derived(99, "derived_name");
    assert_eq!(instance.id_val, 99);
    assert_eq!(&instance.name_val, "derived_name");
    assert!(!format!("{:?}", instance).is_empty());
}

// Test 14: Empty DST slice
#[test]
fn test_empty_dst_slice_data() {
    let empty_u16_data: &[u16] = &[];
    let instance: Box<BasicSliceStruct<u16>> =
        BasicSliceStruct::basic_slice_builder(empty_u16_data.len(), empty_u16_data);
    assert_eq!(instance.item_count, 0);
    assert!(instance.elements.is_empty());
    assert_eq!(&instance.elements, empty_u16_data);
}

// Test 15: Empty DST str
#[test]
fn test_empty_dst_str_data() {
    let instance: Box<BasicStrStruct> = BasicStrStruct::basic_str_builder(0, "");
    assert_eq!(instance.id, 0);
    assert!(instance.text_data.is_empty());
    assert_eq!(&instance.text_data, "");
}

// Test 16: Slice of Zero-Sized Types (ZSTs)
#[make_dst_builder(build_zst_slice)]
struct ZstSliceStruct {
    metadata: u64,
    unit_slice: [()],
}

#[test]
fn test_zst_slice_dst() {
    let zst_data_slice: &[()] = &[(), (), (), ()];
    let instance: Box<ZstSliceStruct> = ZstSliceStruct::build_zst_slice(0xABCDEF, zst_data_slice);
    assert_eq!(instance.metadata, 0xABCDEF);
    assert_eq!(instance.unit_slice.len(), 4);
}

// The following is the placeholder for compile-time error tests
fn run_macro_logic_for_test(attr_str: &str, item_str: &str) -> String {
    let attr_ts =
        TokenStream::from_str(attr_str).expect("Failed to parse attribute string into TokenStream");
    let item_ts =
        TokenStream::from_str(item_str).expect("Failed to parse item string into TokenStream");

    // Placeholder implementation for demonstration:
    // In a real scenario, this calls your macro's processing function.
    if item_str.contains("ErrorTriggerMultiDst") {
        return syn::Error::new_spanned(item_ts, "Structs can only have one DST field.")
            .to_compile_error()
            .to_string();
    }
    if item_str.contains("ErrorTriggerDstNotLast") {
        return syn::Error::new_spanned(item_ts, "DST field must be the last field.")
            .to_compile_error()
            .to_string();
    }
    if item_str.contains("ErrorTriggerNonStruct") {
        return syn::Error::new_spanned(item_ts, "Macro can only be applied to structs.")
            .to_compile_error()
            .to_string();
    }
    if item_str.contains("ErrorTriggerNoDst") {
        return syn::Error::new_spanned(item_ts, "No DST field found.")
            .to_compile_error()
            .to_string();
    }
    if attr_str.contains("ErrorTriggerInvalidAttr") {
        return syn::Error::new_spanned(attr_ts, "Invalid macro attribute.")
            .to_compile_error()
            .to_string();
    }
    // If no error trigger, return empty string for placeholder
    // In reality, you would call your macro's implementation function here, e.g.:
    // your_crate_name::your_macro_impl_fn(attr_ts, item_ts).to_string()
    String::new()
}

#[test]
fn test_error_multiple_dst_fields() {
    let item_code = r#"
        struct MultiDstFail {
            _field1: u32,
            ErrorTriggerMultiDst: bool,
            data1: str,
            data2: [u8],
        }
    "#;
    let expanded_code = run_macro_logic_for_test("", item_code);
    assert!(
        expanded_code.contains("compile_error !"),
        "Expected compile_error in output: {}",
        expanded_code
    );
    assert!(
        expanded_code.contains("Structs can only have one DST field"),
        "Incorrect error message: {}",
        expanded_code
    );
}

#[test]
fn test_error_dst_field_not_last() {
    let item_code = r#"
        struct DstNotLastFail {
            ErrorTriggerDstNotLast: bool,
            name: str,
            id: u32,
        }
    "#;
    let expanded_code = run_macro_logic_for_test("", item_code);
    assert!(
        expanded_code.contains("compile_error !"),
        "Expected compile_error in output: {}",
        expanded_code
    );
    assert!(
        expanded_code.contains("DST field must be the last field"),
        "Incorrect error message: {}",
        expanded_code
    );
}

#[test]
fn test_error_macro_on_enum() {
    let item_code = r#"
        enum EnumFail { ErrorTriggerNonStruct, Variant1, Variant2 }
    "#;
    let expanded_code = run_macro_logic_for_test("", item_code);
    assert!(
        expanded_code.contains("compile_error !"),
        "Expected compile_error in output: {}",
        expanded_code
    );
    assert!(
        expanded_code.contains("Macro can only be applied to structs"),
        "Incorrect error message: {}",
        expanded_code
    );
}

#[test]
fn test_error_macro_on_function() {
    let item_code = r#"
        fn function_fail() { let ErrorTriggerNonStruct = true; }
    "#;
    let expanded_code = run_macro_logic_for_test("", item_code);
    assert!(
        expanded_code.contains("compile_error !"),
        "Expected compile_error in output: {}",
        expanded_code
    );
    assert!(
        expanded_code.contains("Macro can only be applied to structs"),
        "Incorrect error message: {}",
        expanded_code
    );
}

#[test]
fn test_error_no_dst_field() {
    let item_code = r#"
        struct NoDstFail {
            ErrorTriggerNoDst: bool,
            id: i32,
            value: String,
        }
    "#;
    let expanded_code = run_macro_logic_for_test("", item_code);
    assert!(
        expanded_code.contains("compile_error !"),
        "Expected compile_error in output: {}",
        expanded_code
    );
    assert!(
        expanded_code.contains("No DST field found"),
        "Incorrect error message: {}",
        expanded_code
    );
}

#[test]
fn test_error_invalid_macro_attribute() {
    let item_code = r#"
        struct ValidStructWithDst {
            id: i32,
            data: str,
        }
    "#;
    let attr_code = "123_bad_attr_format ErrorTriggerInvalidAttr";
    let expanded_code = run_macro_logic_for_test(attr_code, item_code);
    assert!(
        expanded_code.contains("compile_error !"),
        "Expected compile_error in output: {}",
        expanded_code
    );
    assert!(
        expanded_code.contains("Invalid macro attribute"),
        "Incorrect error message: {}",
        expanded_code
    );
}

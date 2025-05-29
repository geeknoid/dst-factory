#![allow(clippy::literal_string_with_formatting_args)]

use dst_factory::make_dst_factory;
use std::fmt::Debug;

#[make_dst_factory(basic_str_builder)]
struct BasicStrStruct {
    id: usize,
    text_data: str,
}

#[test]
fn test_basic_str_usage() {
    for i in 0..64 {
        let s = ".".repeat(i);

        let instance: Box<BasicStrStruct> = BasicStrStruct::basic_str_builder(i, s.as_str());

        assert_eq!(instance.id, i);
        assert_eq!(&instance.text_data, s.as_str());
    }
}

#[make_dst_factory]
struct LongFormStrStruct {
    id: usize,
    text_data: std::primitive::str,
}

#[test]
fn test_long_form_str_usage() {
    for i in 0..64 {
        let s = ".".repeat(i);

        let instance: Box<LongFormStrStruct> = LongFormStrStruct::build(i, s.as_str());

        assert_eq!(instance.id, i);
        assert_eq!(&instance.text_data, s.as_str());
    }
}

#[make_dst_factory(basic_slice_builder)]
struct BasicSliceStruct<T> {
    id: usize,
    elements: [T],
}

#[test]
fn test_basic_slice_usage() {
    for i in 0..64 {
        let v = vec!['*'; i];

        let instance: Box<BasicSliceStruct<char>> =
            BasicSliceStruct::basic_slice_builder(i, v.as_slice());

        assert_eq!(instance.id, i);
        assert_eq!(&instance.elements, v.as_slice());
    }
}

#[make_dst_factory(create_publicly, pub)]
struct PublicBuilderStruct {
    code: std::primitive::u16,
    message: std::primitive::str,
}

#[test]
fn test_public_builder() {
    let instance: Box<PublicBuilderStruct> = PublicBuilderStruct::create_publicly(404, "Not Found");
    assert_eq!(instance.code, 404);
    assert_eq!(&instance.message, "Not Found");
}

#[make_dst_factory(create_for_crate, pub(crate))]
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

#[make_dst_factory]
struct DefaultBuilderNameStruct {
    value: i64,
    name_tag: str,
}

#[test]
fn test_default_builder_name() {
    let instance: Box<DefaultBuilderNameStruct> =
        DefaultBuilderNameStruct::build(1_234_567_890, "default_tag");
    assert_eq!(instance.value, 1_234_567_890);
    assert_eq!(&instance.name_tag, "default_tag");
}

#[make_dst_factory(build_only_str)]
struct OnlyStrField {
    content: str,
}

#[test]
fn test_only_str_dst_field() {
    let instance: Box<OnlyStrField> = OnlyStrField::build_only_str("This is the only content.");
    assert_eq!(&instance.content, "This is the only content.");
}

#[make_dst_factory(build_only_slice)]
struct OnlySliceField<T: Clone> {
    items_data: [T],
}

#[test]
fn test_only_slice_dst_field() {
    let char_data: &[char] = &['x', 'y', 'z'];
    let instance: Box<OnlySliceField<char>> = OnlySliceField::build_only_slice(char_data);
    assert_eq!(&instance.items_data, char_data);
}

#[make_dst_factory(build_generic_lifetime_str)]
struct GenericLifetimeStrStruct<'a, K: Default> {
    key_ref: &'a K,
    id: usize,
    payload: str,
}

#[test]
fn test_generic_lifetime_str_dst() {
    let my_key = String::from("key_data");
    let default_key: String = String::default();
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

const SOME_CONST: usize = 2;

#[make_dst_factory]
struct GenericConstStruct<const SZ: usize> {
    id: usize,
    array_data: [u8; SZ],
    more: [u8; SOME_CONST],
    payload: str,
}

#[test]
fn test_generic_const_dst() {
    let instance: Box<GenericConstStruct<2>> =
        GenericConstStruct::build(42, [0, 1], [3, 4], "dynamic payload part");
    assert_eq!(instance.id, 42);
    assert_eq!(&instance.payload, "dynamic payload part");
}

#[make_dst_factory(build_complex_fields)]
struct ComplexFieldsStruct<T>
where
    T: Debug,
{
    coordinates: (f32, f32, f32),
    tags: Option<Vec<String>>,
    dbg: T,
    raw_log: str,
}

#[test]
fn test_complex_fields_before_dst() {
    let instance: Box<ComplexFieldsStruct<u8>> = ComplexFieldsStruct::build_complex_fields(
        (1.0, -2.5, 3.0),
        Some(vec!["tag1".to_string(), "tag2".to_string()]),
        0,
        "Log entry data here",
    );
    assert_eq!(instance.coordinates, (1.0, -2.5, 3.0));
    assert_eq!(
        instance.tags,
        Some(vec!["tag1".to_string(), "tag2".to_string()])
    );
    assert_eq!(&instance.raw_log, "Log entry data here"); // This test fails due to a bug in the macro
}

#[make_dst_factory(build_from_iter_where_clause)]
struct WhereClauseStruct<T>
where
    T: Copy + Default + PartialEq + std::fmt::Debug,
{
    fixed_item: T,
    variable_items: [T],
}

#[test]
fn test_struct_from_iter_where_clause() {
    let u8_items: &[u8] = &[11, 22, 33];
    let instance: Box<WhereClauseStruct<u8>> =
        WhereClauseStruct::build_from_iter_where_clause(5u8, u8_items);
    assert_eq!(instance.fixed_item, 5u8);
    assert_eq!(&instance.variable_items, u8_items);
}

#[derive(Debug)]
#[make_dst_factory(build_derived)]
struct DerivedExampleStruct {
    id_val: i32,
    name_val: str,
}

#[test]
fn test_interaction_from_iter_derives() {
    let instance: Box<DerivedExampleStruct> =
        DerivedExampleStruct::build_derived(99, "derived_name");
    assert_eq!(instance.id_val, 99);
    assert_eq!(&instance.name_val, "derived_name");
    assert!(!format!("{instance:?}").is_empty());
}

#[test]
fn test_empty_dst_slice_data() {
    let empty_u16_data: &[u16] = &[];
    let instance: Box<BasicSliceStruct<u16>> =
        BasicSliceStruct::basic_slice_builder(empty_u16_data.len(), empty_u16_data);
    assert_eq!(instance.id, 0);
    assert!(instance.elements.is_empty());
    assert_eq!(&instance.elements, empty_u16_data);
}

#[test]
fn test_empty_dst_str_data() {
    let instance: Box<BasicStrStruct> = BasicStrStruct::basic_str_builder(0, "");
    assert_eq!(instance.id, 0);
    assert!(instance.text_data.is_empty());
    assert_eq!(&instance.text_data, "");
}

#[make_dst_factory(build_zst_slice)]
struct ZstSliceStruct {
    a: u64,
    unit_slice: [()],
}

#[test]
fn test_zst_slice_dst() {
    let zst_data_slice: &[()] = &[(), (), (), ()];
    let instance: Box<ZstSliceStruct> = ZstSliceStruct::build_zst_slice(0xAB_CDEF, zst_data_slice);
    assert_eq!(instance.a, 0xAB_CDEF);
    assert_eq!(instance.unit_slice.len(), 4);
}

// Iterator that incorrectly reports its length via ExactSizeIterator
struct FaultyIter {
    items_to_yield: usize,
    len_to_return: usize,
}

impl Iterator for FaultyIter {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.items_to_yield > 0 {
            self.items_to_yield -= 1;
            Some(42)
        } else {
            None
        }
    }
}

impl ExactSizeIterator for FaultyIter {
    fn len(&self) -> usize {
        self.len_to_return
    }
}

#[test]
#[should_panic(
    expected = "Mismatch between iterator-reported length and the number of items produced by the iterator"
)]
fn test_build_from_iter_with_too_many_items() {
    let iterator_with_wrong_len = FaultyIter {
        items_to_yield: 20,
        len_to_return: 10,
    };

    // This call is expected to panic because the iterator's `len()` is misleading.
    let _ = BasicSliceStruct::<u8>::basic_slice_builder_from_iter(42, iterator_with_wrong_len);
}

#[test]
#[should_panic(
    expected = "Mismatch between iterator-reported length and the number of items produced by the iterator"
)]
fn test_build_from_iter_with_too_few_items() {
    let iterator_with_wrong_len = FaultyIter {
        items_to_yield: 10,
        len_to_return: 20,
    };

    // This call is expected to panic because the iterator's `len()` is misleading.
    let _ = BasicSliceStruct::<u8>::basic_slice_builder_from_iter(42, iterator_with_wrong_len);
}

// a trait we'll use in our DST
trait NumberProducer {
    fn get_number(&self) -> u32;
}

// an implementation of the trait we're going to use
struct FortyTwoProducer {}
impl NumberProducer for FortyTwoProducer {
    fn get_number(&self) -> u32 {
        42
    }
}

// another implementation of the trait we're going to use
struct TenProducer {}
impl NumberProducer for TenProducer {
    fn get_number(&self) -> u32 {
        10
    }
}

#[make_dst_factory]
struct Node {
    count: u32,
    producer: dyn NumberProducer,
}

#[test]
fn test_dst_with_trait_object() {
    // allocate an instance with one implementation of the trait
    let a = Node::build(33, FortyTwoProducer {});
    assert_eq!(42, a.producer.get_number());

    // allocate an instance with another implementation of the trait
    let b = Node::build(33, TenProducer {});
    assert_eq!(10, b.producer.get_number());
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_no_std() {
    let t = trybuild::TestCases::new();
    t.pass("tests/ui/no_std.rs");
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_error_paths() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/string_in_attribute.rs");
    t.compile_fail("tests/ui/struct_with_no_fields.rs");
    t.compile_fail("tests/ui/last_field_not_slice_or_str.rs");
    t.compile_fail("tests/ui/macro_not_on_struct.rs");
    t.compile_fail("tests/ui/unnamed_fields.rs");
    t.compile_fail("tests/ui/dst_field_not_last.rs");
    t.compile_fail("tests/ui/too_many_tokens.rs");
    t.compile_fail("tests/ui/bad_visibility.rs");
    t.compile_fail("tests/ui/no_comma_after_visibility.rs");
    t.compile_fail("tests/ui/need_no_std.rs");
    t.compile_fail("tests/ui/bad_str.rs");

    // this gives a different error in CI then locally (Windows vs. Linux difference?)
    // t.compile_fail("tests/ui/multiple_dst_fields.rs");
}

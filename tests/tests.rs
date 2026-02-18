use core::fmt::Debug;
use core::ptr::read_unaligned;
use dst_factory::make_dst_factory;

#[make_dst_factory(basic_str_builder)]
struct BasicStrStruct {
    id: usize,
    text_data: str,
}

#[test]
fn basic_str_usage() {
    for i in 0..64 {
        let s = ".".repeat(i);

        let instance: Box<BasicStrStruct> = BasicStrStruct::basic_str_builder(i, s.as_str());

        assert_eq!(instance.id, i);
        assert_eq!(&instance.text_data, s.as_str());
    }
}

#[make_dst_factory]
struct BasicTupleStruct(usize, str);

#[test]
fn basic_tuple_usage() {
    for i in 0..64 {
        let s = ".".repeat(i);

        let instance: Box<BasicTupleStruct> = BasicTupleStruct::build(i, s.as_str());

        assert_eq!(instance.0, i);
        assert_eq!(&instance.1, s.as_str());
    }
}

#[make_dst_factory]
struct LongFormStrStruct {
    id: usize,
    text_data: str,
}

#[test]
fn long_form_str_usage() {
    for i in 0..64 {
        let s = ".".repeat(i);

        let instance: Box<LongFormStrStruct> = LongFormStrStruct::build(i, s.as_str());

        assert_eq!(instance.id, i);
        assert_eq!(&instance.text_data, s.as_str());
    }
}

#[make_dst_factory(basic_slice_builder, destructurer = basic_slice_destructure, iterator = IterForBasicSlice, generic = M)]
struct BasicSliceStruct<T> {
    id: usize,
    elements: [T],
}

#[test]
fn basic_slice_usage() {
    for i in 0..64 {
        let v = vec!['*'; i];

        let instance: Box<BasicSliceStruct<char>> = BasicSliceStruct::basic_slice_builder_from_slice(i, v.as_slice());

        assert_eq!(instance.id, i);
        assert_eq!(&instance.elements, v.as_slice());

        let (id, elements_iter): (usize, IterForBasicSlice<char>) = BasicSliceStruct::basic_slice_destructure(instance);
        assert_eq!(id, i);
        assert!(elements_iter.eq(v));
    }
}

#[make_dst_factory(create_publicly, pub)]
struct PublicBuilderStruct {
    code: u16,
    message: str,
}

#[test]
fn public_builder() {
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
fn crate_builder() {
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
fn default_builder_name() {
    let instance: Box<DefaultBuilderNameStruct> = DefaultBuilderNameStruct::build(1_234_567_890, "default_tag");
    assert_eq!(instance.value, 1_234_567_890);
    assert_eq!(&instance.name_tag, "default_tag");
}

#[make_dst_factory(build_only_str)]
struct OnlyStrField {
    content: str,
}

#[test]
fn only_str_dst_field() {
    let instance: Box<OnlyStrField> = OnlyStrField::build_only_str("This is the only content.");
    assert_eq!(&instance.content, "This is the only content.");
}

#[make_dst_factory(build_only_slice)]
struct OnlySliceField<T: Clone> {
    items_data: [T],
}

#[test]
fn only_slice_dst_field() {
    let char_data: &[char] = &['x', 'y', 'z'];
    let instance: Box<OnlySliceField<char>> = OnlySliceField::build_only_slice_from_slice(char_data);
    assert_eq!(&instance.items_data, char_data);
    // notice that this is not a tuple in this case when the slice is the only field
    let items_iter: OnlySliceFieldIter<char> = OnlySliceField::destructure(instance);
    assert!(items_iter.eq(char_data.iter().copied()));
}

#[make_dst_factory]
struct OnlyTraitField {
    producer: dyn NumberProducer,
}

#[test]
fn only_trait_dst_field() {
    let instance: Box<OnlyTraitField> = OnlyTraitField::build(TenProducer {});
    assert_eq!(instance.producer.get_number(), 10);
}

#[make_dst_factory(build_generic_lifetime_str)]
struct GenericLifetimeStrStruct<'a, K: Default> {
    key_ref: &'a K,
    id: usize,
    payload: str,
}

#[test]
fn generic_lifetime_str_dst() {
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
fn generic_const_dst() {
    let instance: Box<GenericConstStruct<2>> = GenericConstStruct::build(42, [0, 1], [3, 4], "dynamic payload part");
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
fn complex_fields_before_dst() {
    let instance: Box<ComplexFieldsStruct<u8>> = ComplexFieldsStruct::build_complex_fields(
        (1.0, -2.5, 3.0),
        Some(vec!["tag1".to_string(), "tag2".to_string()]),
        0,
        "Log entry data here",
    );
    assert_eq!(instance.coordinates, (1.0, -2.5, 3.0));
    assert_eq!(instance.tags, Some(vec!["tag1".to_string(), "tag2".to_string()]));
    assert_eq!(&instance.raw_log, "Log entry data here"); // This test fails due to a bug in the macro
}

#[make_dst_factory(build_where_clause)]
struct WhereClauseStruct<T>
where
    T: Copy + Default + PartialEq + Debug,
{
    fixed_item: T,
    variable_items: [T],
}

#[test]
fn struct_from_iter_where_clause() {
    let u8_items: &[u8] = &[11, 22, 33];
    let instance: Box<WhereClauseStruct<u8>> = WhereClauseStruct::build_where_clause_from_slice(5u8, u8_items);
    assert_eq!(instance.fixed_item, 5u8);
    assert_eq!(&instance.variable_items, u8_items);

    let (fixed_item, variable_items_iter) = WhereClauseStruct::destructure(instance);
    assert_eq!(fixed_item, 5u8);
    assert!(variable_items_iter.eq(u8_items.iter().copied()));
}

#[derive(Debug)]
#[make_dst_factory(build_derived)]
struct DerivedExampleStruct {
    id_val: i32,
    name_val: str,
}

#[test]
fn interaction_from_iter_derives() {
    let instance: Box<DerivedExampleStruct> = DerivedExampleStruct::build_derived(99, "derived_name");
    assert_eq!(instance.id_val, 99);
    assert_eq!(&instance.name_val, "derived_name");
    assert!(!format!("{instance:?}").is_empty());
}

#[test]
fn empty_dst_slice_data() {
    let empty_u16_data: &[u16] = &[];
    let instance: Box<BasicSliceStruct<u16>> = BasicSliceStruct::basic_slice_builder_from_slice(empty_u16_data.len(), empty_u16_data);
    assert_eq!(instance.id, 0);
    assert!(instance.elements.is_empty());
    assert_eq!(&instance.elements, empty_u16_data);
    let (_, mut elements_iter) = BasicSliceStruct::basic_slice_destructure(instance);
    assert_eq!(elements_iter.next(), None);
}

#[test]
fn empty_dst_str_data() {
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
fn zst_slice_dst() {
    let zst_data_slice: &[()] = &[(), (), (), ()];
    let instance: Box<ZstSliceStruct> = ZstSliceStruct::build_zst_slice_from_slice(0xAB_CDEF, zst_data_slice);
    assert_eq!(instance.a, 0xAB_CDEF);
    assert_eq!(instance.unit_slice.len(), 4);
    let (a, mut unit_iter) = ZstSliceStruct::destructure(instance);
    assert_eq!(a, 0xAB_CDEF);
    assert_eq!(unit_iter.next(), Some(()));
    assert_eq!(unit_iter.next(), Some(()));
    assert_eq!(unit_iter.next(), Some(()));
    assert_eq!(unit_iter.next(), Some(()));
    assert_eq!(unit_iter.next(), None);
}

// Iterator that incorrectly reports its length via ExactSizeIterator
struct FaultyIter {
    items_to_yield: usize,
    len_to_return: usize,
}

impl Iterator for FaultyIter {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        (self.items_to_yield > 0).then(|| {
            self.items_to_yield -= 1;
            42u8
        })
    }
}

impl ExactSizeIterator for FaultyIter {
    fn len(&self) -> usize {
        self.len_to_return
    }
}

#[test]
#[should_panic(expected = "Mismatch between iterator-reported length and the number of items produced by the iterator")]
fn build_with_too_many_items() {
    let iterator_with_wrong_len = FaultyIter {
        items_to_yield: 20,
        len_to_return: 10,
    };

    // This call is expected to panic because the iterator's `len()` is misleading.
    let _ = BasicSliceStruct::<u8>::basic_slice_builder(42, iterator_with_wrong_len);
}

#[test]
#[should_panic(expected = "Mismatch between iterator-reported length and the number of items produced by the iterator")]
fn build_with_too_few_items() {
    let iterator_with_wrong_len = FaultyIter {
        items_to_yield: 10,
        len_to_return: 20,
    };

    // This call is expected to panic because the iterator's `len()` is misleading.
    let _ = BasicSliceStruct::<u8>::basic_slice_builder(42, iterator_with_wrong_len);
}

// a trait we'll use in our DST
trait NumberProducer {
    fn get_number(&self) -> u32;
}

// an implementation of the trait we're going to use
struct FortyTwoProducer;
impl NumberProducer for FortyTwoProducer {
    fn get_number(&self) -> u32 {
        42
    }
}

// another implementation of the trait we're going to use
struct TenProducer;
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
fn dst_with_trait_object() {
    // allocate an instance with one implementation of the trait
    let a = Node::build(33, FortyTwoProducer {});
    assert_eq!(42, a.producer.get_number());

    // allocate an instance with another implementation of the trait
    let b = Node::build(33, TenProducer {});
    assert_eq!(10, b.producer.get_number());
}

#[make_dst_factory]
#[repr(Rust, packed(1))]
struct PackedStruct {
    data: u32,
    tail: str,
}

#[test]
fn packed_struct() {
    let instance: Box<PackedStruct> = PackedStruct::build(0xDEAD_BEEF, "packed data");

    // SAFETY: We are reading a packed field that is guaranteed to be aligned correctly
    let data = unsafe { read_unaligned(&raw const instance.data) };

    assert_eq!(data, 0xDEAD_BEEF);
    assert_eq!(&instance.tail, "packed data");
}

#[make_dst_factory]
struct AlignedSliceStruct<T> {
    data: u32,
    tail: [T],
}

#[repr(align(32))]
#[derive(Debug, Eq, PartialEq)]
struct Align32 {
    payload: u32,
}

#[test]
fn aligned_slice_struct() {
    let instance: Box<AlignedSliceStruct<Align32>> = AlignedSliceStruct::build(42, [Align32 { payload: 0xDEAD_BEEF }]);

    assert_eq!(instance.data, 42);
    assert_eq!(instance.tail[0], Align32 { payload: 0xDEA_DBEEF });

    let (data, mut tail_iter) = AlignedSliceStruct::destructure(instance);
    assert_eq!(data, 42);
    assert_eq!(tail_iter.next(), Some(Align32 { payload: 0xDEA_DBEEF }));
}

#[make_dst_factory]
struct AlignedTraitStruct {
    data: u32,
    tail: dyn NumberProducer,
}

#[repr(align(32))]
struct HundredProducer {
    payload: u32,
}

impl NumberProducer for HundredProducer {
    fn get_number(&self) -> u32 {
        self.payload
    }
}

#[test]
fn aligned_trait_struct() {
    let instance: Box<AlignedTraitStruct> = AlignedTraitStruct::build(42, HundredProducer { payload: 100 });

    assert_eq!(instance.data, 42);
    assert_eq!(instance.tail.get_number(), 100);
}

#[make_dst_factory(destructurer = custom_destructure)]
struct CustomDestructureStruct<T> {
    id: usize,
    elements: [T],
}

#[test]
fn custom_destructure_usage() {
    for i in 0..64 {
        let v = vec!['*'; i];

        let instance: Box<CustomDestructureStruct<char>> = CustomDestructureStruct::build_from_slice(i, v.as_slice());

        let (id, elements_iter): (usize, CustomDestructureStructIter<char>) = CustomDestructureStruct::custom_destructure(instance);
        assert_eq!(id, i);
        assert!(elements_iter.eq(v));
    }
}

#[make_dst_factory(iterator = MyIter)]
struct CustomIteratorStruct<T> {
    id: usize,
    elements: [T],
}

#[test]
fn custom_iterator_usage() {
    for i in 0..64 {
        let v = vec!['*'; i];

        let instance: Box<CustomIteratorStruct<char>> = CustomIteratorStruct::build_from_slice(i, v.as_slice());

        let (id, elements_iter): (usize, MyIter<char>) = CustomIteratorStruct::destructure(instance);
        assert_eq!(id, i);
        assert!(elements_iter.eq(v));
    }
}

#[test]
#[cfg_attr(miri, ignore)]
fn no_std() {
    let t = trybuild::TestCases::new();
    t.pass("tests/ui/no_std.rs");
}

#[test]
#[cfg_attr(miri, ignore)]
fn error_paths() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/string_in_attribute.rs");
    t.compile_fail("tests/ui/struct_with_no_fields.rs");
    t.compile_fail("tests/ui/last_field_not_slice_or_str.rs");
    t.compile_fail("tests/ui/macro_not_on_struct.rs");
    t.compile_fail("tests/ui/dst_field_not_last.rs");
    t.compile_fail("tests/ui/too_many_tokens.rs");
    t.compile_fail("tests/ui/bad_visibility.rs");
    t.compile_fail("tests/ui/no_comma_after_visibility.rs");
    t.compile_fail("tests/ui/need_no_std.rs");
    t.compile_fail("tests/ui/bad_str.rs");
    t.compile_fail("tests/ui/higher_rank_trait.rs");
    t.compile_fail("tests/ui/unit_struct.rs");
    t.compile_fail("tests/ui/deserialize_trait_object.rs");
    t.compile_fail("tests/ui/no_comma_after_deserialize.rs");
    t.compile_fail("tests/ui/no_comma_after_no_std.rs");
    t.compile_fail("tests/ui/no_comma_after_factory_name.rs");
    t.compile_fail("tests/ui/no_comma_after_destructurer.rs");
    t.compile_fail("tests/ui/no_comma_after_iterator.rs");
    t.compile_fail("tests/ui/clone_trait_object.rs");
    t.compile_fail("tests/ui/no_comma_after_clone.rs");
}

// --- Clone tests ---

#[make_dst_factory(clone)]
struct CloneableStr {
    id: u32,
    text: str,
}

#[test]
fn clone_str_dst() {
    let original: Box<CloneableStr> = CloneableStr::build(42, "hello clone");
    let cloned = original.clone();

    assert_eq!(original.id, cloned.id);
    assert_eq!(&original.text, &cloned.text);
}

#[make_dst_factory(clone)]
struct CloneableSlice {
    tag: u16,
    values: [f32],
}

#[test]
fn clone_slice_dst() {
    let original: Box<CloneableSlice> = CloneableSlice::build_from_slice(7, &[1.0, 2.5, 3.7]);
    let cloned = original.clone();

    assert_eq!(original.tag, cloned.tag);
    assert_eq!(&original.values, &cloned.values);
}

#[make_dst_factory(clone)]
struct CloneableSliceGeneric<T> {
    id: usize,
    items: [T],
}

#[test]
fn clone_generic_slice_dst() {
    let original: Box<CloneableSliceGeneric<String>> = CloneableSliceGeneric::build(1, vec!["a".to_string(), "b".to_string()]);
    let cloned = original.clone();

    assert_eq!(original.id, cloned.id);
    assert_eq!(&original.items, &cloned.items);
}

#[make_dst_factory(clone)]
struct CloneableOnlyStr {
    content: str,
}

#[test]
fn clone_only_str_field() {
    let original: Box<CloneableOnlyStr> = CloneableOnlyStr::build("only");
    let cloned = original.clone();
    assert_eq!(&original.content, &cloned.content);
}

#[make_dst_factory(clone)]
struct CloneableOnlySlice {
    items: [u32],
}

#[test]
fn clone_only_slice_field() {
    let original: Box<CloneableOnlySlice> = CloneableOnlySlice::build_from_slice(&[10, 20, 30]);
    let cloned = original.clone();
    assert_eq!(&original.items, &cloned.items);
}

// --- ExactSizeIterator and Debug tests ---

#[test]
fn destructurer_exact_size_iterator() {
    let instance: Box<BasicSliceStruct<char>> = BasicSliceStruct::basic_slice_builder_from_slice(0, &['a', 'b', 'c']);
    let (_, iter): (usize, IterForBasicSlice<char>) = BasicSliceStruct::basic_slice_destructure(instance);

    assert_eq!(iter.len(), 3);
}

#[test]
fn destructurer_exact_size_iterator_decrements() {
    let instance: Box<BasicSliceStruct<char>> = BasicSliceStruct::basic_slice_builder_from_slice(0, &['a', 'b', 'c']);
    let (_, mut iter): (usize, IterForBasicSlice<char>) = BasicSliceStruct::basic_slice_destructure(instance);

    assert_eq!(iter.len(), 3);
    let _ = iter.next();
    assert_eq!(iter.len(), 2);
    let _ = iter.next();
    assert_eq!(iter.len(), 1);
    let _ = iter.next();
    assert_eq!(iter.len(), 0);
}

#[test]
fn destructurer_debug_impl() {
    let instance: Box<BasicSliceStruct<char>> = BasicSliceStruct::basic_slice_builder_from_slice(0, &['a', 'b']);
    let (_, iter): (usize, IterForBasicSlice<char>) = BasicSliceStruct::basic_slice_destructure(instance);

    let debug_str = format!("{iter:?}");
    assert!(debug_str.contains("IterForBasicSlice"));
    assert!(debug_str.contains("index"));
    assert!(debug_str.contains("len"));
}

// --- Iterator drop safety test ---
// NOTE: DropSliceStruct lives in test_drop_iter.rs to avoid
// trybuild interaction with non-Copy slice tails.

// --- Trait object with Drop impl (verifies mem::forget fix) ---

use core::sync::atomic::{AtomicUsize, Ordering};

static TRAIT_DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

trait Greeter {
    fn greet(&self) -> &'static str;
}

struct HelloGreeter {
    _data: String,
}

impl Greeter for HelloGreeter {
    fn greet(&self) -> &'static str {
        "hello"
    }
}

impl Drop for HelloGreeter {
    fn drop(&mut self) {
        let _ = TRAIT_DROP_COUNT.fetch_add(1, Ordering::Relaxed);
    }
}

#[make_dst_factory]
struct GreeterNode {
    id: u32,
    greeter: dyn Greeter,
}

#[test]
fn trait_object_with_drop_impl() {
    TRAIT_DROP_COUNT.store(0, Ordering::Relaxed);

    {
        let instance: Box<GreeterNode> = GreeterNode::build(
            1,
            HelloGreeter {
                _data: "heap data".to_string(),
            },
        );
        assert_eq!(instance.greeter.greet(), "hello");
        // instance drops here — should drop exactly once
    }

    assert_eq!(TRAIT_DROP_COUNT.load(Ordering::Relaxed), 1);
}

// --- Serde tests ---

mod serde_tests {
    use dst_factory::make_dst_factory;
    use serde::Serialize;

    #[derive(Serialize)]
    #[make_dst_factory(deserialize)]
    struct SerdeStr {
        id: u32,
        message: str,
    }

    #[derive(Serialize)]
    #[make_dst_factory(deserialize)]
    struct SerdeSlice {
        tag: u16,
        values: [f32],
    }

    #[derive(Serialize)]
    #[make_dst_factory(build_serde, deserialize)]
    struct SerdeTuple(u32, str);

    #[test]
    fn serde_str_round_trip() {
        let original: Box<SerdeStr> = SerdeStr::build(42, "hello world");
        let json = serde_json::to_string(&*original).unwrap();
        let deserialized: Box<SerdeStr> = serde_json::from_str(&json).unwrap();

        assert_eq!(original.id, deserialized.id);
        assert_eq!(&original.message, &deserialized.message);
    }

    #[test]
    fn serde_slice_round_trip() {
        let original: Box<SerdeSlice> = SerdeSlice::build_from_slice(7, &[1.0, 2.5, 3.7, 4.0]);
        let json = serde_json::to_string(&*original).unwrap();
        let deserialized: Box<SerdeSlice> = serde_json::from_str(&json).unwrap();

        assert_eq!(original.tag, deserialized.tag);
        assert_eq!(&original.values, &deserialized.values);
    }

    #[test]
    fn serde_empty_str() {
        let original: Box<SerdeStr> = SerdeStr::build(0, "");
        let json = serde_json::to_string(&*original).unwrap();
        let deserialized: Box<SerdeStr> = serde_json::from_str(&json).unwrap();

        assert_eq!(original.id, deserialized.id);
        assert_eq!(&original.message, &deserialized.message);
    }

    #[test]
    fn serde_empty_slice() {
        let original: Box<SerdeSlice> = SerdeSlice::build_from_slice(0, &[]);
        let json = serde_json::to_string(&*original).unwrap();
        let deserialized: Box<SerdeSlice> = serde_json::from_str(&json).unwrap();

        assert_eq!(original.tag, deserialized.tag);
        assert!(deserialized.values.is_empty());
    }

    #[test]
    fn serde_tuple_round_trip() {
        let original: Box<SerdeTuple> = SerdeTuple::build_serde(99, "abc");
        let json = serde_json::to_string(&*original).unwrap();
        let deserialized: Box<SerdeTuple> = serde_json::from_str(&json).unwrap();

        assert_eq!(original.0, deserialized.0);
        assert_eq!(&original.1, &deserialized.1);
    }

    #[test]
    fn serde_json_structure() {
        let instance: Box<SerdeStr> = SerdeStr::build(123, "test");
        let json = serde_json::to_string(&*instance).unwrap();
        let v: serde_json::Value = serde_json::from_str(&json).unwrap();

        assert_eq!(v["id"], 123);
        assert_eq!(v["message"], "test");
    }

    // --- Field attribute preservation tests ---

    #[derive(Serialize)]
    #[make_dst_factory(deserialize)]
    struct SerdeRenamedFields {
        #[serde(rename = "identifier")]
        id: u32,
        #[serde(rename = "msg")]
        message: str,
    }

    #[test]
    fn serde_rename_fields() {
        let original: Box<SerdeRenamedFields> = SerdeRenamedFields::build(10, "renamed");
        let json = serde_json::to_string(&*original).unwrap();

        // Verify serialization uses renamed keys
        let v: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(v["identifier"], 10);
        assert_eq!(v["msg"], "renamed");

        // Verify deserialization works with renamed keys
        let deserialized: Box<SerdeRenamedFields> = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized.id, 10);
        assert_eq!(&deserialized.message, "renamed");
    }

    #[test]
    fn serde_rename_from_raw_json() {
        let json = r#"{"identifier": 55, "msg": "from_raw"}"#;
        let deserialized: Box<SerdeRenamedFields> = serde_json::from_str(json).unwrap();
        assert_eq!(deserialized.id, 55);
        assert_eq!(&deserialized.message, "from_raw");
    }

    #[derive(Serialize)]
    #[make_dst_factory(deserialize)]
    struct SerdeDefaultField {
        #[serde(default)]
        priority: u32,
        content: str,
    }

    #[test]
    fn serde_default_field() {
        // Deserialize JSON missing the `priority` field — should use default (0)
        let json = r#"{"content": "no priority"}"#;
        let deserialized: Box<SerdeDefaultField> = serde_json::from_str(json).unwrap();
        assert_eq!(deserialized.priority, 0);
        assert_eq!(&deserialized.content, "no priority");
    }

    #[test]
    fn serde_default_field_with_value() {
        let json = r#"{"priority": 5, "content": "has priority"}"#;
        let deserialized: Box<SerdeDefaultField> = serde_json::from_str(json).unwrap();
        assert_eq!(deserialized.priority, 5);
        assert_eq!(&deserialized.content, "has priority");
    }

    #[derive(Serialize)]
    #[serde(rename_all = "camelCase")]
    #[make_dst_factory(deserialize)]
    struct SerdeRenameAll {
        user_name: String,
        display_text: str,
    }

    #[test]
    fn serde_rename_all() {
        let original: Box<SerdeRenameAll> = SerdeRenameAll::build("Alice".to_string(), "Hello");
        let json = serde_json::to_string(&*original).unwrap();

        let v: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(v["userName"], "Alice");
        assert_eq!(v["displayText"], "Hello");

        let deserialized: Box<SerdeRenameAll> = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized.user_name, "Alice");
        assert_eq!(&deserialized.display_text, "Hello");
    }

    // --- Only-DST-field tests ---

    #[derive(Serialize)]
    #[make_dst_factory(deserialize)]
    struct SerdeOnlyStr {
        content: str,
    }

    #[test]
    fn serde_only_str_field() {
        let original: Box<SerdeOnlyStr> = SerdeOnlyStr::build("only field");
        let json = serde_json::to_string(&*original).unwrap();
        let deserialized: Box<SerdeOnlyStr> = serde_json::from_str(&json).unwrap();
        assert_eq!(&original.content, &deserialized.content);
    }

    #[test]
    fn serde_only_str_field_empty() {
        let original: Box<SerdeOnlyStr> = SerdeOnlyStr::build("");
        let json = serde_json::to_string(&*original).unwrap();
        let deserialized: Box<SerdeOnlyStr> = serde_json::from_str(&json).unwrap();
        assert_eq!(&deserialized.content, "");
    }

    #[derive(Serialize)]
    #[make_dst_factory(deserialize)]
    struct SerdeOnlySlice {
        items: [u16],
    }

    #[test]
    fn serde_only_slice_field() {
        let original: Box<SerdeOnlySlice> = SerdeOnlySlice::build_from_slice(&[100, 200, 300]);
        let json = serde_json::to_string(&*original).unwrap();
        let deserialized: Box<SerdeOnlySlice> = serde_json::from_str(&json).unwrap();
        assert_eq!(&original.items, &deserialized.items);
    }

    #[test]
    fn serde_only_slice_field_empty() {
        let original: Box<SerdeOnlySlice> = SerdeOnlySlice::build_from_slice(&[]);
        let json = serde_json::to_string(&*original).unwrap();
        let deserialized: Box<SerdeOnlySlice> = serde_json::from_str(&json).unwrap();
        assert!(deserialized.items.is_empty());
    }

    #[derive(Serialize)]
    #[make_dst_factory(build_only_tuple, deserialize)]
    struct SerdeOnlyTupleStr(str);

    #[test]
    fn serde_only_tuple_str_field() {
        let original: Box<SerdeOnlyTupleStr> = SerdeOnlyTupleStr::build_only_tuple("tuple only");
        let json = serde_json::to_string(&*original).unwrap();
        let deserialized: Box<SerdeOnlyTupleStr> = serde_json::from_str(&json).unwrap();
        assert_eq!(&original.0, &deserialized.0);
    }

    #[derive(Serialize)]
    #[make_dst_factory(build_only_tuple_slice, deserialize)]
    struct SerdeOnlyTupleSlice([i32]);

    #[test]
    fn serde_only_tuple_slice_field() {
        let original: Box<SerdeOnlyTupleSlice> = SerdeOnlyTupleSlice::build_only_tuple_slice_from_slice(&[1, -2, 3]);
        let json = serde_json::to_string(&*original).unwrap();
        let deserialized: Box<SerdeOnlyTupleSlice> = serde_json::from_str(&json).unwrap();
        assert_eq!(&original.0, &deserialized.0);
    }

    // --- Slice with renamed fields ---

    #[derive(Serialize)]
    #[make_dst_factory(deserialize)]
    struct SerdeSliceRenamed {
        #[serde(rename = "count")]
        tag: u16,
        #[serde(rename = "data")]
        values: [f32],
    }

    #[test]
    fn serde_slice_renamed() {
        let json = r#"{"count": 3, "data": [1.0, 2.0]}"#;
        let deserialized: Box<SerdeSliceRenamed> = serde_json::from_str(json).unwrap();
        assert_eq!(deserialized.tag, 3);
        assert_eq!(&deserialized.values, &[1.0, 2.0]);
    }

    // --- Tuple slice with serde ---

    #[derive(Serialize)]
    #[make_dst_factory(build_tuple_slice, deserialize)]
    struct SerdeTupleSlice(u32, [f64]);

    #[test]
    fn serde_tuple_slice_round_trip() {
        let original: Box<SerdeTupleSlice> = SerdeTupleSlice::build_tuple_slice_from_slice(77, &[1.1, 2.2, 3.3]);
        let json = serde_json::to_string(&*original).unwrap();
        let deserialized: Box<SerdeTupleSlice> = serde_json::from_str(&json).unwrap();
        assert_eq!(original.0, deserialized.0);
        assert_eq!(&original.1, &deserialized.1);
    }

    // --- Deserialize with custom factory name ---

    #[derive(Serialize)]
    #[make_dst_factory(create, deserialize)]
    struct SerdeCustomFactory {
        id: u32,
        name: str,
    }

    #[test]
    fn serde_custom_factory_name() {
        let original: Box<SerdeCustomFactory> = SerdeCustomFactory::create(1, "custom");
        let json = serde_json::to_string(&*original).unwrap();
        let deserialized: Box<SerdeCustomFactory> = serde_json::from_str(&json).unwrap();
        assert_eq!(original.id, deserialized.id);
        assert_eq!(&original.name, &deserialized.name);
    }

    // --- Deserialize with custom factory name for slice ---

    #[derive(Serialize)]
    #[make_dst_factory(make, deserialize)]
    struct SerdeCustomSliceFactory {
        tag: u8,
        data: [u32],
    }

    #[test]
    fn serde_custom_slice_factory_name() {
        let original: Box<SerdeCustomSliceFactory> = SerdeCustomSliceFactory::make_from_slice(5, &[10, 20, 30]);
        let json = serde_json::to_string(&*original).unwrap();
        let deserialized: Box<SerdeCustomSliceFactory> = serde_json::from_str(&json).unwrap();
        assert_eq!(original.tag, deserialized.tag);
        assert_eq!(&original.data, &deserialized.data);
    }
}

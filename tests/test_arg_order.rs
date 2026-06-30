//! Verifies that macro arguments after the optional (positional) factory name
//! may appear in any order. The previous parser only accepted flags in a fixed
//! sequence, so combinations like `pub, arena, clone, debug` failed to parse.

use dst_factory::make_dst_factory;

// `clone`/`debug` after `arena` - rejected by the old fixed-order parser.
#[make_dst_factory(pub, arena, clone, debug)]
struct Mixed {
    id: u32,
    text: str,
}

// A different permutation with a custom factory name first and `pub` last.
#[make_dst_factory(make, debug, clone, generic = T, pub)]
struct Reordered {
    tag: u16,
    values: [f32],
}

#[test]
fn flags_after_arena_in_any_order() {
    let original: Box<Mixed> = Mixed::build(1, "alice");
    let cloned = original.clone();

    assert_eq!(original.id, cloned.id);
    assert_eq!(&original.text, &cloned.text);
    // `debug` derived a `Debug` impl.
    assert_eq!(format!("{original:?}"), format!("{cloned:?}"));
}

#[test]
fn custom_factory_name_with_reordered_flags() {
    // `make` is the custom factory-name prefix; `debug`/`clone` come in a
    // non-canonical order and `pub` trails the list.
    let original: Box<Reordered> = Reordered::make_from_slice(7, &[1.0, 2.5]);
    let cloned = original.clone();

    assert_eq!(original.tag, cloned.tag);
    assert_eq!(&original.values, &cloned.values);
    assert_eq!(format!("{original:?}"), format!("{cloned:?}"));
}

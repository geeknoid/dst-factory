
#![allow(dead_code)]

extern crate alloc;

use dst_factory::make_dst_factory;
use serde::Serialize;

#[derive(Serialize)]
#[make_dst_factory(no_std, deserialize)]
struct NoStdSerdeStr {
    id: i32,
    data: str,
}

#[derive(Serialize)]
#[make_dst_factory(no_std, deserialize)]
struct NoStdSerdeSlice {
    id: i32,
    data: [u32],
}

fn main() {
    // Verify str round-trip compiles
    let msg = NoStdSerdeStr::build(1, "hello");
    let json = serde_json::to_string(&*msg).unwrap();
    let restored: alloc::boxed::Box<NoStdSerdeStr> = serde_json::from_str(&json).unwrap();
    assert_eq!(restored.id, 1);
    assert_eq!(&restored.data, "hello");

    // Verify slice round-trip compiles
    let reading = NoStdSerdeSlice::build_from_slice(2, &[10, 20, 30]);
    let json = serde_json::to_string(&*reading).unwrap();
    let restored: alloc::boxed::Box<NoStdSerdeSlice> = serde_json::from_str(&json).unwrap();
    assert_eq!(restored.id, 2);
    assert_eq!(&restored.data, &[10, 20, 30]);
}

use dst_factory::make_dst_factory;
use serde::Serialize;

#[derive(Serialize)]
#[make_dst_factory(build_serde_sl, deserialize)]
struct SerdeSliceTuple(u16, [u8]);

#[test]
fn serde_slice_tuple_round_trip() {
    let original: Box<SerdeSliceTuple> = SerdeSliceTuple::build_serde_sl_from_slice(99, &[1, 2, 3]);
    let json = serde_json::to_string(&*original).unwrap();
    let deserialized: Box<SerdeSliceTuple> = serde_json::from_str(&json).unwrap();

    assert_eq!(original.0, deserialized.0);
    assert_eq!(&original.1, &deserialized.1);
}

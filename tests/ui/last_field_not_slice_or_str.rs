use dst_factory::make_dst_factory;

#[make_dst_factory]
struct ValidStructForAttrTest {
    id: i32,
    data: i32,
}

fn main() {
}

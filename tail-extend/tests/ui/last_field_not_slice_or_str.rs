use tail_extend::make_dst_builder;

#[make_dst_builder]
struct ValidStructForAttrTest {
    id: i32,
    data: i32,
}

fn main() {
}

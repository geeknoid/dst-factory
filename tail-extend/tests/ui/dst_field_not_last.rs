use tail_extend::make_dst_builder;

#[make_dst_builder]
struct DstFieldNotLast {
    id: str,
    data: i32,
}

fn main() {
}

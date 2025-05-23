use tail_extend::make_dst_builder;

#[make_dst_builder]
struct MultipleDstFields {
    id: str,
    data: str,
}

fn main() {
}

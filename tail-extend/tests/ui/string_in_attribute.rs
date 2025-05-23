use tail_extend::make_dst_builder;

#[make_dst_builder("A String Literal")]
struct StringInAttribute {
    id: i32,
    data: str,
}

fn main() {
}

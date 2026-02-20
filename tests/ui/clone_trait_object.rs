use dst_factory::make_dst_factory;

trait MyTrait {
    fn do_thing(&self) -> u32;
}

#[make_dst_factory(clone)]
struct CloneTraitObject {
    id: i32,
    data: dyn MyTrait,
}

fn main() {
}

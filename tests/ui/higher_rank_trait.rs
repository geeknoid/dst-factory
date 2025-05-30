use dst_factory::make_dst_factory;

trait MyTrait<'a> {}

#[make_dst_factory]
struct HigherRankTrait<'b> {
    id: i32,
    data: dyn for<'a> MyTrait<'b>,
}

fn main() {
}

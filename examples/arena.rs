//! Example showing DST arena allocation with `multitude` and `dst-factory`.

use dst_factory::make_dst_factory;
use multitude::{Arc, Arena, Box as MBox, Rc as MRc};

// ── str tail ────────────────────────────────────────────────────────

#[make_dst_factory(arena)]
struct User {
    age: u8,
    name: str,
}

// ── slice tail ──────────────────────────────────────────────────────

#[make_dst_factory(arena)]
struct Measurement {
    sensor_id: u32,
    readings: [f64],
}

// ── trait object tail ───────────────────────────────────────────────
//
// For an arena `dyn Trait` tail, the trait must be annotated with multitude's
// `#[dst::pointee]` attribute (so `dyn Trait: multitude::dst::Pointee`), and —
// for the `build_arena_arc` variant — be `Send + Sync`.

#[multitude::dst::pointee(crate = ::multitude::dst)]
trait Renderer: Send + Sync {
    fn render(&self) -> &'static str;
}

struct HtmlRenderer;
impl Renderer for HtmlRenderer {
    fn render(&self) -> &'static str {
        "<h1>Hello</h1>"
    }
}

struct TextRenderer;
impl Renderer for TextRenderer {
    fn render(&self) -> &'static str {
        "Hello"
    }
}

#[make_dst_factory(arena)]
struct Widget {
    id: u32,
    renderer: dyn Renderer,
}

fn main() {
    let arena = Arena::new();

    // ── Str tail DSTs ───────────────────────────────────────────────
    println!("=== str tail ===");
    let alice: Arc<User> = User::build_arena_arc(&arena, 33, "Alice");
    let bob: Arc<User> = User::build_arena_arc(&arena, 25, "Bob");
    println!("{}: age {}", &alice.name, alice.age);
    println!("{}: age {}", &bob.name, bob.age);

    // ── Slice tail DSTs ─────────────────────────────────────────────
    println!("\n=== slice tail ===");
    let m1: Arc<Measurement> = Measurement::build_arena_arc(&arena, 1, vec![23.5, 24.1, 22.8]);
    let m2: Arc<Measurement> = Measurement::build_arena_arc_from_slice(&arena, 2, &[100.0, 200.0]);
    println!("Sensor {}: {:?}", m1.sensor_id, &m1.readings);
    println!("Sensor {}: {:?}", m2.sensor_id, &m2.readings);

    // ── Trait object tail DSTs ──────────────────────────────────────
    println!("\n=== trait object tail ===");
    let w1: Arc<Widget> = Widget::build_arena_arc(&arena, 1, HtmlRenderer);
    let w2: Arc<Widget> = Widget::build_arena_arc(&arena, 2, TextRenderer);
    println!("Widget {}: {}", w1.id, w1.renderer.render());
    println!("Widget {}: {}", w2.id, w2.renderer.render());

    // ── multitude Box / Rc arena variants ───────────────────────────
    println!("\n=== arena Box / Rc ===");
    let boxed: MBox<User> = User::build_arena_box(&arena, 50, "Boxed");
    let rced: MRc<User> = User::build_arena_rc(&arena, 60, "Rced");
    println!("{}: age {}", &boxed.name, boxed.age);
    println!("{}: age {}", &rced.name, rced.age);

    // ── Mix sized and DST in same arena ─────────────────────────────
    println!("\n=== mixed types ===");
    let config: Arc<u32> = arena.alloc_arc(42_u32);
    println!("Config: {config}");

    // ── Arc outlives the arena ──────────────────────────────────────
    println!("\n=== Arc outlives arena ===");
    let survivor = {
        let temp = Arena::new();
        User::build_arena_arc(&temp, 99, "I outlive my arena!")
    };
    println!("{}: age {}", &survivor.name, survivor.age);

    // ── Send across threads ─────────────────────────────────────────
    println!("\n=== threading ===");
    let user = User::build_arena_arc(&arena, 42, "ThreadUser");
    let handle = std::thread::spawn(move || {
        println!("From thread: {} (age {})", &user.name, user.age);
    });
    handle.join().expect("thread panicked");
}

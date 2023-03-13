use std::time::Instant;

fn main() {
	let outer_count = 80_000;
	let layer_count = 14;

	struct ThingA {
		thing: Option<Box<ThingA>>,
	}

	fn inner_a(depth: u64) -> ThingA {
		let mut thing = ThingA { thing: None };
		if depth > 0 {
			thing.thing = Some(Box::new(inner_a(depth - 1)));
		}

		thing
	}

	fn process_a(thing: &ThingA) {
		std::hint::black_box(thing);
		if let Some(child) = &thing.thing {
			process_a(child);
		}
	}

	let build_start = Instant::now();
	let mut outer = Vec::new();
	for _ in 0..outer_count {
		outer.push(inner_a(layer_count));
	}
	outer = std::hint::black_box(outer);
	let build_end = Instant::now();
	for thing in &outer {
		process_a(thing);
	}
	let process_end = Instant::now();

	println!("A built in {} ms", build_end.duration_since(build_start).as_millis());
	println!("A processed in {} ms", process_end.duration_since(build_end).as_millis());

	println!();

	struct ThingB {
		thing_index: usize,
	}

	fn inner_b(backing: &mut Vec<ThingB>, depth: u64) -> ThingB {
		let mut thing = ThingB { thing_index: 0 };
		if depth > 0 {
			let child = inner_b(backing, depth - 1);
			let index = backing.len();
			backing.push(child);
			thing.thing_index = index;
		}

		thing
	}

	fn process_b(backing: &[ThingB], thing: &ThingB) {
		std::hint::black_box(thing);
		if thing.thing_index > 0 {
			process_b(backing, &backing[thing.thing_index]);
		}
	}

	let build_start = Instant::now();
	let mut outer = Vec::new();
	let mut backing = vec![ThingB { thing_index: 0 }];
	for _ in 0..outer_count {
		outer.push(inner_b(&mut backing, layer_count));
	}
	outer = std::hint::black_box(outer);
	let build_end = Instant::now();
	for thing in &outer {
		process_b(&backing, thing);
	}
	let process_end = Instant::now();

	println!("B built in {} ms", build_end.duration_since(build_start).as_millis());
	println!("B processed in {} ms", process_end.duration_since(build_end).as_millis());
}

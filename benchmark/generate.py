#!/usr/bin/env python3

import os
import glob

def generate_normal_types(index):
		return f"""
struct Struct{index} {{
	first: i32
	second: f64 // This is a field
	third: u8
	fourth: u16
	fifth: i64 // This is too
	sixth: bool
	// Abcd
}}

/*
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore
et dolore magna aliqua. Posuere ac ut consequat semper. Integer enim neque volutpat ac tincidunt
vitae semper quis lectus. Consequat interdum varius sit amet mattis vulputate enim. Habitant morbi
tristique senectus et netus et. Volutpat ac tincidunt vitae semper quis lectus. Rhoncus mattis rhoncus
urna neque viverra justo nec ultrices. Tristique senectus et netus et malesuada fames ac. Dui nunc
mattis enim ut tellus elementum sagittis. Praesent elementum facilisis leo vel fringilla est ullamcorper
eget. Ultricies mi eget mauris pharetra et. Est ultricies integer quis auctor elit sed vulputate. Nunc
scelerisque viverra mauris in aliquam. Leo urna molestie at elementum. Arcu risus quis varius quam
quisque. In fermentum posuere urna nec tincidunt praesent semper feugiat nibh. Ut pharetra sit
amet aliquam id diam maecenas ultricies. Nulla at volutpat diam ut venenatis tellus in. Mauris cursus
mattis molestie a iaculis at erat. Ultricies lacus sed turpis tincidunt id aliquet risus feugiat.
*/

enum Enum {{
	first_shared: f64
	second_shared: i16

	A
	B
	C {{
		first_c: i32
		/* comment in here */ second_c: u8
	}}
	D
	E {{ first_e: file_{index + 1}::Struct{index + 1} }}
}}

enum TransparentEnum {{
	A(i32)
	B(f64) // That's a float
	C(u8)
	D
}}

generic T, E
enum Result {{
	Okay(T)
	Error(E) // Oh no an error
}}

import file_{index + 1}::Entity{index + 1}

generic T
struct EntityStore {{
	previous_time: f64
	elapsed: f64
	entities: List<Entity{index + 1}<T>>
}}

method static EntityStore
fn new(): EntityStore<T> {{
	return EntityStore<T> {{
		previous_time: 0
		elapsed: 0
		entities: List<Entity{index + 1}<T>>.new()
	}}
}}

method mut EntityStore
fn tick(current_time: f64) {{
	// Increment our time state
	let delta = current_time - self.previous_time
	self.previous_time = current_time
	self.elapsed += delta

	mut index: isize = 0
	while index < self.entities.length {{
		self.entities.get_mut(index).tick(delta)
		index += 1
	}}
}}

generic T
enum EnumEntityStore {{
	shared_entities: List<Entity{index + 1}<T>>

	Variant {{ variant_entities: List<Entity{index + 1}<T>> }}
}}

method static EnumEntityStore
fn new(): EnumEntityStore<T> {{
	return .Variant {{
		shared_entities: List<Entity{index + 1}<T>>.new()
		variant_entities: List<Entity{index + 1}<T>>.new()
	}}
}}

method static EnumEntityStore
fn new_variant(): EnumEntityStore<T>.Variant {{
	return EnumEntityStore<T>.Variant {{
		shared_entities: List<Entity{index + 1}<T>>.new()
		variant_entities: List<Entity{index + 1}<T>>.new()
	}}
}}

generic T
struct Entity{index} {{
	health: f64
	payload: T
}}

method static Entity{index}
fn new(payload: T): Entity{index}<T> {{
	return Entity{index}<T> {{ health: 100, payload }}
}}

method mut Entity{index}
fn tick(delta: f64) {{
	const HealthLostPerSecond = 0.25
	self.health -= delta * HealthLostPerSecond
}}
"""

def generate_normal_logic(index):
	return f"""
generic T
fn function(arg: &List<Entity{index + 1}<T>>) {{}}

fn logic_{index}(addend: i32): i32 {{
	mut the_struct: Struct{index} = Struct{index} {{
		first: addend
		second: 4.2 + 5 / 6 - 9.9
		third: b'D'
		fourth: 999
		fifth: -10000000000
		sixth: false.!.!.!.!.!.!
	}}

	// Bunch of meaningless operations
	the_struct.first -= 1
	the_struct.second /= 3
	the_struct.third += 1
	the_struct.fourth %= 5
	the_struct.fifth -= 99
	the_struct.sixth &= true

	let a: i32 = the_struct.first
	let b = -the_struct.second
	let c: u8 = the_struct.third - b'A'
	let d = the_struct.fourth
	let e: i64 = the_struct.fifth
	let f = the_struct.sixth.!

	print_i32(a)
	print_newline()

	print_f64(b)
	print_newline()

	print_u8(c)
	print_newline()

	print_u16(d)
	print_newline()

	print_i64(e)
	print_newline()

	print_bool(f)
	print_newline()

	match do_failable_thing<str>("Hello world\\n", 1) {{
		result: Okay => print_string(result)
		else => print_string("Error 1\\n")
	}}

	if do_failable_thing<u32>('E', 12) is error: Error {{
		print_i32(error.index)
		print_newline()
	}} else => print_string("Error 2\\n")

	print_newline()

	mut store = EntityStore<i16>.new()
	store.entities.push(Entity{index + 1}<i16> {{ health: 100, payload: -500 }})
	function<i16>(store.entities.&)
	print_i16(store.entities.get(0).payload)
	print_newline()

	print_newline()

	mut enum_store: EnumEntityStore<f64> = EnumEntityStore<f64>.new_variant()
	enum_store.shared_entities.push(Entity{index + 1}<f64> {{ health: 100, payload: 42.9 }})
	function<f64>(enum_store.shared_entities.&)
	print_f64(enum_store.shared_entities.get(0).payload)
	print_newline()

	if enum_store is Variant {{
		enum_store.shared_entities.push(Entity{index + 1}<f64> {{ health: 100, payload: 43.8 }})
		function<f64>(enum_store.shared_entities.&)
		print_f64(enum_store.shared_entities.get(1).payload)
		print_newline()

		enum_store.variant_entities.push(Entity{index + 1}<f64> {{ health: 100, payload: 44.7 }})
		function<f64>(enum_store.variant_entities.&)
		print_f64(enum_store.variant_entities.get(0).payload)
		print_newline()
	}}

	print_newline()

	mut variant_store: EnumEntityStore<i32>.Variant = EnumEntityStore<i32>.new_variant()
	variant_store.shared_entities.push(Entity{index + 1}<i32> {{ health: 100, payload: -600 }})
	variant_store.variant_entities.push(Entity{index + 1}<i32> {{ health: 100, payload: -700 }})
	function<i32>(variant_store.shared_entities.&)
	function<i32>(variant_store.variant_entities.&)
	print_i32(variant_store.shared_entities.get(0).payload)
	print_newline()
	print_i32(variant_store.variant_entities.get(0).payload)
	print_newline()

	return file_{index + 1}::logic_{index + 1}(addend) + addend
}}

struct ErrorStruct {{
	index: i32
}}

generic T
fn do_failable_thing(arg: T, index: i32): Result<T, ErrorStruct> {{
	if index > 10 => return .Error(ErrorStruct {{ index }})
	else => return .Okay(arg)
}}
"""

def generate_normal_file(index):
	return f"""
import fae::stdout::print_i16, print_i32, print_f64, print_u8, print_u16, print_i64, print_bool, print_newline, print_string
import fae::collections::list::List

{generate_normal_types(index)}

{generate_normal_logic(index)}
"""

def generate_last_file(index):
		return f"""
// This is the last file, it is shorter as it doesn't have any logic
// It has to be here so that the second to last file has something to reference
// As each normal file references some stuff from the following file

import fae::collections::list::List

struct Struct{index} {{
	first: i32
	second: f64 // This is a field
	third: u8
	fourth: u16
	fifth: i64 // This is too
	sixth: bool
	// Abcd
}}

generic T
struct Entity{index} {{
	health: f64
	payload: T
}}

method static Entity{index}
fn new(payload: T): Entity{index}<T> {{
	return Entity{index}<T> {{ health: 100, payload }}
}}

method mut Entity{index}
fn tick(delta: f64) {{
	const HealthLostPerSecond = 0.25
	self.health -= delta * HealthLostPerSecond
}}

fn logic_{index}(addend: i32): i32 {{
	return addend
}}
"""

for file in glob.glob("./benchmark/generated_program/file_*.fae"):
  os.remove(file)

print("Existing files removed, ctrl-c here to exit without generating again")
print("This is useful if all you want to do is clean up the folder for git")
input("Press enter to generate: ")

last_index = 10_000
for index in range(0, last_index):
	with open(f"./benchmark/generated_program/file_{index}.fae", "w") as file:
		file.write(generate_normal_file(index))

with open(f"./benchmark/generated_program/file_{last_index}.fae", "w") as file:
	file.write(generate_last_file(last_index))


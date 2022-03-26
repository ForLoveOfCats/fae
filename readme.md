## Goals:
 * Capable
 * Consistent
 * Comfortable


## 1,000 foot view:
 * General Rust-like syntax
 * Expression based with explicit "evaluate to" keyword `give` and full return keyword `return`
 * Pattern matching
 * Absolutely no implicit type casting/conversion
 * No macros
 * Module system independent of file system
 * Public by default, private with keyword
 * Variable type inference from the right hand side (maybe Hindley-Milner?) and explicit type syntax
 * Variable shadowing/redeclaration (useful for nested scopes when using pattern matching, worth it?)
 * Constants with propagation and type inference from the right hand side
 * Some sort of `constexpr`/`const fn` mechanism
 * Copy by value
 * Stack arrays
 * Slices
 * Pointers
 * Primitive and pointer casting
 * Structs
 * Enums with algebraic capabilities, algebraic variant may either have a single value or brace enclosed fields
 * Direct value construction with fields required to be in-order
 * No constructors or destructors
 * Defer
 * Error types like enums with subclassing
 * Try `?` operator to unwrap errors
 * Methods on types declarable everywhere (can only access private fields when in the same module)
 * Traits (static and dynamic dispatch with trait objects)
 * Generics with trait bounds on Structs/Enums/Functions/Methods
 * Generic parameters to functions inferred from arguments (maybe Hindley-Milner?)
 * Generic varargs somehow for things like println (????)
 * Immutable by default, mutable with keyword (applies to pointer and slices as well)
 * Out of order symbol definition
 * Functions/Types/Methods declarable in any scope
 * Keyword `using` to bring module contents or enum variants into scope
 * Builtin prelude to `using` things by default
 * Standard library "core"
 * C FFI with `extern` keyword
 * Operator overloading TBD (????)


## Primitive types:
 * `void`
 * `bool` (maybe `b8` instead?)
 * `usize`
 * `isize` (????)
 * `u8`
 * `u16`
 * `u32`
 * `u64`
 * `i8`
 * `i16`
 * `i32`
 * `i64`
 * `f32`
 * `f64`


## Variables:
 * `let name = expression;` type inferred from expression
 * `let name: Type = expression;` type specified explicitly
```rs
let a = 42; // i32
let b: u32 = 42; // u32
```


## Functions:
 * `fn name(parameters): Type { block; }`
```rs
fn multiply(a: i32, b: i32): i32 {
	return a * b;
}
```


## Casting:
 * `.(T)` cast method
```rs
let a = 42; // i32
let b = a.(u32); // u32
```


## Pointers:
 * `&T` non-nullable pointer to immutable value
 * `&mut T` non-nullable pointer to mutable value
 * `.*` deref method
```rs
let a = 42; // i32
let mut b = &mut a; // &mut i32
println("{}", b.*); // "42"

b.* = 50;
println("{}", a); // "50"

b = 0.(&i32); // null
```

```rs
extern fn malloc(usize): &mut u8;
extern fn free(&mut u8): void;

let pointer = malloc(4).(&u32);
defer free(pointer);

pointer.* = 42;
println("{}", pointer.*); // "42"
```


## Arrays:
 * `[T; size]` type syntax
 * `[value; size]` literal syntax
```rs
let array = [0_u8, 5]; // [u8; 5]
let len = array.len(); // usize
```


## Slices:
 * `&[T]` type syntax
 * `&mut [T]` type syntax
 * `&[values]` literal syntax
 * `[..]` slice entire slice/array
 * `[n..]` slice from `n` to end of slice/array
 * `[..n]` slice to `n` of slice/array
 * `[n..m]` slice from index `n` to index `m` of slice/array
```rs
let array = [0_u8, 5];
let slice = array[..]; // &[u8]
let len = slice.len(); // usize

let list = List::from_slice(&[0, 1, 2]); // List<i32>
let slice = list.as_slice(); // &[i32]
let slice = list.as_slice_mut(); // &mut [i32]
```


## Structs:
```rs
struct MyStruct {
	field: i32,
}

let instance = MyStruct { field: 42 };
let integer = instance.field;
```

## Enums:
```rs
enum MyEnum {
	A,
	B(i32),
	C { field: i32 },
}

let a = MyEnum::A;
let b = MyEnum::B(42);
let a = MyEnum::C { field: 42 };

let tag = a.tag(); // u8
```

## Errors:
```rs
error MyError extends core::fs::Error {
	FailStateA,
	FailStateB,
}

fn main(): void {
	let string = match function() {
		Ok(string) => { give string; }
		MyError::FailStateA => { return; }
		MyError::FailStateB => { return; }
		core::fs::Error::FileNotFound => { return; }
		_ => { return; }
	}
	defer string.drop();

	println("{}", string);
}

fn function(): String !MyError {
	let path = core::fs::PathSlice::from_str("path");
	let file = core::fs::open(path)?;
	defer file.drop();
	return file.read_to_string()?;
}
```

## Methods:
```rs
struct MyStruct {
	field: i32,
}

fn MyStruct::field(&self): i32 {
	return self.field;
}

let instance = MyStruct { field: 42 };
let value = instance.field();
```


## Code Examples:
```rs
fn InternTracker::intern(&mut self, input: &[u8]): Id {
	let guard = self.lock.lock();
	defer guard.unlock();

	match self.interned_ids.get(input) {
		Some(id) => {
			return id;
		}

		None => {
			let id = Id::next();
			self.interned_ids.set(input, id);
			return id;
		}
	}
}
```

```rs
module main;

fn main(): void {
	print_slice_sorted(&[48, 46, 58]);
}

generic<T: Display>
fn print_slice_sorted(slice: &[T]): void {
	let mut list = List::from_slice(slice);
	defer list.drop();

	list.sort();
	for item in list.iter() {
		println("{}", item);
	}
}
```

```rs
generic<T>
struct HasGeneric {
	private list: List<T>,
}

fn HasGeneric::push(&mut self, value: T): usize {
	return self.list.push(value);
}
```

```rs
generic<T>
enum CustomOptional {
	CustomNone,
	CustomSome(T),
}

using CustomOptional::*;

let instance = CustomSome(42); // CustomOptional<i32>
```

```rs
module main;

using core::atomic::AtomicU32;

const MY_STRING = "Hello there!"; // &[u8]
let MY_COUNTER = AtomicU32::new(0); // AtomicU32

fn main(): void {
	print("{}", MY_STRING);
	MY_COUNTER.fetch_add(1);
}
```

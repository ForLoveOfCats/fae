## Standard Library Changes
- Moved `fae.string.format` module to `fae.format`
- Moved `fae.string.slice_to_str` function to new `str` static method `from_slice`
- Moved `fae.string.parse_to_integer` function to new `str` method `parse_i64`
- Moved `fae.string.parse_bytes_i64` function to `fae.primative.str.parse_bytes_i64`
- Moved `fae.string` module to `fae.collections.string`

- Renamed `fae.memory.allocate_string_copied` to `allocate_str_copied`
- Ensured that memory returned from `fae.memory` allocation helpers is zeroed
- Changed the argument order of `fae.format.print_to_file_stream`
- Changed all `free` functions to not be marked `mut`

- Added the following functions to the prelude (`fae.prelude`)
  - `null_str(): str`
  - `null_str_mut(): strmut`

- Added the following methods to `str`
  - `static create(pointer=: *u8, length: isize): str`
  - `static from_slice(slice=: []u8): str`
  - `static allocate_copied(input=: str): str`
  - `clone(): str`
  - `clone_mut(): strmut`
  - `equals(other=: str): bool`
  - `parse_i64(): Option<i64>`
  - `lines_iterator(): LinesIterator`
- Added the type `fae.primative.str.LinesIterator` with the following methods
  - `mut next(): Option<str>`
- Added the following methods to the new type `strmut`
  - `static create(pointer=: *mut u8, length: isize): strmut`
  - `static from_slice(slice=: []mut u8): strmut`
  - `static allocate_copied(input=: str): strmut`
  - `lines_iterator(): LinesIteratorMut`
- Added the type `fae.primative.str.LinesIterator` with the following methods
  - `mut next(): Option<strmut>`
- Added the following methods to `fae.collections.string.String`
  - `static new(): String`
  - `static with_capacity(capacity=: isize): String`
  - 
- Added the following methods to `u8`
  - `is_ascii_whitespace(): bool`
- Added the following methods to `fae.io.file.DelemitedReader`
  - `mut next_str(): Option<str>`

- Added the following functions to `fae.format`
  - `print_to_string(message: fstr, buffer: *mut String)`
- Added the following functions to new `fae.primative.str`
  - `parse_bytes_i64(bytes=: []u8): Option<i64>`
- Added the following functions to `fae.intrinsics`
  - `create_str_mut(pointer=: *mut u8, length: isize): strmut`

- Added the following methods to `fae.collections.string.String`
  - `static format(fstring=: fstr): String`
  - `clone(): String`
  - `mut clear()`
  - `mut truncate(to: isize)`
  - `mut set_length(to_length=: isize)`
  - `length(): isize`
  - `mut reserve_exact(new_capacity=: isize)`
  - `mut reserve_at_least(new_capacity=: isize)`
  - `mut push_str(input=: str)`
  - `mut remove_range(range=: Range)`

- Added the following methods to `fae.collections.list.List`
  - `static from_slice(slice=: []T): List<T>`
  - `clone(): List<T>`

- Added the following functions to `fae.memory`
  - `reallocate_str_mut(source=: *mut strmut, old_length: isize, new_length: isize)`

- Added the following functions to `fae.libc`
  - `snprintf(buffer: *mut u8, size: usize, pointer=: *u8, ...): i32`

## Standard Library Bugfixes
- Fixed a panic in `List.extend_from_slice`
- Fixed printing an erronious zero byte when using `fae.io.terminal.clear`

## New Language Changes
- Function signatures now require callsites use parameter labels by default. See the detailed discussion below
- Unions have been added to the language. See the detailed discussion below
- The tag type of enums may now be overridden. See the detailed discussion below
- The tag value of enum variants may now be overridden. See the detailed discussion below
- A special variant of enums has been added, the bitflags enum. See the detailed discussion below
- Bindings may now be zero initialized with `---`. See the detailed discussion below
- Opaque structs have been added to the langauge. See the detailed discussion below
- Statically sized arrays have been added to the language. See the detailed discussion below
- A new `strmut` type has been added, which allows modifying the string slice's backing bytes
- Hexadecimal and binary number literals are now supported
- Enum/union variants in type-position no longer accept type arguments
- Transparent enum variants may now collapse to their wrapped type when coerced
- A variant value typed as itself may now have it's tag "value" accessed with `.tag`
- A compilation error is now emitted when there are too many enum variants for the tag type
- The bitwise not operator `~` has been added
- The new `fae.toml` key `provide_main` disables the std provided `main` function when `false`
- A warning is now emitted when an `fstr` contains no expression items
- There may now be a single newline preceding the arrow `=>` of braceless blocks 
- `trait` and `opaque` are now reserved words
- The `\e` escape sequence is now supported in string, format string, and codepoint literals

## Compiler Usability Improvements
- Error messages now display their file path on a separate line 
- When emitting an LLVM IR file for debugging, the compiler now logs the IR file path
- Name mangling for functions now includes the base type name if a method

## Compiler Bugfixes
- Fixed a bug where imported modules would not be usable in certain contexts
- Fixed a bug where a parameter or return type of certain nested aggregates would generate incorrectly
- Fixed a bug where the unary negation operator would consume all remaining expression to the right as its sub-expression
- Fixed an issue where `static` global variables were not mutable
- Fixed a crash during codegen with an empty `fstr`
- Fixed a crash during codegen with a number literal statement
- Fixed an oversight allowing compile-time bitwise operations on decimal values
- Fixed an oversight where bitshifts were not supported at compile-time
- Fixed an issue where `bundled` was misspelled as `bunded`
- Fixed a crash during validation when a generc enum/union used a struct's generic parameter
- Fixed a crash during codegen with a field-less enum/union variant literal
- Fixed a crash during codegen when using `is` with `and` anywhere other than in `if` or `while` conditions
- Improved link order of the C runtime and project specified additional objects
- Fixed a bug where an import with a single module path token would not function
- Fixed a handful of potential compiler deadlocks
- Improved error underlines in some specific cases with path expressions
- Fixed an issue where errors would have the wrong line number following a multi-line comment or multi-line string
- Fixed an error message which referred to a `when` statement as a `while` statement (Thanks @yazaldefilimone!)
- Fixed an issue where a bitshift-right-assign operator would display incorrectly in error messages (Thanks @yazaldefilimone!)
- Fixed an issue where undefined behavior could be encountered when handling integers larger than 4-bytes
- Fixed an oversight where a `return` statement in a `void` function erronously would accept a non-void expression 
- Fixed an issue where integer casts would incorrectly sign-extend unsigned integers
- Fixed an issue where struct/enum literals would incorrectly initialize any fields following a zero-sized field

## VSCode Extension
- `trait` and `opaque` are now highlighted as keywords

## Detailed Discussion

### Function Parameter Labels

When specifying a function's signature, one must name its parameters. Now by default any call to a function must provide each parameter's label when passing arguments. To not specify these labels is a compile time error.
```
fn function(a: i32, b: f64) {}

function(a: 42, b: 4.2)
```

A function signature may override the outwardly visible parameter label with an equal sign `=`, so the label and the local binding name may be different.
```
fn other_function(a=b: i32) {
  println(f"{a}") // The parameter is still bound to the local symbol `a`
}

// But the label used when passing an argument is `b`
other_function(b: 42)
```

Including the equal sign `=` without providing an overridden label name instead marks that specific parameter to have arguments passed to it *without* the label. Including a label at the callsite for one such parameter is a compile time error.
```
fn yet_another_function(a=: i32) {
  println(f"{a}")
}

yet_another_function(42)
```

### Unions

Fae now includes unions.
```
union Union {
    a: i32
    b: { one: i32, two: i32 }
}
let u: Union = .a(42)
assert(u.b.one == 42)
```

### Overridden Enum Tag Type

Typically when writing an enum the compiler automatically chooses to use a `u8` to represent the tag which differentiates between variants at runtime. 
```
enum Enum {
    A
    B
}

assert(size_of<Enum>() == size_of<u8>()
let instance: Enum = .A
let _: u8 = instance.tag
```

This may now be overridden with an arbitrary integer type.
```
enum Overridden: i32 {
    A
    B
}

assert(size_of<Overridden>() == size_of<i32>()
let instance: Overridden = .A
let _: i32 = instance.tag
```

### Overridden Enum Variant Tag Value

Typically when writing an enum the compiler automatically chooses the value of each variant's tag. This may now be overridden.
```
enum TagValues {
    A = 5
    B = 100
}
```

### Bitflags Enum

Enums may now be specified to be bitflags enums, which enables them to act as type safe bitflag constants.

The most trivial usage automatically picks the bit represntation of each variant.
```
enum(bitflags) SupportedPresentModes {
    Mailbox
    Fifo
    Immediate
}
```

These still mostly operate like normal enums, but with the added ability to perform bitwise manipulation.
```
mut supported: SupportedPresentModes = .Mailbox | .Fifo
```

When applying the `is` operator to a bitflags enum, the operation returns `true` if the specified variant's bit pattern is present. This operation may return `true` for multiple different variants when applied to the same value.
```
let _ = supported is Mailbox // true
let _ = supported is Fifo // true
let _ = supported is Immediate // false
```

A bitflags enum may be initialized with no variants set with a plain `0` (zero) number literal.
```
supported = 0
let _ = supported is Mailbox // false
```

The first variant starts with a bit pattern of `0b1`, with each successive variant using the next most significant bit. Like other enums this value may be manually specified, and any following variant without a specified value will choose the next most significant bit after the most significant bit of the manually specified tag value.
```
enum(bitflags) BitflagsTagValues {
  A // 0b1
  B // 0b10
  C = 0b111
  D // 0b1000
  E // 0b10000
}
```

### Zero Initialized Bindings

Typically when creating a `let` or `mut` binding, one must provide an initialization expression. However when interacting with some C-style APIs which rely on out-pointers, it may be needlessly overwrought to manually initialize a binding just so it can be overwritten via an out pointer. Zero initialized bindings solve this problem by avoiding having to manually initialize the binding while still guaranteeing a well defined value.
```
mut value: SomeComplexType --- // This binding is *fully* zeroed
some_function(value.&mut)
```

### Opaque Structs

A struct may now be marked as opaque, to signify to the compiler that the size and layout of the struct is unknown. This prevents usage of `size_of` on the struct, constructing an instance, storing an instance on the stack, and passing by value. One exception is that currently generic functions may break these rules for practical reasons.
```
struct(opaque) Handle
let _: Handle --- // Compile error!
```

### Statically Sized Arrays

Previously Fae only had a concept of slices; a fat pointer consisting of a pointer to the first item and an item count. Their type signature is written as `[]T` where `T` is the type of the items.

Fae now also supports statically sized arrays, sequences of values where the length is encoded in the type system and the entire contents are logically passed by value. Their type signature is written as `[N]T` where `N` is the length and `T` is the type of the items.

Slice literals are now joined by array literals.
```
// Slice literal
let slice = []i32 { 4, 2 }
let _: []i32 = slice

// Array literal
let array = [_]i32 { 4, 2 }
let _: [2]i32 = array
```

Accessing the fake `.slice` field on arrays reinterprets the array as a slice of its contents.
```
let _: []i32 = array.slice
```

# Fae Language Reference

This document attempts to meaningfully describe the current state of the Fae programming language. As the language is not complete and neither is the compiler, notes are made where appropriate to detail what are temporary implementation limitations or what is intended to change in the future.

Just as the language and compiler are immature, so too is this document. Please report any issues encountered with factual accuracy or where where additional detail is required.


# Table of Contents
- [Fae Language Reference](#fae-language-reference)
- [Table of Contents](#table-of-contents)
- [Project structure](#project-structure)
- [Lexical Structure](#lexical-structure)
    - [File encoding](#file-encoding)
    - [Keywords](#keywords)
- [Syntactic Details](#syntactic-details)
- [Semantics](#semantics)
  - [Primitive types](#primitive-types)
  - [Pointer types](#pointer-types)
  - [Primitive keyword values](#primitive-keyword-values)
  - [Binary operators](#binary-operators)
  - [Unary operators](#unary-operators)
  - [Untyped numeric literals](#untyped-numeric-literals)
  - [Const bindings](#const-bindings)
  - [Immutable binding](#immutable-binding)
  - [Mutable binding](#mutable-binding)
  - [Codepoint literals](#codepoint-literals)
  - [Pointers](#pointers)
  - [Slices](#slices)
  - [Slice range slicing](#slice-range-slicing)
  - [Allocation](#allocation)
  - [String literals](#string-literals)
  - [Format string literals](#format-string-literals)
  - [Type casts](#type-casts)
  - [Structures](#structures)
  - [Rich Enums](#rich-enums)
  - [Field access modifiers](#field-access-modifiers)
  - [Zero size types](#zero-size-types)
  - [`if-else` expression](#if-else-expression)
  - [`when` statement](#when-statement)
  - [Single line blocks](#single-line-blocks)
  - [`is` operator](#is-operator)
  - [`match` expression](#match-expression)
  - [`while` loops](#while-loops)
  - [`for` loops](#for-loops)
  - [Block expressions and `yield`](#block-expressions-and-yield)
  - [`defer` statement](#defer-statement)
  - [Symbol importing](#symbol-importing)
  - [Static variables](#static-variables)
  - [Functions](#functions)
  - [Type coercion](#type-coercion)
      - [Collapse fair](#collapse-fair)
      - [Collapse to](#collapse-to)
  - [Prelude](#prelude)
  - [Intrinsic functions](#intrinsic-functions)


# Project structure

The root of every Fae project must contain a `fae.toml` file written with the `TOML` syntax. It *must* contain the following entries:
- `project_name` (string) Indicates the name of the project
- `source_directory` (string) Indicates the path to the root of the project code

And it *may* contain the following entries:
- `linux_linker` (string) Indicates the executable name of the linker to be used on Linux
- `linux_additional_linker_objects` (array of string) Indicates a list of object file paths to link into the project on Linux
- `darwin_linker` (string) Indicates the executable name of the linker to be used on Darwin (macOS)
- `darwin_additional_linker_objects` (array of string) Indicates a list of object file paths to link into the project on Darwin (macOS)

The project's `main` function must be located in a `.fae` file named the same as the project, located in the root of the source directory. So for a project named "hello_world" and the source directory "./src", the `main` function must be located in "./src/hello_world.fae" relative to the project root. This `main` function must have the following signature.
```
fn main()
```

**Note**: It is planned to replace the `fae.toml` file with build scripts written in Fae, the `toml` file is a temporary solution.


# Lexical Structure

### File encoding

Fae files are represented with the UTF-8 encoding using Unix newlines, and projects must be stored on a hierarchical file system to preserve module structure.


### Keywords

The Fae language contains four keywords different categories, each allowed in different contexts. All keyword, except for private attributes and field access modifiers, are reserved words which may not be used as identifier names.

**Statement keywords** start new statements in block scopes.
- `when`
- `import`
- `const`
- `static`
- `let`
- `mut`
- `fn`
- `struct`
- `enum`
- `while`
- `for`
- `if`
- `else`
- `match`
- `return`
- `defer`
- `break`
- `continue`
- `yield`

**For loop iteration keywords** are placed between a `for` loop's bindings and expression to indicate which type of iteration is to be used.
 - `in`
 - `of`

**Value keywords** represent named constant values built into the language. These are syntactically valid as an atom in any expression configuration.
- `void`
- `true`
- `false`

**Expression keywords** operate inline on values, syntactically much like mathematical operations.
- `and`
- `or`
- `is`

**Attribute keywords** may optionally precede some statements, providing additional semantically significant information.
- `generic`
- `extern`
- `export`
- `method`

**Private attribute keywords** may optionally precede some statements within the Fae standard library, or similarly blessed module tree, but are disallowed in ordinary module trees. These attribute keywords must be preceded by a pound sign `#` and are not reserved words.
- `intrinsic`
- `lang`

**Field access modifiers** may optionally follow `struct` and `enum` fields and are not reserved words.
- `readable`
- `readonly`
- `internal`
- `internal readonly`

A file may contains zero or more of the following statements
- `when`
- `import`
- `const`
- `static`
- `fn`
- `struct`
- `enum`

# Syntactic Details

Fae has a significant-newline syntax, with newlines both terminating statements and being able to substitute for commas anywhere commas would be required if written on a single line.
```
struct A { a: i32, b: i64 }
struct B {
    a: i32
    b: i64
}

function(a, b, c)
function(
    a
    b
    c
)
```

Line comments are prefixed with `//` and multi-line comments use `/* comment */`.
```
// This is a line comment

/* And this is a
multi-line comment */
```

Typed declarations in Fae take the postfix type annotation, written with a name, a colon, and finally the type. In places where omitting the type is valid, such as `let`, `mut`, and `const` declarations, the colon is also omitted.
```
let a: i32 = function()
let b = function()
const C = 299792458
```


# Semantics

## Primitive types

| Type    | Size (bytes)         | Alignment (bytes) | Description                                                  |
| ------- | -------------------- | ----------------- | ------------------------------------------------------------ |
| `void`  | 0                    | 1                 | Zero sized unit type, default function return type           |
| `i8`    | 1                    | 1                 | Signed 8 bit integer                                         |
| `i16`   | 2                    | 2                 | Signed 16 bit integer                                        |
| `i32`   | 4                    | 4                 | Signed 32 bit integer                                        |
| `i64`   | 8                    | 8                 | Signed 64 bit integer                                        |
| `u8`    | 1                    | 1                 | Unsigned 8 bit integer                                       |
| `u16`   | 2                    | 2                 | Unsigned 16 bit integer                                      |
| `u32`   | 4                    | 4                 | Unsigned 32 bit integer                                      |
| `u64`   | 8                    | 8                 | Unsigned 64 bit integer                                      |
| `isize` | Same as a pointer    | Same as a pointer | Signed pointer sized integer                                 |
| `usize` | Same as a pointer    | Same as a pointer | Unsigned pointer sized integer                               |
| `f32`   | 4                    | 4                 | 32 bit floating point number                                 |
| `f64`   | 8                    | 8                 | 64 bit floating point number                                 |
| `bool`  | 1                    | 1                 | Boolean value type, `true` or `false`                        |
| `str`   | Same as two pointers | Same as a pointer | String slice, has `pointer` and `length`                     |
| `fstr`  | Same as two pointers | Same as a pointer | Format string slice, describes a string formatting operation |


## Pointer types

| Type      | Size (bytes)         | Alignment (bytes) | Description                                                                                                     |
| --------- | -------------------- | ----------------- | --------------------------------------------------------------------------------------------------------------- |
| `&T`      | 8 (on 64-bit)        | 8 (on 64-bit)     | Pointer to value of type `T` which *cannot* be used to mutate the pointed value                                 |
| `&mut T`  | 8 (on 64-bit)        | 8 (on 64-bit)     | Pointer to value of type `T` which *can* be used to mutate the pointed value                                    |
| `[]T`     | Same as two pointers | Same as a pointer | Pointer to sequence of type `T` with a runtime known length which *cannot* be used to mutate the pointed values |
| `[]mut T` | Same as two pointers | Same as a pointer | Pointer to sequence of type `T` with a runtime known length which *can* be used to mutate the pointed values    |


## Primitive keyword values

| Keyword | Description                 |
| ------- | --------------------------- |
| `void`  | Unit value of type `void`   |
| `true`  | Truthy value of type `bool` |
| `false` | Falsy value of type `bool`  |


## Binary operators

All binary operators expect both sides to be of the same type, or barring that, able to perform the [[#Collapse fair]] rules to collapse both to the same type. Range initialization does *not* perform the collapse fair rules.

| Name                              | Syntax    | Details                                                                                                                                                                                                                                                                                                             |
| --------------------------------- | --------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Assign                            | `a = b`   | Assigns the value of `b` to `a`                                                                                                                                                                                                                                                                                     |
| Add                               | `a + b`   | Produces the result of adding `a` and `b`<br><br>Currently limited to numeric values                                                                                                                                                                                                                                |
| Add Assign                        | `a += b`  | Adds `a` and `b` while assigning the result to `a`, produces a `void` value<br><br>Currently limited to numeric values                                                                                                                                                                                              |
| Subtract                          | `a - b`   | Produces the result of subtracting `b` from `a`<br><br>Currently limited to numeric values                                                                                                                                                                                                                          |
| Subtract assign                   | `a -= b`  | Subtracts `b` from `a` while assigning the result to `a`, produce a `void` value<br><br>Currently limited to numeric values                                                                                                                                                                                         |
| Multiply                          | `a * b`   | Produces the result of multiplying `a` and `b`<br><br>Currently limited to numeric values                                                                                                                                                                                                                           |
| Multiply assign                   | `a *= b`  | Multiplies `a` and `b` while assigning the result to `a`, produces a `void` value<br><br>Currently limited to numeric values                                                                                                                                                                                        |
| Divide                            | `a / b`   | Produces the result of dividing `a` by `b`<br><br>Currently limited to numeric values                                                                                                                                                                                                                               |
| Divide assign                     | `a /= b`  | Divides `a` by `b` while assigning the result to `a`, produces a `void` value<br><br>Currently limited to numeric values                                                                                                                                                                                            |
| Modulo                            | `a % b`   | Produces the *Euclidean* (see note\*) division remainder of `a` divided by `b`<br><br>Restricted to signed integer and unsigned integer values                                                                                                                                                                      |
| Modulo assign                     | `a %= b`  | Calculates *Euclidean* (see note\*) remainder of `a` divided by `b` while assigning the result to `a`, produces a `void` value<br><br>Restricted to signed integer and unsigned integer values                                                                                                                      |
| Bitshift left                     | `a << b`  | Produces the result of a *logical* left shift of `a` by `b` bits<br><br>Restricted to signed integer and unsigned integer values                                                                                                                                                                                    |
| Bitshift left assign              | `a <<= b` | Assigns the result of a *logical* left shift of `a` by `b` bits to `a`, produces a `void` value<br><br>Restricted to signed integer and unsigned integer values                                                                                                                                                     |
| Bitshift right                    | `a >> b`  | For unsigned integers: Produces the result of a *logical* right shift of `a` by `b` bits<br>For signed integers: Produces the result of an *arithmetic* right shift of `a` by `b` bits<br><br>Restricted to signed integer and unsigned integer values                                                              |
| Bitshift right assign             | `a >>= b` | For unsigned integers: Assigns the result of a *logical* right shift of `a` by `b` bits to `a`, produces a `void` value<br>For signed integers: Assigns the result of a *arithmetic* right shift of `a` by `b` bits to `a`, produces a `void` value<br><br>Restricted to signed integer and unsigned integer values |
| Bitwise AND                       | `a & b`   | Produces the result of bitwise AND-ing `a` and `b`<br><br>Restricted to `bool`, signed integer, and unsigned integer values                                                                                                                                                                                         |
| Bitwise AND assign                | `a &= b`  | Assigns the result of bitwise AND-ing `a` and `b` to `a`, produces a `void` value<br><br>Restricted to `bool`, signed integer, and unsigned integer values                                                                                                                                                          |
| Bitwise OR                        | `a \| or` | Produces the result of bitwise OR-ing `a` and `b`<br><br>Restricted to `bool`, signed integer, and unsigned integer values                                                                                                                                                                                          |
| Bitwise OR assign                 | `a \|= b` | Assigns the result of bitwise OR-ing `a` and `b` to `a`, produces a `void` value<br><br>Restricted to `bool`, signed integer, and unsigned integer values                                                                                                                                                           |
| Bitwise XOR                       | `a ^ b`   | Produces the result of bitwise XOR-ing `a` and `b`<br><br>Restricted to `bool`, signed integer, and unsigned integer values                                                                                                                                                                                         |
| Bitwise XOR assign                | `a ^= b`  | Assigns the result of bitwise XOR-ing `a` and `b` to `a`, produces a `void` value<br><br>Restricted to `bool`, signed integer, and unsigned integer values                                                                                                                                                          |
| Equality comparison               | `a == b`  | Produces the `bool` value indicating if `a` and `b` are equal in value<br><br>Currently limited to numeric, pointer, `bool`, and `void` values                                                                                                                                                                      |
| Inequality comparison             | `a != b`  | Produces the `bool` value indicating if `a` and `b` are unequal in value<br><br>Currently limited to numeric, pointer, `bool`, and `void` values                                                                                                                                                                    |
| Greater than comparison           | `a > b`   | Produces the `bool` value indicating if `a` is larger than `b`<br><br>Currently limited to numeric values                                                                                                                                                                                                           |
| Greater than or equals comparison | `a >= b`  | Produces the `bool` value indicating if `a` is larger than or equal to `b`<br><br>Currently limited to numeric values                                                                                                                                                                                               |
| Less than comparison              | `a < b`   | Produces the `bool` value indicating if `a` is smaller than `b`<br><br>Currently limited to numeric values                                                                                                                                                                                                          |
| Less than or equals comparison    | `a <= b`  | Produces the `bool` value indicating if `a` is smaller than or equal to `b`<br><br>Currently limited to numeric values                                                                                                                                                                                              |
| Logical AND                       | `a and b` | Evaluates the boolean expression `a`, producing a `false` value if `a` is `false`, otherwise evaluates boolean expression `b`, and produces the `bool` value of `b`<br><br>Restricted to `bool` values                                                                                                              |
| Logical OR                        | `a or b`  | Evaluates the the boolean expression `a`, producing a `true` value if `a` is true, otherwise evaluates boolean expression `b` and produces the `bool` value of `b`<br><br>Restricted to `bool` values                                                                                                               |
| Range initializer                 | `a..b`    | Produces a `Range` value indicating the value range from `a` to `b` where `a` is inclusive and `b` is exclusive<br><br>Restricted to `isize` values                                                                                                                                                                 |

\* Fae calculates the modulo of two values with the *Euclidean* division definition. This will produce a different result than C/C++/Rust/Zig which employ *truncated* division or Python which utilizes *floored* division but will produce the same result as Dart which also uses Euclidean remainder. Some examples of Euclidean remainder are:
 - `5 % 4` = 1
 - `-5 % 4` = 3
 - `5 % -4` = 1
 - `-5 % -4` = 3

## Unary operators

| Name               | Syntax   | Details                                                                                                                                                         |
| ------------------ | -------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Negate             | `-a`     | Produces the result of negatating the sign of the signed integer or floating point number `a`                                                                   |
| Invert             | `a.!`    | Produces the result of inverting the truthiness of the boolean value `a`                                                                                        |
| Address of         | `a.&`    | Produces a pointer of type `&T` pointing to value `a` of type `T`                                                                                               |
| Address of mutable | `a.&mut` | Produces a pointer of type `&mut T` pointing to value `a` of type `T`, where `a` is mutable                                                                     |
| Dereference        | `a.*`    | Produces the value of type `T` pointed to by pointer `a`, allowing modification of the original pointed value if `a` is a pointer of type `&mut T`              |
| Cast               | `a.(T)`  | Produces the result of converting pointer or numeric value `a` to a different pointer or numeric type `T`                                                       |
| Index              | `a[b]`   | Produces the value of type `T` at index `b` contained within slice `a`, allowing modification of the original pointed value if `a` is a slice of type `[]mut T` |
| Slice              | `a[b]`   | Produces a slice pointing to the items of slice `a` described by index range `b`, with the same mutability as the original slice                                |


## Untyped numeric literals

Numeric literals in Fae do not have a type like `i32` or `f64`. Rather they represent the abstract concept of the number, and math between untyped numerals is performed at compile time with large capacity decimal values.

As untyped numerals have no inherit "runtime" type, they must be coerced to a sized numeric type like `i32` or `f64` in order to be assigned to bindings, stored in fields, or passed as arguments. The compiler will determine this type automatically from context when required, throwing an error if the value is out of the representable bounds of the type. See [[#Type coercion]] for details.

Untyped numerals can be used as the value of a `const` and the untyped numeral will be substituted into all usages of that `const`, allowing for the same `const` to be coerced to different "runtime" numeric types at different substitution points, throwing an error anytime a loss in precision would occur. 

| Syntax Examples |
| --------------- |
| `1`             |
| `-99`           |
| `3.1415`        |
| `-7.0`          |

Decimal literals require a digit follow the period.

**Note**: It is planned to provide the ability to write numeric literals with binary, hexadecimal, and exponential syntax in the future, as well as to allow for digit separators.


## Const bindings

A `const` statement has some compile time known value which is substituted into all usage sites of the `const`. The `const` does not have a memory location at runtime. Syntactically it takes the following forms.
```
// Inferred type of `untyped integer`
const ConstName = 5

// Inferred type of `untyped decimal`
const Gravity = 9.8

// Specified type of `u32`
const Four: u32 = 1 << 2
```

**Note**: Due to a temporary compiler limitation a `const` may only reference another `const` if it is defined before it within the same file.

**Note**: Due to a temporary compiler limitation a `const` may only have one of the following types: `untyped number`, `str`, or concrete numeric types such as `i32`.


## Immutable binding

A `let` statement binds a runtime value to a name and does *not* allow further mutation of the value stored within it, though mutable data pointed to by a `let` binding or a field within a `let` binding may still be mutated *through* the `let` binding. It takes the following forms.
```
// Inferred type of `bool`
let a = true

// Specified type of `i32`
let b: i32 = 8

// A `let` with a single underscore name still executes the expression, but does not store it
let _ = "Hello"
let _: isize = 0
```

A `let` statement may use the name of another `let` or `mut` binding already in scope, thereby *shadowing* the existing name for the duration of the new binding's containing block.
```
let a = true
assert(a == true)

{
    let a = false
    assert(a != true)
}

assert(a == true)
```


## Mutable binding

A `mut` statement binds a runtime value to a name and *does* allow further mutation of the value, both in modifying fields and replacing the value outright. Syntactically it takes the following forms.
```
// Inferred type of `bool`
mut a = true

// Specified type of `i32`
mut b: i32 = 8

// A `mut` with a single underscore name still executes the expression, but does not store it
mut _ = "Hello"
mut _: isize = 0
```

A `mut` statement may use the name of another `let` or `mut` binding already in scope, thereby *shadowing* the existing name for the duration of the new binding's containing block.
```
let a = true
assert(a == true)

{
    mut a = true
    assert(a == true)
    a = false
    assert(a == false)
}

assert(a == true)
```


## Codepoint literals

A singular Unicode codepoint value literal may be written between two single quotes and produces a value of type `u32`.
```
let letter_a = 'A'
let smiley: u32 = '😀'
```

A singular *byte* codepoint value literal may be written the same but with a `b` preceding the first single quote and produces a value of type `u8`. The codepoint written must be one which can be represented in a single byte, such as those in the ASCII range.
```
let letter_a = b'A'
let letter_b: u8 = b'B'
```

Both types of literals may contain the following escape sequences.
 - `\n` -> newline
 - `\r` -> carriage return
 - `\t` -> horizontal tab
 - `\\` -> backslash
 - `\'` -> single quote
 - `\0` -> null


## Pointers

Pointers to values on the stack may be acquired by using the "address of" or "address of mut" unary operators on a binding. The "address of mut" unary operator may not be used on immutable bindings.
```
mut value: i32 = 42

let _ = value.&
let _: &i32 = value.&

let _ = value.&mut
let _: &mut i32 = value.&mut
```

The "address of" and "address of mut" unary operators may also be used on arbitrary expressions which will cause the compiler to store the product of the sub-expression on the stack within the current block scope and produce a pointer to the value.
```
let _: &bool = function_returning_bool().&
let _: &mut bool = function_returning_bool().&mut
```

The value pointed to by a pointer may be accessed via the "dereference" unary operator. Pointers of type `&mut T` may be used to mutate the value of type `T` by reaching through the pointer with a dereference. Mutating the pointed value does not require the pointer itself to be mutable, only that the pointed value be mutable.
```
let ptr = true.&mut
assert(ptr.*)

ptr.* = false
assert(ptr.* == false)
```

A pointer to a mutable value may not be used to modify the value when accessed immutably through another pointer.
```
mut value = false
let ptr = value.&mut
let ptr_ptr: &&mut bool = ptr.&
ptr_ptr.*.* = true // Error: Cannot assign to immutable memory location 
```

When pointing to a value with fields, the pointer may be *implicitly* dereferenced when accessing the fields through the normal dot access.
```
let ptr: &mut SomeStruct = function()
ptr.field_name = 42
```

Pointer arithmetic is not directly supported, instead a pointer may be cast to `usize` which arithmetic may be performed on before casting back to a pointer. Helper functions `offset_pointer<T>(ptr: &T, bytes: isize)` and `offset_pointer_mut<T>(ptr: &mut T, bytes: isize)` are provided via the prelude and are automatically in scope.

**Note**: It is planned to remove the ability to cast integers to pointers, and turn the offset helper functions into intrinsics.


## Slices

Slices are fat pointers consisting of a pointer to the sequence of zero or more items and a number representing the number of items in the sequence. They may either point to mutable or immutable items and their types are written as `[]T` and `[]mut T` respectively, where `T` is the type of the items.

For a slice `[]T`, it has the following two fields:
 - `pointer` of type `&T`
 - `length` of type `isize`

For a slice `[]mut T`, it has the following two fields:
 - `pointer` of type `&mut T`
 - `length` of type `isize`

A slice initializer produces a value of type `[]mut T` where `T` is the type of the items pointed to by the slice. The underlying buffer is placed on the stack within the current block scope and the values *are* mutable unless the type is coerced to a non-mutable slice. Syntactically it takes the following forms.
```
// Slice of 3 values of specified type `i32`
let _ = []i32 { 42, 78, 99 }

// Slice of 4 values of inferred type `bool`
let _ = []{ true, false, true, false }

// Slice of 0 values of specified type `f64`
let _ = []f64 {}
```
**Note**: It is planned to add statically sized array types to Fae at which point the slice initializer syntax will become the array initializer syntax instead. Using this syntax to initialize slices is a stopgap solution.

The items pointed to by the slice may be accessed with the index unary operator and a zero-based index of type `isize`. If the slice is of mutable values then indexing allows modification of the indexed item. Taking the address of the item is also valid. If the index is beyond the range of the slice length the operation will trigger a panic, terminating the application.
```
let slice: []mut u32 = []u32 { 3, 20, 6, 50, 77 }
assert(slice[2] == 6)

slice[3] += 1
assert(slice[3] == 51)

let ptr = slice[1].&mut
ptr.* += 1
assert(slice[1] == 21)

slice[5] // PANIC! Out of bounds access!
```

## Slice range slicing

Slices, including `str`, may be *sliced* by using a value of type `Range`, such as produced by a range initializer binary operator, as the value of a slice index operation. The lower bound of the range is inclusive and the upper range is exclusive.
```
let slice: []u32 = []u32 { 3, 20, 6, 50, 77 }
let result = slice[0..2]
assert(result.length == 2)
assert(result[0] == 3)
assert(result[1] == 20)
```

If either the upper or lower range bound are out of bounds of the slice length the operation will trigger a panic, terminating the application.
```
let slice = []u32 { 0, 1, 2, 3 }
slice[0..5] // PANIC! Out of bounds access!
```

The resulting slice produced by the slicing operation will point to the values of the original collection, it will *not* copy or otherwise clone the pointed data. Mutations of the data pointed to by the sub-slice are observable from the original slice, and other sub-slices, assuming they cover the range of modified data.
```
let slice = []u32 { 0, 1, 2, 3 }
let subslice = slice[1..4]
subslice[0] == 9
assert(slice[1] == 9)
```


## Allocation

Pointers and slices to heap allocated data may be acquired by calling the collection of helper functions included in the standard library module `fae::memory`. These functions are designed to handle zero sized types automatically and transparently, allowing for use in generic code.

**Note**: These functions are currently backed by libc malloc/realloc/free but it is planned to make them take an allocator object and for most normal allocating code in Fae to be parameterized by an allocator.


## String literals

A string literal's contents are written with two double quotes and produces a value of type `str`.
```
let greeting = "Hello there!"
let welcome: str = "Welcome"
```

String literals in Fae guarantee the presence of a null terminator following the string contents themselves. The `str`'s `length` field does *not* include this null terminator.
```
let message = "Hello Fae"
assert(strlen(message.pointer) == message.length)
```

A string literal may contain the following escape sequences.
 - `\n` -> newline
 - `\r` -> carriage return
 - `\t` -> horizontal tab
 - `\\` -> backslash
 - `\"` -> double quote
 - `\0` -> null

## Format string literals

Format strings are written like a string literal but with the letter `f` before the opening double quote and produce a value of type `fstr`. Expressions within curly brace pairs in the format string are *eagerly evaluated* and their results included in the format string item slice.
```
let index: isize = 42
let message = f"The index is {index}"

let name = "Doris"
let greeting: fstr = f"Hello {name}!"
```

The types of expressions which may be included in a format string are:
- `i8`
- `i16`
- `i32`
- `i64`
- `u8`
- `u16`
- `u32`
- `u64`
- `isize`
- `usize`
- `f32`
- `f64`
- `bool`
- `str`
- `fstr`

A format string literal describes a string formatting operation, but *does not* perform the formatting itself. Instead it is a slice of formattable items which code may walk and format at runtime. The standard library provides a set of `print`/`println` and `eprint`/`eprintln` functions to format a format string to stdout and stderr respectively.
```
let name = "Tina"
// Prints "Hello there Tina!\n" to stdout
println(f"Hello there {name}!")
```

Any location, such as a binding or function parameter, which is expecting a format string `fstr` will also accept an ordinary string `str` value which the compiler will transparently convert to a single item format string containing the original `str` value.
```
// Prints "This is a normal string literal\n" to stdout
println("This is a normal string literal")
```

A format string literal may contain the following escape sequences.
 - `\n` -> newline
 - `\r` -> carriage return
 - `\t` -> horizontal tab
 - `\\` -> backslash
 - `\"` -> double quote
 - `\0` -> null
 - `\{` -> open curly brace


## Type casts

All numerical types may be freely cast between each other with the cast unary operator, performing the expected truncation, widening, or wrapping required for the destination type.
```
let a: i32 = -42
assert(a.(u32) == 4294967254)
assert(a.(f64) == -42)
```

Pointer types may be cast between each other so long as the cast does not attempt to promote a pointer to immutable data into a pointer to mutable data, that is convert `&T` into `&mut T` which is disallowed.
```
mut a: u32 = 0
let b = a.&mut.(&mut u16)
b.* = 2
assert(a == 2) // Example assumes a little-endian system
```
**Note**: Fae does not *currently* perform type based pointer aliasing analysis and so memory accesses to the same location through pointers of different types are well defined and sound.

Pointers may be freely cast to numeric types, and integers of sufficient size for the target platform may be cast back into pointers of any mutability.

**Note**: It is planned to remove the ability to cast integers to pointers. See [[#Pointers]] for more details.


## Structures

Struct may be defined within any block, including within functions, will inherit any generic type parameters from their definition scope, and their fields are public by default. They take the following forms.
```
// Empty struct, has a byte size of `0`, byte alignment of `1`
struct EmptyStruct {}

// Struct with a single `u64` field
struct PlayerId {
    representation: u64
}

// Struct with two `f64` fields
struct DoubleDouble {
    a: f64
    b: f64
}

// Generic struct with two fields of the same generic type
generic T
struct Pair {
    a: T
    b: T
}

// Generic struct with two field of different generic types
generic A, B
struct Combo {
    left: A
    right: B
}
```

**Note**: Currently all structs are guaranteed to have the same layout, size, and alignment as if defined in a C program compiled with the target platform's default ABI. It is planned that in the future the compiler will automatically pack structs which have not been marked as exported.

Struct initialization takes the following forms.
```
let _ = EmptyStruct {}

let _ = PlayerId { representation: 5 }
let _ = PlayerId {
    representation: 6
}

let representation: u64 = 7
let _ = PlayerId { representation }

let _ = DoubleDouble { a: 1, b: 2 }
let _ = DoubleDouble {
    a: 3
    b: 4
}

let b: i32 = 2
let _ = Pair<i32> { a: 1, b }
let _ = Pair<i32> {
    a: 3
    b
}

let _ = Combo<bool, i32> { left: true, right: 6 }
let _ = Combo<f64, f32> {
    left: 7
    right: 8
}

let left: i32 = 9
let right = false
let _ = Combo<i32, bool> { left, right }
let _ = Combo<i32, bool> {
    left
    right
}
```


## Rich Enums

Enum may be defined in any block, including in functions, will inherit any generic type parameters from their definition scope, and their fields are public by default. They take the following forms.
```
// Empty enum, has no variants so cannot may only have a single be initialized
enum EmptyEnum {}

// Enum with two separate variants
enum OffsetKind {
    Absolute
    Relative
}

// Enum with two seperate "struct-like" variants, each with one field
enum Pet {
    Cat { hours_asleep: i32 }
    Dog { time_since_last_walk: i32 }
}

// Enum with two separate "struct-like" variants, both of which have
// the shared field, and one of which has a second field. The presence
// of the shared field makes `CameraDrone` a struct-like variant
// despite not having braces
enum Enemy {
    health: i32

    CameraDrone
    Brute { weapon: Weapon }
}

// Enum with two separate *transparent* variants. Transparent variants
// contain a single value which may not be accessed directly. The
// value is accessed via type matching in a `match` expression
enum Value {
    String(str)
    Number(f64)
}
```
**Note**: Due to a temporary compiler limitation an enum may not have more than 256 variants.

An enum variant may be initialized by writing the enum name followed by a dot and the variant name. This produces an instance of the variant itself as each variant of an enum is a separate type.
```
let _ = OffsetKind.Absolute
let _ = OffsetKind.Relative
```

The type of a variant can be referenced by writing the enum name followed by a dot and the variant name, matching the initialization syntax. For simple field-less enums the size of a variant's type is zero bytes as the variant tag is encoded in the type system.
```
let _: OffsetKind.Relative = OffsetKind.Relative
assert(size_of<OffsetKind.Relative>() == 0)
```

When the compiler knows that the enum type itself is required, it will coerce an instance of the variant to the parent enum type.
```
let _: OffsetKind = OffsetKind.Relative
assert(size_of<OffsetKind>() == 1) // Single byte tag indicating variant
```

In some contexts the compiler knows which enum to expect, there the enum name may be omitted.
```
let _: OffsetKind = .Absolute
function_expecting_offset_kind(.Relative)
```

When the variant is struct-like, the variant name must be followed by braces containing field initializers in the correct order.
```
let _: Pet = Pet.Cat { hours_asleep: 4 }
let _: Pet = .Dog { time_since_last_walk: 6 }
```

Shared fields defined within the body of the enum itself are the first fields of every variant of the enum. They can be accessed on an instance of the enum without having to first narrow the type to a variant.
```
spawn_enemy(.CameraDrone { health: 30 })
spawn_enemy(.Brute { health: 60, weapon: .Club })

let enemy: Enemy = .CameraDrone { health: 20 }
assert(enemy.health == 20)
```

Transparent variants are initialized with parenthesis rather than braces and field initializers.
```
let _: Value = Value.String("Hello world"))
let _: Value = .Number(3.14))
```

The numerical "tag" of the enum may be accessed from any instance of an enum itself by accessing the `tag` field.
```
let kind: OffsetKind = .Absolute
assert(kind.tag == 0)

let kind: OffsetKind = .Relative
assert(kind.tag == 1)
```
**Note**: Due to a temporary compiler limitation the enum tag is restricted to be type `u8` and an enum may not have more than 256 variants.

For which variant an enum is and accessing the values in a specific variant see [[#`is` operator]] and [[#`match` expression]] .


## Field access modifiers

Struct and enum fields may be annotated with access modifiers after the field type. There are two types of modifiers, those which control field visibility and those which control field mutability. By default all struct and enum fields are allowed to be publically initialized, read, and mutated if the instance is mutable.

Field visibility:
 - `internal` restricts field access and initialization to within *non-extension* methods on the type containing the field.

Field mutability:
 - `readable` prevents the field from *itself* being mutated or initialized from outside *non-extension* methods on the type containing the field.
 - `readonly` prevents all mutation of the field *itself* after initialization.

They may be composed in the following combinations:
 - `internal`
 - `readable`
 - `readonly`
 - `internal readonly`

When a field's mutation is disallowed due to `readable` or `readonly`, that does not prevent mutation of values pointed to with a mutable pointer or mutable slice contained *within* the field which may not itself be mutated. This mirrors how a mutable pointer may be stored in a `let` binding while still allowing the pointed data to be mutated.

```
struct MyStruct {
    length: isize readable
    id: usize internal readonly
}

enum MyEnum {
    VariantName {
        field: readonly
    }
}
```


## Zero size types

Some types such as `void`, empty structs, and non-struct-like enum variants have no runtime size, that is they have a size of zero bytes. Generally Fae code need not know it is handling zero size types as the compiler will not generate any memory operations on "values" of these types. Instead the compiler reasons about them purely in the type system and they disappear during codegen, are omitted from function argument lists, ect.

Attempting to query the size of a zero size type with `size_of<T>()` will return a value of `0` (zero), and attempting to query the alignment of a zero size type with `alignment_of<T>()` will return a value of `1` (one). All types in Fae are guaranteed to have an alignment greater than or equal to `1` (one).

Pointers to and slices of zero size types are represented with a value of `1` (one). They will not erroneously trigger comparisons against null, yet on systems with a protected zero page any access will still trigger a segmentation fault rather than silently misbehaving. Fae code should never hit this, but in the case of erroneously passing one of these pointers or slices across FFI to a language with no concept of zero size types, external code may attempt to dereference these pointers.

The memory allocation functions in the standard library module `fae::memory` handle zero size types transparently and automatically. Furthermore the standard library provides helper intrinsics `create_non_null_invalid_pointer` and `create_non_null_invalid_slice` in the module `fae::intrinsics` to manually create these pointers and slices.


## `if-else` expression

An `if` expression branches into the specified block if the condition expression returns a `true` value, otherwise continues to do the same for any following `else if`, and if none of those branch then it will branch into the `else` block if present. It is an expression as it may be used as a block expression, see [[#Block expressions and `yield`]] for more. 

Syntactically it takes the following forms.
```
if foo() {
    println("foo")
}

if foo() {
    println("foo")
} else if bar() {
    println("bar")
}

if foo() {
    println("foo")
} else {
    println("else")
}

if foo() {
    println("foo")
} else if bar() {
    println("bar")
} else if baz() {
    println("baz")
} else {
    println("else")
} 
```


## `when` statement

The `when` statement enables limited conditional compilation, where code which is "compiled out" will be parsed by the compiler, but not validated nor included in the final binary. Syntactically the `when` statement resembles an `if-else` expression which is evaluated at compile time. Conceptually whichever branch is "taken" is merged into the containing block. So constructs like `const` or `struct` defined within are accessable outside the `when` statement, assuming the construct is present in the branch which is chosen.
```
when PlatformLinux {
    const MagicNumber = 1
} else when PlatformDarwin {
    const MagicNumber = 2
} else {
    const MagicNumber = 3
}

println(f"{MagicNumber}") // Prints a different value depending on the target platform
```

The `when` statement's condition may contain a single word predicate which the compiler evaluates the truthiness of at compile time. It may *not* include operators such as `and` or `or`. The list of allowed predicates are:
- `PlatformLinux` (the target platform is Linux based, such as desktop GNU/Linux)
- `PlatformDarwin` (the target platform is Darwin based, such as macOS)
- `DebugBuild` (the project is being built in debug mode)
- `ReleaseBuild` (the project is being built in release mode)
- `InCompilerTest` (the code is being compiled in the context of a compiler test)

**Note**: It is planned for `when` to gain the ability to evaulate arbitrary compile time expressions, replacing the compiler built in word predicates.


## Single line blocks

Constructs such as `when`, `if-else`, `match`, `while`, and `for` all have body blocks which may be written either as braced `{}` blocks or *single line* blocks with `=>`.
```
if true => println("if")
else if false => println("else if")
else => println("else")
```


## `is` operator

The binary operator `is` can check if the `enum` value of the expression on the left is an instance of its variant specified on the right. The expression must produce an `enum` value and the variant specified must be a variant of aforementioned `enum` type produced by the expression.
```
enum OffsetKind {
    Absolute
    Relative
}

let offset: OffsetKind = .Relative
assert(offset is Relative)
```

When used in the condition of an `if-else` expression the `is` operator may provide a name to bind the narrowed variant to for the duration of the associated body. This new name, like other bindings, may shadow existing binding names.
```
enum Color {
    Rgb { r: u8, g: u8, b: u8 }
    Hsl { hue: u8, saturation: u8, lightness: u8 }
}

let color: Color = .Rgb { r: 50, g: 40, b: 30 }
if color is color: Rgb {
    assert(color.g == 40)
}
```

If the left hand expression is a binding read and no new binding name is specified on right, then the compiler will create a new binding with the same name as the original expression, but with the narrowed variant type.
```
let color: Color = .Rgb { r: 50, g: 40, b: 30 }
if color is Rgb {
    assert(color.b == 30)
}
```

When using the `is` operator to re-bind a narrowed `enum` value an `and` operator may be used to chain expressions, and later expression in the chain may reference the new binding which prior `is` operators in the chain have created.
```
let color: Color = .Rgb { r: 50, g: 40, b: 30 }
if color is Rgb and color.b < 50 {
    assert(color.r == 50)
}
```

An `enum`'s shared fields may be accessed both before and after narrowing with `is`.
```
enum Color
    r: u8
    g: u8
    b: u8

    Rgb
    Rgba { a: u8 }
}

let color: Color = .Rgba { r: 60, g: 50, b: 40, a: 30 }
assert(color.g == 50)

if color is Rgba {
    assert(color.b == 40)
    assert(color.a == 30)
}
```

## `match` expression

A `match` expression selects a branch, known as an arm, based on which `enum` variant the condition expression produces. Arms do not fall through to other arms. It is an expression as it may be used as a block expression, see [[#Block expressions and `yield`]] for more.
```
enum OffsetKind {
    Absolute
    Relative
}

match function_returning_offset_kind() {
    Absolute {
        println("Absolute offset")
        foo()
    }

    Relative {
        println("Relative offset")
        bar()
    }
}
```

An arm may specify multiple variants to match on.
```
enum Type {
    String
    Integer
    Float
}

match function_returning_type() {
    String {
        println("It is a String")
    }

    Integer, Float {
        println("It is some kind of number")
    }
}
```

A `match` expression is required to cover all variants of the `enum` it is matching on. In cases where only a subset of variants need to be checked, an `else` arm will catch all missed variants.
```
enum Animal {
    Cat
    Dog
    Hampster
    Snake
}

match function_returning_animal() {
    Cat {
        println("It is a cat")
    }

    Hampster {
        println("It is a hampster")
    }

    else {
        println("It is some kind of animal")
    }
}
```



## `while` loops

A `while` loop repeatedly executes the block while the condition continues to evaluate to `true`.
```
while true {
    println("Printing forever")
}

mut index: isize = 0
while index < 5 {
    println(f"{index}")
    index += 1
}

/* Prints the following
0
1
2
3
4
*/
```

The `while` condition may use the `is` binary operator to bind a value for the duration of the block.
```
// Note: The Fae standard library does not yet have a hashmap
// this is merely a contrived example

mut iterator = hashmap.value_iterator()
while iterator.next() is value: Some {
    println(f"{value}")
}
```


## `for` loops

A `for` loop iterates over a `Range` or slice value, binding the current item to a name.
```
for item in 0..10 {
    println(f"{item}")
}

/* Prints the following
item: 0
item: 1
item: 2
item: 3
item: 4
*/
```

Like all other constructs which can bind, the name may be omitted by using an underscore instead.
```
for _ in 0..10 {
    println("Prints ten times")
}
```

A second name may be provided with a comma which will contain the iteration index.
```
for item, iteration in 10..15 {
    println(f"item: {item}, iteration: {iteration}")
}

/* Prints the following
item: 10, iteration 0
item: 11, iteration 1
item: 12, iteration 2
item: 13, iteration 3
item: 14, iteration 4
*/
```

A third name may be provided with a comma which will contain a `bool` value indicating if the current item is the *last* item in the range or slice being iterated.
```
for _, _, is_last in 0..5 {
    println(f"{is_last}")
}

/* Prints the following
false
false
false
false
true
*/
```

When iterating over a slice, the items may be accessed either by value or reference by using the `in` or `of` keywords respectively. Iterating with `in` will *copy* the item onto the stack and bind it to the item name specified.
```
let slice = []{ true, false }
for item in slice {
    println(f"{item}")
}

/* Prints the following
true
false
*/
```

However iterating with `of` will bind a pointer to the *original* pointed value to the name, allow the loop body to mutate the items in the slice.
```
let slice = []{ true, false }
for item of slice {
    item.* = false
}

for item in slice {
    println(f"{item}")
}

/* Prints the following
false
false
*/
```


## Block expressions and `yield`

Code blocks, `if-else`, and `match` may be used as "block expressions", which requires all codepaths in the block expression to `yield` a value which is what the block expression produces, or exit the function with `return`, or `break`/`continue` a loop *containing* the block expression.
```
let value: i64 = {
    yield 4321
}
assert(value == 4321)

let value = if condition() {
    yield "one"
} else {
    yield "two"
}

let data = []i32 { /* ... */ }
let value = match calculate_index() {
    index: Some => yield data[index]
    None => return
}

for index in 0..10 {
    let doubled_even = if index % 2 == 0 {
        yield index * 2
    } else => continue
}
```


## `defer` statement

The `defer` statement will execute the specified statement at any exit point of the block the `defer` statement was defined in. These exit points include `return`, `break`, `continue`, and the end of the block.
```
defer println("1")

{
    defer println("2")
}

defer println("3")

/* Prints the following
2
3
1
*/
```

Defer-ed statements are executed in *reverse* order to their definitions.
```
defer println("1")
defer println("2")
defer println("3")

/* Prints the following
3
2
1
*/
```


## Symbol importing

An `import` statement may import one or more symbols from a file specified by the module path.
```
// Import `Symbol` from a file at the path "./path/to/file.fae"
// relative to the root of the project source directory
import path::to::file::Symbol

// Import `AnotherSymbol` from "./another.fae"
import another::AnotherSymbol

// Import `YetAnother` from "./module/module.fae"
// Note that the file becomes the folder it shares
// the name of, such that the module path is not
// written like `module::module::YetAnother`
import module::YetAnother

// Import `A`, `B` and `C` from "./some/path.fae"
import some::path::A, B, C

// Import `List<T>` from the standard library
import fae::collections::list::List
```

An `import` statement may also import a module itself so it may be referenced by name without the rest of the module path preceding the module in question.
```
import fae::collections::list

// ...

let _ = list::List.new()
```

**Note**: It is planned to provide the ability to import everything from within a module path with a glob import.


## Static variables

A `static` has the following form.
```
extern "LinkName"
static StaticName: Type
```

**Note**: Due to a temporary compiler limitation it is not possible to provide an initial value for a static.


## Functions

Functions take the following forms and can be defined in any block, including within other functions, and can inherit the generic type parameters from any outer function.
```
// `void` function taking no arguments
fn function_name() {
    // Block body
}

// Function returning a value of `ReturnType`
fn returns_something(): ReturnType {}

// Function taking two `i32` arguments and returning a new `i32`
fn multiply_i32(a: i32, b: i32): i32 {
    return a * b
}

// Generic function accepting a value of any type and returning it
generic T
fn identity(value: T): T {
    return value
}

// Generic function accepting two arguments, of different generic
// types, and returning the second argument
generic A, B
fn returns_second_argument(a: A, b: B): B {
    return b
}

// Generic function containing another function which inherits
// the outer function's generic type parameters, allowing it
// to know about `T` and handle the type accordingly
generic T
fn push_four_times(list: &mut List<T>, value: T) {
    fn push_twice(list: &mut List<T>, value: T) {
        list.push(value)
        list.push(value)
    }

    push_twice(list, value)
    push_twice(list, value)
}
```

**Note**: It is planned to add traits to the language, extending what generic code can achieve. For example, while the current semantics allow for creating collections such as a type safe heap allocated `List<T>` they are not yet sufficient to create a `HashMap<K, V>`.

By prepending with a `method` annotation, a function may be placed as methods on structs and enums within the same scope. These methods are allowed to access `internal` fields and mutate `readable` fields. When the struct or enum has generic type parameters, the method automatically inherits these type parameters.
```
generic T
struct MyType {
    number: f64 readable
    field: T readable
}

// Static methods are called on the type itself
// rather than on instances of the type
method static MyType
fn new(number: f64, field: T): MyType<T> {
    return MyType<T> { number, field }
}

// "mut" methods may modify the `self` value
method mut MyType
fn increment() {
    self.number += 1.1
}

// Methods inherit the generic type parameters
// of their `self` type
method mut MyType
fn replace_field(value: T) {
    self.field = value
}

// Non-"mut" methods may *not* modify the
// `self` value
method MyType
fn print() {
    println(f"number: {self.number}")
}

// ...

mut value = MyType<bool>.new(41, false)
value.increment()
value.replace_field(true)
value.print() // "number: 42.1"
println(f"field: {value.field}") // "true"
```

Methods may also be added to a struct or enum defined in another module or scope. These methods are known as extension methods and may *not* access `internal` fields or mutate `readable` fields.
```
import fae::collections::list::List

method mut List
fn push_twice(value: T) {
    self.push(value)
    self.push(value)
}

// ...

mut list = List<str>.new()
list.push_twice("Hello")
for string in list.items => println(string)

/* Prints the following
Hello
Hello
*/
```

**Note**: It is planned to add named parameters to the language, which would allow a function to *require* the call site to spell out a parameter's name similar to how struct field initialization works.


## Type coercion

Fae includes a minimal set of type coercion rules, implemented with two algorithms used in different circumstances. Conversions of untyped numerals to a concrete "runtime" numeric type will throw a compile time error if the target type cannot represent the untyped numeral without a loss of precision.
#### Collapse fair
All binary operations, except for for range initialization, run the "collapse fair" algorithm on their left and right expressions. It consists of the following rules.
 - If both are of the same type, do nothing and return
 - If either type is an untyped number we know the other isn't, collapse to the other type
 - If either type is a pointer, collapse the other to it, preferring to collapse mutable to immutable
 - If either type is a slice, collapse the other to it, preferring to collapse mutable to immutable
 - If either type is an enum, collapse the other to it

#### Collapse to
Used by the "collapse fair" algorithm, when passing an argument to a parameter, initializing a field with a value, ect, It consists of the following rules.
 - If the expression is of the desired type, do nothing and return
 - Otherwise attempt the following conversions in order
     - untyped number -> signed of large enough if whole number or unsigned of large enough if not negative whole number or float of large enough
     - mutable reference -> immutable reference
     - mutable slice -> immutable slice
     - str -> fstr
     - enum variant -> enum


## Prelude

The Fae prelude is a module in the standard library which the contents of are automatically imported into all Fae modules by default.

**Get a type's size** with a compiler intrinsic
```
generic T
fn size_of(): isize
```

**Get a type's alignment** with a compiler intrinsic
```
generic T
fn alignment_of(): isize
```

**Create a null pointer**
```
generic T
fn null_pointer(): &mut T
```

**Create a null slice**
```
generic T
fn null_slice(): []mut T
```

**Offset a pointer by specified signed byte distance**
```
generic T
fn offset_pointer(pointer: &T, by_bytes: isize): &T
```

**Offset a mutable pointer by specified signed byte distance**
```
generic T
fn offset_pointer_mut(pointer: &mut T, by_bytes: isize): &mut T
```

**Print a formatted message to stdout**
```
fn print(message: fstr)
```

**Print a formatted message and a newline to stdout**
```
fn println(message: fstr)
```

**Print a formatted message to stderr**
```
fn eprint(message: fstr)
```

**Print a formatted message and a newline to stderr**
```
fn eprintln(message: fstr)
```

**Terminate the application immediately** _with_ a stack trace
```
fn panic(): noreturn
```

**Print a formatted _failure_ message to stderr and terminate the application** _with_ a stack trace
```
fn panicf(message: fstr): noreturn
```

**Terminate the application** with a success code _without_ a stack trace
```
fn exit_success(): noreturn
```

**Terminate the application** with a failure code _without_ a stack trace
```
fn exit_error(): noreturn
```

**Print a formatted _user error_ message to stderr and terminate the application** _without_ a stack trace
```
fn exit_errorf(message: fstr): noreturn
```

**If the value is false, terminate the application**, included in all build configurations
```
fn assert(value: bool)
```

**If the value is false, print a formatted message to stderr and terminate the application**, included in all build configurations
```
fn assertf(value: bool, message: fstr)
```

**Range type** produced by range initialization binary operator and used to slice index on slices
```
struct Range {
    start: isize
    end: isize
}
```

**Optional type** modeling optionality of a value
```
generic T
enum Option {
    None
    Some(T)
}

method Option
fn unwrap(): T
```


## Intrinsic functions

The Fae compiler implements a number of functions "built-into" the compiler to generate specific data or behavior. Unless otherwise specified, all listed intrinsics are located in the standard library module `fae::intrinsics`.

**Get a type's size**
```
// Located in prelude, automatically available in all modules
generic T
fn size_of(): isize
```

**Get a type's alignment**
```
// Located in prelude, automatically available in all modules
generic T
fn alignment_of(): isize
```

**Create slice** from pointer and length
```
generic T
fn create_slice(pointer: &T, length: isize): []T
```

**Create mutable slice** from mutable pointer and length
```
generic T
fn create_slice_mut(pointer: &mut T, length: isize): []mut T
```

**Create str** from pointer and length
```
fn create_str(pointer: &u8, length: isize): str
```

**Create non-null invalid pointer** for "pointing to" a zero size "value"
```
generic T
fn create_non_null_invalid_pointer(): &mut T
```

**Create non-null invalid slice** for a "slice of" zero size "values"
```
generic T
fn create_non_null_invalid_slice(length: isize): []mut T
```

**Trigger debugger breakpoint** if debugger attached; if no debugger attached do nothing or crash depending on the platform.
```
fn debugger_break()
```

**Call the user defined application main function** without needing to know the module path
```
fn user_main_function()
```

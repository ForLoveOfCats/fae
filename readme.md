# Changes since last video:
 - Lots of generic type system bug fixes
 - Overhaul of how the compiler detects recursive types
 - Compiler now knows/calculates the size and alignment of all types in the program
 - Zero size type support
 	- `void` is now an actual unit value
 - Primative numeric and pointer casts
 - Pointer dereference (read and assign)
 - Boolean values
 	- Boolean literals
  	- Comparison operators (very naive typechecking)
   	- Logical `and` and `or` (thinking about moving to `&&` and `||` instead)
 - Slice indexing (get value at index and assign value at index)
 	- Generic type syntax changed (slight parsing hack)
  	- Extremely primative runtime bounds checking (to be superseded by in-language error handling)
 - Slice type syntax changed
 - Array literals (currently only give a slice of the values rather than the actual array)
 - Extremely basic flow control with `if` (missing `else if` and `else`, struct literals are a bit weird)
 - A whole slew of new tests & test infrastructure improvements
 	- Previously tests could either generate expected errors or were expected to run to completion, now they can include expected stdout/stderr text which the resulting binary must output to pass
  	- Tests can now expect to panic (to test things like runtype bounds checking)
 - Started to tinker with a custom SSA representation


# Short-medium term plans:
 - Custom SSA representation
 - Beginnings of a custom optimization pass
 - Custom AMD64 codegen
 - Rip out existing C codegen (maybe replace with new one based on the SSA representation)
 - More control flow
    - Enum variant flow typing
    - Loops with `break` and `continue`
    - Actual block expression type inferrence and value returning
 - Infer generic arguments from actual function arguments
 - Methods
 - Visibility modifiers for methods and fields
 - Enums
    - Independent variant types
    - Flow control based typing

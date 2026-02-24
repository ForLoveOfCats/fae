## Standard Library Changes
- Renamed module `fae.primative` to `fae.primitive`
- Added the following methods to `str`
  - `starts_with(other=: str): bool`
- Renamed the following methods on `fae.os.arguments.Arguments`
  - `parse_user_arguments` became `load_user_arguments`
  - `parse_all_arguments` became `load_all_arguments`
- Added new global variable `UserArgs: fae.os.arguments.Argument` to `fae.runtime`
- Added new type `fae.os.arguments.ArgumentsParser` with the following methods
  - `static new(program: str, version: str): ArgumentsParser`
  - `free()`
  - `mut bool_flag(name=: str, ptr=: *mut bool)`
  - `mut inverted_bool_flag(name=: str, ptr=: *mut bool)`
  - `mut i64_flag(name=: str, ptr=: *mut i64)`
  - `mut parse()`

## Standard Library Bugfixes

## New Language Changes

## Compiler Usability Improvements

## Compiler Bugfixes
- Fixed a bug where single-line braceless blocks could be parsed with a garbage token

## VSCode Extension

## Detailed Discussion

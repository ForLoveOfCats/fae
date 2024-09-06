# Fae, a friendly and useful systems language

Fae is a relatively small, unsafe, systems programming language with a focus on utility, comfort, and tooling. It steals with flattery from such languages as Rust, Zig, Odin, and Swift. Read the [language reference](./docs/language_reference.md) for a detailed snapshot of the *current* status of the Fae language.

 - **Small Yet Comfortably Useful**: Language features are chosen in an attempt to maximize the ratio of utility to language complexity while remaining pleasant to write and reason about
 - **Unsafe**: Fae is an unsafe language and allows for deep developer control over memory at the cost of compile time safety guarantees
 - **High Quality Tooling**: A language is only half the experience; having fast, accurate, and pleasant tooling is crucial

```
>>> cat main.fae
import fae::collections::list::List

fn main() {
    mut list = List<str>.new()
    defer list.free()

    list.push("Hello")
    list.push("World!")

    for item, _, is_last in list.items {
        print(item)
        if is_last.! => print(", ")
    }
    println("")
}

>>> fae run --quiet
Hello, World!
```

```
>>> cat main.fae
fn main() {
    println(f"Wecome to {language()}!")
}

fn language(): str {
    return "Fae"
}

>>> fae run --quiet
Welcome to Fae!
```

Browse a selection of example programs [here](./examples)

## Language details

The Fae compiler and standard library are *very* incomplete, missing features include traits, allocator system, error system, unions, and much more.

Read the [language reference](./docs/language_reference.md) for a detailed snapshot of the *current* status of the Fae language.

## Installation

The Fae compiler is immature and incomplete but it can be installed and experimented with today. Here there be dragons, *please open an issue* when you encounter bugs!

- **Linux x64**: Current primary supported platform, binaries available on the [releases](https://github.com/ForLoveOfCats/fae/releases) page.
    Statically linked compiler, should run on any recent Linux system
    By default expects GNU `ld` to be in the path
- **macOS AArch64**: Secondary supported platform, binaries available on the [releases](https://github.com/ForLoveOfCats/fae/releases) page.
    Tested on macOS 12-14 (may work on 11)
    By default expects platform shipped `ld` to be in the path
- **Windows x64**: Untested and unsupported as of yet. *Extremely high priority* to support in the near future.

These are the core set of intended supported platforms. Supporting each of these operating systems on the opposite architectures is a goal but lower priority at present.

Primitive syntax support for VSCode is available with [this extension](https://marketplace.visualstudio.com/items?itemName=fae-lang.vscode-fae), though it lacks code formatting and language server support as those components remain to be built.

## Licenses

The Fae compiler, associated tests, scripts, and supporting infrastructure code, *excluding* the standard library, are licensed under the 3-Clause BSD license.

The Fae standard library, *including* portions required for proper compiler function, are licensed under the Unlicense license for maximally unencumbered distribution of applications built with Fae and including portions of the standard library.
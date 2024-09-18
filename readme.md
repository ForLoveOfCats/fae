# Fae, a friendly and useful systems language

Fae is an unsafe systems programming language with a focus on utility, comfort, and tooling. It steals with flattery from such languages as Rust, Zig, Odin, and Swift. Read the [language reference](./docs/language_reference.md) for a detailed snapshot of the *current* status of the Fae language.

 - **Comfortably Useful**: Language features are chosen in an attempt to maximize the ratio of utility to language complexity while remaining pleasant to write and reason about
 - **Unsafe**: Fae is an unsafe language and allows for deep developer control over memory at the cost of compile time safety guarantees
 - **High Quality Tooling**: A language is only half the experience; having fast, accurate, and pleasant tooling is crucial

```
>>> cat main.fae
import fae::collections::list::List

fn main() {
    mut list = List<str>.new()
    defer list.free()

    list.push("Hello")
    list.push("There!")

    for item, _, is_last in list.items {
        print(item)
        if is_last.! => print(", ")
    }
    println("")
}

>>> fae run --quiet
Hello, There!
```

```
>>> cat main.fae
fn main() {
    println("Hello world")
    println(f"Welcome to {language()}!")
}

fn language(): str {
    return "Fae"
}

>>> fae run --quiet
Hello world
Welcome to Fae!
```

Browse a selection of example programs [here](./examples)

## Language details

The Fae compiler and standard library are *very* incomplete, missing features include traits, allocator system, error system, unions, and much more.

Read the [language reference](./docs/language_reference.md) for a detailed snapshot of the *current* status of the Fae language.

## Community

Join the Fae project [Discord server](https://discord.gg/uAufKTVYeB) to participate in discussion and ask questions. While not ideal, Discord was chosen to maximize approachability and accessiblity.

## Installation

The Fae compiler is immature and incomplete but it can be installed and experimented with today. Here there be dragons, *please open an issue* when you encounter bugs!

- **Linux x64**: Current primary supported platform, binaries available on the [releases](https://github.com/ForLoveOfCats/fae/releases) page.
    <br/>Statically linked compiler, should run on any recent Linux system
    <br/>By default expects GNU `ld` to be in the path
- **macOS AArch64**: Secondary supported platform, binaries available on the [releases](https://github.com/ForLoveOfCats/fae/releases) page.
    <br/>Tested on macOS 12-14 (may work on 11)
    <br/>By default expects platform shipped `ld` to be in the path
- **Windows x64**: Untested and unsupported as of yet. *Extremely high priority* to support in the near future.

These are the core set of intended supported platforms. Supporting each of these operating systems on the opposite architectures is a goal but lower priority at present.

Primitive syntax support for VSCode is available with [this extension](https://marketplace.visualstudio.com/items?itemName=fae-lang.vscode-fae), though it lacks code formatting and language server support as those components remain to be built.

## Building

See the [Build Instructions](./docs/build_instructions.md) for directions on how to get up and running with a development build of the Fae language toolchain.

## Licenses

The Fae compiler, associated tests, scripts, and supporting infrastructure code, *excluding* the standard library, are licensed under the 3-Clause BSD license.

The Fae standard library, *including* portions required for proper compiler function, are licensed under the Unlicense license for maximally unencumbered distribution of applications built with Fae and including portions of the standard library.

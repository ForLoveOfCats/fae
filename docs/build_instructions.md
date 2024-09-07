# Building

## Supported platforms

The Fae compiler and build system currently supports being built on Linux AMD64 and macOS AArch64 *without* cross compilation. Attempting to build the compiler for any other platform or cross compiling the compiler for a supported platform will not succeed.


## Instructions

The Fae compiler relies on LLVM and so it must be fetched and built before attempting to build the compiler itself. Run the `fetch_llvm.py` script from within the *root* of the Fae toolchain project. This will clone and build the nessesary components required to build the Fae compiler.
```
./meta/fetch_llvm.py
```

Once that has completed, the Fae compiler may be built and run like any other Rust `cargo` project. For example, this invocation in the *root* of the Fae toolchain project will build the compiler and ask it to print its help message.
```
cargo run -- --help
```


## Creating a distributable bundle

To create a bundle of the Fae compiler, standard library, documentation, and licenses the `bundle.py` script may be run from within the *root* of the Fae toolchain project. It will produce a platform appropriate bundle.
```
./meta/bundle.py
```

On macOS the `bundle.py` script may be called with `--identity-uuid` and `--notarization-keychain-profile`, passing approprate arguments, to sign and noterize the `.dmg` bundle respectively.


## Development tips

The Fae compiler includes a number of command line options normally hidden in bundled builds which are useful for testing and debugging the compiler. Most importantly the `ct` compiler command from the *root* of the Fae toolchain project will run the compiler test suite. All PRs to the Fae project are expected to pass this test suite as a bare minimum.
```
cargo run -- ct
```

Run the `--help` command on a local compiler build to read the other options.
```
cargo run -- --help
```

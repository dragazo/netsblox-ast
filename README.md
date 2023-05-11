# NetsBlox-AST

**For the time being, this crate is in active development and its API is highly unstable**

[NetsBlox](https://netsblox.org/) is a block-based programming language (extended from [Snap!](https://snap.berkeley.edu/)) which provides access to advanced web and distributed computing capabilities such as accessing web APIs and sending messages between clients (e.g. to create multi-user programs). NetsBlox-AST is a rust crate that takes as input a NetsBlox project XML file and outputs a sanitized Abstract Syntax Tree, which is useful for code generation.

## Development

This crate is intended to be used directly on some embedded platforms.
Thus, because the parser is recursive, we need to keep the stack size to an absolute minimum.
To do this, we use `cargo-call-stack` to check call stack usage.

For the time being, you should install from [this fork](https://github.com/dragazo/cargo-call-stack).

You'll also need `dot` from `graphviz`.

```bash
sudo apt install graphviz
```

To generate the call graph, run the following commands.

```bash
RUSTFLAGS="-C embed-bitcode" cargo +nightly call-stack --bin netsblox_ast --target x86_64-unknown-linux-gnu >cg.dot
dot -Tsvg cg.dot > cg.svg
```

#![forbid(unsafe_code)]
#![no_std]

#[macro_use] extern crate alloc;

pub use compact_str::{self, CompactString, format_compact};

mod ast;
mod rpcs;
pub mod util;

#[cfg(test)]
mod test;

pub use ast::*;

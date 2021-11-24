#![forbid(unsafe_code)]

#[macro_use] extern crate lazy_static;

mod ast;
pub mod util;

#[cfg(test)]
mod test;

pub use ast::*;

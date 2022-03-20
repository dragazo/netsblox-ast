use std::fmt::{self, Debug, Display};
use regex::Regex;

pub struct Punctuated<'a, T: Iterator + Clone>(pub T, pub &'a str);
macro_rules! impl_punctuated {
    ($($req:ident => $fmt:literal),*$(,)?) => {$(
        impl<'a, T: Iterator + Clone> $req for Punctuated<'a, T> where <T as Iterator>::Item: $req {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let mut vals = self.0.clone();
                if let Some(first) = vals.next() {
                    write!(f, $fmt, first)?;
                    for rest in vals {
                        write!(f, concat!("{}", $fmt), self.1, rest)?;
                    }
                }
                Ok(())
            }
        }
    )*}
}
impl_punctuated! { Debug => "{:?}", Display => "{}" }

/// Returns a new string which is indented by 4 spaces.
pub fn indent(code: &str) -> String {
    Punctuated(code.lines().map(|s| format!("    {}", s)), "\n").to_string()
}
#[test]
fn test_indent() {
    assert_eq!(indent(""), "");
    assert_eq!(indent("hello"), "    hello");
    assert_eq!(indent("hello\nworld"), "    hello\n    world");
}

/// Returns a new string which encodes special characters as the typical backslash escape sequences.
/// Notably, this includes single and double quotes, so you can safely translate a string literal by wrapping the result in quotes.
pub fn escape(raw: &str) -> String {
    let mut res = String::with_capacity(raw.len());
    for c in raw.chars() {
        match c {
            '\"' => res += "\\\"",
            '\\' => res += "\\\\",
            '\'' => res += "\\'",
            '\n' => res += "\\n",
            '\r' => res += "\\r",
            '\t' => res += "\\t",
            _ => res.push(c),
        }
    }
    res
}
#[test]
fn test_escape() {
    assert_eq!(escape("hello world"), "hello world");
    assert_eq!(escape("hello\n\r\t\\'\"world"), "hello\\n\\r\\t\\\\\\'\\\"world");
}

pub fn normalize_space(raw: &str) -> String {
    let mut res = String::new();
    let mut chars = raw.trim().chars();
    while let Some(c) = chars.next() {
        if c.is_whitespace() {
            res.push(' ');
            for cc in chars.by_ref() {
                if !cc.is_whitespace() {
                    res.push(cc);
                    break
                }
            }
        }
        else { res.push(c) }
    }
    res
}
#[test]
fn test_normalize_space() {
    assert_eq!(normalize_space(" \t  hello \r\n \r\n\n \t\t   \t world \t\t  "), "hello world");
}

/// Converts a Snap! identifier into a valid C-like identifier.
pub fn c_ident(raw: &str) -> Result<String, ()> {
    lazy_static! {
        static ref INVALID_CHAR: Regex = Regex::new(r"[^_a-zA-Z0-9]+").unwrap();
    }
    let res = INVALID_CHAR.replace_all(raw.trim(), " ").trim().replace(' ', "_");
    match res.chars().next() {
        None => Err(()),
        Some(v) => Ok(if ('0'..='9').contains(&v) { format!("var_{}", res) } else { res })
    }
}
#[test]
fn test_c_ident() {
    assert_eq!(c_ident("foo").unwrap(), "foo");
    assert_eq!(c_ident("foo!").unwrap(), "foo");
    assert_eq!(c_ident("foo[]").unwrap(), "foo");
    assert_eq!(c_ident("(foo)").unwrap(), "foo");
    assert_eq!(c_ident(" (foo) ").unwrap(), "foo");
    assert_eq!(c_ident(" (foo-bar) ").unwrap(), "foo_bar");
    assert_eq!(c_ident(" (foo    bar 27) ").unwrap(), "foo_bar_27");
    assert_eq!(c_ident(" (foo bar*} 27)[]{} ").unwrap(), "foo_bar_27");
    assert_eq!(c_ident(" ( foo bar*} 27)[]{} ").unwrap(), "foo_bar_27");
    assert_eq!(c_ident(" ( foo ba*[]}r*} 27)[]{} ").unwrap(), "foo_ba_r_27");
    assert_eq!(c_ident("foo's parent").unwrap(), "foo_s_parent");
    assert_eq!(c_ident("6foo").unwrap(), "var_6foo");
    assert_eq!(c_ident("[6foo").unwrap(), "var_6foo");
    assert_eq!(c_ident("[ 6foo").unwrap(), "var_6foo");
}

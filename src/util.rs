use core::fmt::{self, Debug, Display};
use alloc::string::ToString;

#[cfg(test)]
use proptest::prelude::*;

use crate::*;

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
pub fn indent(code: &str) -> CompactString {
    Punctuated(code.lines().map(|s| format!("    {}", s)), "\n").to_string().into()
}
#[test]
fn test_indent() {
    assert_eq!(indent(""), "");
    assert_eq!(indent("hello"), "    hello");
    assert_eq!(indent("hello\nworld"), "    hello\n    world");
}

/// Returns a new string which encodes special characters as the typical backslash escape sequences.
/// Notably, this includes single and double quotes, so you can safely translate a string literal by wrapping the result in quotes.
pub fn escape(raw: &str) -> CompactString {
    let mut res = alloc::string::String::with_capacity(raw.len());
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
    res.into()
}
#[test]
fn test_escape() {
    assert_eq!(escape("hello world"), "hello world");
    assert_eq!(escape("hello\n\r\t\\'\"world"), "hello\\n\\r\\t\\\\\\'\\\"world");
}

pub fn normalize_space(raw: &str) -> CompactString {
    let mut res = CompactString::default();
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
pub fn c_ident(raw: &str) -> Result<CompactString, ()> {
    let cleaned: CompactString = raw.chars().map(|ch| match ch {
        '_' | 'a'..='z' | 'A'..='Z' | '0'..='9' => ch,
        _ => ' ',
    }).collect();
    let res: CompactString = Punctuated(cleaned.split_ascii_whitespace(), "_").to_string().into();
    match res.chars().next() {
        None => Err(()),
        Some(v) => Ok(if ('0'..='9').contains(&v) { format!("var_{}", res).into() } else { res })
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

// source: https://docs.babelmonkeys.de/RustyXML/src/xml/lib.rs.html#41-55
#[inline(never)]
pub fn xml_escape(input: &str) -> CompactString {
    let mut result = alloc::string::String::with_capacity(input.len());
    for c in input.chars() {
        match c {
            '&' => result.push_str("&amp;"),
            '<' => result.push_str("&lt;"),
            '>' => result.push_str("&gt;"),
            '\'' => result.push_str("&apos;"),
            '"' => result.push_str("&quot;"),
            o => result.push(o),
        }
    }
    result.into()
}

#[inline(never)]
pub fn xml_unescape(input: &str) -> Result<CompactString, XmlError> {
    let mut result = alloc::string::String::with_capacity(input.len());

    let mut chars = input.char_indices().fuse();
    while let Some((start, start_ch)) = chars.next() {
        match start_ch {
            '&' => match chars.clone().skip_while(|(_, x)| x.is_ascii_digit() || x.is_ascii_alphabetic() || *x == '#').next() {
                Some((stop, stop_ch)) if stop_ch == ';' => {
                    match &input[start + 1..stop] {
                        "quot" => result.push('"'),
                        "apos" => result.push('\''),
                        "gt" => result.push('>'),
                        "lt" => result.push('<'),
                        "amp" => result.push('&'),
                        ent => {
                            let val = if ent.starts_with("#x") {
                                u32::from_str_radix(&ent[2..], 16).ok()
                            } else if ent.starts_with('#') {
                                u32::from_str_radix(&ent[1..], 10).ok()
                            } else {
                                None
                            };
                            match val.and_then(char::from_u32) {
                                Some(c) => result.push(c),
                                None => return Err(XmlError::IllegalSequence { sequence: format_compact!("&{};", ent) }),
                            }
                        }
                    }
                    while let Some((pos, _)) = chars.next() {
                        if pos == stop { break }
                    }
                }
                _ => result.push(start_ch),
            }
            _ => result.push(start_ch),
        }
    }

    Ok(result.into())
}

#[cfg(test)]
proptest! {
    #[test]
    fn test_xml_enc_dec(raw in r".*") {
        let encoded = xml_escape(&raw);
        let back = xml_unescape(&encoded).unwrap();
        prop_assert_eq!(raw, back);
    }
}

#[test]
fn test_xml_dec() {
    assert_eq!(xml_unescape("hello world").unwrap(), "hello world");
    assert_eq!(xml_unescape("hello &quot; world").unwrap(), "hello \" world");
    assert_eq!(xml_unescape("hello &apos; world").unwrap(), "hello ' world");
    assert_eq!(xml_unescape("hello &gt; world").unwrap(), "hello > world");
    assert_eq!(xml_unescape("hello &lt; world").unwrap(), "hello < world");
    assert_eq!(xml_unescape("hello &amp; world").unwrap(), "hello & world");
    assert_eq!(xml_unescape("hello &#63; world").unwrap(), "hello ? world");
    assert_eq!(xml_unescape("hello &#x3f; world").unwrap(), "hello ? world");
    assert_eq!(xml_unescape("hello &#x3F; world").unwrap(), "hello ? world");
    assert_eq!(xml_unescape("hello &#126; world").unwrap(), "hello ~ world");
    assert_eq!(xml_unescape("hello &#126; world&#126").unwrap(), "hello ~ world&#126");
    assert_eq!(xml_unescape("hello &#126; world&#126;").unwrap(), "hello ~ world~");
    assert_eq!(xml_unescape("hello &#x7e; world").unwrap(), "hello ~ world");
    assert_eq!(xml_unescape("hello &#x7e; world&#x7e").unwrap(), "hello ~ world&#x7e");
    assert_eq!(xml_unescape("hello &#x7e; world&#x7e;").unwrap(), "hello ~ world~");
    assert_eq!(xml_unescape("hello & ;world").unwrap(), "hello & ;world");
    assert_eq!(xml_unescape("hello & world").unwrap(), "hello & world");
    assert_eq!(xml_unescape("hello && world").unwrap(), "hello && world");
    assert_eq!(xml_unescape("hello &&& world").unwrap(), "hello &&& world");
    assert_eq!(xml_unescape("he&llo &&& world").unwrap(), "he&llo &&& world");
    assert_eq!(xml_unescape("he&llo &&& world&").unwrap(), "he&llo &&& world&");
    assert_eq!(xml_unescape("&he&llo &&& world&").unwrap(), "&he&llo &&& world&");
    assert_eq!(xml_unescape("&&he&llo &&& world&").unwrap(), "&&he&llo &&& world&");
    assert_eq!(xml_unescape("&&he&llo &&& world&&").unwrap(), "&&he&llo &&& world&&");
}

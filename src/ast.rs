use alloc::rc::Rc;
use alloc::vec::Vec;
use alloc::boxed::Box;
use alloc::borrow::ToOwned;
use core::{mem, iter, fmt};

use compact_str::CompactString;

use base64::engine::Engine as Base64Engine;
use base64::DecodeError as Base64Error;

use crate::rpcs::*;

#[cfg(test)]
use proptest::prelude::*;

fn base64_decode(content: &str) -> Result<Vec<u8>, Base64Error> {
    base64::engine::general_purpose::STANDARD.decode(content)
}

trait BoxExt<T> {
    fn new_with<F: FnOnce() -> T>(f: F) -> Self;
    fn try_new_with<E, F: FnOnce() -> Result<T, E>>(f: F) -> Result<Self, E> where Self: Sized;
}
impl<T> BoxExt<T> for Box<T> {
    #[inline(never)]
    fn new_with<F: FnOnce() -> T>(f: F) -> Self {
        Box::new(f())
    }
    #[inline(never)]
    fn try_new_with<E, F: FnOnce() -> Result<T, E>>(f: F) -> Result<Self, E> {
        f().map(Box::new)
    }
}

trait VecExt<T> {
    fn push_boxed(&mut self, value: Box<T>);
    fn push_with<F: FnOnce() -> T>(&mut self, f: F);
}
impl<T> VecExt<T> for Vec<T> {
    #[inline(never)]
    fn push_boxed(&mut self, value: Box<T>) {
        self.push(*value);
    }
    #[inline(never)]
    fn push_with<F: FnOnce() -> T>(&mut self, f: F) {
        self.push(f());
    }
}

// regex equivalent: r"%'([^']*)'"
struct ParamIter<'a>(iter::Fuse<core::str::CharIndices<'a>>);
impl<'a> ParamIter<'a> {
    fn new(src: &'a str) -> Self {
        Self(src.char_indices().fuse())
    }
}
impl Iterator for ParamIter<'_> {
    type Item = (usize, usize);
    fn next(&mut self) -> Option<Self::Item> {
        while let Some((i, ch)) = self.0.next() {
            if ch != '%' || self.0.next().map(|x| x.1) != Some('\'') { continue }
            while let Some((j, ch)) = self.0.next() {
                if ch == '\'' { return Some((i, j + 1)) }
            }
        }
        None
    }
}
#[test]
fn test_param_iter() {
    assert_eq!(ParamIter::new("hello world").collect::<Vec<_>>(), vec![]);
    assert_eq!(ParamIter::new("hello %'helo' world").collect::<Vec<_>>(), vec![(6, 13)]);
    assert_eq!(ParamIter::new("hello %'helo'world").collect::<Vec<_>>(), vec![(6, 13)]);
    assert_eq!(ParamIter::new("hello %'heloworld").collect::<Vec<_>>(), vec![]);
    assert_eq!(ParamIter::new("hello %'helo' %'world''''").collect::<Vec<_>>(), vec![(6, 13), (14, 22)]);
}

// regex equivalent: r"%\S*"
struct ArgIter<'a>(iter::Fuse<core::str::CharIndices<'a>>, usize);
impl<'a> ArgIter<'a> {
    fn new(src: &'a str) -> Self {
        Self(src.char_indices().fuse(), src.len())
    }
}
impl Iterator for ArgIter<'_> {
    type Item = (usize, usize);
    fn next(&mut self) -> Option<Self::Item> {
        while let Some((i, ch)) = self.0.next() {
            if ch != '%' { continue }
            while let Some((j, ch)) = self.0.next() {
                if ch.is_whitespace() { return Some((i, j)) }
            }
            return Some((i, self.1));
        }
        None
    }
}
#[test]
fn test_arg_iter() {
    assert_eq!(ArgIter::new("hello world").collect::<Vec<_>>(), vec![]);
    assert_eq!(ArgIter::new("hello %world").collect::<Vec<_>>(), vec![(6, 12)]);
    assert_eq!(ArgIter::new("hello %world ").collect::<Vec<_>>(), vec![(6, 12)]);
    assert_eq!(ArgIter::new("hello %world      %gjherg3495830_ ").collect::<Vec<_>>(), vec![(6, 12), (18, 33)]);
}

struct InlineListIter<'a>(iter::Peekable<iter::Fuse<core::str::Chars<'a>>>);
impl<'a> InlineListIter<'a> {
    fn new(s: &'a str) -> Self {
        Self(s.chars().fuse().peekable())
    }
}
impl<'a> Iterator for InlineListIter<'a> {
    type Item = CompactString;
    fn next(&mut self) -> Option<Self::Item> {
        let mut res = CompactString::default();
        let mut in_quote = false;
        while let Some(ch) = self.0.next() {
            if ch == '"' {
                if !in_quote {
                    in_quote = true;
                    continue;
                }

                if let Some('"') = self.0.peek() {
                    res.push(self.0.next().unwrap());
                } else {
                    in_quote = false;
                }
            }
            else if ch == ',' && !in_quote {
                return Some(res.into());
            } else {
                res.push(ch);
            }
        }
        if !res.is_empty() { Some(res.into()) } else { None }
    }
}
#[test]
fn test_inline_list_iter() {
    assert_eq!(InlineListIter::new(r#""#).collect::<Vec<_>>(), &[] as &[&str]);
    assert_eq!(InlineListIter::new(r#"1"#).collect::<Vec<_>>(), &["1"]);
    assert_eq!(InlineListIter::new(r#"1,2"#).collect::<Vec<_>>(), &["1", "2"]);
    assert_eq!(InlineListIter::new(r#"1,2,test,53"#).collect::<Vec<_>>(), &["1", "2", "test", "53"]);
    assert_eq!(InlineListIter::new(r#""""test","test""","""test""","""","""""","test""test""#).collect::<Vec<_>>(), &["\"test", "test\"", "\"test\"", "\"", "\"\"", "test\"test"]);
    assert_eq!(InlineListIter::new(r#",,",",",,",""",",",""",""",""",","","",""#).collect::<Vec<_>>(), &["", "", ",", ",,", "\",", ",\"", "\",\"", ",\",\","]);
}

#[inline(never)]
fn clean_newlines(s: &str) -> CompactString {
    let mut res = alloc::string::String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    loop {
        match chars.next() {
            Some('\r') => {
                res.push('\n');
                if chars.peek().copied() == Some('\n') { chars.next(); }
            }
            Some('\n') => res.push('\n'),
            Some(x) => res.push(x),
            None => break,
        }
    }
    res.into()
}
#[test]
fn test_clean_newlines() {
    assert_eq!(clean_newlines("hello world"), "hello world");
    assert_eq!(clean_newlines("hello\nworld"), "hello\nworld");
    assert_eq!(clean_newlines("hello\rworld"), "hello\nworld");
    assert_eq!(clean_newlines("hello\r\nworld"), "hello\nworld");
    assert_eq!(clean_newlines("hello\r\n\nworld"), "hello\n\nworld");
    assert_eq!(clean_newlines("hello\r\n\n\rworld"), "hello\n\n\nworld");
    assert_eq!(clean_newlines("hello\r\n\n\rworld\n"), "hello\n\n\nworld\n");
    assert_eq!(clean_newlines("hello\r\n\n\rworld\r"), "hello\n\n\nworld\n");
    assert_eq!(clean_newlines("hello\r\n\n\rworld\r\n"), "hello\n\n\nworld\n");
    assert_eq!(clean_newlines("hello,\"one\rtwo\rthree\"\rworld,test,\"one\rtwo\r\"\ragain,\"\rtwo\",\"\rtwo\r\""), "hello,\"one\ntwo\nthree\"\nworld,test,\"one\ntwo\n\"\nagain,\"\ntwo\",\"\ntwo\n\"");
}

#[inline(never)]
fn get_collab_id(block: &Xml) -> Option<&str> {
    block.attr("collabId").map(|x| x.value.as_str()).filter(|x| !x.is_empty())
}

// source: https://docs.babelmonkeys.de/RustyXML/src/xml/lib.rs.html#41-55
#[cfg(test)]
fn xml_escape(input: &str) -> CompactString {
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

// source: https://docs.babelmonkeys.de/RustyXML/src/xml/lib.rs.html#60-100
// note: modified to suite our needs
#[inline(never)]
fn xml_unescape(input: &str) -> Result<CompactString, XmlError> {
    let mut result = alloc::string::String::with_capacity(input.len());

    let mut it = input.split('&');
    if let Some(sub) = it.next() {
        result.push_str(sub); // Push everything before the first '&'
    }

    for sub in it {
        match sub.find(';') {
            Some(idx) => {
                let ent = &sub[..idx];
                match ent {
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
                            None => return Err(XmlError::IllegalSequence { sequence: format!("&{};", ent).into() }),
                        }
                    }
                }
                result.push_str(&sub[idx + 1..]);
            }
            None => return Err(XmlError::IllegalSequence { sequence: format!("&{}", sub).into() }),
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

#[derive(Debug)]
struct XmlAttr {
    name: CompactString,
    value: CompactString,
}
#[derive(Debug)]
struct Xml {
    name: CompactString,
    text: CompactString,
    attrs: Vec<XmlAttr>,
    children: Vec<Xml>,
}
impl Xml {
    fn get(&self, path: &[&str]) -> Option<&Xml> {
        match path {
            [] => Some(self),
            [first, rest @ ..] => self.children.iter().find(|x| x.name == *first).map(|x| x.get(rest)).flatten(),
        }
    }
    fn attr(&self, name: &str) -> Option<&XmlAttr> {
        self.attrs.iter().find(|a| a.name == name)
    }
}
fn parse_xml_root<'a>(xml: &mut xmlparser::Tokenizer<'a>, root_name: &'a str) -> Result<Xml, XmlError> {
    let mut stack = vec![Xml { name: root_name.into(), text: CompactString::default(), attrs: vec![], children: vec![] }];
    loop {
        match xml.next() {
            Some(e) => match e {
                Err(e) => return Err(XmlError::Read { error: e }),
                Ok(e) => match e {
                    xmlparser::Token::Attribute { local, value, .. } => stack.last_mut().unwrap().attrs.push(XmlAttr { name: xml_unescape(local.as_str())?, value: xml_unescape(value.as_str())? }),
                    xmlparser::Token::Text { text: t } => stack.last_mut().unwrap().text.push_str(&xml_unescape(t.as_str())?),
                    xmlparser::Token::ElementStart { local, .. } => stack.push(Xml { name: local.as_str().into(), text: CompactString::default(), attrs: vec![], children: vec![] }),
                    xmlparser::Token::ElementEnd { end, .. } => match end {
                        xmlparser::ElementEnd::Close(_, _) | xmlparser::ElementEnd::Empty => {
                            let mut res = stack.pop().unwrap();
                            res.text = clean_newlines(&res.text);
                            match stack.last_mut() {
                                Some(parent) => parent.children.push(res),
                                None => return Ok(res),
                            }
                        }
                        xmlparser::ElementEnd::Open => (),
                    }
                    _ => (),
                }
            }
            None => return Err(XmlError::UnexpectedEof),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub location: Location,
}

#[derive(Debug)]
pub struct Location {
    pub role: Option<CompactString>,
    pub entity: Option<CompactString>,
    pub collab_id: Option<CompactString>,
    pub block_type: Option<CompactString>,
}

struct LocationRef<'a> {
    pub role: Option<&'a str>,
    pub entity: Option<&'a str>,
    pub collab_id: Option<&'a str>,
    pub block_type: Option<&'a str>,
}
impl LocationRef<'_> {
    fn to_owned(&self) -> Location {
        Location {
            role: self.role.map(CompactString::new),
            entity: self.entity.map(CompactString::new),
            collab_id: self.collab_id.map(CompactString::new),
            block_type: self.block_type.map(CompactString::new),
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    XmlError(XmlError),
    Base64Error(Base64Error),
    ProjectError(ProjectError),
    CompileError(CompileError),
}
impl From<XmlError> for ErrorKind { fn from(e: XmlError) -> Self { Self::XmlError(e) } }
impl From<Base64Error> for ErrorKind { fn from(e: Base64Error) -> Self { Self::Base64Error(e) } }
impl From<ProjectError> for ErrorKind { fn from(e: ProjectError) -> Self { Self::ProjectError(e) } }
impl From<CompileError> for ErrorKind { fn from(e: CompileError) -> Self { Self::CompileError(e) } }

#[derive(Debug)]
pub enum XmlError {
    Read { error: xmlparser::Error },
    IllegalSequence { sequence: CompactString },
    UnexpectedEof,
}

#[derive(Debug)]
pub enum ProjectError {
    NoRoot,
    NoStage,
    RoleNoName,
    RoleNoContent,
    RefMissingId,
    ValueNotEvaluated,
    UpvarNotConst,

    UnnamedGlobal,
    GlobalsWithSameName { name: CompactString },

    UnnamedEntity,
    EntitiesWithSameName { name: CompactString },

    UnnamedField,
    FieldNoValue { name: CompactString },
    FieldsWithSameName { name: CompactString },

    BlockWithoutType,
    BlockUnknownType,
    BlockChildCount { needed: usize, got: usize },
    BlockMissingOption,
    BlockOptionUnknown { got: CompactString },

    ImageWithoutId,
    ImagesWithSameId { id: CompactString },
    ImageWithoutContent { id: CompactString },
    ImageUnknownFormat { id: CompactString, content: CompactString },

    SoundWithoutId,
    SoundsWithSameId { id: CompactString },
    SoundWithoutContent { id: CompactString },
    SoundUnknownFormat { id: CompactString, content: CompactString },

    CostumeIdFormat { id: CompactString },
    CostumeUndefinedRef { id: CompactString },
    CostumesWithSameName { name: CompactString },

    SoundIdFormat { id: CompactString },
    SoundUndefinedRef { id: CompactString },
    SoundsWithSameName { name: CompactString },

    BoolNoValue,
    BoolUnknownValue { got: CompactString },

    ColorUnknownValue { color: CompactString },

    CustomBlockWithoutName,
    CustomBlockWithoutInputsMeta,
    CustomBlockInputsMetaCorrupted,
    CustomBlockWithoutType,
    CustomBlockUnknownType { ty: CompactString },

    MessageTypeMissingName,
    MessageTypeMissingFields { msg_type: CompactString },
    MessageTypeFieldEmpty { msg_type: CompactString },
    MessageTypeMultiplyDefined { msg_type: CompactString },
}

#[derive(Debug)]
pub enum CompileError {
    AutofillGenerateError { input: usize },
    NameTransformError { name: CompactString},
    UnknownBlockType,
    DerefAssignment,
    UndefinedVariable { name: CompactString },
    UndefinedFn { name: CompactString },
    BlockOptionNotConst,
    BlockOptionNotSelected,
    UnknownEntity { unknown: CompactString },
    UnknownEffect { effect: CompactString },
    UnknownPenAttr { attr: CompactString },

    UnknownMessageType { msg_type: CompactString },
    MessageTypeWrongNumberArgs { msg_type: CompactString, got: usize, expected: usize },

    UnknownService { service: CompactString },
    UnknownRPC { service: CompactString, rpc: CompactString },

    GlobalsWithSameTransName { trans_name: CompactString, names: (CompactString, CompactString) },
    EntitiesWithSameTransName { trans_name: CompactString, names: (CompactString, CompactString) },
    FieldsWithSameTransName { trans_name: CompactString, names: (CompactString, CompactString) },
    LocalsWithSameTransName { trans_name: CompactString, names: (CompactString, CompactString) },
    CostumesWithSameTransName { trans_name: CompactString, names: (CompactString, CompactString) },
    SoundsWithSameTransName { trans_name: CompactString, names: (CompactString, CompactString) },
    BlocksWithSameTransName { trans_name: CompactString, names: (CompactString, CompactString) },

    InputsWithSameName { name: CompactString },
    BlocksWithSameName { name: CompactString, sigs: (CompactString, CompactString) },

    CurrentlyUnsupported { msg: CompactString },
}

#[derive(Debug)]
pub enum SymbolError {
    NameTransformError { name: CompactString },
    ConflictingTrans { trans_name: CompactString, names: (CompactString, CompactString) },
}

#[derive(Clone)]
struct VecMap<K, V>(Vec<(K, V)>);
impl<K: fmt::Debug, V: fmt::Debug> fmt::Debug for VecMap<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{ {:?} }}", self.0)
    }
}
impl<K, V> Default for VecMap<K, V> {
    fn default() -> Self {
        Self(Default::default())
    }
}
impl<K, V> VecMap<K, V> {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    fn len(&self) -> usize {
        self.0.len()
    }
    fn into_iter(self) -> alloc::vec::IntoIter<(K, V)> {
        self.0.into_iter()
    }
    fn get<Q: PartialEq + ?Sized>(&self, key: &Q) -> Option<&V> where K: core::borrow::Borrow<Q> {
        self.0.iter().find(|x| x.0.borrow() == key).map(|x| &x.1)
    }
    fn get_mut<Q: PartialEq + ?Sized>(&mut self, key: &Q) -> Option<&mut V> where K: core::borrow::Borrow<Q> {
        self.0.iter_mut().find(|x| x.0.borrow() == key).map(|x| &mut x.1)
    }
    fn insert(&mut self, k: K, v: V) -> Option<V> where K: PartialEq {
        match self.get_mut(&k) {
            Some(x) => Some(mem::replace(x, v)),
            None => {
                self.0.push((k, v));
                None
            }
        }
    }
}

#[derive(Clone)]
struct SymbolTable<'a> {
    parser: &'a Parser,
    orig_to_def: VecMap<CompactString, VariableDefInit>,
    trans_to_orig: VecMap<CompactString, CompactString>,
}
impl fmt::Debug for SymbolTable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "SymbolTable {{ orig_to_def: {:?}, trans_to_orig: {:?} }}", self.orig_to_def, self.trans_to_orig)
    }
}
impl<'a> SymbolTable<'a> {
    fn new(parser: &'a Parser) -> Self {
        Self { parser, orig_to_def: Default::default(), trans_to_orig: Default::default() }
    }
    fn transform_name(&self, name: &str) -> Result<CompactString, SymbolError> {
        match self.parser.name_transformer.as_ref()(name) {
            Ok(v) => Ok(v),
            Err(()) => Err(SymbolError::NameTransformError { name: name.into() }),
        }
    }
    /// Defines a new symbol or replaces an existing definition.
    /// Fails if the name cannot be properly transformed or the transformed name already exists.
    /// On success, returns the previous definition (if one existed).
    /// On failure, the symbol table is not modified, and an error context object is returned.
    fn define(&mut self, name: CompactString, value: Value) -> Result<Option<VariableDefInit>, SymbolError> {
        let trans_name = self.transform_name(&name)?;
        if let Some(orig) = self.trans_to_orig.get(&trans_name) {
            let def = self.orig_to_def.get(orig).unwrap();
            return Err(SymbolError::ConflictingTrans { trans_name, names: (def.def.name.clone(), name) });
        }

        let entry = VariableDefInit { def: VariableDef { name: name.clone(), trans_name: trans_name.clone() }, init: value };
        self.trans_to_orig.insert(trans_name, name.clone());
        Ok(self.orig_to_def.insert(name, entry))
    }
    /// Returns the definition of the given variable if it exists.
    fn get(&self, name: &str) -> Option<&VariableDefInit> {
        self.orig_to_def.get(name)
    }
    /// Gets the list of all defined variables.
    /// This is guaranteed to be in order of definition.
    fn into_defs(self) -> Vec<VariableDef> {
        self.orig_to_def.into_iter().map(|x| x.1.def).collect()
    }
    /// Equivalent to [`SymbolTable::into_defs`] but preserves the initialized value.
    fn into_def_inits(self) -> Vec<VariableDefInit> {
        self.orig_to_def.into_iter().map(|x| x.1).collect()
    }
    fn len(&self) -> usize {
        self.orig_to_def.len()
    }
    fn is_empty(&self) -> bool {
        self.orig_to_def.is_empty()
    }
}
#[test]
fn test_sym_tab() {
    let parser = Parser { name_transformer: Rc::new(crate::util::c_ident), ..Default::default() };
    let mut sym = SymbolTable::new(&parser);
    assert!(sym.orig_to_def.is_empty());
    assert!(sym.trans_to_orig.is_empty());
    assert!(sym.define("hello world!".into(), 0f64.into()).unwrap().is_none());
    assert_eq!(sym.orig_to_def.get("hello world!").unwrap().def.name, "hello world!");
    assert_eq!(sym.orig_to_def.get("hello world!").unwrap().def.trans_name, "hello_world");
    assert_eq!(sym.trans_to_orig.get("hello_world").unwrap().as_str(), "hello world!");
}

#[derive(Debug)]
struct Rpc {
    service: CompactString,
    rpc: CompactString,
    args: Vec<(CompactString, Expr)>,
    info: Box<BlockInfo>,
}
#[derive(Debug)]
struct FnCall {
    function: FnRef,
    args: Vec<Expr>,
    upvars: Vec<VariableRef>,
    info: Box<BlockInfo>,
}

#[derive(Debug, Clone)]
pub struct BlockInfo {
    pub comment: Option<CompactString>,
    pub location: Option<CompactString>,
}
impl BlockInfo {
    fn none() -> Box<Self> {
        Box::new_with(|| BlockInfo { comment: None, location: None })
    }
}

#[derive(Debug, Clone)]
pub struct Project {
    pub name: CompactString,
    pub roles: Vec<Role>,
}
#[derive(Debug, Clone)]
pub struct Role {
    pub name: CompactString,
    pub notes: CompactString,
    pub stage_size: (usize, usize),
    pub globals: Vec<VariableDefInit>,
    pub funcs: Vec<Function>,
    pub entities: Vec<Entity>,
}
#[derive(Debug, Clone)]
pub struct Function {
    pub name: CompactString,
    pub trans_name: CompactString,
    pub params: Vec<VariableDef>,
    pub upvars: Vec<VariableRef>, // refer into params
    pub returns: bool,
    pub stmts: Vec<Stmt>,
}
#[derive(Debug, Clone)]
pub struct Entity {
    pub name: CompactString,
    pub trans_name: CompactString,
    pub fields: Vec<VariableDefInit>,
    pub costumes: Vec<VariableDefInit>,
    pub sounds: Vec<VariableDefInit>,
    pub funcs: Vec<Function>,
    pub scripts: Vec<Script>,

    pub active_costume: Option<usize>,
    pub visible: bool,
    pub color: (u8, u8, u8, u8),
    pub pos: (f64, f64),
    pub heading: f64,
    pub scale: f64,
}
#[derive(Debug, Clone)]
pub struct VariableDefInit {
    pub def: VariableDef,
    pub init: Value,
}
#[derive(Debug, Clone)]
pub struct VariableDef {
    pub name: CompactString,
    pub trans_name: CompactString,
}
impl VariableDef {
    #[inline(always)]
    fn ref_at(&self, location: VarLocation) -> Box<VariableRef> {
        Box::new_with(|| VariableRef { name: self.name.clone(), trans_name: self.trans_name.clone(), location })
    }
    #[inline(always)]
    fn fn_ref_at(&self, location: FnLocation) -> Box<FnRef> {
        Box::new_with(|| FnRef { name: self.name.clone(), trans_name: self.trans_name.clone(), location })
    }
}
#[derive(Debug, Clone)]
pub struct VariableRef {
    pub name: CompactString,
    pub trans_name: CompactString,
    pub location: VarLocation,
}
#[derive(Debug, Clone)]
pub struct FnRef {
    pub name: CompactString,
    pub trans_name: CompactString,
    pub location: FnLocation,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarLocation {
    Global, Field, Local,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FnLocation {
    Global, Method,
}
#[derive(Debug, Clone)]
pub struct Script {
    pub hat: Option<Box<Hat>>,
    pub stmts: Vec<Stmt>,
}
#[derive(Debug, Clone)]
pub struct Hat {
    pub kind: HatKind,
    pub info: Box<BlockInfo>,
}
#[derive(Debug, Clone)]
pub enum HatKind {
    OnFlag,
    OnClone,
    OnKey { key: CompactString },
    MouseDown,
    MouseUp,
    MouseEnter,
    MouseLeave,
    ScrollUp,
    ScrollDown,
    Dropped,
    Stopped,
    When { condition: Box<Expr> },
    LocalMessage { msg_type: Option<CompactString> },
    NetworkMessage { msg_type: CompactString, fields: Vec<VariableRef> },
    Unknown { name: CompactString, fields: Vec<VariableRef> },
}
#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub info: Box<BlockInfo>,
}
#[derive(Debug, Clone)]
pub enum StmtKind {
    DeclareLocals { vars: Vec<VariableDef> },
    Assign { var: VariableRef, value: Box<Expr> },
    AddAssign { var: VariableRef, value: Box<Expr> },

    ShowVar { var: VariableRef },
    HideVar { var: VariableRef },

    Warp { stmts: Vec<Stmt> },

    InfLoop { stmts: Vec<Stmt> },
    ForeachLoop { var: VariableRef, items: Box<Expr>, stmts: Vec<Stmt> },
    ForLoop { var: VariableRef, start: Box<Expr>, stop: Box<Expr>, stmts: Vec<Stmt> },
    UntilLoop { condition: Box<Expr>, stmts: Vec<Stmt> },
    Repeat { times: Box<Expr>, stmts: Vec<Stmt> },

    If { condition: Box<Expr>, then: Vec<Stmt> },
    IfElse { condition: Box<Expr>, then: Vec<Stmt>, otherwise: Vec<Stmt> },

    TryCatch { code: Vec<Stmt>, var: VariableRef, handler: Vec<Stmt> },
    Throw { error: Box<Expr> },

    ListInsert { list: Box<Expr>, value: Box<Expr>, index: Box<Expr> },
    ListInsertLast { list: Box<Expr>, value: Box<Expr> },
    ListInsertRandom { list: Box<Expr>, value: Box<Expr> },

    ListRemove { list: Box<Expr>, index: Box<Expr> },
    ListRemoveLast { list: Box<Expr> },
    ListRemoveAll { list: Box<Expr> },

    ListAssign { list: Box<Expr>, value: Box<Expr>, index: Box<Expr> },
    ListAssignLast { list: Box<Expr>, value: Box<Expr> },
    ListAssignRandom { list: Box<Expr>, value: Box<Expr> },

    Return { value: Box<Expr> },

    Sleep { seconds: Box<Expr> },
    WaitUntil { condition: Box<Expr> },

    SetCostume { costume: Box<Expr> },
    NextCostume,

    PlaySound { sound: Box<Expr>, blocking: bool },
    StopSounds,

    Forward { distance: Box<Expr> },
    SetX { value: Box<Expr> },
    ChangeX { delta: Box<Expr> },
    SetY { value: Box<Expr> },
    ChangeY { delta: Box<Expr> },
    GotoXY { x: Box<Expr>, y: Box<Expr> },
    GotoMouse,
    GotoRandom,
    /// Similar to `SetPos` except that the target can be either a list of `[x, y]` coordinates or a entity.
    Goto { target: Box<Expr> },
    PointTowards { target: Box<Expr> },
    PointTowardsXY { x: Box<Expr>, y: Box<Expr> },

    TurnRight { angle: Box<Expr> },
    TurnLeft { angle: Box<Expr> },
    SetHeading { value: Box<Expr> },
    SetHeadingRandom,

    BounceOffEdge,

    SetPenDown { value: bool },
    PenClear,
    Stamp,
    Write { content: Box<Expr>, font_size: Box<Expr> },
    SetPenColor { color: (u8, u8, u8, u8) },

    Say { content: Box<Expr>, duration: Option<Box<Expr>> },
    Think { content: Box<Expr>, duration: Option<Box<Expr>> },

    SetVisible { value: bool },
    ChangeSize { delta: Box<Expr> },
    SetSize { value: Box<Expr> },

    ChangePenSize { delta: Box<Expr> },
    SetPenSize { value: Box<Expr> },

    CallRpc { service: CompactString, rpc: CompactString, args: Vec<(CompactString, Expr)> },
    CallFn { function: FnRef, args: Vec<Expr>, upvars: Vec<VariableRef> },
    CallClosure { new_entity: Option<Box<Expr>>, closure: Box<Expr>, args: Vec<Expr> },
    ForkClosure { closure: Box<Expr>, args: Vec<Expr> },

    Clone { target: Box<Expr> },
    DeleteClone,

    /// Sends a message to local entities (not over the network).
    /// If `target` is `None`, this should broadcast to all entities.
    /// Otherwise `target` is either a single target or a list of targets to send to.
    /// The `wait` flag determines if the broadcast should be blocking (wait for receivers to terminate).
    SendLocalMessage { target: Option<Box<Expr>>, msg_type: Box<Expr>, wait: bool },
    /// Sends a message over the network to the specified targets.
    /// `target` may be a single target or a list of targets.
    SendNetworkMessage { target: Box<Expr>, msg_type: CompactString, values: Vec<(CompactString, Expr)> },
    /// Sends a reply from a received message that was blocking (sender's `wait` flag was `true`).
    SendNetworkReply { value: Box<Expr> },

    Ask { prompt: Box<Expr> },

    ResetTimer,

    Pause,

    SetEffect { kind: EffectKind, value: Box<Expr> },
    ChangeEffect { kind: EffectKind, delta: Box<Expr> },
    ClearEffects,

    SetPenAttr { attr: PenAttribute, value: Box<Expr> },
    ChangePenAttr { attr: PenAttribute, delta: Box<Expr> },

    Stop { mode: StopMode },

    UnknownBlock { name: CompactString, args: Vec<Expr> },
}
impl From<Rpc> for Stmt {
    fn from(rpc: Rpc) -> Stmt {
        let Rpc { service, rpc, args, info } = rpc;
        Stmt { kind: StmtKind::CallRpc { service, rpc, args }, info }
    }
}

#[derive(Debug, Clone)]
pub struct RefId(pub usize);

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Number(f64),
    Constant(Constant),
    String(CompactString),
    Image(Rc<(Vec<u8>, Option<(f64, f64)>)>),
    Audio(Rc<Vec<u8>>),
    List(Vec<Value>, Option<RefId>),
    Ref(RefId),
}

impl From<f64> for Value { fn from(v: f64) -> Value { Value::Number(v) } }
impl From<&str> for Value { fn from(v: &str) -> Value { Value::String(v.into()) } }
impl From<bool> for Value { fn from(v: bool) -> Value { Value::Bool(v) } }
impl From<CompactString> for Value { fn from(v: CompactString) -> Value { Value::String(v) } }
impl From<Constant> for Value { fn from(v: Constant) -> Value { Value::Constant(v) } }

#[derive(Debug, Clone, Copy)]
pub enum Constant {
    E, Pi,
}
#[derive(Debug, Clone)]
pub enum TextSplitMode {
    Letter, Word, Tab, CR, LF, Csv, Json,
    Custom(Box<Expr>),
}
#[derive(Debug, Clone)]
pub enum EffectKind {
    Color, Saturation, Brightness, Ghost,
    Fisheye, Whirl, Pixelate, Mosaic, Negative,
}
#[derive(Debug, Clone)]
pub enum PenAttribute {
    Size, Hue, Saturation, Brightness, Transparency,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClosureKind {
    Command, Reporter, Predicate,
}
#[derive(Debug, Clone)]
pub enum ValueType {
    Number, Text, Bool, List, Sprite, Costume, Sound, Command, Reporter, Predicate,
}
#[derive(Debug, Clone)]
pub enum TimeQuery {
    Year, Month, Date, DayOfWeek, Hour, Minute, Second, UnixTimestampMs,
}
#[derive(Debug, Clone)]
pub enum StopMode {
    All, AllScenes, ThisScript, ThisBlock, AllButThisScript, OtherScriptsInSprite,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub info: Box<BlockInfo>,
}
#[derive(Debug, Clone)]
pub enum ExprKind {
    Value(Value),
    Variable { var: VariableRef },

    Add { values: Box<Expr> },
    Mul { values: Box<Expr> },
    Min { values: Box<Expr> },
    Max { values: Box<Expr> },

    Sub { left: Box<Expr>, right: Box<Expr> },
    Div { left: Box<Expr>, right: Box<Expr> },
    /// Mathematical modulus (not remainder!). For instance, `-1 mod 7 == 6`.
    Mod { left: Box<Expr>, right: Box<Expr> },

    Pow { base: Box<Expr>, power: Box<Expr> },
    Log { value: Box<Expr>, base: Box<Expr> },

    Atan2 { y: Box<Expr>, x: Box<Expr> },

    /// Short-circuiting logical `or`.
    And { left: Box<Expr>, right: Box<Expr> },
    /// Short-circuiting logical `and`.
    Or { left: Box<Expr>, right: Box<Expr> },
    /// Lazily-evaluated conditional expression. Returns `then` if `condition` is true, otherwise `otherwise`.
    Conditional { condition: Box<Expr>, then: Box<Expr>, otherwise: Box<Expr> },

    /// If both values are lists, returns true of they are references to the same list.
    /// If both values are non-lists, returns true if the values are equal.
    /// Otherwise returns `false`.
    Identical { left: Box<Expr>, right: Box<Expr> },
    Eq { left: Box<Expr>, right: Box<Expr> },
    Neq { left: Box<Expr>, right: Box<Expr> },
    Less { left: Box<Expr>, right: Box<Expr> },
    LessEq { left: Box<Expr>, right: Box<Expr> },
    Greater { left: Box<Expr>, right: Box<Expr> },
    GreaterEq { left: Box<Expr>, right: Box<Expr> },

    /// Get a random number between `a` and `b` (inclusive).
    /// There are no ordering guarantees (swapping `a` and `b` is equivalent).
    /// If both values are integers, the result is an integer, otherwise continuous floats are returned.
    Random { a: Box<Expr>, b: Box<Expr> },
    /// Get a list of all the numbers starting at `start` and stepping towards `stop` (by `+1` or `-1`), but not going past `stop`.
    Range { start: Box<Expr>, stop: Box<Expr> },

    MakeList { values: Vec<Expr> },
    CopyList { list: Box<Expr> },
    ListCat { lists: Box<Expr> },

    ListLen { value: Box<Expr> },
    ListRank { value: Box<Expr> },
    ListDims { value: Box<Expr> },
    ListFlatten { value: Box<Expr> },
    ListColumns { value: Box<Expr> },
    ListRev { value: Box<Expr> },

    ListLines { value: Box<Expr> },
    ListCsv { value: Box<Expr> },
    ListJson { value: Box<Expr> },

    ListReshape { value: Box<Expr>, dims: Box<Expr> },
    ListCombinations { sources: Box<Expr> },

    ListIsEmpty { value: Box<Expr> },
    /// Given a list, returns a new (shallow copy) of all the items except the first.
    /// If the list is empty, an empty list is returned.
    ListCdr { value: Box<Expr> },
    /// Given a value and a list, returns a new list (shallow copy) with the item prepended.
    ListCons { item: Box<Expr>, list: Box<Expr> },
    /// Returns the (1-based) index of value in the list, or 0 if not present.
    ListFind { list: Box<Expr>, value: Box<Expr> },
    ListContains { list: Box<Expr>, value: Box<Expr> },

    ListGet { list: Box<Expr>, index: Box<Expr> },
    ListGetLast { list: Box<Expr> },
    ListGetRandom { list: Box<Expr> },

    StrGet { string: Box<Expr>, index: Box<Expr> },
    StrGetLast { string: Box<Expr> },
    StrGetRandom { string: Box<Expr> },

    StrCat { values: Box<Expr> },
    /// String length in terms of unicode code points (not bytes or grapheme clusters!).
    StrLen { value: Box<Expr> },

    /// Convert a unicode code point into a 1-character string.
    UnicodeToChar { value: Box<Expr> },
    /// Convert a 1-character string into its unicode code point.
    CharToUnicode { value: Box<Expr> },

    Not { value: Box<Expr> },
    Neg { value: Box<Expr> },
    Abs { value: Box<Expr> },
    Sign { value: Box<Expr> },
    Sqrt { value: Box<Expr> },

    Floor { value: Box<Expr> },
    Ceil { value: Box<Expr> },
    Round { value: Box<Expr> },

    Sin { value: Box<Expr> },
    Cos { value: Box<Expr> },
    Tan { value: Box<Expr> },

    Asin { value: Box<Expr> },
    Acos { value: Box<Expr> },
    Atan { value: Box<Expr> },

    CallRpc { service: CompactString, rpc: CompactString, args: Vec<(CompactString, Expr)> },
    CallFn { function: FnRef, args: Vec<Expr>, upvars: Vec<VariableRef>, },
    CallClosure { new_entity: Option<Box<Expr>>, closure: Box<Expr>, args: Vec<Expr> },

    StageWidth,
    StageHeight,

    MouseX,
    MouseY,

    Latitude,
    Longitude,

    YPos,
    XPos,
    Heading,

    PenDown,

    Size,
    IsVisible,

    This,
    Entity { name: CompactString, trans_name: CompactString },

    ImageOfEntity { entity: Box<Expr> },
    ImageOfDrawings,

    IsTouchingEntity { entity: Box<Expr> },
    IsTouchingMouse,
    IsTouchingEdge,
    IsTouchingDrawings,

    RpcError,

    Closure { kind: ClosureKind, params: Vec<VariableDef>, captures: Vec<VariableRef>, stmts: Vec<Stmt> },

    TextSplit { text: Box<Expr>, mode: TextSplitMode },

    Answer,
    Message,

    Timer,

    Map { f: Box<Expr>, list: Box<Expr> },
    Keep { f: Box<Expr>, list: Box<Expr> },
    FindFirst { f: Box<Expr>, list: Box<Expr> },
    Combine { f: Box<Expr>, list: Box<Expr> },

    NetworkMessageReply { target: Box<Expr>, msg_type: CompactString, values: Vec<(CompactString, Expr)> },

    Effect { kind: EffectKind },
    PenAttr { attr: PenAttribute },

    CostumeList,
    Costume,
    CostumeNumber,

    SoundList,

    Clone { target: Box<Expr> },

    TypeQuery { value: Box<Expr>, ty: ValueType },
    RealTime { query: TimeQuery },

    UnknownBlock { name: CompactString, args: Vec<Expr> },
}
impl<T: Into<Value>> From<T> for Expr {
    fn from(v: T) -> Expr {
        Expr { kind: ExprKind::Value(v.into()), info: BlockInfo::none() }
    }
}
impl From<Rpc> for Expr {
    fn from(rpc: Rpc) -> Expr {
        let Rpc { service, rpc, args, info } = rpc;
        Expr { kind: ExprKind::CallRpc { service, rpc, args }, info }
    }
}

fn parse_color(value: &str) -> Option<(u8, u8, u8, u8)> {
    let vals: Vec<_> = value.split(',').map(|v| v.parse::<f64>().ok()).flatten().collect();
    match vals.as_slice() {
        [r, g, b] => Some((*r as u8, *g as u8, *b as u8, 255)),
        [r, g, b, a] => Some((*r as u8, *g as u8, *b as u8, (*a * 255.0) as u8)),
        _ => None,
    }
}

struct NetworkMessage {
    target: Box<Expr>,
    msg_type: CompactString,
    values: Vec<(CompactString, Expr)>,
    info: Box<BlockInfo>,
}

struct ScriptInfo<'a, 'b, 'c> {
    parser: &'a Parser,
    role: &'c RoleInfo<'a>,
    entity: &'c EntityInfo<'a, 'b>,
    locals: Vec<(SymbolTable<'a>, Vec<VariableRef>)>, // tuples of (locals, captures)
    autofill_args: Option<Vec<VariableRef>>,
}
impl<'a, 'b, 'c> ScriptInfo<'a, 'b, 'c> {
    fn new(entity: &'c EntityInfo<'a, 'b>) -> Box<Self> {
        Box::new_with(|| Self {
            parser: entity.parser,
            role: entity.role,
            entity,
            locals: vec![(SymbolTable::new(entity.parser), Default::default())],
            autofill_args: None,
        })
    }
    #[inline(never)]
    fn check_children_get_info(&self, expr: &Xml, req: usize, location: &LocationRef) -> Result<Box<BlockInfo>, Box<Error>> {
        if expr.children.len() < req {
            return Err(Box::new_with(|| Error { kind: ErrorKind::ProjectError(ProjectError::BlockChildCount { needed: req, got: expr.children.len() }), location: location.to_owned() }));
        }
        let comment = match expr.children.get(req) {
            Some(comment) => if comment.name == "comment" { Some(comment.text.clone()) } else { None },
            None => None,
        };
        Ok(Box::new_with(|| BlockInfo { comment, location: location.collab_id.map(CompactString::new) }))
    }
    #[inline(never)]
    fn decl_local(&mut self, name: CompactString, value: Value, location: &LocationRef) -> Result<&VariableDefInit, Box<Error>> {
        let locals = &mut self.locals.last_mut().unwrap().0;
        match locals.define(name.clone(), value) {
            Ok(_) => (), // redefining locals is fine
            Err(SymbolError::ConflictingTrans { trans_name, names }) => if names.0 != names.1 { // redefining locals is fine
                return Err(Box::new_with(|| Error { kind: CompileError::LocalsWithSameTransName { trans_name, names }.into(), location: location.to_owned() }));
            }
            Err(SymbolError::NameTransformError { name }) => return Err(Box::new_with(|| Error { kind: CompileError::NameTransformError { name }.into(), location: location.to_owned() })),
        }
        Ok(locals.get(&name).unwrap())
    }
    #[inline(never)]
    fn grab_option<'x>(&self, child: &'x Xml, location: &LocationRef) -> Result<&'x str, Box<Error>> {
        let res = match child.get(&["option"]) {
            None => return Err(Box::new_with(|| Error { kind: ProjectError::BlockMissingOption.into(), location: location.to_owned() })),
            Some(f) => {
                if f.children.len() != 0 { return Err(Box::new_with(|| Error { kind: CompileError::BlockOptionNotConst.into(), location: location.to_owned() })) }
                f.text.as_str()
            }
        };
        if res == "" { return Err(Box::new_with(|| Error { kind: CompileError::BlockOptionNotSelected.into(), location: location.to_owned() })) }
        Ok(res)
    }
    #[inline(never)]
    fn grab_entity(&mut self, child: &Xml, info: Box<BlockInfo>, location: &LocationRef) -> Result<Box<Expr>, Box<Error>> {
        match child.text.as_str() {
            "" => match child.get(&["option"]) {
                Some(x) => match x.text.as_str() {
                    "myself" => Ok(Box::new_with(|| Expr { kind: ExprKind::This, info })),
                    x => Err(Box::new_with(|| Error { kind: ProjectError::BlockOptionUnknown { got: x.into() }.into(), location: location.to_owned() })),
                }
                None => self.parse_expr(child, location),
            }
            name => match self.role.entities.get(name) {
                None => Err(Box::new_with(|| Error { kind: CompileError::UnknownEntity { unknown: name.into() }.into(), location: location.to_owned() })),
                Some(entity) => Ok(Box::new_with(|| Expr { kind: ExprKind::Entity { name: entity.def.name.clone(), trans_name: entity.def.trans_name.clone() }, info })),
            }
        }
    }
    #[inline(never)]
    fn parse(&mut self, script: &Xml) -> Result<Script, Box<Error>> {
        if script.children.is_empty() { return Ok(Script { hat: None, stmts: vec![] }) }

        let (hat, stmts_xml) = match self.parse_hat(&script.children[0])? {
            None => (None, script.children.as_slice()),
            Some(hat) => (Some(hat), &script.children[1..]),
        };

        let mut stmts = vec![];
        for stmt in stmts_xml {
            let location = Box::new_with(|| LocationRef {
                role: Some(&self.role.name),
                entity: Some(&self.entity.name),
                collab_id: get_collab_id(stmt),
                block_type: Some(&stmt.name),
            });
            match stmt.name.as_str() {
                "block" => stmts.push_boxed(self.parse_block(stmt)?),
                "custom-block" => {
                    let res = self.parse_fn_call(stmt, &location)?;
                    stmts.push_with(|| {
                        let FnCall { function, args, upvars, info } = *res;
                        Stmt { kind: StmtKind::CallFn { function, args, upvars }, info }
                    });
                }
                _ => return Err(Box::new_with(|| Error { kind: ProjectError::BlockUnknownType.into(), location: location.to_owned() })),
            }
        }
        Ok(Script { hat, stmts })
    }
    #[inline(never)]
    fn parse_hat(&mut self, stmt: &Xml) -> Result<Option<Box<Hat>>, Box<Error>> {
        let mut location = Box::new_with(|| LocationRef {
            role: Some(&self.role.name),
            entity: Some(&self.entity.name),
            collab_id: get_collab_id(stmt),
            block_type: None,
        });
        let s = match stmt.attr("s") {
            None => return Err(Box::new_with(|| Error { kind: ProjectError::BlockWithoutType.into(), location: location.to_owned() })),
            Some(v) => v.value.as_str(),
        };
        location.block_type = Some(s);

        fn parse_fields(script: &mut ScriptInfo, children: &[Xml], location: &LocationRef) -> Result<(Vec<VariableRef>, Option<CompactString>), Box<Error>> {
            let mut fields = vec![];
            let mut comment = None;
            for child in children {
                if child.name == "comment" {
                    comment = Some(child.text.clone());
                }
                if child.name != "l" { break }
                let var = script.decl_local(child.text.clone(), 0f64.into(), &location)?.def.ref_at(VarLocation::Local);
                fields.push_boxed(var);
            }
            Ok((fields, comment))
        }

        Ok(Some(match s {
            "receiveGo" => {
                let info = self.check_children_get_info(stmt, 0, &location)?;
                Box::new_with(|| Hat { kind: HatKind::OnFlag, info })
            }
            "receiveOnClone" => {
                let info = self.check_children_get_info(stmt, 0, &location)?;
                Box::new_with(|| Hat { kind: HatKind::OnClone, info })
            }
            "receiveCondition" => {
                let info = self.check_children_get_info(stmt, 1, &location)?;
                let condition = self.parse_expr(&stmt.children[0], &location)?;
                Box::new_with(|| Hat { kind: HatKind::When { condition }, info })
            }
            "receiveKey" => {
                let info = self.check_children_get_info(stmt, 1, &location)?;
                let key = self.grab_option(&stmt.children[0], &location)?;
                Box::new_with(|| Hat { kind: HatKind::OnKey { key: key.into() }, info })
            }
            "receiveInteraction" => {
                let info = self.check_children_get_info(stmt, 1, &location)?;
                match self.grab_option(&stmt.children[0], &location)? {
                    "pressed" => Box::new_with(|| Hat { kind: HatKind::MouseDown, info }),
                    "clicked" => Box::new_with(|| Hat { kind: HatKind::MouseUp, info }),
                    "mouse-entered" => Box::new_with(|| Hat { kind: HatKind::MouseEnter, info }),
                    "mouse-departed" => Box::new_with(|| Hat { kind: HatKind::MouseLeave, info }),
                    "scrolled-up" => Box::new_with(|| Hat { kind: HatKind::ScrollUp, info }),
                    "scrolled-down" => Box::new_with(|| Hat { kind: HatKind::ScrollDown, info }),
                    "dropped" => Box::new_with(|| Hat { kind: HatKind::Dropped, info }),
                    "stopped" => Box::new_with(|| Hat { kind: HatKind::Stopped, info }),
                    x => return Err(Box::new_with(|| Error { kind: ProjectError::BlockOptionUnknown { got: x.into() }.into(), location: location.to_owned() })),
                }
            }
            "receiveMessage" => {
                let info = self.check_children_get_info(stmt, 1, &location)?;
                let child = &stmt.children[0];
                if child.name != "l" { return Err(Box::new_with(|| Error { kind: CompileError::BlockOptionNotConst.into(), location: location.to_owned() })) }
                let msg_type = match child.text.as_str() {
                    "" => match child.get(&["option"]) {
                        Some(opt) => match opt.text.as_str() {
                            "any message" => None,
                            x => return Err(Box::new_with(|| Error { kind: ProjectError::BlockOptionUnknown { got: x.into() }.into(), location: location.to_owned() })),
                        }
                        None => return Err(Box::new_with(|| Error { kind: CompileError::BlockOptionNotSelected.into(), location: location.to_owned() })),
                    }
                    x => Some(CompactString::new(x)),
                };
                Box::new_with(|| Hat { kind: HatKind::LocalMessage { msg_type }, info })
            }
            "receiveSocketMessage" => {
                if stmt.children.is_empty() { return Err(Box::new_with(|| Error { kind: ProjectError::BlockChildCount { needed: 1, got: 0 }.into(), location: location.to_owned() })) }
                if stmt.children[0].name != "l" { return Err(Box::new_with(|| Error { kind: CompileError::BlockOptionNotConst.into(), location: location.to_owned() })) }

                let msg_type = match stmt.children[0].text.as_str() {
                    "" => return Err(Box::new_with(|| Error { kind: CompileError::BlockOptionNotSelected.into(), location: location.to_owned() })),
                    x => CompactString::new(x),
                };

                let (fields, comment) = parse_fields(self, &stmt.children[1..], &location)?;
                let info = Box::new_with(|| BlockInfo { comment, location: location.collab_id.map(CompactString::new) });
                Box::new_with(|| Hat { kind: HatKind::NetworkMessage { msg_type, fields }, info })
            }
            x if x.starts_with("receive") => {
                let (fields, comment) = parse_fields(self, &stmt.children, &location)?;
                let info = Box::new_with(|| BlockInfo { comment, location: location.collab_id.map(CompactString::new) });
                Box::new_with(|| Hat { kind: HatKind::Unknown { fields, name: x.into() }, info })
            }
            _ => return Ok(None),
        }))
    }
    #[inline(never)]
    fn parse_effect(&mut self, effect: &Xml, location: &LocationRef) -> Result<EffectKind, Box<Error>> {
        Ok(match self.grab_option(effect, location)? {
            "color" => EffectKind::Color,
            "saturation" => EffectKind::Saturation,
            "brightness" => EffectKind::Brightness,
            "ghost" => EffectKind::Ghost,
            "fisheye" => EffectKind::Fisheye,
            "whirl" => EffectKind::Whirl,
            "pixelate" => EffectKind::Pixelate,
            "mosaic" => EffectKind::Mosaic,
            "negative" => EffectKind::Negative,
            x => return Err(Box::new_with(|| Error { kind: CompileError::UnknownEffect { effect: CompactString::new(x) }.into(), location: location.to_owned() })),
        })
    }
    #[inline(never)]
    fn parse_pen_attr(&mut self, attr: &Xml, location: &LocationRef) -> Result<PenAttribute, Box<Error>> {
        Ok(match self.grab_option(attr, location)? {
            "size" => PenAttribute::Size,
            "hue" => PenAttribute::Hue,
            "saturation" => PenAttribute::Saturation,
            "brightness" => PenAttribute::Brightness,
            "transparency" => PenAttribute::Transparency,
            x => return Err(Box::new_with(|| Error { kind: CompileError::UnknownPenAttr { attr: CompactString::new(x) }.into(), location: location.to_owned() })),
        })
    }
    #[inline(never)]
    fn parse_rpc(&mut self, stmt: &Xml, location: &LocationRef) -> Result<Box<Rpc>, Box<Error>> {
        if stmt.children.len() < 2 { return Err(Box::new_with(|| Error { kind: ProjectError::BlockChildCount { needed: 2, got: stmt.children.len() }.into(), location: location.to_owned() })) }
        for child in &stmt.children[..2] {
            if child.name != "l" || child.text.is_empty() { return Err(Box::new_with(|| Error { kind: CompileError::BlockOptionNotConst.into(), location: location.to_owned() })) }
        }

        let service = stmt.children[0].text.clone();
        let rpc = stmt.children[1].text.clone();

        let arg_names = match stmt.attr("inputNames").map(|x| x.value.split(';').map(str::trim).filter(|v| !v.is_empty()).collect::<Vec<_>>()) {
            Some(x) => x,
            None => match SERVICE_INFO.iter().find(|x| x.0 == service) {
                None => return Err(Box::new_with(|| Error { kind: CompileError::UnknownService { service }.into(), location: location.to_owned() })),
                Some(x) => match x.1.iter().find(|x| x.0 == rpc) {
                    None => return Err(Box::new_with(|| Error { kind: CompileError::UnknownRPC { service, rpc }.into(), location: location.to_owned() })),
                    Some(x) => x.1.to_owned(),
                }
            }
        };

        let info = self.check_children_get_info(stmt, 2 + arg_names.len(), location)?;
        let mut args = Vec::with_capacity(arg_names.len());
        for (&arg_name, child) in arg_names.iter().zip(&stmt.children[2 .. 2 + arg_names.len()]) {
            let val = self.parse_expr(child, location)?;
            args.push_with(|| (CompactString::new(arg_name), *val));
        }
        Ok(Box::new_with(|| Rpc { service, rpc, args, info }))
    }
    #[inline(never)]
    fn parse_fn_call(&mut self, stmt: &Xml, location: &LocationRef) -> Result<Box<FnCall>, Box<Error>> {
        let s = match stmt.attr("s") {
            Some(v) => v.value.as_str(),
            None => return Err(Box::new_with(|| Error { kind: ProjectError::CustomBlockWithoutName.into(), location: location.to_owned() })),
        };
        let location = Box::new_with(|| LocationRef {
            role: location.role,
            entity: location.entity,
            collab_id: location.collab_id,
            block_type: Some(s),
        });

        let name = block_name_from_ref(s);
        let function = self.reference_fn(&name, &location)?;
        let block_info = get_block_info(&function.1);
        let argc = block_info.params.len();
        debug_assert_eq!(ArgIter::new(s).count(), argc);
        let info = self.check_children_get_info(stmt, argc, &location)?;

        let mut upvars = vec![];
        for upvar in block_info.upvars.iter() {
            let i = match block_info.params.iter().position(|x| x.0 == *upvar) {
                Some(x) => x,
                None => return Err(Box::new_with(|| Error { kind: ProjectError::CustomBlockInputsMetaCorrupted.into(), location: location.to_owned() })),
            };
            let upvar_target = match stmt.children.get(i) {
                Some(x) if x.name == "l" && !x.text.is_empty() => x.text.as_str(),
                _ => return Err(Box::new_with(|| Error { kind: ProjectError::CustomBlockInputsMetaCorrupted.into(), location: location.to_owned() })),
            };
            let def = self.decl_local(upvar_target.into(), Value::from(0.0f64), &location)?;
            upvars.push_boxed(def.def.ref_at(VarLocation::Local));
        }

        let mut args = Vec::with_capacity(argc);
        for (param, expr) in iter::zip(&block_info.params, &stmt.children[..argc]) {
            match param.1 {
                ParamType::Evaluated => args.push_boxed(self.parse_expr(expr, &location)?),
                ParamType::Unevaluated => args.push_boxed(self.parse_closure(expr, ClosureKind::Reporter, true, &location)?),
            }
        }

        Ok(Box::new_with(|| FnCall { function: function.0, args, upvars, info }))
    }
    #[inline(never)]
    fn parse_send_message_common(&mut self, stmt: &Xml, location: &LocationRef) -> Result<Box<NetworkMessage>, Box<Error>> {
        let msg_type = match stmt.children.get(0) {
            Some(value) if value.name != "comment" => value.text.as_str(),
            _ => return Err(Box::new_with(|| Error { kind: ProjectError::BlockMissingOption.into(), location: location.to_owned() })),
        };
        let fields = match self.role.msg_types.get(msg_type) {
            None => return Err(Box::new_with(|| Error { kind: CompileError::UnknownMessageType { msg_type: msg_type.into() }.into(), location: location.to_owned() })),
            Some(x) => x,
        };

        let (argc, comment) = stmt.children.iter().enumerate().find(|(_, x)| x.name == "comment").map(|(i, x)| (i, Some(x.text.as_str()))).unwrap_or((stmt.children.len(), None));
        assert!(argc >= 1); // due to msg_type from above

        let values = stmt.children[1..argc - 1].iter().map(|x| self.parse_expr(x, location)).collect::<Result<Vec<_>,_>>()?;
        if fields.len() != values.len() {
            return Err(Box::new_with(|| Error { kind: CompileError::MessageTypeWrongNumberArgs { msg_type: msg_type.into(), got: values.len(), expected: fields.len() }.into(), location: location.to_owned() }));
        }

        let target_xml = &stmt.children[argc - 1];
        let target = match target_xml.get(&["option"]) {
            Some(x) => match x.text.as_str() {
                "" => return Err(Box::new_with(|| Error { kind: CompileError::BlockOptionNotSelected.into(), location: location.to_owned() })),
                x => Box::new_with(|| x.into()),
            }
            None => self.parse_expr(target_xml, location)?,
        };

        let info = Box::new_with(|| BlockInfo { comment: comment.map(CompactString::new), location: location.collab_id.map(CompactString::new) });
        Ok(Box::new_with(|| NetworkMessage { target, msg_type: msg_type.into(), values: fields.iter().map(|&x| CompactString::new(x)).zip(values.into_iter().map(|x| *x)).collect(), info }))
    }
    #[inline(never)]
    fn parse_unknown_common(&mut self, stmt: &Xml, location: &LocationRef) -> Result<(Vec<Expr>, Box<BlockInfo>), Box<Error>> {
        let (argc, comment) = stmt.children.iter().enumerate().find(|(_, x)| x.name == "comment").map(|(i, x)| (i, Some(x.text.as_str()))).unwrap_or((stmt.children.len(), None));
        let mut args = Vec::with_capacity(argc);
        for arg in stmt.children[..argc].iter() {
            args.push_boxed(self.parse_expr(arg, &location)?);
        }
        Ok((args, Box::new_with(|| BlockInfo { comment: comment.map(CompactString::new), location: location.collab_id.map(CompactString::new) })))
    }
    #[inline(never)]
    fn parse_block(&mut self, stmt: &Xml) -> Result<Box<Stmt>, Box<Error>> {
        let mut location = Box::new_with(|| LocationRef {
            role: Some(&self.role.name),
            entity: Some(&self.entity.name),
            collab_id: get_collab_id(stmt),
            block_type: None,
        });
        let s = match stmt.attr("s") {
            None => return Err(Box::new_with(|| Error { kind: ProjectError::BlockWithoutType.into(), location: location.to_owned() })),
            Some(v) => v.value.as_str(),
        };
        location.block_type = Some(s);

        match s {
            "doDeclareVariables" => {
                let info = self.check_children_get_info(stmt, 1, &location)?;
                let mut vars = vec![];
                for var in stmt.children[0].children.iter() {
                    let entry = self.decl_local(var.text.clone(), 0f64.into(), &location)?;
                    vars.push(entry.def.clone());
                }
                Ok(Box::new_with(|| Stmt { kind: StmtKind::DeclareLocals { vars }, info }))
            }
            "doSetVar" | "doChangeVar" => {
                let info = self.check_children_get_info(stmt, 2, &location)?;
                let var = match stmt.children[0].name.as_str() {
                    "l" => self.reference_var(&stmt.children[0].text, &location)?,
                    _ => return Err(Box::new_with(|| Error { kind: CompileError::DerefAssignment.into(), location: location.to_owned() })),
                };
                let value = self.parse_expr(&stmt.children[1], &location)?;
                match s {
                    "doSetVar" => Ok(Box::new_with(|| Stmt { kind: StmtKind::Assign { var: *var, value }, info })),
                    "doChangeVar" => Ok(Box::new_with(|| Stmt { kind: StmtKind::AddAssign { var: *var, value }, info })),
                    _ => unreachable!(),
                }
            }
            "doShowVar" | "doHideVar" => {
                let info = self.check_children_get_info(stmt, 1, &location)?;
                let var = match stmt.children[0].name.as_str() {
                    "l" => self.reference_var(&stmt.children[0].text, &location)?,
                    _ => return Err(Box::new_with(|| Error { kind: CompileError::DerefAssignment.into(), location: location.to_owned() })),
                };
                match s {
                    "doShowVar" => Ok(Box::new_with(|| Stmt { kind: StmtKind::ShowVar { var: *var }, info })),
                    "doHideVar" => Ok(Box::new_with(|| Stmt { kind: StmtKind::HideVar { var: *var }, info })),
                    _ => unreachable!(),
                }
            }
            "doFor" => {
                let info = self.check_children_get_info(stmt, 4, &location)?;

                let var = match stmt.children[0].name.as_str() {
                    "l" => stmt.children[0].text.as_str(),
                    _ => return Err(Box::new_with(|| Error { kind: ProjectError::UpvarNotConst.into(), location: location.to_owned() })),
                };
                let start = self.parse_expr(&stmt.children[1], &location)?;
                let stop = self.parse_expr(&stmt.children[2], &location)?;
                let var = self.decl_local(CompactString::new(var), 0f64.into(), &location)?.def.ref_at(VarLocation::Local); // define after bounds, but before loop body
                let stmts = self.parse(&stmt.children[3])?.stmts;

                Ok(Box::new_with(|| Stmt { kind: StmtKind::ForLoop { var: *var, start, stop, stmts }, info }))
            }
            "doForEach" => {
                let info = self.check_children_get_info(stmt, 3, &location)?;

                let var = match stmt.children[0].name.as_str() {
                    "l" => stmt.children[0].text.as_str(),
                    _ => return Err(Box::new_with(|| Error { kind: ProjectError::UpvarNotConst.into(), location: location.to_owned() })),
                };
                let items = self.parse_expr(&stmt.children[1], &location)?;
                let var = self.decl_local(CompactString::new(var), 0f64.into(), &location)?.def.ref_at(VarLocation::Local); // define after bounds, but before loop body
                let stmts = self.parse(&stmt.children[2])?.stmts;

                Ok(Box::new_with(|| Stmt { kind: StmtKind::ForeachLoop { var: *var, items, stmts }, info }))
            }
            "doRepeat" | "doUntil" | "doIf" => {
                let info = self.check_children_get_info(stmt, 2, &location)?;
                let expr = self.parse_expr(&stmt.children[0], &location)?;
                let stmts = self.parse(&stmt.children[1])?.stmts;
                match s {
                    "doRepeat" => Ok(Box::new_with(|| Stmt { kind: StmtKind::Repeat { times: expr, stmts }, info })),
                    "doUntil" => Ok(Box::new_with(|| Stmt { kind: StmtKind::UntilLoop { condition: expr, stmts }, info })),
                    "doIf" => Ok(Box::new_with(|| Stmt { kind: StmtKind::If { condition: expr, then: stmts }, info })),
                    _ => unreachable!(),
                }
            }
            "doForever" => {
                let info = self.check_children_get_info(stmt, 1, &location)?;
                let stmts = self.parse(&stmt.children[0])?.stmts;
                Ok(Box::new_with(|| Stmt { kind: StmtKind::InfLoop { stmts }, info }))
            }
            "doIfElse" => {
                let info = self.check_children_get_info(stmt, 3, &location)?;
                let condition = self.parse_expr(&stmt.children[0], &location)?;
                let then = self.parse(&stmt.children[1])?.stmts;
                let otherwise = self.parse(&stmt.children[2])?.stmts;
                Ok(Box::new_with(|| Stmt { kind: StmtKind::IfElse { condition, then, otherwise }, info }))
            }
            "doTryCatch" => {
                let info = self.check_children_get_info(stmt, 3, &location)?;
                let code = self.parse(&stmt.children[0])?.stmts;
                let var = match stmt.children[1].name.as_str() {
                    "l" => self.decl_local(stmt.children[1].text.clone(), 0f64.into(), &location)?.def.ref_at(VarLocation::Local),
                    _ => return Err(Box::new_with(|| Error { kind: ProjectError::UpvarNotConst.into(), location: location.to_owned() })),
                };
                let handler = self.parse(&stmt.children[2])?.stmts;
                Ok(Box::new_with(|| Stmt { kind: StmtKind::TryCatch { code, var: *var, handler }, info }))
            }
            "doWarp" => {
                let info = self.check_children_get_info(stmt, 1, &location)?;
                let stmts = self.parse(&stmt.children[0])?.stmts;
                Ok(Box::new_with(|| Stmt { kind: StmtKind::Warp { stmts }, info }))
            }
            "doDeleteFromList" => {
                let info = self.check_children_get_info(stmt, 2, &location)?;
                let list = self.parse_expr(&stmt.children[1], &location)?;
                match stmt.children[0].get(&["option"]) {
                    Some(opt) => match opt.text.as_str() {
                        "last" => Ok(Box::new_with(|| Stmt { kind: StmtKind::ListRemoveLast { list }, info })),
                        "all" => Ok(Box::new_with(|| Stmt { kind: StmtKind::ListRemoveAll { list }, info })),
                        "" => Err(Box::new_with(|| Error { kind: CompileError::BlockOptionNotSelected.into(), location: location.to_owned() })),
                        x => Err(Box::new_with(|| Error { kind: ProjectError::BlockOptionUnknown { got: x.into() }.into(), location: location.to_owned() })),
                    }
                    None => {
                        let index = self.parse_expr(&stmt.children[0], &location)?;
                        Ok(Box::new_with(|| Stmt { kind: StmtKind::ListRemove { list, index }, info }))
                    }
                }
            }
            "doInsertInList" => {
                let info = self.check_children_get_info(stmt, 3, &location)?;
                let value = self.parse_expr(&stmt.children[0], &location)?;
                let list = self.parse_expr(&stmt.children[2], &location)?;
                match stmt.children[1].get(&["option"]) {
                    Some(opt) => match opt.text.as_str() {
                        "last" => Ok(Box::new_with(|| Stmt { kind: StmtKind::ListInsertLast { list, value }, info })),
                        "random" | "any" => Ok(Box::new_with(|| Stmt { kind: StmtKind::ListInsertRandom { list, value }, info })),
                        "" => Err(Box::new_with(|| Error { kind: CompileError::BlockOptionNotSelected.into(), location: location.to_owned() })),
                        x => Err(Box::new_with(|| Error { kind: ProjectError::BlockOptionUnknown { got: x.into() }.into(), location: location.to_owned() })),
                    }
                    None => {
                        let index = self.parse_expr(&stmt.children[1], &location)?;
                        Ok(Box::new_with(|| Stmt { kind: StmtKind::ListInsert { list, value, index }, info }))
                    }
                }
            }
            "doReplaceInList" => {
                let info = self.check_children_get_info(stmt, 3, &location)?;
                let value = self.parse_expr(&stmt.children[2], &location)?;
                let list = self.parse_expr(&stmt.children[1], &location)?;
                match stmt.children[0].get(&["option"]) {
                    Some(opt) => match opt.text.as_str() {
                        "last" => Ok(Box::new_with(|| Stmt { kind: StmtKind::ListAssignLast { list, value }, info })),
                        "random" | "any" => Ok(Box::new_with(|| Stmt { kind: StmtKind::ListAssignRandom { list, value }, info })),
                        "" => Err(Box::new_with(|| Error{ kind: CompileError::BlockOptionNotSelected.into(), location: location.to_owned() })),
                        x => Err(Box::new_with(|| Error { kind: ProjectError::BlockOptionUnknown { got: x.into() }.into(), location: location.to_owned() })),
                    }
                    None => {
                        let index = self.parse_expr(&stmt.children[0], &location)?;
                        Ok(Box::new_with(|| Stmt { kind: StmtKind::ListAssign { list, value, index }, info }))
                    }
                }
            }
            "doStopThis" => {
                let info = self.check_children_get_info(stmt, 1, &location)?;
                let mode = match self.grab_option(&stmt.children[0], &location)? {
                    "all" => StopMode::All,
                    "all scenes" => StopMode::AllScenes,
                    "this script" => StopMode::ThisScript,
                    "this block" => StopMode::ThisBlock,
                    "all but this script" => StopMode::AllButThisScript,
                    "other scripts in sprite" => StopMode::OtherScriptsInSprite,
                    x => return Err(Box::new_with(|| Error { kind: CompileError::CurrentlyUnsupported { msg: format!("{s} with stop mode {x:?} is currently not supported").into() }.into(), location: location.to_owned() })),
                };
                Ok(Box::new_with(|| Stmt { kind: StmtKind::Stop { mode }, info }))
            }
            "doSwitchToCostume" => {
                let info = self.check_children_get_info(stmt, 1, &location)?;
                let val = &stmt.children[0];

                if val.name == "l" && val.get(&["option"]).is_some() {
                    match self.grab_option(val, &location)? {
                        "Turtle" => Ok(Box::new_with(|| Stmt { kind: StmtKind::SetCostume { costume: Box::new_with(|| "".into()) }, info })),
                        x => Err(Box::new_with(|| Error { kind: CompileError::CurrentlyUnsupported { msg: format!("{s} with builtin project costume ({x}) currently not supported").into() }.into(), location: location.to_owned() })),
                    }
                } else {
                    let costume = self.parse_expr(val, &location)?;
                    Ok(Box::new_with(|| Stmt { kind: StmtKind::SetCostume { costume }, info }))
                }
            }
            "playSound" | "doPlaySoundUntilDone" => {
                let blocking = s == "doPlaySoundUntilDone";
                let info = self.check_children_get_info(stmt, 1, &location)?;
                let sound = self.parse_expr(&stmt.children[0], &location)?;
                Ok(Box::new_with(|| Stmt { kind: StmtKind::PlaySound { sound, blocking }, info }))
            }
            "setHeading" => {
                let info = self.check_children_get_info(stmt, 1, &location)?;
                let child = &stmt.children[0];

                if child.name == "l" && child.get(&["option"]).is_some() {
                    let opt = self.grab_option(child, &location)?;
                    match opt {
                        "random" => Ok(Box::new_with(|| Stmt { kind: StmtKind::SetHeadingRandom, info })),
                        _ => Err(Box::new_with(|| Error { kind: ProjectError::BlockOptionUnknown { got: opt.into() }.into(), location: location.to_owned() })),
                    }
                } else {
                    let value = self.parse_expr(child, &location)?;
                    Ok(Box::new_with(|| Stmt { kind: StmtKind::SetHeading { value }, info }))
                }
            }
            "doGotoObject" => {
                let info = self.check_children_get_info(stmt, 1, &location)?;
                let child = &stmt.children[0];

                if child.name == "l" && child.get(&["option"]).is_some() {
                    let opt = self.grab_option(child, &location)?;
                    match opt {
                        "random position" => Ok(Box::new_with(|| Stmt { kind: StmtKind::GotoRandom, info })),
                        "mouse-pointer" => Ok(Box::new_with(|| Stmt { kind: StmtKind::GotoMouse, info })),
                        "center" => Ok(Box::new_with(|| Stmt { kind: StmtKind::GotoXY { x: Box::new_with(|| 0f64.into()), y: Box::new_with(|| 0f64.into()) }, info })),
                        _ => Err(Box::new_with(|| Error { kind: ProjectError::BlockOptionUnknown { got: opt.into() }.into(), location: location.to_owned() })),
                    }
                }
                else {
                    let target = self.parse_expr(child, &location)?;
                    Ok(Box::new_with(|| Stmt { kind: StmtKind::Goto { target }, info }))
                }
            }
            "doFaceTowards" => {
                let info = self.check_children_get_info(stmt, 1, &location)?;
                let child = &stmt.children[0];

                if child.name == "l" && child.get(&["option"]).is_some() {
                    let opt = self.grab_option(child, &location)?;
                    match opt {
                        "center" => Ok(Box::new_with(|| Stmt { kind: StmtKind::PointTowardsXY { x: Box::new_with(|| 0.0.into()), y: Box::new_with(|| 0.0.into()) }, info })),
                        _ => Err(Box::new_with(|| Error { kind: ProjectError::BlockOptionUnknown { got: opt.into() }.into(), location: location.to_owned() })),
                    }
                } else {
                    let target = self.parse_expr(child, &location)?;
                    Ok(Box::new_with(|| Stmt { kind: StmtKind::PointTowards { target }, info }))
                }
            }
            "setColor" => {
                let info = self.check_children_get_info(stmt, 1, &location)?;
                match stmt.get(&["color"]) {
                    Some(color) => match parse_color(&color.text) {
                        Some(color) => Ok(Box::new_with(|| Stmt { kind: StmtKind::SetPenColor { color }, info })),
                        None => Err(Box::new_with(|| Error { kind: ProjectError::ColorUnknownValue { color: color.text.clone() }.into(), location: location.to_owned() })),
                    }
                    None => Err(Box::new_with(|| Error { kind: CompileError::BlockOptionNotConst.into(), location: location.to_owned() })),
                }
            }
            "doSocketMessage" => {
                let res = self.parse_send_message_common(stmt, &location)?;
                Ok(Box::new_with(|| {
                    let NetworkMessage { target, msg_type, values, info } = *res;
                    Stmt { kind: StmtKind::SendNetworkMessage { target, msg_type, values }, info }
                }))
            }
            "doRun" | "fork" => {
                let is_run = s == "doRun";
                let info = self.check_children_get_info(stmt, 2, &location)?;
                let closure = self.parse_expr(&stmt.children[0], &location)?;
                let mut args = Vec::with_capacity(stmt.children[1].children.len());
                for arg in stmt.children[1].children.iter() {
                    args.push_boxed(self.parse_expr(arg, &location)?);
                }
                match is_run {
                    true => Ok(Box::new_with(|| Stmt { kind: StmtKind::CallClosure { new_entity: None, closure, args }, info })),
                    false => Ok(Box::new_with(|| Stmt { kind: StmtKind::ForkClosure { closure, args }, info })),
                }
            }
            "doTellTo" => {
                let info = self.check_children_get_info(stmt, 3, &location)?;
                let entity = self.grab_entity(&stmt.children[0], BlockInfo::none(), &location)?;
                let closure = self.parse_expr(&stmt.children[1], &location)?;
                let mut args = Vec::with_capacity(stmt.children[2].children.len());
                for arg in stmt.children[2].children.iter() {
                    args.push_boxed(self.parse_expr(arg, &location)?);
                }
                Ok(Box::new_with(|| Stmt { kind: StmtKind::CallClosure { new_entity: Some(entity), closure, args }, info }))
            }
            "setEffect" => {
                let info = self.check_children_get_info(stmt, 2, &location)?;
                let effect = self.parse_effect(&stmt.children[0], &location)?;
                let value = self.parse_expr(&stmt.children[1], &location)?;
                Ok(Box::new_with(|| Stmt { kind: StmtKind::SetEffect { kind: effect, value }, info }))
            }
            "changeEffect" => {
                let info = self.check_children_get_info(stmt, 2, &location)?;
                let effect = self.parse_effect(&stmt.children[0], &location)?;
                let delta = self.parse_expr(&stmt.children[1], &location)?;
                Ok(Box::new_with(|| Stmt { kind: StmtKind::ChangeEffect { kind: effect, delta }, info }))
            }
            "setPenHSVA" => {
                let info = self.check_children_get_info(stmt, 2, &location)?;
                let attr = self.parse_pen_attr(&stmt.children[0], &location)?;
                let value = self.parse_expr(&stmt.children[1], &location)?;
                Ok(Box::new_with(|| Stmt { kind: StmtKind::SetPenAttr { attr, value }, info }))
            }
            "changePenHSVA" => {
                let info = self.check_children_get_info(stmt, 2, &location)?;
                let attr = self.parse_pen_attr(&stmt.children[0], &location)?;
                let delta = self.parse_expr(&stmt.children[1], &location)?;
                Ok(Box::new_with(|| Stmt { kind: StmtKind::ChangePenAttr { attr, delta }, info }))
            }
            "doRunRPC" => {
                let rpc = self.parse_rpc(stmt, &location)?;
                Ok(Box::new_with(|| (*rpc).into()))
            }
            "createClone" => {
                let info = self.check_children_get_info(stmt, 1, &location)?;
                let target = self.grab_entity(&stmt.children[0], BlockInfo::none(), &location)?;
                Ok(Box::new_with(|| Stmt { kind: StmtKind::Clone { target }, info }))
            }
            "doSend" => {
                let info = self.check_children_get_info(stmt, 2, &location)?;
                let msg_type = self.parse_expr(&stmt.children[0], &location)?;
                let target = Some(self.grab_entity(&stmt.children[1], BlockInfo::none(), &location)?);
                Ok(Box::new_with(|| Stmt { kind: StmtKind::SendLocalMessage { msg_type, target, wait: false }, info }))
            }
            "doStopAllSounds" => self.parse_0_args(stmt, &location).map(|info| Box::new_with(|| Stmt { kind: StmtKind::StopSounds, info })),
            "doBroadcast" => self.parse_1_args(stmt, &location).map(|(msg_type, info)| Box::new_with(|| Stmt { kind: StmtKind::SendLocalMessage { msg_type, target: None, wait: false }, info })),
            "doBroadcastAndWait" => self.parse_1_args(stmt, &location).map(|(msg_type, info)| Box::new_with(|| Stmt { kind: StmtKind::SendLocalMessage { msg_type, target: None, wait: true }, info })),
            "doPauseAll" => self.parse_0_args(stmt, &location).map(|info| Box::new_with(|| Stmt { kind: StmtKind::Pause, info })),
            "write" => self.parse_2_args(stmt, &location).map(|(content, font_size, info)| Box::new_with(|| Stmt { kind: StmtKind::Write { content, font_size }, info })),
            "doSocketResponse" => self.parse_1_args(stmt, &location).map(|(value, info)| Box::new_with(|| Stmt { kind: StmtKind::SendNetworkReply { value }, info })),
            "changeScale" => self.parse_1_args(stmt, &location).map(|(delta, info)| Box::new_with(|| Stmt { kind: StmtKind::ChangeSize { delta, }, info })),
            "setScale" => self.parse_1_args(stmt, &location).map(|(value, info)| Box::new_with(|| Stmt { kind: StmtKind::SetSize { value }, info })),
            "doSayFor" => self.parse_2_args(stmt, &location).map(|(content, duration, info)| Box::new_with(|| Stmt { kind: StmtKind::Say { content, duration: Some(duration) }, info })),
            "doThinkFor" => self.parse_2_args(stmt, &location).map(|(content, duration, info)| Box::new_with(|| Stmt { kind: StmtKind::Think { content, duration: Some(duration) }, info })),
            "bubble" => self.parse_1_args(stmt, &location).map(|(content, info)| Box::new_with(|| Stmt { kind: StmtKind::Say { content, duration: None }, info })),
            "doThink" => self.parse_1_args(stmt, &location).map(|(content, info)| Box::new_with(|| Stmt { kind: StmtKind::Think { content, duration: None }, info })),
            "doThrow" => self.parse_1_args(stmt, &location).map(|(error, info)| Box::new_with(|| Stmt { kind: StmtKind::Throw { error }, info })),
            "hide" => self.parse_0_args(stmt, &location).map(|info| Box::new_with(|| Stmt { kind: StmtKind::SetVisible { value: false }, info })),
            "show" => self.parse_0_args(stmt, &location).map(|info| Box::new_with(|| Stmt { kind: StmtKind::SetVisible { value: true }, info })),
            "removeClone" => self.parse_0_args(stmt, &location).map(|info| Box::new_with(|| Stmt { kind: StmtKind::DeleteClone, info })),
            "doWaitUntil" => self.parse_1_args(stmt, &location).map(|(condition, info)| Box::new_with(|| Stmt { kind: StmtKind::WaitUntil { condition, }, info })),
            "changeSize" => self.parse_1_args(stmt, &location).map(|(delta, info)| Box::new_with(|| Stmt { kind: StmtKind::ChangePenSize { delta, }, info })),
            "setSize" => self.parse_1_args(stmt, &location).map(|(value, info)| Box::new_with(|| Stmt { kind: StmtKind::SetPenSize { value }, info })),
            "doAddToList" => self.parse_2_args(stmt, &location).map(|(value, list, info)| Box::new_with(|| Stmt { kind: StmtKind::ListInsertLast { value, list }, info })),
            "doReport" => self.parse_1_args(stmt, &location).map(|(value, info)| Box::new_with(|| Stmt { kind: StmtKind::Return { value }, info })),
            "doStamp" => self.parse_0_args(stmt, &location).map(|info| Box::new_with(|| Stmt { kind: StmtKind::Stamp, info })),
            "doWait" => self.parse_1_args(stmt, &location).map(|(seconds, info)| Box::new_with(|| Stmt { kind: StmtKind::Sleep { seconds, }, info })),
            "forward" => self.parse_1_args(stmt, &location).map(|(distance, info)| Box::new_with(|| Stmt { kind: StmtKind::Forward { distance, }, info })),
            "turn" => self.parse_1_args(stmt, &location).map(|(angle, info)| Box::new_with(|| Stmt { kind: StmtKind::TurnRight { angle, }, info })),
            "turnLeft" => self.parse_1_args(stmt, &location).map(|(angle, info)| Box::new_with(|| Stmt { kind: StmtKind::TurnLeft { angle, }, info })),
            "setXPosition" => self.parse_1_args(stmt, &location).map(|(value, info)| Box::new_with(|| Stmt { kind: StmtKind::SetX { value }, info })),
            "setYPosition" => self.parse_1_args(stmt, &location).map(|(value, info)| Box::new_with(|| Stmt { kind: StmtKind::SetY { value }, info })),
            "changeXPosition" => self.parse_1_args(stmt, &location).map(|(delta, info)| Box::new_with(|| Stmt { kind: StmtKind::ChangeX { delta }, info })),
            "changeYPosition" => self.parse_1_args(stmt, &location).map(|(delta, info)| Box::new_with(|| Stmt { kind: StmtKind::ChangeY { delta }, info })),
            "gotoXY" => self.parse_2_args(stmt, &location).map(|(x, y, info)| Box::new_with(|| Stmt { kind: StmtKind::GotoXY { x, y }, info })),
            "bounceOffEdge" => self.parse_0_args(stmt, &location).map(|info| Box::new_with(|| Stmt { kind: StmtKind::BounceOffEdge, info })),
            "down" => self.parse_0_args(stmt, &location).map(|info| Box::new_with(|| Stmt { kind: StmtKind::SetPenDown { value: true }, info })),
            "up" => self.parse_0_args(stmt, &location).map(|info| Box::new_with(|| Stmt { kind: StmtKind::SetPenDown { value: false }, info })),
            "clear" => self.parse_0_args(stmt, &location).map(|info| Box::new_with(|| Stmt { kind: StmtKind::PenClear, info })),
            "doAsk" => self.parse_1_args(stmt, &location).map(|(prompt, info)| Box::new_with(|| Stmt { kind: StmtKind::Ask { prompt, }, info })),
            "doResetTimer" => self.parse_0_args(stmt, &location).map(|info| Box::new_with(|| Stmt { kind: StmtKind::ResetTimer, info })),
            "clearEffects" => self.parse_0_args(stmt, &location).map(|info| Box::new_with(|| Stmt { kind: StmtKind::ClearEffects, info })),
            "doWearNextCostume" => self.parse_0_args(stmt, &location).map(|info| Box::new_with(|| Stmt { kind: StmtKind::NextCostume, info })),
            x => self.parse_unknown_common(stmt, &location).map(|(args, info)| Box::new_with(|| Stmt { kind: StmtKind::UnknownBlock { name: x.into(), args }, info })),
        }
    }
    #[inline(never)]
    fn reference_var(&mut self, name: &str, location: &LocationRef) -> Result<Box<VariableRef>, Box<Error>> {
        for (i, locals) in self.locals.iter().rev().enumerate() {
            if let Some(x) = locals.0.get(name) {
                let local_ref = x.def.ref_at(VarLocation::Local);
                if i != 0 {
                    let (locals, captures) = self.locals.last_mut().unwrap();
                    locals.define(local_ref.name.clone(), 0.0.into()).unwrap();
                    captures.push_boxed(local_ref.clone());
                }
                return Ok(local_ref);
            }
        }
        if let Some(x) = self.entity.fields.get(name) {
            let field_ref = x.def.ref_at(VarLocation::Field);
            if self.locals.len() >= 2 {
                let (locals, captures) = self.locals.last_mut().unwrap();
                locals.define(field_ref.name.clone(), 0.0.into()).unwrap();
                captures.push_boxed(field_ref);
                return Ok(x.def.ref_at(VarLocation::Local));
            } else {
                return Ok(field_ref);
            }
        }
        if let Some(x) = self.role.globals.get(name) {
            return Ok(x.def.ref_at(VarLocation::Global));
        }
        Err(Box::new_with(|| Error { kind: CompileError::UndefinedVariable { name: name.into() }.into(), location: location.to_owned() }))
    }
    #[inline(never)]
    fn reference_fn(&self, name: &str, location: &LocationRef) -> Result<Box<(FnRef, Value)>, Box<Error>> {
        let locs = [(&self.entity.funcs, FnLocation::Method), (&self.role.funcs, FnLocation::Global)];
        match locs.iter().find_map(|v| v.0.get(name).map(|x| (v, x))) {
            Some((v, x)) => Ok(Box::new_with(|| (*x.def.fn_ref_at(v.1), x.init.clone()))),
            None => Err(Box::new_with(|| Error { kind: CompileError::UndefinedFn { name: name.into() }.into(), location: location.to_owned() }))
        }
    }
    #[inline(always)]
    fn parse_0_args(&mut self, expr: &Xml, location: &LocationRef) -> Result<Box<BlockInfo>, Box<Error>> {
        self.check_children_get_info(expr, 0, location)
    }
    #[inline(always)]
    fn parse_1_args(&mut self, expr: &Xml, location: &LocationRef) -> Result<(Box<Expr>, Box<BlockInfo>), Box<Error>> {
        let info = self.check_children_get_info(expr, 1, location)?;
        let a = self.parse_expr(&expr.children[0], location)?;
        Ok((a, info))
    }
    #[inline(always)]
    fn parse_2_args(&mut self, expr: &Xml, location: &LocationRef) -> Result<(Box<Expr>, Box<Expr>, Box<BlockInfo>), Box<Error>> {
        let info = self.check_children_get_info(expr, 1, location)?;
        let a = self.parse_expr(&expr.children[0], location)?;
        let b = self.parse_expr(&expr.children[1], location)?;
        Ok((a, b, info))
    }
    #[inline(never)]
    fn parse_bool(&self, val: &str, location: &LocationRef) -> Result<Box<Expr>, Box<Error>> {
        match val {
            "true" => Ok(Box::new_with(|| true.into())),
            "false" => Ok(Box::new_with(|| false.into())),
            _ => Err(Box::new_with(|| Error { kind: ProjectError::BoolUnknownValue { got: val.into() }.into(), location: location.to_owned() })),
        }
    }
    #[inline(never)]
    fn parse_closure(&mut self, expr: &Xml, kind: ClosureKind, inline_script: bool, location: &LocationRef) -> Result<Box<Expr>, Box<Error>> {
        let (info, script) = match inline_script {
            false => (self.check_children_get_info(expr, 2, location)?, &expr.children[0]),
            true => (BlockInfo::none(), expr),
        };

        let mut params = SymbolTable::new(self.parser);
        fn define_param(params: &mut SymbolTable, name: CompactString, location: &LocationRef) -> Result<(), Box<Error>> {
            match params.define(name, 0.0.into()) {
                Ok(None) => Ok(()),
                Ok(Some(prev)) => Err(Box::new_with(|| Error { kind: CompileError::InputsWithSameName { name: prev.def.name }.into(), location: location.to_owned() })),
                Err(SymbolError::ConflictingTrans { trans_name, names }) => Err(Box::new_with(|| Error { kind: CompileError::LocalsWithSameTransName { trans_name, names }.into(), location: location.to_owned() })),
                Err(SymbolError::NameTransformError { name }) => Err(Box::new_with(|| Error { kind: CompileError::NameTransformError { name }.into(), location: location.to_owned() })),
            }
        }
        if !inline_script {
            for input in expr.children[1].children.iter() {
                define_param(&mut params, input.text.clone(), location)?;
            }
        }

        let prev_autofill_args_len = self.autofill_args.as_ref().map(|x| x.len()).unwrap_or_default();
        let prev_autofill_args = match params.is_empty() && !inline_script {
            true => Some(mem::replace(&mut self.autofill_args, Some(vec![]))),
            false => None,
        };

        self.locals.push((params.clone(), Default::default()));
        let locals_len = self.locals.len();
        let stmts = match kind {
            ClosureKind::Command => self.parse(script)?.stmts,
            ClosureKind::Reporter | ClosureKind::Predicate => {
                let value = match script.name.as_str() {
                    "autolambda" => {
                        let _ = self.check_children_get_info(script, 1, location)?;
                        self.parse_expr(&script.children[0], location)?
                    }
                    _ => self.parse_expr(script, location)?,
                };
                vec![Stmt { kind: StmtKind::Return { value }, info: BlockInfo::none() }]
            }
        };
        assert_eq!(locals_len, self.locals.len());
        let (_, mut captures) = self.locals.pop().unwrap();
        for var in captures.iter() {
            self.reference_var(&var.name, location)?;
        }

        match prev_autofill_args {
            Some(prev_autofill_args) => for autofill_arg in mem::replace(&mut self.autofill_args, prev_autofill_args).unwrap_or_default() {
                define_param(&mut params, autofill_arg.name, location)?;
            }
            None => for autofill_arg in self.autofill_args.as_deref().map(|x| &x[prev_autofill_args_len..]).unwrap_or_default() {
                captures.push(autofill_arg.clone());
            }
        }

        Ok(Box::new_with(|| Expr { kind: ExprKind::Closure { params: params.into_defs(), captures, kind, stmts }, info }))
    }
    #[inline(never)]
    fn parse_expr(&mut self, expr: &Xml, location: &LocationRef) -> Result<Box<Expr>, Box<Error>> {
        let mut location = Box::new_with(|| LocationRef {
            role: location.role,
            entity: location.entity,
            collab_id: get_collab_id(expr).or(location.collab_id),
            block_type: location.block_type,
        });

        match expr.name.as_str() {
            "l" => match expr.children.first() {
                Some(child) if child.name == "bool" => self.parse_bool(&child.text, &location),
                _ => match self.autofill_args.as_mut() {
                    Some(autofill_args) if expr.text.is_empty() => {
                        let var = Box::try_new_with(|| {
                            let input = autofill_args.len() + 1;
                            let name = match self.parser.autofill_generator.as_ref()(input) {
                                Ok(x) => x,
                                Err(()) => return Err(Box::new_with(|| Error { kind: CompileError::AutofillGenerateError { input }.into(), location: location.to_owned() })),
                            };
                            let trans_name = match self.parser.name_transformer.as_ref()(&name) {
                                Ok(x) => x,
                                Err(()) => return Err(Box::new_with(|| Error { kind: CompileError::NameTransformError { name }.into(), location: location.to_owned() })),
                            };
                            Ok(VariableRef { name, trans_name, location: VarLocation::Local })
                        })?;

                        autofill_args.push_with(|| (*var).clone());

                        Ok(Box::new_with(|| Expr { kind: ExprKind::Variable { var: *var }, info: BlockInfo::none() }))
                    }
                    _ => Ok(Box::new_with(|| expr.text.clone().into())),
                }
            }
            "bool" => self.parse_bool(&expr.text, &location),
            "list" => {
                let ref_id = expr.attr("id").and_then(|x| x.value.parse().ok()).map(RefId);
                let values = match expr.attr("struct").map(|x| x.value.as_str()) {
                    Some("atomic") => InlineListIter::new(&expr.text).map(Into::into).collect(),
                    _ => {
                        let mut values = Vec::with_capacity(expr.children.len());
                        for item in expr.children.iter() {
                            values.push_boxed(match item.name.as_str() {
                                "item" => match item.children.get(0) {
                                    Some(x) => self.parse_expr(x, &location)?,
                                    None => Box::new_with(|| Expr { kind: ExprKind::Value(Value::String(item.text.clone())), info: BlockInfo::none() }),
                                }
                                _ => self.parse_expr(item, &location)?,
                            });
                        }
                        values
                    }
                };

                let mut evaluated = Vec::with_capacity(values.len());
                for value in values.iter() {
                    match &value.kind {
                        ExprKind::Value(x) => evaluated.push_with(|| x.clone()),
                        _ => match ref_id {
                            Some(_) => return Err(Box::new_with(|| Error { kind: ProjectError::ValueNotEvaluated.into(), location: location.to_owned() })),
                            None => return Ok(Box::new_with(|| Expr { kind: ExprKind::MakeList { values }, info: BlockInfo::none() })),
                        }
                    }
                }
                Ok(Box::new_with(|| Value::List(evaluated, ref_id).into()))
            }
            "ref" => match expr.attr("id").and_then(|x| x.value.parse().ok()).map(RefId) {
                Some(ref_id) => Ok(Box::new_with(|| Value::Ref(ref_id).into())),
                None => return Err(Box::new_with(|| Error { kind: ProjectError::RefMissingId.into(), location: location.to_owned() })),
            }
            "custom-block" => {
                let res = self.parse_fn_call(expr, &location)?;
                Ok(Box::new_with(|| {
                    let FnCall { function, args, upvars, info } = *res;
                    Expr { kind: ExprKind::CallFn { function, args, upvars }, info }
                }))
            }
            "script" => self.parse_closure(expr, ClosureKind::Command, true, &location),
            "block" => {
                if let Some(var) = expr.attr("var") {
                    let info = self.check_children_get_info(expr, 0, &location)?;
                    let var = self.reference_var(&var.value, &location)?;
                    return Ok(Box::new_with(|| Expr { kind: ExprKind::Variable { var: *var }, info }));
                }
                let s = match expr.attr("s") {
                    None => return Err(Box::new_with(|| Error { kind: ProjectError::BlockWithoutType.into(), location: location.to_owned() })),
                    Some(v) => v.value.as_str(),
                };
                location.block_type = Some(s);

                match s {
                    "reportVariadicSum" => self.parse_1_args(expr, &location).map(|(values, info)| Box::new_with(|| Expr { kind: ExprKind::Add { values }, info })),
                    "reportVariadicProduct" => self.parse_1_args(expr, &location).map(|(values, info)| Box::new_with(|| Expr { kind: ExprKind::Mul { values }, info })),
                    "reportVariadicMin" => self.parse_1_args(expr, &location).map(|(values, info)| Box::new_with(|| Expr { kind: ExprKind::Min { values }, info })),
                    "reportVariadicMax" => self.parse_1_args(expr, &location).map(|(values, info)| Box::new_with(|| Expr { kind: ExprKind::Max { values }, info })),

                    "reportSum" => self.parse_2_args(expr, &location).map(|(left, right, info)| Box::new_with(|| Expr { kind: ExprKind::Add { values: Box::new_with(|| Expr { kind: ExprKind::MakeList { values: vec![*left, *right] }, info: BlockInfo::none() }) }, info })),
                    "reportProduct" => self.parse_2_args(expr, &location).map(|(left, right, info)| Box::new_with(|| Expr { kind: ExprKind::Mul { values: Box::new_with(|| Expr { kind: ExprKind::MakeList { values: vec![*left, *right] }, info: BlockInfo::none() }) }, info })),
                    "reportMin" => self.parse_2_args(expr, &location).map(|(left, right, info)| Box::new_with(|| Expr { kind: ExprKind::Min { values: Box::new_with(|| Expr { kind: ExprKind::MakeList { values: vec![*left, *right] }, info: BlockInfo::none() }) }, info })),
                    "reportMax" => self.parse_2_args(expr, &location).map(|(left, right, info)| Box::new_with(|| Expr { kind: ExprKind::Max { values: Box::new_with(|| Expr { kind: ExprKind::MakeList { values: vec![*left, *right] }, info: BlockInfo::none() }) }, info })),

                    "reportDifference" => self.parse_2_args(expr, &location).map(|(left, right, info)| Box::new_with(|| Expr { kind: ExprKind::Sub { left, right }, info })),
                    "reportQuotient" => self.parse_2_args(expr, &location).map(|(left, right, info)| Box::new_with(|| Expr { kind: ExprKind::Div { left, right }, info })),
                    "reportModulus" => self.parse_2_args(expr, &location).map(|(left, right, info)| Box::new_with(|| Expr { kind: ExprKind::Mod { left, right }, info })),
                    "reportPower" => self.parse_2_args(expr, &location).map(|(base, power, info)| Box::new_with(|| Expr { kind: ExprKind::Pow { base, power }, info })),
                    "reportAtan2" => self.parse_2_args(expr, &location).map(|(y, x, info)| Box::new_with(|| Expr { kind: ExprKind::Atan2 { y, x }, info })),

                    "reportAnd" => self.parse_2_args(expr, &location).map(|(left, right, info)| Box::new_with(|| Expr { kind: ExprKind::And { left, right }, info })),
                    "reportOr" => self.parse_2_args(expr, &location).map(|(left, right, info)| Box::new_with(|| Expr { kind: ExprKind::Or { left, right }, info })),

                    "reportIsIdentical" => self.parse_2_args(expr, &location).map(|(left, right, info)| Box::new_with(|| Expr { kind: ExprKind::Identical { left, right }, info })),
                    "reportEquals" => self.parse_2_args(expr, &location).map(|(left, right, info)| Box::new_with(|| Expr { kind: ExprKind::Eq { left, right }, info })),
                    "reportNotEquals" => self.parse_2_args(expr, &location).map(|(left, right, info)| Box::new_with(|| Expr { kind: ExprKind::Neq { left, right }, info })),
                    "reportLessThan" => self.parse_2_args(expr, &location).map(|(left, right, info)| Box::new_with(|| Expr { kind: ExprKind::Less { left, right }, info })),
                    "reportLessThanOrEquals" => self.parse_2_args(expr, &location).map(|(left, right, info)| Box::new_with(|| Expr { kind: ExprKind::LessEq { left, right }, info })),
                    "reportGreaterThan" => self.parse_2_args(expr, &location).map(|(left, right, info)| Box::new_with(|| Expr { kind: ExprKind::Greater { left, right }, info })),
                    "reportGreaterThanOrEquals" => self.parse_2_args(expr, &location).map(|(left, right, info)| Box::new_with(|| Expr { kind: ExprKind::GreaterEq { left, right }, info })),

                    "reportRandom" => self.parse_2_args(expr, &location).map(|(a, b, info)| Box::new_with(|| Expr { kind: ExprKind::Random { a, b }, info })),
                    "reportNumbers" => self.parse_2_args(expr, &location).map(|(start, stop, info)| Box::new_with(|| Expr { kind: ExprKind::Range { start, stop }, info })),

                    "reportNot" => self.parse_1_args(expr, &location).map(|(value, info)| Box::new_with(|| Expr { kind: ExprKind::Not { value }, info })),
                    "reportRound" => self.parse_1_args(expr, &location).map(|(value, info)| Box::new_with(|| Expr { kind: ExprKind::Round { value }, info })),

                    "reportListLength" => self.parse_1_args(expr, &location).map(|(value, info)| Box::new_with(|| Expr { kind: ExprKind::ListLen { value }, info })),
                    "reportListIsEmpty" => self.parse_1_args(expr, &location).map(|(value, info)| Box::new_with(|| Expr { kind: ExprKind::ListIsEmpty { value }, info })),

                    "reportStringSize" => self.parse_1_args(expr, &location).map(|(value, info)| Box::new_with(|| Expr { kind: ExprKind::StrLen { value }, info })),
                    "reportUnicodeAsLetter" => self.parse_1_args(expr, &location).map(|(value, info)| Box::new_with(|| Expr { kind: ExprKind::UnicodeToChar { value }, info })),
                    "reportUnicode" => self.parse_1_args(expr, &location).map(|(value, info)| Box::new_with(|| Expr { kind: ExprKind::CharToUnicode { value }, info })),

                    "reportCDR" => self.parse_1_args(expr, &location).map(|(value, info)| Box::new_with(|| Expr { kind: ExprKind::ListCdr { value }, info })),
                    "reportCONS" => self.parse_2_args(expr, &location).map(|(item, list, info)| Box::new_with(|| Expr { kind: ExprKind::ListCons { item, list }, info })),

                    "reportJoinWords" => self.parse_1_args(expr, &location).map(|(values, info)| Box::new_with(|| Expr { kind: ExprKind::StrCat { values }, info })),
                    "reportConcatenatedLists" => self.parse_1_args(expr, &location).map(|(lists, info)| Box::new_with(|| Expr { kind: ExprKind::ListCat { lists }, info })),
                    "reportCrossproduct" => self.parse_1_args(expr, &location).map(|(sources, info)| Box::new_with(|| Expr { kind: ExprKind::ListCombinations { sources }, info })),

                    "reportStageWidth" => self.parse_0_args(expr, &location).map(|info| Box::new_with(|| Expr { kind: ExprKind::StageWidth, info })),
                    "reportStageHeight" => self.parse_0_args(expr, &location).map(|info| Box::new_with(|| Expr { kind: ExprKind::StageHeight, info })),

                    "reportMouseX" => self.parse_0_args(expr, &location).map(|info| Box::new_with(|| Expr { kind: ExprKind::MouseX, info })),
                    "reportMouseY" => self.parse_0_args(expr, &location).map(|info| Box::new_with(|| Expr { kind: ExprKind::MouseY, info })),

                    "reportLatitude" => self.parse_0_args(expr, &location).map(|info| Box::new_with(|| Expr { kind: ExprKind::Latitude, info })),
                    "reportLongitude" => self.parse_0_args(expr, &location).map(|info| Box::new_with(|| Expr { kind: ExprKind::Longitude, info })),

                    "reportPenTrailsAsCostume" => self.parse_0_args(expr, &location).map(|info| Box::new_with(|| Expr { kind: ExprKind::ImageOfDrawings, info })),

                    "reportListContainsItem" => self.parse_2_args(expr, &location).map(|(list, value, info)| Box::new_with(|| Expr { kind: ExprKind::ListContains { list, value }, info })),

                    "reportRPCError" => self.parse_0_args(expr, &location).map(|info| Box::new_with(|| Expr { kind: ExprKind::RpcError, info })),

                    "getScale" => self.parse_0_args(expr, &location).map(|info| Box::new_with(|| Expr { kind: ExprKind::Size, info })),
                    "reportShown" => self.parse_0_args(expr, &location).map(|info| Box::new_with(|| Expr { kind: ExprKind::IsVisible, info })),

                    "xPosition" => self.parse_0_args(expr, &location).map(|info| Box::new_with(|| Expr { kind: ExprKind::XPos, info })),
                    "yPosition" => self.parse_0_args(expr, &location).map(|info| Box::new_with(|| Expr { kind: ExprKind::YPos, info })),
                    "direction" => self.parse_0_args(expr, &location).map(|info| Box::new_with(|| Expr { kind: ExprKind::Heading, info })),

                    "getPenDown" => self.parse_0_args(expr, &location).map(|info| Box::new_with(|| Expr { kind: ExprKind::PenDown, info })),

                    "getLastAnswer" => self.parse_0_args(expr, &location).map(|info| Box::new_with(|| Expr { kind: ExprKind::Answer, info })),
                    "getLastMessage" => self.parse_0_args(expr, &location).map(|info| Box::new_with(|| Expr { kind: ExprKind::Message, info })),

                    "getTimer" => self.parse_0_args(expr, &location).map(|info| Box::new_with(|| Expr { kind: ExprKind::Timer, info })),

                    "reportMap" => self.parse_2_args(expr, &location).map(|(f, list, info)| Box::new_with(|| Expr { kind: ExprKind::Map { f, list }, info })),
                    "reportKeep" => self.parse_2_args(expr, &location).map(|(f, list, info)| Box::new_with(|| Expr { kind: ExprKind::Keep { f, list }, info })),
                    "reportFindFirst" => self.parse_2_args(expr, &location).map(|(f, list, info)| Box::new_with(|| Expr { kind: ExprKind::FindFirst { f, list }, info })),
                    "reportCombine" => self.parse_2_args(expr, &location).map(|(list, f, info)| Box::new_with(|| Expr { kind: ExprKind::Combine { list, f }, info })),

                    "reifyScript" => self.parse_closure(expr, ClosureKind::Command, false, &location),
                    "reifyReporter" => self.parse_closure(expr, ClosureKind::Reporter, false, &location),
                    "reifyPredicate" => self.parse_closure(expr, ClosureKind::Predicate, false, &location),

                    "getCostumeIdx" => self.parse_0_args(expr, &location).map(|info| Box::new_with(|| Expr { kind: ExprKind::CostumeNumber, info })),

                    "reportNewList" => {
                        let (mut list, info) = self.parse_1_args(expr, &location)?;
                        let already_owning = match &list.kind {
                            ExprKind::Value(Value::List( .. )) => true,
                            ExprKind::MakeList { .. } => true,
                            _ => false,
                        };
                        Ok(match already_owning {
                            true => {
                                list.info = info;
                                list
                            }
                            false => Box::new_with(|| Expr { kind: ExprKind::CopyList { list }, info }),
                        })
                    }

                    "reportListIndex" => self.parse_2_args(expr, &location).map(|(value, list, info)| Box::new_with(|| Expr { kind: ExprKind::ListFind { value, list }, info })),
                    "reportListItem" => {
                        let info = self.check_children_get_info(expr, 2, &location)?;
                        let list = self.parse_expr(&expr.children[1], &location)?.into();
                        match expr.children[0].get(&["option"]) {
                            Some(opt) => match opt.text.as_str() {
                                "last" => Ok(Box::new_with(|| Expr { kind: ExprKind::ListGetLast { list }, info })),
                                "any" => Ok(Box::new_with(|| Expr { kind: ExprKind::ListGetRandom { list }, info })),
                                "" => Err(Box::new_with(|| Error { kind: CompileError::BlockOptionNotSelected.into(), location: location.to_owned() })),
                                x => Err(Box::new_with(|| Error { kind: ProjectError::BlockOptionUnknown { got: x.into() }.into(), location: location.to_owned() })),
                            }
                            None => {
                                let index = self.parse_expr(&expr.children[0], &location)?;
                                Ok(Box::new_with(|| Expr { kind: ExprKind::ListGet { list, index }, info }))
                            }
                        }
                    }
                    "reportLetter" => {
                        let info = self.check_children_get_info(expr, 2, &location)?;
                        let string = self.parse_expr(&expr.children[1], &location)?.into();
                        match expr.children[0].get(&["option"]) {
                            Some(opt) => match opt.text.as_str() {
                                "last" => Ok(Box::new_with(|| Expr { kind: ExprKind::StrGetLast { string }, info })),
                                "any" => Ok(Box::new_with(|| Expr { kind: ExprKind::StrGetRandom { string }, info })),
                                "" => Err(Box::new_with(|| Error { kind: CompileError::BlockOptionNotSelected.into(), location: location.to_owned() })),
                                x => Err(Box::new_with(|| Error { kind: ProjectError::BlockOptionUnknown { got: x.into() }.into(), location: location.to_owned() })),
                            }
                            None => {
                                let index = self.parse_expr(&expr.children[0], &location)?;
                                Ok(Box::new_with(|| Expr { kind: ExprKind::StrGet { string, index }, info }))
                            }
                        }
                    }
                    "reportTextSplit" => {
                        let info = self.check_children_get_info(expr, 2, &location)?;
                        let text = self.parse_expr(&expr.children[0], &location)?.into();
                        let mode = match expr.children[1].get(&["option"]) {
                            Some(opt) => match opt.text.as_str() {
                                "letter" => TextSplitMode::Letter,
                                "word" => TextSplitMode::Word,
                                "line" => TextSplitMode::LF,
                                "tab" => TextSplitMode::Tab,
                                "cr" => TextSplitMode::CR,
                                "csv" => TextSplitMode::Csv,
                                "json" => TextSplitMode::Json,
                                "" => return Err(Box::new_with(|| Error { kind: CompileError::BlockOptionNotSelected.into(), location: location.to_owned() })),
                                x => return Err(Box::new_with(|| Error { kind: ProjectError::BlockOptionUnknown { got: x.into() }.into(), location: location.to_owned() })),
                            }
                            None => TextSplitMode::Custom(self.parse_expr(&expr.children[1], &location)?.into()),
                        };
                        Ok(Box::new_with(|| Expr { kind: ExprKind::TextSplit { text, mode }, info }))
                    }
                    "reportBoolean" => match expr.get(&["l", "bool"]) {
                        Some(x) => self.parse_bool(&x.text, &location),
                        None => Err(Box::new_with(|| Error { kind: ProjectError::BoolNoValue.into(), location: location.to_owned() })),
                    }
                    "reportMonadic" => {
                        let info = self.check_children_get_info(expr, 2, &location)?;
                        let func = self.grab_option(&expr.children[0], &location)?;
                        let value = self.parse_expr(&expr.children[1], &location)?;
                        match func {
                            "id" => Ok(value),

                            "neg" => Ok(Box::new_with(|| Expr { kind: ExprKind::Neg { value }, info })),
                            "abs" => Ok(Box::new_with(|| Expr { kind: ExprKind::Abs { value }, info })),
                            "sign" => Ok(Box::new_with(|| Expr { kind: ExprKind::Sign { value }, info })),
                            "sqrt" => Ok(Box::new_with(|| Expr { kind: ExprKind::Sqrt { value }, info })),
                            "floor" => Ok(Box::new_with(|| Expr { kind: ExprKind::Floor { value }, info })),
                            "ceiling" => Ok(Box::new_with(|| Expr { kind: ExprKind::Ceil { value }, info })),

                            "sin" => Ok(Box::new_with(|| Expr { kind: ExprKind::Sin { value }, info })),
                            "cos" => Ok(Box::new_with(|| Expr { kind: ExprKind::Cos { value }, info })),
                            "tan" => Ok(Box::new_with(|| Expr { kind: ExprKind::Tan { value }, info })),

                            "asin" => Ok(Box::new_with(|| Expr { kind: ExprKind::Asin { value }, info })),
                            "acos" => Ok(Box::new_with(|| Expr { kind: ExprKind::Acos { value }, info })),
                            "atan" => Ok(Box::new_with(|| Expr { kind: ExprKind::Atan { value }, info })),

                            "ln" => Ok(Box::new_with(|| Expr { kind: ExprKind::Log { value, base: Box::new_with(|| Constant::E.into()) }, info })),
                            "lg" => Ok(Box::new_with(|| Expr { kind: ExprKind::Log { value, base: Box::new_with(|| 2f64.into()) }, info })),
                            "log" => Ok(Box::new_with(|| Expr { kind: ExprKind::Log { value, base: Box::new_with(|| 10f64.into()) }, info })),

                            "e^" => Ok(Box::new_with(|| Expr { kind: ExprKind::Pow { base: Box::new_with(|| Constant::E.into()), power: value }, info })),
                            "2^" => Ok(Box::new_with(|| Expr { kind: ExprKind::Pow { base: Box::new_with(|| 2f64.into()), power: value }, info })),
                            "10^" => Ok(Box::new_with(|| Expr { kind: ExprKind::Pow { base: Box::new_with(|| 10f64.into()), power: value }, info })),

                            _ => Err(Box::new_with(|| Error { kind: ProjectError::BlockOptionUnknown { got: func.into() }.into(), location: location.to_owned() })),
                        }
                    }
                    "reportListAttribute" => {
                        let info = self.check_children_get_info(expr, 2, &location)?;
                        let func = self.grab_option(&expr.children[0], &location)?;
                        let value = self.parse_expr(&expr.children[1], &location)?;
                        match func {
                            "length" => Ok(Box::new_with(|| Expr { kind: ExprKind::ListLen { value }, info })),
                            "rank" => Ok(Box::new_with(|| Expr { kind: ExprKind::ListRank { value }, info })),
                            "dimensions" => Ok(Box::new_with(|| Expr { kind: ExprKind::ListDims { value }, info })),
                            "flatten" => Ok(Box::new_with(|| Expr { kind: ExprKind::ListFlatten { value }, info })),
                            "columns" => Ok(Box::new_with(|| Expr { kind: ExprKind::ListColumns { value }, info })),
                            "reverse" => Ok(Box::new_with(|| Expr { kind: ExprKind::ListRev { value }, info })),

                            "lines" => Ok(Box::new_with(|| Expr { kind: ExprKind::ListLines { value }, info })),
                            "csv" => Ok(Box::new_with(|| Expr { kind: ExprKind::ListCsv { value }, info })),
                            "json" => Ok(Box::new_with(|| Expr { kind: ExprKind::ListJson { value }, info })),

                            _ => Err(Box::new_with(|| Error { kind: ProjectError::BlockOptionUnknown { got: func.into() }.into(), location: location.to_owned() })),
                        }
                    }
                    "reportReshape" => {
                        let info = self.check_children_get_info(expr, 2, &location)?;
                        let value = self.parse_expr(&expr.children[0], &location)?;
                        let dims = self.parse_expr(&expr.children[1], &location)?;
                        Ok(Box::new_with(|| Expr { kind: ExprKind::ListReshape { value, dims }, info }))
                    }
                    "reportIfElse" => {
                        let info = self.check_children_get_info(expr, 3, &location)?;
                        let condition = self.parse_expr(&expr.children[0], &location)?;
                        let then = self.parse_expr(&expr.children[1], &location)?;
                        let otherwise = self.parse_expr(&expr.children[2], &location)?;
                        Ok(Box::new_with(|| Expr { kind: ExprKind::Conditional { condition, then, otherwise }, info }))
                    }
                    "getJSFromRPCStruct" => {
                        let rpc = self.parse_rpc(expr, &location)?;
                        Ok(Box::new_with(|| (*rpc).into()))
                    }
                    "reportImageOfObject" => {
                        let info = self.check_children_get_info(expr, 1, &location)?;
                        let entity = self.grab_entity(&expr.children[0], BlockInfo::none(), &location)?.into();
                        Ok(Box::new_with(|| Expr { kind: ExprKind::ImageOfEntity { entity }, info }))
                    }
                    "reportTouchingObject" => {
                        let info = self.check_children_get_info(expr, 1, &location)?;
                        let child = &expr.children[0];
                        if child.name == "l" && child.get(&["option"]).is_some() {
                            match self.grab_option(child, &location)? {
                                "mouse-pointer" => Ok(Box::new_with(|| Expr { kind: ExprKind::IsTouchingMouse, info })),
                                "pen trails" => Ok(Box::new_with(|| Expr { kind: ExprKind::IsTouchingDrawings, info })),
                                "edge" => Ok(Box::new_with(|| Expr { kind: ExprKind::IsTouchingEdge, info })),
                                x => Err(Box::new_with(|| Error { kind: ProjectError::BlockOptionUnknown { got: x.into() }.into(), location: location.to_owned() })),
                            }
                        }
                        else {
                            let entity = self.grab_entity(child, BlockInfo::none(), &location)?.into();
                            Ok(Box::new_with(|| Expr { kind: ExprKind::IsTouchingEntity { entity }, info }))
                        }
                    }
                    "evaluate" => {
                        let info = self.check_children_get_info(expr, 2, &location)?;
                        let closure = self.parse_expr(&expr.children[0], &location)?;
                        let mut args = Vec::with_capacity(expr.children[1].children.len());
                        for input in expr.children[1].children.iter() {
                            args.push_boxed(self.parse_expr(input, &location)?);
                        }
                        Ok(Box::new_with(|| Expr { kind: ExprKind::CallClosure { new_entity: None, closure, args }, info }))
                    }
                    "reportAskFor" => {
                        let info = self.check_children_get_info(expr, 3, &location)?;
                        let entity = self.grab_entity(&expr.children[0], BlockInfo::none(), &location)?;
                        let closure = self.parse_expr(&expr.children[1], &location)?;
                        let mut args = Vec::with_capacity(expr.children[2].children.len());
                        for input in expr.children[2].children.iter() {
                            args.push_boxed(self.parse_expr(input, &location)?);
                        }
                        Ok(Box::new_with(|| Expr { kind: ExprKind::CallClosure { new_entity: Some(entity), closure, args }, info }))
                    }
                    "doSocketRequest" => {
                        let res = self.parse_send_message_common(expr, &location)?;
                        Ok(Box::new_with(|| {
                            let NetworkMessage { target, msg_type, values, info } = *res;
                            Expr { kind: ExprKind::NetworkMessageReply { target, msg_type, values }, info }
                        }))
                    }
                    "getEffect" => {
                        let info = self.check_children_get_info(expr, 1, &location)?;
                        let effect = self.parse_effect(&expr.children[0], &location)?;
                        Ok(Box::new_with(|| Expr { kind: ExprKind::Effect { kind: effect }, info }))
                    }
                    "getPenAttribute" => {
                        let info = self.check_children_get_info(expr, 1, &location)?;
                        let attr = self.parse_pen_attr(&expr.children[0], &location)?;
                        Ok(Box::new_with(|| Expr { kind: ExprKind::PenAttr { attr }, info }))
                    }
                    "reportGet" => {
                        let info = self.check_children_get_info(expr, 1, &location)?;
                        match self.grab_option(&expr.children[0], &location)? {
                            "costumes" => Ok(Box::new_with(|| Expr { kind: ExprKind::CostumeList, info })),
                            "costume" => Ok(Box::new_with(|| Expr { kind: ExprKind::Costume, info })),
                            "sounds" => Ok(Box::new_with(|| Expr { kind: ExprKind::SoundList, info })),
                            m => Err(Box::new_with(|| Error { kind: CompileError::CurrentlyUnsupported { msg: format!("the {s} block with option '{m}' is currently not supported").into() }.into(), location: location.to_owned() })),
                        }
                    }
                    "reportObject" => {
                        let info = self.check_children_get_info(expr, 1, &location)?;
                        self.grab_entity(&expr.children[0], info, &location)
                    }
                    "newClone" => {
                        let info = self.check_children_get_info(expr, 1, &location)?;
                        let target = self.grab_entity(&expr.children[0], BlockInfo::none(), &location)?;
                        Ok(Box::new_with(|| Expr { kind: ExprKind::Clone { target }, info }))
                    }
                    "reportIsA" => {
                        let info = self.check_children_get_info(expr, 2, &location)?;
                        let value = self.parse_expr(&expr.children[0], &location)?;
                        let ty = match self.grab_option(&expr.children[1], &location)? {
                            "number" => ValueType::Number,
                            "text" => ValueType::Text,
                            "Boolean" => ValueType::Bool,
                            "list" => ValueType::List,
                            "sprite" => ValueType::Sprite,
                            "costume" => ValueType::Costume,
                            "sound" => ValueType::Sound,
                            "command" => ValueType::Command,
                            "reporter" => ValueType::Reporter,
                            "predicate" => ValueType::Predicate,
                            x => return Err(Box::new_with(|| Error { kind: ProjectError::BlockOptionUnknown { got: x.into() }.into(), location: location.to_owned() })),
                        };
                        Ok(Box::new_with(|| Expr { kind: ExprKind::TypeQuery { value, ty }, info }))
                    }
                    "reportDate" => {
                        let info = self.check_children_get_info(expr, 1, &location)?;
                        let query = match self.grab_option(&expr.children[0], &location)? {
                            "year" => TimeQuery::Year,
                            "month" => TimeQuery::Month,
                            "date" => TimeQuery::Date,
                            "day of week" => TimeQuery::DayOfWeek,
                            "hour" => TimeQuery::Hour,
                            "minute" => TimeQuery::Minute,
                            "second" => TimeQuery::Second,
                            "time in milliseconds" => TimeQuery::UnixTimestampMs,
                            x => return Err(Box::new_with(|| Error { kind: ProjectError::BlockOptionUnknown { got: x.into() }.into(), location: location.to_owned() })),
                        };
                        Ok(Box::new_with(|| Expr { kind: ExprKind::RealTime { query }, info }))
                    }
                    x => self.parse_unknown_common(expr, &location).map(|(args, info)| Box::new_with(|| Expr { kind: ExprKind::UnknownBlock { name: x.into(), args }, info })),
                }
            }
            _ => Err(Box::new_with(|| Error { kind: CompileError::UnknownBlockType.into(), location: location.to_owned() })),
        }
    }
}

struct EntityInfo<'a, 'b> {
    parser: &'a Parser,
    role: &'b RoleInfo<'a>,
    name: CompactString,
    trans_name: CompactString,
    fields: SymbolTable<'a>,
    funcs: SymbolTable<'a>,
    costumes: SymbolTable<'a>,
    sounds: SymbolTable<'a>,
}
impl<'a, 'b> EntityInfo<'a, 'b> {
    fn new(role: &'b RoleInfo<'a>, name: VariableRef) -> Box<Self> {
        Box::new_with(|| Self {
            parser: role.parser,
            role,
            name: name.name,
            trans_name: name.trans_name,
            fields: SymbolTable::new(role.parser),
            funcs: SymbolTable::new(role.parser),
            costumes: SymbolTable::new(role.parser),
            sounds: SymbolTable::new(role.parser),
        })
    }
    fn parse(mut self, entity: &'a Xml) -> Result<Entity, Box<Error>> {
        let location = Box::new_with(|| LocationRef {
            role: Some(&self.role.name),
            entity: Some(&self.name),
            collab_id: None,
            block_type: None,
        });

        for costume in entity.get(&["costumes", "list"]).map(|c| c.children.as_slice()).unwrap_or(&[]) {
            if let Some(ident) = costume.get(&["ref"]).and_then(|r| r.attr("mediaID")) {
                let ident = ident.value.as_str();
                if !ident.starts_with(self.name.as_str()) || !ident[self.name.len()..].starts_with("_cst_") {
                    return Err(Box::new_with(|| Error { kind: ProjectError::CostumeIdFormat { id: ident.into() }.into(), location: location.to_owned() }));
                }
                let name = &ident[self.name.len() + 5..];

                let img = match self.role.images.get(ident) {
                    Some(x) => x.clone(),
                    None => return Err(Box::new_with(|| Error { kind: ProjectError::CostumeUndefinedRef { id: ident.into() }.into(), location: location.to_owned() })),
                };

                match self.costumes.define(name.into(), Value::Image(img)) {
                    Ok(None) => (),
                    Ok(Some(prev)) => return Err(Box::new_with(|| Error { kind: ProjectError::CostumesWithSameName { name: prev.def.name }.into(), location: location.to_owned() })),
                    Err(SymbolError::NameTransformError { name }) => return Err(Box::new_with(|| Error { kind: CompileError::NameTransformError { name }.into(), location: location.to_owned() })),
                    Err(SymbolError::ConflictingTrans { trans_name, names }) => return Err(Box::new_with(|| Error { kind: CompileError::CostumesWithSameTransName { trans_name, names }.into(), location: location.to_owned() })),
                }
            }
        }

        for sound in entity.get(&["sounds", "list"]).map(|c| c.children.as_slice()).unwrap_or(&[]) {
            if let Some(ident) = sound.get(&["ref"]).and_then(|r| r.attr("mediaID")) {
                let ident = ident.value.as_str();
                if !ident.starts_with(self.name.as_str()) || !ident[self.name.len()..].starts_with("_snd_") {
                    return Err(Box::new_with(|| Error { kind: ProjectError::SoundIdFormat { id: ident.into() }.into(), location: location.to_owned() }));
                }
                let name = &ident[self.name.len() + 5..];

                let sound = match self.role.sounds.get(ident) {
                    Some(x) => x.clone(),
                    None => return Err(Box::new_with(|| Error { kind: ProjectError::SoundUndefinedRef { id: ident.into() }.into(), location: location.to_owned() })),
                };

                match self.sounds.define(name.into(), Value::Audio(sound)) {
                    Ok(None) => (),
                    Ok(Some(prev)) => return Err(Box::new_with(|| Error { kind: ProjectError::SoundsWithSameName { name: prev.def.name }.into(), location: location.to_owned() })),
                    Err(SymbolError::NameTransformError { name }) => return Err(Box::new_with(|| Error { kind: CompileError::NameTransformError { name }.into(), location: location.to_owned() })),
                    Err(SymbolError::ConflictingTrans { trans_name, names }) => return Err(Box::new_with(|| Error { kind: CompileError::SoundsWithSameTransName { trans_name, names }.into(), location: location.to_owned() })),
                }
            }
        }

        let blocks = entity.get(&["blocks"]).map(|v| v.children.as_slice()).unwrap_or(&[]);
        for block in blocks {
            parse_block_header(block, &mut self.funcs, &location)?;
        }

        let active_costume = match entity.attr("costume").map(|v| v.value.parse::<usize>().ok()).flatten() {
            Some(idx) if idx >= 1 && idx <= self.costumes.len() => Some(idx - 1),
            _ => None,
        };
        let color = entity.attr("color").map(|v| parse_color(&v.value)).flatten().unwrap_or((0, 0, 0, 255));
        let visible = !entity.attr("hidden").and_then(|s| s.value.parse::<bool>().ok()).unwrap_or(false);

        let float_attr = |attr: &str| entity.attr(attr).map(|v| v.value.parse::<f64>().ok().filter(|v| v.is_finite())).flatten();
        let pos = (float_attr("x").unwrap_or(0.0), float_attr("y").unwrap_or(0.0));
        let heading = float_attr("heading").unwrap_or(0.0);
        let scale = float_attr("scale").unwrap_or(1.0);

        if let Some(fields) = entity.get(&["variables"]) {
            let mut dummy_script = ScriptInfo::new(&self);

            let mut defs = vec![];
            for def in fields.children.iter().filter(|v| v.name == "variable") {
                let name = match def.attr("name") {
                    None => return Err(Box::new_with(|| Error { kind: ProjectError::UnnamedField.into(), location: location.to_owned() })),
                    Some(x) => x.value.clone(),
                };
                let value = match def.children.get(0) {
                    None => return Err(Box::new_with(|| Error { kind: ProjectError::FieldNoValue { name }.into(), location: location.to_owned() })),
                    Some(x) => match dummy_script.parse_expr(x, &location)?.kind {
                        ExprKind::Value(v) => v,
                        _ => return Err(Box::new_with(|| Error { kind: ProjectError::ValueNotEvaluated.into(), location: location.to_owned() })),
                    }
                };
                defs.push((name, value));
            }

            for (name, value) in defs {
                match self.fields.define(name.clone(), value) {
                    Ok(None) => (),
                    Ok(Some(prev)) => return Err(Box::new_with(|| Error { kind: ProjectError::FieldsWithSameName { name: prev.def.name }.into(), location: location.to_owned() })),
                    Err(SymbolError::NameTransformError { name }) => return Err(Box::new_with(|| Error { kind: CompileError::NameTransformError { name }.into(), location: location.to_owned() })),
                    Err(SymbolError::ConflictingTrans { trans_name, names }) => return Err(Box::new_with(|| Error { kind: CompileError::FieldsWithSameTransName { trans_name, names }.into(), location: location.to_owned() })),
                }
            }
        }

        let mut funcs = vec![];
        for block in blocks {
            funcs.push(parse_block(block, &self.funcs, self.role, Some(&self))?);
        }

        let mut scripts = vec![];
        if let Some(scripts_xml) = entity.get(&["scripts"]) {
            for script_xml in scripts_xml.children.iter() {
                match script_xml.children.as_slice() {
                    [] => continue,
                    [stmt, rest @ ..] => {
                        if rest.is_empty() && (stmt.attr("var").is_some() || stmt.attr("s").map(|s| s.value.starts_with("report")).unwrap_or(false)) {
                            continue
                        }
                        if self.parser.omit_nonhat_scripts && ScriptInfo::new(&self).parse_hat(stmt)?.is_none() {
                            continue
                        }
                    }
                }

                scripts.push(ScriptInfo::new(&self).parse(script_xml)?);
            }
        }

        Ok(Entity {
            name: self.name,
            trans_name: self.trans_name,
            fields: self.fields.into_def_inits(),
            costumes: self.costumes.into_def_inits(),
            sounds: self.sounds.into_def_inits(),
            funcs,
            scripts,

            active_costume,
            visible,
            color,
            pos,
            heading,
            scale,
        })
    }
}

enum ParamType {
    Evaluated, Unevaluated,
}
struct BlockHeaderInfo<'a> {
    s: &'a str,
    returns: bool,
    params: Vec<(CompactString, ParamType)>,
    upvars: Vec<CompactString>,
}

// returns the signature and returns flag of the block header value
#[inline(never)]
fn get_block_info(value: &Value) -> Box<BlockHeaderInfo> {
    match value {
        Value::List(vals, _) => {
            assert_eq!(vals.len(), 4);
            let s = match &vals[0] {
                Value::String(v) => v.as_str(),
                _ => panic!(),
            };
            let returns = match &vals[1] {
                Value::Bool(v) => *v,
                _ => panic!(),
            };
            let params = match &vals[2] {
                Value::List(x, None) => x.iter().map(|x| match x {
                    Value::List(x, None) => match x.as_slice() {
                        [Value::String(v1), Value::Bool(v2)] => (v1.clone(), if *v2 { ParamType::Evaluated } else { ParamType::Unevaluated }),
                        _ => panic!(),
                    }
                    _ => panic!(),
                }).collect(),
                _ => panic!(),
            };
            let upvars = match &vals[3] {
                Value::List(x, None) => x.iter().map(|x| match x {
                    Value::String(x) => x.clone(),
                    _ => panic!(),
                }).collect(),
                _ => panic!(),
            };
            Box::new_with(|| BlockHeaderInfo { s, returns, params, upvars })
        }
        _ => panic!(), // header parser would never do this
    }
}

fn replace_ranges<It>(s: &str, ranges: It, with: &str) -> CompactString where It: Iterator<Item = (usize, usize)>{
    let mut res = alloc::string::String::with_capacity(s.len());
    let mut last_stop = 0;
    for (a, b) in ranges {
        res += &s[last_stop..a];
        last_stop = b;
        res += with;
    }
    res += &s[last_stop..];
    res.into()
}

#[inline(never)]
fn block_name_from_def(s: &str) -> CompactString {
    replace_ranges(s, ParamIter::new(s), "\t") // tabs leave a marker for args which disappears after ident renaming
}
#[inline(never)]
fn block_name_from_ref(s: &str) -> CompactString {
    replace_ranges(s, ArgIter::new(s), "\t") // tabs leave a marker for args which disappears after ident renaming
}

#[test]
fn test_block_name_from_def() {
    assert_eq!(block_name_from_def("hello world"), "hello world");
    assert_eq!(block_name_from_def("hello %'wor'ld"), "hello \tld");
    assert_eq!(block_name_from_def("hello %'wor' ld "), "hello \t ld ");
    assert_eq!(block_name_from_def("hello %'wor'l%'d'"), "hello \tl\t");
    assert_eq!(block_name_from_def("hello %'wor'l%'d' "), "hello \tl\t ");
    assert_eq!(block_name_from_def("hello %'wor'l%'d'%' "), "hello \tl\t%' ");
}
#[test]
fn test_block_name_from_ref() {
    assert_eq!(block_name_from_ref("hello world"), "hello world");
    assert_eq!(block_name_from_ref("hello %world"), "hello \t");
    assert_eq!(block_name_from_ref("hello %world "), "hello \t ");
}

fn parse_block_header<'a>(block: &'a Xml, funcs: &mut SymbolTable<'a>, location: &LocationRef) -> Result<(), Box<Error>> {
    let mut location = Box::new_with(|| LocationRef {
        role: location.role,
        entity: location.entity,
        collab_id: get_collab_id(block),
        block_type: None,
    });
    let s = match block.attr("s") {
        Some(v) => v.value.as_str(),
        None => return Err(Box::new_with(|| Error { kind: ProjectError::CustomBlockWithoutName.into(), location: location.to_owned() })),
    };
    location.block_type = Some(s);

    let returns = match block.attr("type") {
        Some(v) => match v.value.as_str() {
            "command" => false,
            "reporter" | "predicate" => true,
            x => return Err(Box::new_with(|| Error { kind: ProjectError::CustomBlockUnknownType { ty: x.into() }.into(), location: location.to_owned() })),
        }
        None => return Err(Box::new_with(|| Error { kind: ProjectError::CustomBlockWithoutType.into(), location: location.to_owned() })),
    };

    let (params, upvars) = match block.get(&["inputs"]) {
        Some(inputs) => {
            let mut params = vec![];
            let mut upvars = vec![];

            let param_names: Vec<_> = ParamIter::new(s).map(|(a, b)| &s[a+2..b-1]).collect();
            if param_names.len() != inputs.children.len() {
                return Err(Box::new_with(|| Error { kind: ProjectError::CustomBlockInputsMetaCorrupted.into(), location: location.to_owned() }));
            }
            for (param, input) in iter::zip(param_names, &inputs.children) {
                let t = match input.attr("type") {
                    Some(x) if !x.value.is_empty() => x.value.as_str(),
                    _ => return Err(Box::new_with(|| Error { kind: ProjectError::CustomBlockInputsMetaCorrupted.into(), location: location.to_owned() })),
                };
                let evaluated = match t {
                    "%anyUE" | "%boolUE" => false,
                    _ => true,
                };

                params.push(Value::List(vec![CompactString::new(param).into(), evaluated.into()], None));
                if t == "%upvar" {
                    upvars.push(Value::String(CompactString::new(param)));
                }
            }

            (params, upvars)
        }
        None => return Err(Box::new_with(|| Error { kind: ProjectError::CustomBlockWithoutInputsMeta.into(), location: location.to_owned() })),
    };

    let name = block_name_from_def(s);
    match funcs.define(name, Value::List(vec![Value::from(s), Value::from(returns), Value::List(params, None), Value::List(upvars, None)], None)) {
        Ok(None) => Ok(()),
        Ok(Some(prev)) => Err(Box::new_with(|| Error { kind: CompileError::BlocksWithSameName { name: prev.def.name, sigs: (get_block_info(&prev.init).s.into(), s.into()) }.into(), location: location.to_owned() })),
        Err(SymbolError::NameTransformError { name }) => Err(Box::new_with(|| Error { kind: CompileError::NameTransformError { name }.into(), location: location.to_owned() })),
        Err(SymbolError::ConflictingTrans { trans_name, names }) => Err(Box::new_with(|| Error { kind: CompileError::BlocksWithSameTransName { trans_name, names }.into(), location: location.to_owned() })),
    }
}
fn parse_block<'a>(block: &'a Xml, funcs: &SymbolTable<'a>, role: &RoleInfo, entity: Option<&EntityInfo>) -> Result<Function, Box<Error>> {
    let s = block.attr("s").unwrap().value.as_str(); // unwrap ok because we assume parse_block_header() was called before
    let entry = funcs.get(&block_name_from_def(s)).unwrap();
    let block_header = get_block_info(&entry.init);
    assert_eq!(s, block_header.s);

    let location = Box::new_with(|| LocationRef {
        role: Some(&role.name),
        entity: entity.map(|x| x.name.as_str()),
        collab_id: get_collab_id(block),
        block_type: Some(s),
    });

    let finalize = |entity_info: &EntityInfo| {
        let mut script_info = ScriptInfo::new(entity_info);
        for param in block_header.params {
            script_info.decl_local(param.0, 0f64.into(), &location)?;
        }
        debug_assert_eq!(script_info.locals.len(), 1);
        debug_assert_eq!(script_info.locals[0].1.len(), 0);
        let params = script_info.locals[0].0.clone().into_defs();

        let stmts = match block.get(&["script"]) {
            Some(script) => script_info.parse(script)?.stmts,
            None => vec![],
        };

        let upvars = {
            let mut res = vec![];
            for upvar in block_header.upvars.iter() {
                match params.iter().find(|x| x.name == *upvar) {
                    Some(x) => res.push_boxed(x.ref_at(VarLocation::Local)),
                    None => return Err(Box::new_with(|| Error { kind: ProjectError::CustomBlockInputsMetaCorrupted.into(), location: location.to_owned() })),
                };
            }
            res
        };

        Ok(Function {
            name: entry.def.name.clone(),
            trans_name: entry.def.trans_name.clone(),
            upvars,
            params,
            returns: block_header.returns,
            stmts,
        })
    };
    match entity {
        Some(v) => finalize(v),
        None => {
            let entity = EntityInfo::new(role, VariableRef { name: "global".into(), trans_name: "global".into(), location: VarLocation::Global });
            finalize(&entity)
        }
    }
}

struct RoleInfo<'a> {
    parser: &'a Parser,
    name: CompactString,
    globals: SymbolTable<'a>,
    entities: SymbolTable<'a>,
    funcs: SymbolTable<'a>,
    images: VecMap<&'a str, Rc<(Vec<u8>, Option<(f64, f64)>)>>,
    sounds: VecMap<&'a str, Rc<Vec<u8>>>,
    msg_types: VecMap<&'a str, Vec<&'a str>>,
}
impl<'a> RoleInfo<'a> {
    fn new(parser: &'a Parser, name: CompactString) -> Box<Self> {
        Box::new_with(|| Self {
            parser,
            name,
            globals: SymbolTable::new(parser),
            entities: SymbolTable::new(parser),
            funcs: SymbolTable::new(parser),
            images: Default::default(),
            sounds: Default::default(),
            msg_types: Default::default(),
        })
    }
    fn parse(mut self, role_root: &'a Xml) -> Result<Role, Box<Error>> {
        let mut location = Box::new_with(|| LocationRef {
            role: None,
            entity: None,
            collab_id: None,
            block_type: None,
        });

        assert_eq!(role_root.name, "role");
        let role = match role_root.attr("name") {
            None => return Err(Box::new_with(|| Error { kind: ProjectError::RoleNoName.into(), location: location.to_owned() })),
            Some(x) => x.value.clone(),
        };
        location.role = Some(&role);

        let content = match role_root.get(&["project"]) {
            None => return Err(Box::new_with(|| Error { kind: ProjectError::RoleNoContent.into(), location: location.to_owned() })),
            Some(x) => x,
        };
        let notes = CompactString::new(content.get(&["notes"]).map(|v| v.text.as_str()).unwrap_or(""));
        let stage = match content.get(&["stage"]) {
            None => return Err(Box::new_with(|| Error { kind: ProjectError::NoStage.into(), location: location.to_owned() })),
            Some(x) => x,
        };
        let stage_width = stage.attr("width").and_then(|x| x.value.parse::<usize>().ok()).unwrap_or(480);
        let stage_height = stage.attr("height").and_then(|x| x.value.parse::<usize>().ok()).unwrap_or(360);

        let msg_types = stage.get(&["messageTypes"]).map(|x| x.children.as_slice()).unwrap_or(&[]);
        for msg_type in msg_types {
            let name = match msg_type.get(&["name"]) {
                None => return Err(Box::new_with(|| Error { kind: ProjectError::MessageTypeMissingName.into(), location: location.to_owned() })),
                Some(x) => x.text.as_str(),
            };
            let fields = match msg_type.get(&["fields"]) {
                None => return Err(Box::new_with(|| Error { kind: ProjectError::MessageTypeMissingFields { msg_type: name.into() }.into(), location: location.to_owned() })),
                Some(x) => {
                    let mut res = vec![];
                    for field in x.children.iter() {
                        if field.name != "field" { continue }
                        res.push(match field.text.as_str() {
                            "" => return Err(Box::new_with(|| Error { kind: ProjectError::MessageTypeFieldEmpty { msg_type: name.into() }.into(), location: location.to_owned() })),
                            x => x,
                        });
                    }
                    res
                }
            };

            if self.msg_types.insert(name, fields).is_some() {
                return Err(Box::new_with(|| Error { kind: ProjectError::MessageTypeMultiplyDefined { msg_type: name.into() }.into(), location: location.to_owned() }));
            }
        }

        for entry in role_root.get(&["media"]).map(|v| v.children.as_slice()).unwrap_or(&[]) {
            match entry.name.as_str() {
                "costume" => {
                    let id = match entry.attr("mediaID") {
                        Some(x) => x.value.as_str(),
                        None => return Err(Box::new_with(|| Error { kind: ProjectError::ImageWithoutId.into(), location: location.to_owned() })),
                    };

                    let center = match (entry.attr("center-x").and_then(|x| x.value.parse().ok()), entry.attr("center-y").and_then(|y| y.value.parse().ok())) {
                        (Some(x), Some(y)) => Some((x, y)),
                        _ => None,
                    };

                    let content = match entry.attr("image") {
                        Some(x) => match x.value.as_str().starts_with("data:image/").then(|| x.value.as_str().split(";base64,").nth(1)).flatten() {
                            Some(x) => match base64_decode(x) {
                                Ok(x) => x,
                                Err(e) => return Err(Box::new_with(|| Error { kind: e.into(), location: location.to_owned() })),
                            }
                            _ => return Err(Box::new_with(|| Error { kind: ProjectError::ImageUnknownFormat { id: id.into(), content: x.value.clone() }.into(), location: location.to_owned() })),
                        }
                        None => return Err(Box::new_with(|| Error { kind: ProjectError::ImageWithoutContent { id: id.into() }.into(), location: location.to_owned() })),
                    };

                    if self.images.insert(id, Rc::new((content, center))).is_some() {
                        return Err(Box::new_with(|| Error { kind: ProjectError::ImagesWithSameId { id: id.into() }.into(), location: location.to_owned() }));
                    }
                }
                "sound" => {
                    let id = match entry.attr("mediaID") {
                        Some(x) => x.value.as_str(),
                        None => return Err(Box::new_with(|| Error { kind: ProjectError::SoundWithoutId.into(), location: location.to_owned() })),
                    };

                    let content = match entry.attr("sound") {
                        Some(x) => match x.value.as_str().starts_with("data:audio/").then(|| x.value.as_str().split(";base64,").nth(1)).flatten() {
                            Some(x) => match base64_decode(x) {
                                Ok(x) => x,
                                Err(e) => return Err(Box::new_with(|| Error { kind: e.into(), location: location.to_owned() })),
                            }
                            _ => return Err(Box::new_with(|| Error { kind: ProjectError::SoundUnknownFormat { id: id.into(), content: x.value.clone() }.into(), location: location.to_owned() })),
                        }
                        None => return Err(Box::new_with(|| Error { kind: ProjectError::SoundWithoutContent { id: id.into() }.into(), location: location.to_owned() })),
                    };

                    if self.sounds.insert(id, Rc::new(content)).is_some() {
                        return Err(Box::new_with(|| Error { kind: ProjectError::SoundsWithSameId { id: id.into() }.into(), location: location.to_owned() }));
                    }
                }
                _ => (),
            }
        }

        if let Some(globals) = content.get(&["variables"]) {
            let dummy_name = VariableRef { name: "global".into(), trans_name: "global".into(), location: VarLocation::Global };
            let dummy_entity = EntityInfo::new(&self, dummy_name); // fine to do before entities/blocks/etc. since globals are just values (not stmts or exprs)
            let mut dummy_script = ScriptInfo::new(&dummy_entity);

            let mut defs = vec![];
            for def in globals.children.iter().filter(|v| v.name == "variable") {
                let name = match def.attr("name") {
                    None => return Err(Box::new_with(|| Error { kind: ProjectError::UnnamedGlobal.into(), location: location.to_owned() })),
                    Some(x) => x.value.clone(),
                };
                let value = match def.children.get(0) {
                    None => Value::Number(0.0),
                    Some(x) => match dummy_script.parse_expr(x, &location)?.kind {
                        ExprKind::Value(v) => v,
                        _ => return Err(Box::new_with(|| Error { kind: ProjectError::ValueNotEvaluated.into(), location: location.to_owned() })),
                    }
                };
                defs.push((name, value));
            }

            for (name, value) in defs {
                match self.globals.define(name.clone(), value) {
                    Ok(None) => (),
                    Ok(Some(prev)) => return Err(Box::new_with(|| Error { kind: ProjectError::GlobalsWithSameName { name: prev.def.name }.into(), location: location.to_owned() })),
                    Err(SymbolError::NameTransformError { name }) => return Err(Box::new_with(|| Error { kind: CompileError::NameTransformError { name }.into(), location: location.to_owned() })),
                    Err(SymbolError::ConflictingTrans { trans_name, names }) => return Err(Box::new_with(|| Error { kind: CompileError::GlobalsWithSameTransName { trans_name, names }.into(), location: location.to_owned() })),
                }
            }
        }

        let mut entities_raw = vec![];
        if let Some(entities_xml) = stage.get(&["sprites"]) {
            for entity in iter::once(stage).chain(entities_xml.children.iter().filter(|s| s.name == "sprite")) {
                let name = match entity.attr("name") {
                    None => return Err(Box::new_with(|| Error { kind: ProjectError::UnnamedEntity.into(), location: location.to_owned() })),
                    Some(x) => match self.entities.define(x.value.clone(), 0f64.into()) {
                        Ok(None) => self.entities.get(&x.value).unwrap().def.ref_at(VarLocation::Global),
                        Ok(Some(prev)) => return Err(Box::new_with(|| Error { kind: ProjectError::EntitiesWithSameName { name: prev.def.name }.into(), location: location.to_owned() })),
                        Err(SymbolError::NameTransformError { name }) => return Err(Box::new_with(|| Error { kind: CompileError::NameTransformError { name }.into(), location: location.to_owned() })),
                        Err(SymbolError::ConflictingTrans { trans_name, names }) => return Err(Box::new_with(|| Error { kind: CompileError::EntitiesWithSameTransName { trans_name, names }.into(), location: location.to_owned() })),
                    }
                };
                entities_raw.push((entity, name));
            }
        }

        let blocks = content.get(&["blocks"]).map(|v| v.children.as_slice()).unwrap_or(&[]);
        for block in blocks {
            parse_block_header(block, &mut self.funcs, &location)?;
        }

        // ----------------------------------------------------------------------------------- //
        // -- we now have all the necessary items defined to parse exprs, stmts, and entity -- //
        // ----------------------------------------------------------------------------------- //

        let funcs = blocks.iter().map(|block| parse_block(block, &self.funcs, &self, None)).collect::<Result<Vec<_>,_>>()?;
        let entities = entities_raw.into_iter().map(|(entity, name)| EntityInfo::new(&self, *name).parse(entity)).collect::<Result<Vec<_>,_>>()?;

        Ok(Role {
            name: role,
            notes,
            stage_size: (stage_width, stage_height),
            globals: self.globals.into_def_inits(),
            funcs,
            entities,
        })
    }
}

pub struct Parser {
    /// If `true`, the parser will skip script blocks that lack a hat block.
    /// This is typically desirable since free floating blocks are never automatically executed,
    /// and thus are typically not needed for translation efforts.
    /// Defaults to `true`.
    pub omit_nonhat_scripts: bool,

    /// All symbol names in the program will be passed through this function,
    /// allowing easy conversion of Snap! names to, e.g., valid C-like identifiers.
    /// The default operation performs no conversion.
    /// Note that non-default transform strategies may also require a custom [`Parser::autofill_generator`].
    pub name_transformer: Rc<dyn Fn(&str) -> Result<CompactString, ()>>,

    /// A generator used to produce symbol names for auto-fill closure arguments.
    /// The function receives a number that can be used to differentiate different generated arguments.
    /// It is expected that multiple calls to this function with the same input will produce the same output symbol name.
    /// The default is to produce a string of format `%n` where `n` is the input number.
    /// Note that, after generation, symbol names are still passed through [`Parser::name_transformer`] as usual.
    pub autofill_generator: Rc<dyn Fn(usize) -> Result<CompactString, ()>>,
}
impl Default for Parser {
    fn default() -> Self {
        Self {
            omit_nonhat_scripts: true,
            name_transformer: Rc::new(|v| Ok(v.into())),
            autofill_generator: Rc::new(|v| Ok(format!("%{}", v).into())),
        }
    }
}
impl Parser {
    pub fn parse(&self, xml: &str) -> Result<Project, Box<Error>> {
        let location = Box::new_with(|| LocationRef {
            role: None,
            entity: None,
            collab_id: None,
            block_type: None,
        });

        let mut xml = xmlparser::Tokenizer::from(xml);
        while let Some(Ok(e)) = xml.next() {
            if let xmlparser::Token::ElementStart { local, .. } = e {
                let (proj_name, roles) = match local.as_str() {
                    "room" => {
                        let project_xml = match parse_xml_root(&mut xml, local.as_str()) {
                            Ok(x) => x,
                            Err(e) => return Err(Box::new_with(|| Error { kind: e.into(), location: location.to_owned() })),
                        };
                        let proj_name = CompactString::new(project_xml.attr("name").map(|v| v.value.as_str()).unwrap_or("untitled"));

                        let mut roles = Vec::with_capacity(project_xml.children.len());
                        for child in project_xml.children.iter() {
                            if child.name == "role" {
                                let role_name = match child.attr("name") {
                                    None => return Err(Box::new_with(|| Error { kind: ProjectError::RoleNoName.into(), location: location.to_owned() })),
                                    Some(x) => x.value.clone(),
                                };
                                roles.push(RoleInfo::new(self, role_name).parse(child)?);
                            }
                        }

                        (proj_name, roles)
                    }
                    "role" => {
                        let role_xml = match parse_xml_root(&mut xml, local.as_str()) {
                            Ok(x) => x,
                            Err(e) => return Err(Box::new_with(|| Error { kind: e.into(), location: location.to_owned() })),
                        };
                        let proj_name = CompactString::new(role_xml.attr("name").map(|v| v.value.as_str()).unwrap_or("untitled"));

                        let role = RoleInfo::new(self, proj_name.clone()).parse(&role_xml)?;

                        (proj_name, vec![role])
                    }
                    "project" => {
                        let project_xml = match parse_xml_root(&mut xml, local.as_str()) {
                            Ok(x) => x,
                            Err(e) => return Err(Box::new_with(|| Error { kind: e.into(), location: location.to_owned() })),
                        };
                        let proj_name = CompactString::new(project_xml.attr("name").map(|v| v.value.as_str()).unwrap_or("untitled").to_owned());

                        let role_xml = Xml {
                            name: "role".into(),
                            text: "".into(),
                            attrs: vec![XmlAttr { name: "name".into(), value: proj_name.clone() }],
                            children: vec![project_xml]
                        };
                        let role = RoleInfo::new(self, proj_name.clone()).parse(&role_xml)?;

                        (proj_name, vec![role])
                    }
                    _ => continue,
                };

                return Ok(Project { name: proj_name, roles })
            }
        }
        Err(Box::new_with(|| Error { kind: ProjectError::NoRoot.into(), location: location.to_owned() }))
    }
}

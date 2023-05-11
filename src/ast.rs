use std::prelude::v1::*;

use std::rc::Rc;
use std::{mem, iter, fmt};

use base64::engine::Engine as Base64Engine;
use base64::DecodeError as Base64DecodeError;

use crate::util::Punctuated;
use crate::rpcs::*;

#[cfg(test)]
use proptest::prelude::*;

fn base64_decode(content: &str) -> Result<Vec<u8>, Box<Error>> {
    base64::engine::general_purpose::STANDARD.decode(content).map_err(|e| Box::new(e.into()))
}

// regex equivalent: r"%'([^']*)'"
struct ParamIter<'a>(iter::Fuse<std::str::CharIndices<'a>>);
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
struct ArgIter<'a>(iter::Fuse<std::str::CharIndices<'a>>, usize);
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

struct InlineListIter<'a>(iter::Peekable<iter::Fuse<std::str::Chars<'a>>>);
impl<'a> InlineListIter<'a> {
    fn new(s: &'a str) -> Self {
        Self(s.chars().fuse().peekable())
    }
}
impl<'a> Iterator for InlineListIter<'a> {
    type Item = String;
    fn next(&mut self) -> Option<Self::Item> {
        let mut res = String::new();
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
                return Some(res);
            } else {
                res.push(ch);
            }
        }
        if !res.is_empty() { Some(res) } else { None }
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

fn replace_ranges<It>(s: &str, ranges: It, with: &str) -> String where It: Iterator<Item = (usize, usize)>{
    let mut res = String::with_capacity(s.len());
    let mut last_stop = 0;
    for (a, b) in ranges {
        res += &s[last_stop..a];
        last_stop = b;
        res += with;
    }
    res += &s[last_stop..];
    res
}

fn clean_newlines(s: &str) -> String {
    Punctuated(s.lines(), "\n").to_string()
}

// source: https://docs.babelmonkeys.de/RustyXML/src/xml/lib.rs.html#41-55
#[cfg(test)]
fn xml_escape(input: &str) -> String {
    let mut result = String::with_capacity(input.len());
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
    result
}

// source: https://docs.babelmonkeys.de/RustyXML/src/xml/lib.rs.html#60-100
// note: modified to suite our needs
fn xml_unescape(input: &str) -> Result<String, Box<Error>> {
    let mut result = String::with_capacity(input.len());

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
                            None => return Err(Box::new(Error::XmlUnescapeError { illegal_sequence: format!("&{};", ent) })),
                        }
                    }
                }
                result.push_str(&sub[idx + 1..]);
            }
            None => return Err(Box::new(Error::XmlUnescapeError { illegal_sequence: format!("&{}", sub) })),
        }
    }

    Ok(result)
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
    name: String,
    value: String,
}
#[derive(Debug)]
struct Xml {
    name: String,
    text: String,
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
fn parse_xml_root<'a>(xml: &mut xmlparser::Tokenizer<'a>, root_name: &'a str) -> Result<Xml, Box<Error>> {
    let mut attrs = vec![];
    let mut text = String::new();
    let mut children = vec![];
    while let Some(e) = xml.next() {
        match e {
            Err(e) => return Err(Box::new(e.into())),
            Ok(e) => match e {
                xmlparser::Token::Attribute { local, value, .. } => attrs.push(XmlAttr { name: xml_unescape(local.as_str())?, value: xml_unescape(value.as_str())? }),
                xmlparser::Token::Text { text: t } => text += &xml_unescape(t.as_str())?,
                xmlparser::Token::ElementStart { local, .. } => children.push(parse_xml_root(xml, local.as_str())?),
                xmlparser::Token::ElementEnd { end, .. } => match end {
                    xmlparser::ElementEnd::Close(_, _) => break,
                    xmlparser::ElementEnd::Empty => break,
                    xmlparser::ElementEnd::Open => (),
                }
                _ => (),
            }
        }
    }
    Ok(Xml { name: root_name.to_owned(), attrs, children, text: clean_newlines(&text) })
}

#[derive(Debug)]
pub enum ProjectError {
    NoRoot,
    UnnamedRole,
    RefMissingId { role: String, entity: String },
    ValueNotEvaluated { role: String, entity: Option<String> },
    NoRoleContent { role: String },
    NoStageDef { role: String },

    CustomBlockWithoutName { role: String, entity: Option<String> },
    CustomBlockWithoutType { role: String, entity: Option<String>, sig: String },
    CustomBlockUnknownType { role: String, entity: Option<String>, sig: String, ty: String },

    ImageWithoutId { role: String },
    ImagesWithSameId { role: String, id: String },
    ImageWithoutContent { role: String, id: String },
    ImageUnknownFormat { role: String, id: String, content: String },

    EntitiesWithSameName { role: String, name: String },

    CostumeIdFmt { role: String, entity: String, id: String },
    CostumeUndefinedRef { role: String, entity: String, id: String },
    CostumesWithSameName { role: String, entity: String, name: String },

    UnnamedGlobal { role: String },
    GlobalsWithSameName { role: String, name: String },

    UnnamedField { role: String, entity: String },
    FieldNoValue { role: String, entity: String, name: String },
    FieldsWithSameName { role: String, entity: String, name: String },

    BoolNoValue { role: String, entity: String },
    BoolUnknownValue { role: String, entity: String, value: String },
    UnnamedEntity { role: String },

    UnknownBlockMetaType { role: String, entity: String, meta_type: String },
    BlockWithoutType { role: String, entity: String },
    BlockChildCount { role: String, entity: String, block_type: String, needed: usize, got: usize },

    BlockMissingOption { role: String, entity: String, block_type: String },
    BlockOptionUnknown { role: String, entity: String, block_type: String, got: String },

    InvalidBoolLiteral { role: String, entity: String },
    NonConstantUpvar { role: String, entity: String, block_type: String },

    FailedToParseColor { role: String, entity: String, color: String },

    MessageTypeMissingName { role: String },
    MessageTypeNameEmpty { role: String },
    MessageTypeMissingFields { role: String, msg_type: String },
    MessageTypeFieldEmpty { role: String, msg_type: String },
    MessageTypeMultiplyDefined { role: String, msg_type: String },
}
#[derive(Debug)]
pub enum Error {
    XmlReadError { error: xmlparser::Error },
    XmlUnescapeError { illegal_sequence: String },

    InvalidBase64 { error: Base64DecodeError },

    InvalidProject { error: ProjectError },
    AutofillGenerateError { input: usize },
    NameTransformError { name: String, role: Option<String>, entity: Option<String> },
    UnknownBlockType { role: String, entity: String, block_type: String },
    DerefAssignment { role: String, entity: String },
    UndefinedVariable { role: String, entity: String, name: String },
    UndefinedFn { role: String, entity: String, name: String },
    BlockOptionNotConst { role: String, entity: String, block_type: String },
    BlockOptionNotSelected { role: String, entity: String, block_type: String },
    UnknownEntity { role: String, entity: String, unknown: String },
    UnknownEffect { role: String, entity: String, effect: String },
    UnknownPenAttr { role: String, entity: String, attr: String },

    UnknownMessageType { role: String, entity: String, msg_type: String },
    MessageTypeWrongNumberArgs { role: String, entity: String, msg_type: String, block_type: String, got: usize, expected: usize },

    UnknownService { role: String, entity: String, block_type: String, service: String },
    UnknownRPC { role: String, entity: String, block_type: String, service: String, rpc: String },

    GlobalsWithSameTransName { role: String, trans_name: String, names: (String, String) },
    EntitiesWithSameTransName { role: String, trans_name: String, names: (String, String) },
    FieldsWithSameTransName { role: String, entity: String, trans_name: String, names: (String, String) },
    LocalsWithSameTransName { role: String, entity: String, trans_name: String, names: (String, String) },
    CostumesWithSameTransName { role: String, entity: String, trans_name: String, names: (String, String) },
    BlocksWithSameTransName { role: String, entity: Option<String>, trans_name: String, names: (String, String) },

    InputsWithSameName { role: String, entity: Option<String>, name: String },
    BlocksWithSameName { role: String, entity: Option<String>, name: String, sigs: (String, String) },

    // TODO: get rid of these cases when new features are added
    BlockCurrentlyUnsupported { role: String, entity: String, block_type: String, what: String },
}
impl From<xmlparser::Error> for Error {
    fn from(error: xmlparser::Error) -> Error {
        Error::XmlReadError { error }
    }
}
impl From<Base64DecodeError> for Error {
    fn from(error: Base64DecodeError) -> Self {
        Error::InvalidBase64 { error }
    }
}

#[derive(Debug)]
pub enum SymbolError {
    NameTransformError { name: String },
    ConflictingTrans { trans_name: String, names: (String, String) },
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
    fn into_iter(self) -> std::vec::IntoIter<(K, V)> {
        self.0.into_iter()
    }
    fn get<Q: PartialEq + ?Sized>(&self, key: &Q) -> Option<&V> where K: std::borrow::Borrow<Q> {
        self.0.iter().find(|x| x.0.borrow() == key).map(|x| &x.1)
    }
    fn get_mut<Q: PartialEq + ?Sized>(&mut self, key: &Q) -> Option<&mut V> where K: std::borrow::Borrow<Q> {
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
    orig_to_def: VecMap<String, VariableDefInit>,
    trans_to_orig: VecMap<String, String>,
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
    fn transform_name(&self, name: &str) -> Result<String, SymbolError> {
        match self.parser.name_transformer.as_ref()(name) {
            Ok(v) => Ok(v),
            Err(()) => Err(SymbolError::NameTransformError { name: name.into() }),
        }
    }
    /// Defines a new symbol or replaces an existing definition.
    /// Fails if the name cannot be properly transformed or the transformed name already exists.
    /// On success, returns the previous definition (if one existed).
    /// On failure, the symbol table is not modified, and an error context object is returned.
    fn define(&mut self, name: String, value: Value) -> Result<Option<VariableDefInit>, SymbolError> {
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
    assert_eq!(sym.trans_to_orig.get("hello_world").unwrap(), "hello world!");
}

#[derive(Debug)]
struct Syscall {
    name: Box<Expr>,
    args: VariadicInput,
    info: BlockInfo,
}
#[derive(Debug)]
struct Rpc {
    service: String,
    rpc: String,
    args: Vec<(String, Expr)>,
    info: BlockInfo,
}
#[derive(Debug)]
struct FnCall {
    function: FnRef,
    args: Vec<Expr>,
    info: BlockInfo,
}

#[derive(Debug, Clone)]
pub struct BlockInfo {
    pub comment: Option<String>,
    pub location: Option<String>,
}
impl BlockInfo {
    fn none() -> Self {
        BlockInfo { comment: None, location: None }
    }
}

#[derive(Debug, Clone)]
pub struct Project {
    pub name: String,
    pub roles: Vec<Role>,
}
#[derive(Debug, Clone)]
pub struct Role {
    pub name: String,
    pub notes: String,
    pub stage_size: (usize, usize),
    pub globals: Vec<VariableDefInit>,
    pub funcs: Vec<Function>,
    pub entities: Vec<Entity>,
}
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub trans_name: String,
    pub params: Vec<VariableDef>,
    pub returns: bool,
    pub stmts: Vec<Stmt>,
}
#[derive(Debug, Clone)]
pub struct Entity {
    pub name: String,
    pub trans_name: String,
    pub fields: Vec<VariableDefInit>,
    pub costumes: Vec<VariableDefInit>,
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
    pub name: String,
    pub trans_name: String,
}
impl VariableDef {
    fn ref_at(&self, location: VarLocation) -> VariableRef {
        VariableRef { name: self.name.clone(), trans_name: self.trans_name.clone(), location }
    }
    fn fn_ref_at(&self, location: FnLocation) -> FnRef {
        FnRef { name: self.name.clone(), trans_name: self.trans_name.clone(), location }
    }
}
#[derive(Debug, Clone)]
pub struct VariableRef {
    pub name: String,
    pub trans_name: String,
    pub location: VarLocation,
}
#[derive(Debug, Clone)]
pub struct FnRef {
    pub name: String,
    pub trans_name: String,
    pub location: FnLocation,
}
#[derive(Debug, Clone, Copy)]
pub enum VarLocation {
    Global, Field, Local,
}
#[derive(Debug, Clone, Copy)]
pub enum FnLocation {
    Global, Method,
}
#[derive(Debug, Clone)]
pub struct Script {
    pub hat: Option<Hat>,
    pub stmts: Vec<Stmt>,
}
#[derive(Debug, Clone)]
pub struct Hat {
    pub kind: HatKind,
    pub info: BlockInfo,
}
#[derive(Debug, Clone)]
pub enum HatKind {
    OnFlag,
    OnKey { key: String },
    MouseDown,
    MouseUp,
    MouseEnter,
    MouseLeave,
    ScrollUp,
    ScrollDown,
    Dropped,
    Stopped,
    When { condition: Box<Expr> },
    LocalMessage { msg_type: String },
    NetworkMessage { msg_type: String, fields: Vec<VariableRef> },
}
#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub info: BlockInfo,
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

    SetCostume { costume: Option<Box<Expr>> },
    NextCostume,

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

    RunRpc { service: String, rpc: String, args: Vec<(String, Expr)> },
    RunFn { function: FnRef, args: Vec<Expr> },
    RunClosure { new_entity: Option<Box<Expr>>, closure: Box<Expr>, args: Vec<Expr> },

    /// Sends a message to local entities (not over the network).
    /// If `target` is `None`, this should broadcast to all entities.
    /// Otherwise `target` is either a single target or a list of targets to send to.
    /// The `wait` flag determines if the broadcast should be blocking (wait for receivers to terminate).
    SendLocalMessage { target: Option<Box<Expr>>, msg_type: Box<Expr>, wait: bool },
    /// Sends a message over the network to the specified targets.
    /// `target` may be a single target or a list of targets.
    SendNetworkMessage { target: Box<Expr>, msg_type: String, values: Vec<(String, Expr)> },
    /// Sends a reply from a received message that was blocking (sender's `wait` flag was `true`).
    SendNetworkReply { value: Box<Expr> },

    Ask { prompt: Box<Expr> },

    ResetTimer,

    Syscall { name: Box<Expr>, args: VariadicInput },

    SetEffect { kind: EffectKind, value: Box<Expr> },
    ChangeEffect { kind: EffectKind, delta: Box<Expr> },
    ClearEffects,

    SetPenAttr { attr: PenAttribute, value: Box<Expr> },
    ChangePenAttr { attr: PenAttribute, delta: Box<Expr> },
}
impl From<Rpc> for Stmt {
    fn from(rpc: Rpc) -> Stmt {
        let Rpc { service, rpc, args, info } = rpc;
        Stmt { kind: StmtKind::RunRpc { service, rpc, args }, info }
    }
}

#[derive(Debug, Clone)]
pub struct RefId(pub usize);

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Number(f64),
    Constant(Constant),
    String(String),
    Image(Rc<Vec<u8>>),
    List(Vec<Value>, Option<RefId>),
    Ref(RefId),
}

impl From<f64> for Value { fn from(v: f64) -> Value { Value::Number(v) } }
impl From<&str> for Value { fn from(v: &str) -> Value { Value::String(v.into()) } }
impl From<bool> for Value { fn from(v: bool) -> Value { Value::Bool(v) } }
impl From<String> for Value { fn from(v: String) -> Value { Value::String(v) } }
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
#[derive(Debug, Clone)]
pub enum VariadicInput {
    /// A fixed list of inputs specified inline.
    Fixed(Vec<Expr>),
    /// Inputs are the contents of an expression, which is expected to evaluate to a list.
    VarArgs(Box<Expr>),
}
#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub info: BlockInfo,
}
#[derive(Debug, Clone)]
pub enum ExprKind {
    Value(Value),
    Variable { var: VariableRef },

    Add { values: VariadicInput },
    Mul { values: VariadicInput },
    Min { values: VariadicInput },
    Max { values: VariadicInput },

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

    MakeList { values: VariadicInput },
    ListCat { lists: VariadicInput },

    ListLength { value: Box<Expr> },
    ListRank { value: Box<Expr> },
    ListDims { value: Box<Expr> },
    ListFlatten { value: Box<Expr> },
    ListColumns { value: Box<Expr> },
    ListRev { value: Box<Expr> },

    ListLines { value: Box<Expr> },
    ListCsv { value: Box<Expr> },
    ListJson { value: Box<Expr> },

    ListReshape { value: Box<Expr>, dims: VariadicInput },
    ListCombinations { sources: VariadicInput },

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

    StrCat { values: VariadicInput },
    /// String length in terms of unicode code points (not bytes or grapheme clusters!).
    StrLen { value: Box<Expr> },

    /// Convert a unicode code point into a 1-character string.
    UnicodeToChar { value: Box<Expr> },
    /// Convert a 1-character string into its unicode code point.
    CharToUnicode { value: Box<Expr> },

    Not { value: Box<Expr> },
    Neg { value: Box<Expr> },
    Abs { value: Box<Expr> },
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

    CallRpc { service: String, rpc: String, args: Vec<(String, Expr)> },
    CallFn { function: FnRef, args: Vec<Expr> },

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
    Entity { name: String, trans_name: String },

    ImageOfEntity { entity: Box<Expr> },
    ImageOfDrawings,

    IsTouchingEntity { entity: Box<Expr> },
    IsTouchingMouse,
    IsTouchingEdge,
    IsTouchingDrawings,

    RpcError,

    Closure { params: Vec<VariableDef>, captures: Vec<VariableRef>, stmts: Vec<Stmt> },
    CallClosure { new_entity: Option<Box<Expr>>, closure: Box<Expr>, args: Vec<Expr> },

    TextSplit { text: Box<Expr>, mode: TextSplitMode },

    Answer,

    Timer,

    Map { f: Box<Expr>, list: Box<Expr> },
    Keep { f: Box<Expr>, list: Box<Expr> },
    FindFirst { f: Box<Expr>, list: Box<Expr> },
    Combine { f: Box<Expr>, list: Box<Expr> },

    NetworkMessageReply { target: Box<Expr>, msg_type: String, values: Vec<(String, Expr)> },

    Syscall { name: Box<Expr>, args: VariadicInput },
    SyscallError,

    Effect { kind: EffectKind },
    PenAttr { attr: PenAttribute },

    CostumeList,
    Costume,
    CostumeNumber,
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
    msg_type: String,
    values: Vec<(String, Expr)>,
    info: BlockInfo,
}

struct ScriptInfo<'a, 'b, 'c> {
    parser: &'a Parser,
    role: &'c RoleInfo<'a>,
    entity: &'c EntityInfo<'a, 'b>,
    locals: Vec<(SymbolTable<'a>, Vec<VariableRef>)>, // tuples of (locals, captures)

    autofill_args: Vec<String>,
    in_autofill_mode: bool,
}
impl<'a, 'b, 'c> ScriptInfo<'a, 'b, 'c> {
    fn new(entity: &'c EntityInfo<'a, 'b>) -> Self {
        Self {
            parser: entity.parser,
            role: entity.role,
            entity,
            locals: vec![(SymbolTable::new(entity.parser), Default::default())],

            autofill_args: vec![],
            in_autofill_mode: false,
        }
    }
    fn check_children_get_info(&self, expr: &Xml, s: &str, req: usize) -> Result<BlockInfo, Box<Error>> {
        if expr.children.len() < req {
            return Err(Box::new(Error::InvalidProject { error: ProjectError::BlockChildCount { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into(), needed: req, got: expr.children.len() } }));
        }
        let comment = match expr.children.get(req) {
            Some(comment) => if comment.name == "comment" { Some(clean_newlines(&comment.text)) } else { None },
            None => None,
        };
        let location = expr.attr("collabId").map(|x| x.value.clone());
        Ok(BlockInfo { comment, location })
    }
    fn decl_local(&mut self, name: String, value: Value) -> Result<&VariableDefInit, Box<Error>> {
        let locals = &mut self.locals.last_mut().unwrap().0;
        match locals.define(name.clone(), value) {
            Ok(_) => (), // redefining locals is fine
            Err(SymbolError::ConflictingTrans { trans_name, names }) => if names.0 != names.1 { // redefining locals is fine
                return Err(Box::new(Error::LocalsWithSameTransName { role: self.role.name.clone(), entity: self.entity.name.clone(), trans_name, names }));
            }
            Err(SymbolError::NameTransformError { name }) => return Err(Box::new(Error::NameTransformError { name, role: Some(self.role.name.clone()), entity: Some(self.entity.name.clone()) })),
        }
        Ok(locals.get(&name).unwrap())
    }
    fn grab_option<'x>(&self, s: &str, child: &'x Xml) -> Result<&'x str, Box<Error>> {
        let res = match child.get(&["option"]) {
            None => return Err(Box::new(Error::InvalidProject { error: ProjectError::BlockMissingOption { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() } })),
            Some(f) => {
                if f.children.len() != 0 { return Err(Box::new(Error::BlockOptionNotConst { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() })) }
                f.text.as_str()
            }
        };
        if res == "" { return Err(Box::new(Error::BlockOptionNotSelected { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() })) }
        Ok(res)
    }
    fn grab_entity(&mut self, child: &Xml, info: BlockInfo) -> Result<Box<Expr>, Box<Error>> {
        Ok(match child.text.as_str() {
            "" => self.parse_expr(child)?,
            "myself" => Box::new(Expr { kind: ExprKind::This, info }),
            name => match self.role.entities.get(name) {
                None => return Err(Box::new(Error::UnknownEntity { role: self.role.name.clone(), entity: self.entity.name.clone(), unknown: name.into() })),
                Some(entity) => Box::new(Expr { kind: ExprKind::Entity { name: entity.def.name.clone(), trans_name: entity.def.trans_name.clone() }, info }),
            }
        })
    }
    fn parse(&mut self, script: &Xml) -> Result<Script, Box<Error>> {
        if script.children.is_empty() { return Ok(Script { hat: None, stmts: vec![] }) }

        let (hat, stmts_xml) = match self.parse_hat(&script.children[0])? {
            None => (None, script.children.as_slice()),
            Some(hat) => (Some(hat), &script.children[1..]),
        };

        let mut stmts = vec![];
        for stmt in stmts_xml {
            match stmt.name.as_str() {
                "block" => stmts.push(self.parse_block(stmt)?),
                "custom-block" => {
                    let FnCall { function, args, info } = self.parse_fn_call(stmt)?;
                    stmts.push(Stmt { kind: StmtKind::RunFn { function, args }, info });
                }
                x => return Err(Box::new(Error::InvalidProject { error: ProjectError::UnknownBlockMetaType { role: self.role.name.clone(), entity: self.entity.name.clone(), meta_type: x.to_owned() } })),
            }
        }
        Ok(Script { hat, stmts })
    }
    fn parse_hat(&mut self, stmt: &Xml) -> Result<Option<Hat>, Box<Error>> {
        let s = match stmt.attr("s") {
            None => return Err(Box::new(Error::InvalidProject { error: ProjectError::BlockWithoutType { role: self.role.name.clone(), entity: self.entity.name.clone() } })),
            Some(v) => v.value.as_str(),
        };
        Ok(Some(match s {
            "receiveGo" => {
                let info = self.check_children_get_info(stmt, s, 0)?;
                Hat { kind: HatKind::OnFlag, info }
            }
            "receiveCondition" => {
                let info = self.check_children_get_info(stmt, s, 1)?;
                let condition = self.parse_expr(&stmt.children[0])?;
                Hat { kind: HatKind::When { condition }, info }
            }
            "receiveKey" => {
                let info = self.check_children_get_info(stmt, s, 1)?;
                let key = self.grab_option(s, &stmt.children[0])?;
                Hat { kind: HatKind::OnKey { key: key.into() }, info }
            }
            "receiveInteraction" => {
                let info = self.check_children_get_info(stmt, s, 1)?;
                match self.grab_option(s, &stmt.children[0])? {
                    "pressed" => Hat { kind: HatKind::MouseDown, info },
                    "clicked" => Hat { kind: HatKind::MouseUp, info },
                    "mouse-entered" => Hat { kind: HatKind::MouseEnter, info },
                    "mouse-departed" => Hat { kind: HatKind::MouseLeave, info },
                    "scrolled-up" => Hat { kind: HatKind::ScrollUp, info },
                    "scrolled-down" => Hat { kind: HatKind::ScrollDown, info },
                    "dropped" => Hat { kind: HatKind::Dropped, info },
                    "stopped" => Hat { kind: HatKind::Stopped, info },
                    x => return Err(Box::new(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into(), got: x.into() } })),
                }
            }
            "receiveMessage" => {
                let info = self.check_children_get_info(stmt, s, 1)?;
                let child = &stmt.children[0];
                if child.name != "l" { return Err(Box::new(Error::BlockOptionNotConst { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() })) }
                let msg_type = match child.text.as_str() {
                    "" => return Err(Box::new(Error::BlockOptionNotSelected { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() })),
                    x => x.to_owned(),
                };
                Hat { kind: HatKind::LocalMessage { msg_type }, info }
            }
            "receiveSocketMessage" => {
                if stmt.children.is_empty() { return Err(Box::new(Error::InvalidProject { error: ProjectError::BlockChildCount { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into(), needed: 1, got: 0 } })) }
                if stmt.children[0].name != "l" { return Err(Box::new(Error::BlockOptionNotConst { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() })) }

                let msg_type = match stmt.children[0].text.as_str() {
                    "" => return Err(Box::new(Error::BlockOptionNotSelected { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() })),
                    x => x.to_owned(),
                };

                let mut fields = vec![];
                let mut comment = None;
                for child in stmt.children[1..].iter() {
                    if child.name == "comment" {
                        comment = Some(clean_newlines(&child.text));
                    }
                    if child.name != "l" { break }
                    let var = self.decl_local(child.text.clone(), 0f64.into())?.def.ref_at(VarLocation::Local);
                    fields.push(var);
                }
                let location = stmt.attr("collabId").map(|x| x.value.clone());
                Hat { kind: HatKind::NetworkMessage { msg_type, fields }, info: BlockInfo { comment, location } }
            }
            x if x.starts_with("receive") => return Err(Box::new(Error::UnknownBlockType { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: x.into() })),
            _ => return Ok(None),
        }))
    }
    fn parse_syscall(&mut self, stmt: &Xml, block_type: &str) -> Result<Syscall, Box<Error>> {
        let info = self.check_children_get_info(stmt, block_type, 2)?;
        let name = self.parse_expr(&stmt.children[0])?;
        let args = self.parse_varargs(&stmt.children[1])?;
        Ok(Syscall { name, args, info })
    }
    fn parse_effect(&mut self, effect: &Xml, s: &str) -> Result<EffectKind, Box<Error>> {
        Ok(match self.grab_option(s, effect)? {
            "color" => EffectKind::Color,
            "saturation" => EffectKind::Saturation,
            "brightness" => EffectKind::Brightness,
            "ghost" => EffectKind::Ghost,
            "fisheye" => EffectKind::Fisheye,
            "whirl" => EffectKind::Whirl,
            "pixelate" => EffectKind::Pixelate,
            "mosaic" => EffectKind::Mosaic,
            "negative" => EffectKind::Negative,
            x => return Err(Box::new(Error::UnknownEffect { role: self.role.name.clone(), entity: self.entity.name.clone(), effect: x.to_owned() })),
        })
    }
    fn parse_pen_attr(&mut self, attr: &Xml, s: &str) -> Result<PenAttribute, Box<Error>> {
        Ok(match self.grab_option(s, attr)? {
            "size" => PenAttribute::Size,
            "hue" => PenAttribute::Hue,
            "saturation" => PenAttribute::Saturation,
            "brightness" => PenAttribute::Brightness,
            "transparency" => PenAttribute::Transparency,
            x => return Err(Box::new(Error::UnknownPenAttr { role: self.role.name.clone(), entity: self.entity.name.clone(), attr: x.to_owned() })),
        })
    }
    fn parse_rpc(&mut self, stmt: &Xml, block_type: &str) -> Result<Rpc, Box<Error>> {
        if stmt.children.len() < 2 { return Err(Box::new(Error::InvalidProject { error: ProjectError::BlockChildCount { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: block_type.into(), needed: 2, got: stmt.children.len() } })) }
        for i in 0..=1 { if stmt.children[i].name != "l" { return Err(Box::new(Error::BlockOptionNotConst { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: block_type.into() })) } }
        for i in 0..=1 { if stmt.children[i].name.is_empty() { return Err(Box::new(Error::BlockOptionNotSelected { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: block_type.into() })) } }

        let service = stmt.children[0].text.clone();
        let rpc = stmt.children[1].text.clone();

        let arg_names = match stmt.attr("inputNames").map(|x| x.value.split(';').map(str::trim).filter(|v| !v.is_empty()).collect::<Vec<_>>()) {
            Some(x) => x,
            None => match SERVICE_INFO.get(service.as_str()) {
                None => return Err(Box::new(Error::UnknownService { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: block_type.into(), service })),
                Some(x) => match x.get(rpc.as_str()) {
                    None => return Err(Box::new(Error::UnknownRPC { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: block_type.into(), service, rpc })),
                    Some(&x) => x.to_owned(),
                }
            }
        };

        let info = self.check_children_get_info(stmt, block_type, 2 + arg_names.len())?;
        let mut args = Vec::with_capacity(arg_names.len());
        for (&arg_name, child) in arg_names.iter().zip(&stmt.children[2 .. 2 + arg_names.len()]) {
            let val = self.parse_expr(child)?;
            args.push((arg_name.to_owned(), *val));
        }
        Ok(Rpc { service, rpc, args, info })
    }
    fn parse_fn_call(&mut self, stmt: &Xml) -> Result<FnCall, Box<Error>> {
        let s = match stmt.attr("s") {
            Some(v) => v.value.as_str(),
            None => return Err(Box::new(Error::InvalidProject { error: ProjectError::CustomBlockWithoutName { role: self.role.name.clone(), entity: Some(self.entity.name.clone()) } })),
        };

        let name = block_name_from_ref(s);
        let argc = ArgIter::new(s).count();
        let function = self.reference_fn(&name)?;
        let info = self.check_children_get_info(stmt, s, argc)?;

        let mut args = Vec::with_capacity(argc);
        for expr in stmt.children[..argc].iter() {
            args.push(*self.parse_expr(expr)?);
        }

        Ok(FnCall { function, args, info })
    }
    fn parse_send_message_common(&mut self, stmt: &Xml, s: &str) -> Result<NetworkMessage, Box<Error>> {
        let msg_type = match stmt.children.get(0) {
            Some(value) if value.name != "comment" => value.text.as_str(),
            _ => return Err(Box::new(Error::InvalidProject { error: ProjectError::BlockMissingOption { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() } })),
        };
        let fields = match self.role.msg_types.get(msg_type) {
            None => return Err(Box::new(Error::UnknownMessageType { role: self.role.name.clone(), entity: self.entity.name.clone(), msg_type: msg_type.into() })),
            Some(x) => x,
        };

        let (argc, comment) = stmt.children.iter().enumerate().find(|(_, x)| x.name == "comment").map(|(i, x)| (i, Some(x.text.as_str()))).unwrap_or((stmt.children.len(), None));
        assert!(argc >= 1); // due to msg_type from above

        let values = stmt.children[1..argc - 1].iter().map(|x| self.parse_expr(x)).collect::<Result<Vec<_>,_>>()?;
        if fields.len() != values.len() {
            return Err(Box::new(Error::MessageTypeWrongNumberArgs { role: self.role.name.clone(), entity: self.entity.name.clone(), msg_type: msg_type.into(), block_type: s.into(), got: values.len(), expected: fields.len() }));
        }

        let target_xml = &stmt.children[argc - 1];
        let target = match target_xml.get(&["option"]) {
            Some(x) => match x.text.as_str() {
                "" => return Err(Box::new(Error::BlockOptionNotSelected { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() })),
                x => Box::new(x.into()),
            }
            None => self.parse_expr(target_xml)?,
        };

        let comment = comment.map(|x| x.to_owned());
        let location = stmt.attr("collabId").map(|x| x.value.clone());
        Ok(NetworkMessage { target, msg_type: msg_type.into(), values: fields.iter().map(|&x| x.to_owned()).zip(values.into_iter().map(|x| *x)).collect(), info: BlockInfo { comment, location } })
    }
    fn parse_block(&mut self, stmt: &Xml) -> Result<Stmt, Box<Error>> {
        let s = match stmt.attr("s") {
            None => return Err(Box::new(Error::InvalidProject { error: ProjectError::BlockWithoutType { role: self.role.name.clone(), entity: self.entity.name.clone() } })),
            Some(v) => v.value.as_str(),
        };
        match s {
            "doDeclareVariables" => {
                let info = self.check_children_get_info(stmt, s, 1)?;
                let mut vars = vec![];
                for var in stmt.children[0].children.iter() {
                    vars.push(self.decl_local(var.text.clone(), 0f64.into())?.def.clone());
                }
                Ok(Stmt { kind: StmtKind::DeclareLocals { vars }, info })
            }
            "doSetVar" | "doChangeVar" => {
                let info = self.check_children_get_info(stmt, s, 2)?;
                let var = match stmt.children[0].name.as_str() {
                    "l" => self.reference_var(&stmt.children[0].text)?,
                    _ => return Err(Box::new(Error::DerefAssignment { role: self.role.name.clone(), entity: self.entity.name.clone() })),
                };
                let value = self.parse_expr(&stmt.children[1])?;
                match s {
                    "doSetVar" => Ok(Stmt { kind: StmtKind::Assign { var, value }, info }),
                    "doChangeVar" => Ok(Stmt { kind: StmtKind::AddAssign { var, value }, info }),
                    _ => unreachable!(),
                }
            }
            "doShowVar" | "doHideVar" => {
                let info = self.check_children_get_info(stmt, s, 1)?;
                let var = match stmt.children[0].name.as_str() {
                    "l" => self.reference_var(&stmt.children[0].text)?,
                    _ => return Err(Box::new(Error::DerefAssignment { role: self.role.name.clone(), entity: self.entity.name.clone() })),
                };
                match s {
                    "doShowVar" => Ok(Stmt { kind: StmtKind::ShowVar { var }, info }),
                    "doHideVar" => Ok(Stmt { kind: StmtKind::HideVar { var }, info }),
                    _ => unreachable!(),
                }
            }
            "doFor" => {
                let info = self.check_children_get_info(stmt, s, 4)?;

                let var = match stmt.children[0].name.as_str() {
                    "l" => stmt.children[0].text.as_str(),
                    _ => return Err(Box::new(Error::InvalidProject { error: ProjectError::NonConstantUpvar { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() } })),
                };
                let start = self.parse_expr(&stmt.children[1])?;
                let stop = self.parse_expr(&stmt.children[2])?;
                let var = self.decl_local(var.to_owned(), 0f64.into())?.def.ref_at(VarLocation::Local); // define after bounds, but before loop body
                let stmts = self.parse(&stmt.children[3])?.stmts;

                Ok(Stmt { kind: StmtKind::ForLoop { var, start, stop, stmts }, info })
            }
            "doForEach" => {
                let info = self.check_children_get_info(stmt, s, 3)?;

                let var = match stmt.children[0].name.as_str() {
                    "l" => stmt.children[0].text.as_str(),
                    _ => return Err(Box::new(Error::InvalidProject { error: ProjectError::NonConstantUpvar { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() } })),
                };
                let items = self.parse_expr(&stmt.children[1])?;
                let var = self.decl_local(var.to_owned(), 0f64.into())?.def.ref_at(VarLocation::Local); // define after bounds, but before loop body
                let stmts = self.parse(&stmt.children[2])?.stmts;

                Ok(Stmt { kind: StmtKind::ForeachLoop { var, items, stmts }, info })
            }
            "doRepeat" | "doUntil" | "doIf" => {
                let info = self.check_children_get_info(stmt, s, 2)?;
                let expr = self.parse_expr(&stmt.children[0])?;
                let stmts = self.parse(&stmt.children[1])?.stmts;
                match s {
                    "doRepeat" => Ok(Stmt { kind: StmtKind::Repeat { times: expr, stmts }, info }),
                    "doUntil" => Ok(Stmt { kind: StmtKind::UntilLoop { condition: expr, stmts }, info }),
                    "doIf" => Ok(Stmt { kind: StmtKind::If { condition: expr, then: stmts }, info }),
                    _ => unreachable!(),
                }
            }
            "doForever" => {
                let info = self.check_children_get_info(stmt, s, 1)?;
                let stmts = self.parse(&stmt.children[0])?.stmts;
                Ok(Stmt { kind: StmtKind::InfLoop { stmts }, info })
            }
            "doIfElse" => {
                let info = self.check_children_get_info(stmt, s, 3)?;
                let condition = self.parse_expr(&stmt.children[0])?;
                let then = self.parse(&stmt.children[1])?.stmts;
                let otherwise = self.parse(&stmt.children[2])?.stmts;
                Ok(Stmt { kind: StmtKind::IfElse { condition, then, otherwise }, info })
            }
            "doTryCatch" => {
                let info = self.check_children_get_info(stmt, s, 3)?;
                let code = self.parse(&stmt.children[0])?.stmts;
                let var = match stmt.children[1].name.as_str() {
                    "l" => self.decl_local(stmt.children[1].text.clone(), 0f64.into())?.def.ref_at(VarLocation::Local),
                    _ => return Err(Box::new(Error::InvalidProject { error: ProjectError::NonConstantUpvar { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() } })),
                };
                let handler = self.parse(&stmt.children[2])?.stmts;
                Ok(Stmt { kind: StmtKind::TryCatch { code, var, handler }, info })
            }
            "doWarp" => {
                let info = self.check_children_get_info(stmt, s, 1)?;
                let stmts = self.parse(&stmt.children[0])?.stmts;
                Ok(Stmt { kind: StmtKind::Warp { stmts }, info })
            }
            "doDeleteFromList" => {
                let info = self.check_children_get_info(stmt, s, 2)?;
                let list = self.parse_expr(&stmt.children[1])?;
                match stmt.children[0].get(&["option"]) {
                    Some(opt) => match opt.text.as_str() {
                        "last" => Ok(Stmt { kind: StmtKind::ListRemoveLast { list }, info }),
                        "all" => Ok(Stmt { kind: StmtKind::ListRemoveAll { list }, info }),
                        "" => Err(Box::new(Error::BlockOptionNotSelected { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() })),
                        x => Err(Box::new(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into(), got: x.into() } })),
                    }
                    None => {
                        let index = self.parse_expr(&stmt.children[0])?;
                        Ok(Stmt { kind: StmtKind::ListRemove { list, index }, info })
                    }
                }
            }
            "doInsertInList" => {
                let info = self.check_children_get_info(stmt, s, 3)?;
                let value = self.parse_expr(&stmt.children[0])?;
                let list = self.parse_expr(&stmt.children[2])?;
                match stmt.children[1].get(&["option"]) {
                    Some(opt) => match opt.text.as_str() {
                        "last" => Ok(Stmt { kind: StmtKind::ListInsertLast { list, value }, info }),
                        "random" | "any" => Ok(Stmt { kind: StmtKind::ListInsertRandom { list, value }, info }),
                        "" => Err(Box::new(Error::BlockOptionNotSelected { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() })),
                        x => Err(Box::new(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into(), got: x.into() } })),
                    }
                    None => {
                        let index = self.parse_expr(&stmt.children[1])?;
                        Ok(Stmt { kind: StmtKind::ListInsert { list, value, index }, info })
                    }
                }
            }
            "doReplaceInList" => {
                let info = self.check_children_get_info(stmt, s, 3)?;
                let value = self.parse_expr(&stmt.children[2])?;
                let list = self.parse_expr(&stmt.children[1])?;
                match stmt.children[0].get(&["option"]) {
                    Some(opt) => match opt.text.as_str() {
                        "last" => Ok(Stmt { kind: StmtKind::ListAssignLast { list, value }, info }),
                        "random" | "any" => Ok(Stmt { kind: StmtKind::ListAssignRandom { list, value }, info }),
                        "" => Err(Box::new(Error::BlockOptionNotSelected { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() })),
                        x => Err(Box::new(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into(), got: x.into() } })),
                    }
                    None => {
                        let index = self.parse_expr(&stmt.children[0])?;
                        Ok(Stmt { kind: StmtKind::ListAssign { list, value, index }, info })
                    }
                }
            }
            "doSwitchToCostume" => {
                let info = self.check_children_get_info(stmt, s, 1)?;
                let val = &stmt.children[0];

                if val.name == "l" && val.get(&["option"]).is_some() {
                    match self.grab_option(s, val)? {
                        "Turtle" => Ok(Stmt { kind: StmtKind::SetCostume { costume: None }, info }),
                        x => Err(Box::new(Error::BlockCurrentlyUnsupported { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into(), what: format!("{s} with project costume ({x}) currently not supported") })),
                    }
                } else if val.name == "l" {
                    match val.text.as_str() {
                        "" => Ok(Stmt { kind: StmtKind::SetCostume { costume: None }, info }),
                        x => Ok(Stmt { kind: StmtKind::SetCostume { costume: Some(Box::new(x.into())) }, info }),
                    }
                } else {
                    Ok(Stmt { kind: StmtKind::SetCostume { costume: Some(self.parse_expr(val)?) }, info })
                }
            }
            "setHeading" => {
                let info = self.check_children_get_info(stmt, s, 1)?;
                let child = &stmt.children[0];

                if child.name == "l" && child.get(&["option"]).is_some() {
                    let opt = self.grab_option(s, child)?;
                    match opt {
                        "random" => Ok(Stmt { kind: StmtKind::SetHeadingRandom, info }),
                        _ => Err(Box::new(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into(), got: opt.into() } })),
                    }
                } else {
                    let value = self.parse_expr(child)?;
                    Ok(Stmt { kind: StmtKind::SetHeading { value }, info })
                }
            }
            "doGotoObject" => {
                let info = self.check_children_get_info(stmt, s, 1)?;
                let child = &stmt.children[0];

                if child.name == "l" && child.get(&["option"]).is_some() {
                    let opt = self.grab_option(s, child)?;
                    match opt {
                        "random position" => Ok(Stmt { kind: StmtKind::GotoRandom, info }),
                        "mouse-pointer" => Ok(Stmt { kind: StmtKind::GotoMouse, info }),
                        "center" => Ok(Stmt { kind: StmtKind::GotoXY { x: Box::new(0f64.into()), y: Box::new(0f64.into()) }, info }),
                        _ => Err(Box::new(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into(), got: opt.into() } })),                    }
                }
                else {
                    Ok(Stmt { kind: StmtKind::Goto { target: self.parse_expr(child)? }, info })
                }
            }
            "doFaceTowards" => {
                let info = self.check_children_get_info(stmt, s, 1)?;
                let child = &stmt.children[0];

                if child.name == "l" && child.get(&["option"]).is_some() {
                    let opt = self.grab_option(s, child)?;
                    match opt {
                        "center" => Ok(Stmt { kind: StmtKind::PointTowardsXY { x: Box::new(0.0.into()), y: Box::new(0.0.into()) }, info }),
                        _ => Err(Box::new(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into(), got: opt.into() } })),
                    }
                } else {
                    Ok(Stmt { kind: StmtKind::PointTowards { target: self.parse_expr(child)? }, info })
                }
            }
            "setColor" => {
                let info = self.check_children_get_info(stmt, s, 1)?;
                match stmt.get(&["color"]) {
                    Some(color) => match parse_color(&color.text) {
                        Some(color) => Ok(Stmt { kind: StmtKind::SetPenColor { color }, info }),
                        None => Err(Box::new(Error::InvalidProject { error: ProjectError::FailedToParseColor { role: self.role.name.clone(), entity: self.entity.name.clone(), color: color.text.clone() } })),
                    }
                    None => Err(Box::new(Error::BlockOptionNotConst { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() })),
                }
            }
            "doSocketMessage" => {
                let NetworkMessage { target, msg_type, values, info } = self.parse_send_message_common(stmt, s)?;
                Ok(Stmt { kind: StmtKind::SendNetworkMessage { target, msg_type, values }, info })
            }
            "doRun" => {
                let info = self.check_children_get_info(stmt, s, 2)?;
                let closure = self.parse_expr(&stmt.children[0])?;
                let mut args = Vec::with_capacity(stmt.children[1].children.len());
                for arg in stmt.children[1].children.iter() {
                    args.push(*self.parse_expr(arg)?);
                }
                Ok(Stmt { kind: StmtKind::RunClosure { new_entity: None, closure, args }, info })
            }
            "doTellTo" => {
                let info = self.check_children_get_info(stmt, s, 3)?;
                let entity = self.grab_entity(&stmt.children[0], BlockInfo::none())?;
                let closure = self.parse_expr(&stmt.children[1])?;
                let mut args = Vec::with_capacity(stmt.children[2].children.len());
                for arg in stmt.children[2].children.iter() {
                    args.push(*self.parse_expr(arg)?);
                }
                Ok(Stmt { kind: StmtKind::RunClosure { new_entity: Some(entity), closure, args }, info })
            }
            "nativeRunSyscall" => {
                let info = self.parse_syscall(stmt, s)?;
                Ok(Stmt { kind: StmtKind::Syscall { name: info.name, args: info.args }, info: info.info })
            }
            "setEffect" => {
                let info = self.check_children_get_info(stmt, s, 2)?;
                let effect = self.parse_effect(&stmt.children[0], s)?;
                let value = self.parse_expr(&stmt.children[1])?;
                Ok(Stmt { kind: StmtKind::SetEffect { kind: effect, value }, info })
            }
            "changeEffect" => {
                let info = self.check_children_get_info(stmt, s, 2)?;
                let effect = self.parse_effect(&stmt.children[0], s)?;
                let delta = self.parse_expr(&stmt.children[1])?;
                Ok(Stmt { kind: StmtKind::ChangeEffect { kind: effect, delta }, info })
            }
            "setPenHSVA" => {
                let info = self.check_children_get_info(stmt, s, 2)?;
                let attr = self.parse_pen_attr(&stmt.children[0], s)?;
                let value = self.parse_expr(&stmt.children[1])?;
                Ok(Stmt { kind: StmtKind::SetPenAttr { attr, value }, info })
            }
            "changePenHSVA" => {
                let info = self.check_children_get_info(stmt, s, 2)?;
                let attr = self.parse_pen_attr(&stmt.children[0], s)?;
                let delta = self.parse_expr(&stmt.children[1])?;
                Ok(Stmt { kind: StmtKind::ChangePenAttr { attr, delta }, info })
            }
            "write" => self.parse_2_args(stmt, s).map(|(content, font_size, info)| Stmt { kind: StmtKind::Write { content, font_size }, info }),
            "doBroadcast" => self.parse_1_args(stmt, s).map(|(msg_type, info)| Stmt { kind: StmtKind::SendLocalMessage { msg_type, target: None, wait: false }, info }),
            "doBroadcastAndWait" => self.parse_1_args(stmt, s).map(|(msg_type, info)| Stmt { kind: StmtKind::SendLocalMessage { msg_type, target: None, wait: true }, info }),
            "doSocketResponse" => self.parse_1_args(stmt, s).map(|(value, info)| Stmt { kind: StmtKind::SendNetworkReply { value }, info }),
            "changeScale" => self.parse_1_args(stmt, s).map(|(delta, info)| Stmt { kind: StmtKind::ChangeSize { delta, }, info }),
            "setScale" => self.parse_1_args(stmt, s).map(|(value, info)| Stmt { kind: StmtKind::SetSize { value }, info }),
            "doSayFor" => self.parse_2_args(stmt, s).map(|(content, duration, info)| Stmt { kind: StmtKind::Say { content, duration: Some(duration) }, info }),
            "doThinkFor" => self.parse_2_args(stmt, s).map(|(content, duration, info)| Stmt { kind: StmtKind::Think { content, duration: Some(duration) }, info }),
            "bubble" => self.parse_1_args(stmt, s).map(|(content, info)| Stmt { kind: StmtKind::Say { content, duration: None }, info }),
            "doThink" => self.parse_1_args(stmt, s).map(|(content, info)| Stmt { kind: StmtKind::Think { content, duration: None }, info }),
            "doThrow" => self.parse_1_args(stmt, s).map(|(error, info)| Stmt { kind: StmtKind::Throw { error }, info }),
            "hide" => self.parse_0_args(stmt, s).map(|info| Stmt { kind: StmtKind::SetVisible { value: false }, info }),
            "show" => self.parse_0_args(stmt, s).map(|info| Stmt { kind: StmtKind::SetVisible { value: true }, info }),
            "doWaitUntil" => self.parse_1_args(stmt, s).map(|(condition, info)| Stmt { kind: StmtKind::WaitUntil { condition, }, info }),
            "changeSize" => self.parse_1_args(stmt, s).map(|(delta, info)| Stmt { kind: StmtKind::ChangePenSize { delta, }, info }),
            "setSize" => self.parse_1_args(stmt, s).map(|(value, info)| Stmt { kind: StmtKind::SetPenSize { value }, info }),
            "doAddToList" => self.parse_2_args(stmt, s).map(|(value, list, info)| Stmt { kind: StmtKind::ListInsertLast { value, list }, info }),
            "doReport" => self.parse_1_args(stmt, s).map(|(value, info)| Stmt { kind: StmtKind::Return { value }, info }),
            "doStamp" => self.parse_0_args(stmt, s).map(|info| Stmt { kind: StmtKind::Stamp, info }),
            "doWait" => self.parse_1_args(stmt, s).map(|(seconds, info)| Stmt { kind: StmtKind::Sleep { seconds, }, info }),
            "forward" => self.parse_1_args(stmt, s).map(|(distance, info)| Stmt { kind: StmtKind::Forward { distance, }, info }),
            "turn" => self.parse_1_args(stmt, s).map(|(angle, info)| Stmt { kind: StmtKind::TurnRight { angle, }, info }),
            "turnLeft" => self.parse_1_args(stmt, s).map(|(angle, info)| Stmt { kind: StmtKind::TurnLeft { angle, }, info }),
            "setXPosition" => self.parse_1_args(stmt, s).map(|(value, info)| Stmt { kind: StmtKind::SetX { value }, info }),
            "setYPosition" => self.parse_1_args(stmt, s).map(|(value, info)| Stmt { kind: StmtKind::SetY { value }, info }),
            "changeXPosition" => self.parse_1_args(stmt, s).map(|(delta, info)| Stmt { kind: StmtKind::ChangeX { delta }, info }),
            "changeYPosition" => self.parse_1_args(stmt, s).map(|(delta, info)| Stmt { kind: StmtKind::ChangeY { delta }, info }),
            "gotoXY" => self.parse_2_args(stmt, s).map(|(x, y, info)| Stmt { kind: StmtKind::GotoXY { x, y }, info }),
            "bounceOffEdge" => self.parse_0_args(stmt, s).map(|info| Stmt { kind: StmtKind::BounceOffEdge, info }),
            "down" => self.parse_0_args(stmt, s).map(|info| Stmt { kind: StmtKind::SetPenDown { value: true }, info }),
            "up" => self.parse_0_args(stmt, s).map(|info| Stmt { kind: StmtKind::SetPenDown { value: false }, info }),
            "clear" => self.parse_0_args(stmt, s).map(|info| Stmt { kind: StmtKind::PenClear, info }),
            "doRunRPC" => Ok(self.parse_rpc(stmt, s)?.into()),
            "doAsk" => self.parse_1_args(stmt, s).map(|(prompt, info)| Stmt { kind: StmtKind::Ask { prompt, }, info }),
            "doResetTimer" => self.parse_0_args(stmt, s).map(|info| Stmt { kind: StmtKind::ResetTimer, info }),
            "clearEffects" => self.parse_0_args(stmt, s).map(|info| Stmt { kind: StmtKind::ClearEffects, info }),
            "doWearNextCostume" => self.parse_0_args(stmt, s).map(|info| Stmt { kind: StmtKind::NextCostume, info }),
            _ => Err(Box::new(Error::UnknownBlockType { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.to_owned() })),
        }
    }
    fn reference_var(&mut self, name: &str) -> Result<VariableRef, Box<Error>> {
        for (i, locals) in self.locals.iter().rev().enumerate() {
            if let Some(x) = locals.0.get(name) {
                let res = x.def.ref_at(VarLocation::Local);
                if i != 0 {
                    let (locals, captures) = self.locals.last_mut().unwrap();
                    locals.define(res.name.clone(), 0.0.into()).unwrap();
                    captures.push(res.clone());
                }
                return Ok(res)
            }
        }
        if let Some(x) = self.entity.fields.get(name) { return Ok(x.def.ref_at(VarLocation::Field)) }
        if let Some(x) = self.role.globals.get(name) { return Ok(x.def.ref_at(VarLocation::Global)) }
        Err(Box::new(Error::UndefinedVariable { role: self.role.name.clone(), entity: self.entity.name.clone(), name: name.into() }))
    }
    fn reference_fn(&self, name: &str) -> Result<FnRef, Box<Error>> {
        let locs = [(&self.entity.funcs, FnLocation::Method), (&self.role.funcs, FnLocation::Global)];
        match locs.iter().find_map(|v| v.0.get(name).map(|x| x.def.fn_ref_at(v.1))) {
            Some(v) => Ok(v),
            None => Err(Box::new(Error::UndefinedFn { role: self.role.name.clone(), entity: self.entity.name.clone(), name: name.into() }))
        }
    }
    fn cnd_adjust_index(&self, index: Box<Expr>, condition: bool, delta: f64) -> Box<Expr> {
        match condition {
            true => Box::new(Expr { kind: ExprKind::Add { values: VariadicInput::Fixed(vec![*index, delta.into()]) }, info: BlockInfo::none() }),
            false => index,
        }
    }
    fn parse_0_args(&mut self, expr: &Xml, s: &str) -> Result<BlockInfo, Box<Error>> {
        self.check_children_get_info(expr, s, 0)
    }
    fn parse_1_args(&mut self, expr: &Xml, s: &str) -> Result<(Box<Expr>, BlockInfo), Box<Error>> {
        let info = self.check_children_get_info(expr, s, 1)?;
        let a = self.parse_expr(&expr.children[0])?;
        Ok((a, info))
    }
    fn parse_2_args(&mut self, expr: &Xml, s: &str) -> Result<(Box<Expr>, Box<Expr>, BlockInfo), Box<Error>> {
        let info = self.check_children_get_info(expr, s, 1)?;
        let a = self.parse_expr(&expr.children[0])?;
        let b = self.parse_expr(&expr.children[1])?;
        Ok((a, b, info))
    }
    fn parse_1_varargs(&mut self, expr: &Xml, s: &str) -> Result<(VariadicInput, BlockInfo), Box<Error>> {
        let info = self.check_children_get_info(expr, s, 1)?;
        let values = self.parse_varargs(&expr.children[0])?;
        Ok((values, info))
    }
    fn parse_varargs(&mut self, varargs_root: &Xml) -> Result<VariadicInput, Box<Error>> {
        Ok(match varargs_root.name.as_str() {
            "list" => {
                let mut res = vec![];
                for item in varargs_root.children.iter() {
                    res.push(*self.parse_expr(item)?);
                }
                VariadicInput::Fixed(res)
            }
            _ => VariadicInput::VarArgs(self.parse_expr(varargs_root)?),
        })
    }
    fn parse_bool(&self, val: &str) -> Result<Box<Expr>, Box<Error>> {
        match val {
            "true" => Ok(Box::new(true.into())),
            "false" => Ok(Box::new(false.into())),
            _ => Err(Box::new(Error::InvalidProject { error: ProjectError::BoolUnknownValue { role: self.role.name.clone(), entity: self.entity.name.clone(), value: val.into() } }))
        }
    }
    fn parse_expr(&mut self, expr: &Xml) -> Result<Box<Expr>, Box<Error>> {
        match expr.name.as_str() {
            "l" => match expr.children.first() {
                Some(child) if child.name == "bool" => self.parse_bool(&child.text),
                _ => match !expr.text.is_empty() || !self.in_autofill_mode {
                    true => Ok(Box::new(expr.text.clone().into())),
                    false => {
                        let input = self.autofill_args.len() + 1;
                        let name = self.parser.autofill_generator.as_ref()(input).map_err(|_| Error::AutofillGenerateError { input })?;
                        self.autofill_args.push(name.clone());

                        let trans_name = match self.parser.name_transformer.as_ref()(&name) {
                            Ok(x) => x,
                            Err(_) => return Err(Box::new(Error::NameTransformError { name, role: Some(self.role.name.clone()), entity: Some(self.entity.name.clone()) })),
                        };
                        Ok(Box::new(Expr { kind: ExprKind::Variable { var: VariableRef { name, trans_name, location: VarLocation::Local } }, info: BlockInfo::none() }))
                    }
                }
            }
            "bool" => self.parse_bool(&expr.text),
            "list" => {
                let ref_id = expr.attr("id").and_then(|x| x.value.parse().ok()).map(RefId);
                let values = match expr.attr("struct").map(|x| x.value.as_str()) {
                    Some("atomic") => InlineListIter::new(&expr.text).map(Into::into).collect(),
                    _ => {
                        let mut values = Vec::with_capacity(expr.children.len());
                        for item in expr.children.iter() {
                            match item.children.get(0) {
                                None => values.push(String::new().into()),
                                Some(x) => match self.parse_expr(x)?.kind {
                                    ExprKind::Value(v) => values.push(v),
                                    _ => return Err(Box::new(Error::InvalidProject { error: ProjectError::ValueNotEvaluated { role: self.role.name.clone(), entity: Some(self.entity.name.clone()) } })),
                                }
                            }
                        }
                        values
                    }
                };
                Ok(Box::new(Value::List(values, ref_id).into()))
            }
            "ref" => match expr.attr("id").and_then(|x| x.value.parse().ok()).map(RefId) {
                Some(ref_id) => Ok(Box::new(Value::Ref(ref_id).into())),
                None => return Err(Box::new(Error::InvalidProject { error: ProjectError::RefMissingId { role: self.role.name.clone(), entity: self.entity.name.clone() } })),
            }
            "custom-block" => {
                let FnCall { function, args, info } = self.parse_fn_call(expr)?;
                Ok(Box::new(Expr { kind: ExprKind::CallFn { function, args }, info }))
            }
            "block" => {
                if let Some(var) = expr.attr("var") {
                    let info = self.check_children_get_info(expr, "var", 0)?;
                    let var = self.reference_var(&var.value)?;
                    return Ok(Box::new(Expr { kind: ExprKind::Variable { var }, info }));
                }
                let s = match expr.attr("s") {
                    None => return Err(Box::new(Error::InvalidProject { error: ProjectError::BlockWithoutType { role: self.role.name.clone(), entity: self.entity.name.clone() } })),
                    Some(v) => v.value.as_str(),
                };
                println!("    >> inside {s}");
                match s {
                    "reportVariadicSum" => self.parse_1_varargs(expr, s).map(|(values, info)| Box::new(Expr { kind: ExprKind::Add { values }, info })),
                    "reportVariadicProduct" => self.parse_1_varargs(expr, s).map(|(values, info)| Box::new(Expr { kind: ExprKind::Mul { values }, info })),
                    "reportVariadicMin" => self.parse_1_varargs(expr, s).map(|(values, info)| Box::new(Expr { kind: ExprKind::Min { values }, info })),
                    "reportVariadicMax" => self.parse_1_varargs(expr, s).map(|(values, info)| Box::new(Expr { kind: ExprKind::Max { values }, info })),

                    "reportSum" => self.parse_2_args(expr, s).map(|(left, right, info)| Box::new(Expr { kind: ExprKind::Add { values: VariadicInput::Fixed(vec![*left, *right]) }, info })),
                    "reportProduct" => self.parse_2_args(expr, s).map(|(left, right, info)| Box::new(Expr { kind: ExprKind::Mul { values: VariadicInput::Fixed(vec![*left, *right]) }, info })),
                    "reportMin" => self.parse_2_args(expr, s).map(|(left, right, info)| Box::new(Expr { kind: ExprKind::Min { values: VariadicInput::Fixed(vec![*left, *right]) }, info })),
                    "reportMax" => self.parse_2_args(expr, s).map(|(left, right, info)| Box::new(Expr { kind: ExprKind::Max { values: VariadicInput::Fixed(vec![*left, *right]) }, info })),

                    "reportDifference" => self.parse_2_args(expr, s).map(|(left, right, info)| Box::new(Expr { kind: ExprKind::Sub { left, right }, info })),
                    "reportQuotient" => self.parse_2_args(expr, s).map(|(left, right, info)| Box::new(Expr { kind: ExprKind::Div { left, right }, info })),
                    "reportModulus" => self.parse_2_args(expr, s).map(|(left, right, info)| Box::new(Expr { kind: ExprKind::Mod { left, right }, info })),
                    "reportPower" => self.parse_2_args(expr, s).map(|(base, power, info)| Box::new(Expr { kind: ExprKind::Pow { base, power }, info })),
                    "reportAtan2" => self.parse_2_args(expr, s).map(|(y, x, info)| Box::new(Expr { kind: ExprKind::Atan2 { y, x }, info })),

                    "reportAnd" => self.parse_2_args(expr, s).map(|(left, right, info)| Box::new(Expr { kind: ExprKind::And { left, right }, info })),
                    "reportOr" => self.parse_2_args(expr, s).map(|(left, right, info)| Box::new(Expr { kind: ExprKind::Or { left, right }, info })),

                    "reportIsIdentical" => self.parse_2_args(expr, s).map(|(left, right, info)| Box::new(Expr { kind: ExprKind::Identical { left, right }, info })),
                    "reportEquals" => self.parse_2_args(expr, s).map(|(left, right, info)| Box::new(Expr { kind: ExprKind::Eq { left, right }, info })),
                    "reportNotEquals" => self.parse_2_args(expr, s).map(|(left, right, info)| Box::new(Expr { kind: ExprKind::Neq { left, right }, info })),
                    "reportLessThan" => self.parse_2_args(expr, s).map(|(left, right, info)| Box::new(Expr { kind: ExprKind::Less { left, right }, info })),
                    "reportLessThanOrEquals" => self.parse_2_args(expr, s).map(|(left, right, info)| Box::new(Expr { kind: ExprKind::LessEq { left, right }, info })),
                    "reportGreaterThan" => self.parse_2_args(expr, s).map(|(left, right, info)| Box::new(Expr { kind: ExprKind::Greater { left, right }, info })),
                    "reportGreaterThanOrEquals" => self.parse_2_args(expr, s).map(|(left, right, info)| Box::new(Expr { kind: ExprKind::GreaterEq { left, right }, info })),

                    "reportRandom" => self.parse_2_args(expr, s).map(|(a, b, info)| Box::new(Expr { kind: ExprKind::Random { a, b }, info })),
                    "reportNumbers" => self.parse_2_args(expr, s).map(|(start, stop, info)| Box::new(Expr { kind: ExprKind::Range { start, stop }, info })),

                    "reportNot" => self.parse_1_args(expr, s).map(|(value, info)| Box::new(Expr { kind: ExprKind::Not { value }, info })),
                    "reportRound" => self.parse_1_args(expr, s).map(|(value, info)| Box::new(Expr { kind: ExprKind::Round { value }, info })),

                    "reportListLength" => self.parse_1_args(expr, s).map(|(value, info)| Box::new(Expr { kind: ExprKind::ListLength { value }, info })),
                    "reportListIsEmpty" => self.parse_1_args(expr, s).map(|(value, info)| Box::new(Expr { kind: ExprKind::ListIsEmpty { value }, info })),

                    "reportListIndex" => {
                        let index = self.parse_2_args(expr, s).map(|(value, list, info)| Box::new(Expr { kind: ExprKind::ListFind { value, list }, info }))?;
                        Ok(self.cnd_adjust_index(index, self.parser.adjust_to_zero_index, 1.0))
                    }
                    "reportListContainsItem" => self.parse_2_args(expr, s).map(|(list, value, info)| Box::new(Expr { kind: ExprKind::ListContains { list, value }, info })),

                    "reportListItem" => {
                        let info = self.check_children_get_info(expr, s, 2)?;
                        let list = self.parse_expr(&expr.children[1])?.into();
                        match expr.children[0].get(&["option"]) {
                            Some(opt) => match opt.text.as_str() {
                                "last" => Ok(Box::new(Expr { kind: ExprKind::ListGetLast { list }, info })),
                                "any" => Ok(Box::new(Expr { kind: ExprKind::ListGetRandom { list }, info })),
                                "" => Err(Box::new(Error::BlockOptionNotSelected { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() })),
                                x => Err(Box::new(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into(), got: x.into() } })),
                            }
                            None => {
                                let index = self.parse_expr(&expr.children[0])?;
                                let index = self.cnd_adjust_index(index, self.parser.adjust_to_zero_index, -1.0).into();
                                Ok(Box::new(Expr { kind: ExprKind::ListGet { list, index }, info }))
                            }
                        }
                    }
                    "reportLetter" => {
                        let info = self.check_children_get_info(expr, s, 2)?;
                        let string = self.parse_expr(&expr.children[1])?.into();
                        match expr.children[0].get(&["option"]) {
                            Some(opt) => match opt.text.as_str() {
                                "last" => Ok(Box::new(Expr { kind: ExprKind::StrGetLast { string }, info })),
                                "any" => Ok(Box::new(Expr { kind: ExprKind::StrGetRandom { string }, info })),
                                "" => Err(Box::new(Error::BlockOptionNotSelected { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() })),
                                x => Err(Box::new(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into(), got: x.into() } })),
                            }
                            None => {
                                let index = self.parse_expr(&expr.children[0])?;
                                let index = self.cnd_adjust_index(index, self.parser.adjust_to_zero_index, -1.0).into();
                                Ok(Box::new(Expr { kind: ExprKind::StrGet { string, index }, info }))
                            }
                        }
                    }
                    "reportTextSplit" => {
                        let info = self.check_children_get_info(expr, s, 2)?;
                        let text = self.parse_expr(&expr.children[0])?.into();
                        let mode = match expr.children[1].get(&["option"]) {
                            Some(opt) => match opt.text.as_str() {
                                "letter" => TextSplitMode::Letter,
                                "word" => TextSplitMode::Word,
                                "line" => TextSplitMode::LF,
                                "tab" => TextSplitMode::Tab,
                                "cr" => TextSplitMode::CR,
                                "csv" => TextSplitMode::Csv,
                                "json" => TextSplitMode::Json,
                                "" => return Err(Box::new(Error::BlockOptionNotSelected { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into() })),
                                x => return Err(Box::new(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into(), got: x.into() } })),
                            }
                            None => TextSplitMode::Custom(self.parse_expr(&expr.children[1])?.into()),
                        };
                        Ok(Box::new(Expr { kind: ExprKind::TextSplit { text, mode }, info }))
                    }

                    "reportStringSize" => self.parse_1_args(expr, s).map(|(value, info)| Box::new(Expr { kind: ExprKind::StrLen { value }, info })),
                    "reportUnicodeAsLetter" => self.parse_1_args(expr, s).map(|(value, info)| Box::new(Expr { kind: ExprKind::UnicodeToChar { value }, info })),
                    "reportUnicode" => self.parse_1_args(expr, s).map(|(value, info)| Box::new(Expr { kind: ExprKind::CharToUnicode { value }, info })),

                    "reportCDR" => self.parse_1_args(expr, s).map(|(value, info)| Box::new(Expr { kind: ExprKind::ListCdr { value }, info })),
                    "reportCONS" => self.parse_2_args(expr, s).map(|(item, list, info)| Box::new(Expr { kind: ExprKind::ListCons { item, list }, info })),

                    "reportJoinWords" => self.parse_1_varargs(expr, s).map(|(values, info)| Box::new(Expr { kind: ExprKind::StrCat { values }, info })),
                    "reportConcatenatedLists" => self.parse_1_varargs(expr, s).map(|(lists, info)| Box::new(Expr { kind: ExprKind::ListCat { lists }, info })),
                    "reportNewList" => self.parse_1_varargs(expr, s).map(|(values, info)| Box::new(Expr { kind: ExprKind::MakeList { values }, info })),
                    "reportCrossproduct" => self.parse_1_varargs(expr, s).map(|(sources, info)| Box::new(Expr { kind: ExprKind::ListCombinations { sources }, info })),

                    "reportBoolean" => match expr.get(&["l", "bool"]) {
                        Some(v) if v.text == "true" => Ok(Box::new(true.into())),
                        Some(v) if v.text == "false" => Ok(Box::new(false.into())),
                        _ => Err(Box::new(Error::InvalidProject { error: ProjectError::InvalidBoolLiteral { role: self.role.name.clone(), entity: self.entity.name.clone() } })),
                    }
                    "reportMonadic" => {
                        let info = self.check_children_get_info(expr, s, 2)?;
                        let func = self.grab_option(s, &expr.children[0])?;
                        let value = self.parse_expr(&expr.children[1])?;
                        match func {
                            "id" => Ok(value),

                            "neg" => Ok(Box::new(Expr { kind: ExprKind::Neg { value }, info })),
                            "abs" => Ok(Box::new(Expr { kind: ExprKind::Abs { value }, info })),
                            "sqrt" => Ok(Box::new(Expr { kind: ExprKind::Sqrt { value }, info })),
                            "floor" => Ok(Box::new(Expr { kind: ExprKind::Floor { value }, info })),
                            "ceiling" => Ok(Box::new(Expr { kind: ExprKind::Ceil { value }, info })),

                            "sin" => Ok(Box::new(Expr { kind: ExprKind::Sin { value }, info })),
                            "cos" => Ok(Box::new(Expr { kind: ExprKind::Cos { value }, info })),
                            "tan" => Ok(Box::new(Expr { kind: ExprKind::Tan { value }, info })),

                            "asin" => Ok(Box::new(Expr { kind: ExprKind::Asin { value }, info })),
                            "acos" => Ok(Box::new(Expr { kind: ExprKind::Acos { value }, info })),
                            "atan" => Ok(Box::new(Expr { kind: ExprKind::Atan { value }, info })),

                            "ln" => Ok(Box::new(Expr { kind: ExprKind::Log { value, base: Box::new(Constant::E.into()) }, info })),
                            "lg" => Ok(Box::new(Expr { kind: ExprKind::Log { value, base: Box::new(2f64.into()) }, info })),
                            "log" => Ok(Box::new(Expr { kind: ExprKind::Log { value, base: Box::new(10f64.into()) }, info })),

                            "e^" => Ok(Box::new(Expr { kind: ExprKind::Pow { base: Box::new(Constant::E.into()), power: value }, info })),
                            "2^" => Ok(Box::new(Expr { kind: ExprKind::Pow { base: Box::new(2f64.into()), power: value }, info })),
                            "10^" => Ok(Box::new(Expr { kind: ExprKind::Pow { base: Box::new(10f64.into()), power: value }, info })),

                            _ => Err(Box::new(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into(), got: func.into() } })),
                        }
                    }
                    "reportListAttribute" => {
                        let info = self.check_children_get_info(expr, s, 2)?;
                        let func = self.grab_option(s, &expr.children[0])?;
                        let value = self.parse_expr(&expr.children[1])?;
                        match func {
                            "length" => Ok(Box::new(Expr { kind: ExprKind::ListLength { value }, info })),
                            "rank" => Ok(Box::new(Expr { kind: ExprKind::ListRank { value }, info })),
                            "dimensions" => Ok(Box::new(Expr { kind: ExprKind::ListDims { value }, info })),
                            "flatten" => Ok(Box::new(Expr { kind: ExprKind::ListFlatten { value }, info })),
                            "columns" => Ok(Box::new(Expr { kind: ExprKind::ListColumns { value }, info })),
                            "reverse" => Ok(Box::new(Expr { kind: ExprKind::ListRev { value }, info })),

                            "lines" => Ok(Box::new(Expr { kind: ExprKind::ListLines { value }, info })),
                            "csv" => Ok(Box::new(Expr { kind: ExprKind::ListCsv { value }, info })),
                            "json" => Ok(Box::new(Expr { kind: ExprKind::ListJson { value }, info })),

                            _ => Err(Box::new(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into(), got: func.into() } })),
                        }
                    }
                    "reportReshape" => {
                        let info = self.check_children_get_info(expr, s, 2)?;
                        let value = self.parse_expr(&expr.children[0])?;
                        let dims = self.parse_varargs(&expr.children[1])?;
                        Ok(Box::new(Expr { kind: ExprKind::ListReshape { value, dims }, info }))
                    }

                    "reportIfElse" => {
                        let info = self.check_children_get_info(expr, s, 3)?;
                        let condition = self.parse_expr(&expr.children[0])?;
                        let then = self.parse_expr(&expr.children[1])?;
                        let otherwise = self.parse_expr(&expr.children[2])?;
                        Ok(Box::new(Expr { kind: ExprKind::Conditional { condition, then, otherwise }, info }))
                    }
                    "getJSFromRPCStruct" => Ok(Box::new(self.parse_rpc(expr, s)?.into())),

                    "reportStageWidth" => self.parse_0_args(expr, s).map(|info| Box::new(Expr { kind: ExprKind::StageWidth, info })),
                    "reportStageHeight" => self.parse_0_args(expr, s).map(|info| Box::new(Expr { kind: ExprKind::StageHeight, info })),

                    "reportMouseX" => self.parse_0_args(expr, s).map(|info| Box::new(Expr { kind: ExprKind::MouseX, info })),
                    "reportMouseY" => self.parse_0_args(expr, s).map(|info| Box::new(Expr { kind: ExprKind::MouseY, info })),

                    "reportLatitude" => self.parse_0_args(expr, s).map(|info| Box::new(Expr { kind: ExprKind::Latitude, info })),
                    "reportLongitude" => self.parse_0_args(expr, s).map(|info| Box::new(Expr { kind: ExprKind::Longitude, info })),

                    "reportPenTrailsAsCostume" => self.parse_0_args(expr, s).map(|info| Box::new(Expr { kind: ExprKind::ImageOfDrawings, info })),
                    "reportImageOfObject" => {
                        let info = self.check_children_get_info(expr, s, 1)?;
                        let entity = self.grab_entity(&expr.children[0], BlockInfo::none())?.into();
                        Ok(Box::new(Expr { kind: ExprKind::ImageOfEntity { entity }, info }))
                    }
                    "reportTouchingObject" => {
                        let info = self.check_children_get_info(expr, s, 1)?;
                        let child = &expr.children[0];
                        if child.name == "l" && child.get(&["option"]).is_some() {
                            match self.grab_option(s, child)? {
                                "mouse-pointer" => Ok(Box::new(Expr { kind: ExprKind::IsTouchingMouse, info })),
                                "pen trails" => Ok(Box::new(Expr { kind: ExprKind::IsTouchingDrawings, info })),
                                "edge" => Ok(Box::new(Expr { kind: ExprKind::IsTouchingEdge, info })),
                                x => Err(Box::new(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into(), got: x.into() } })),
                            }
                        }
                        else {
                            let entity = self.grab_entity(child, BlockInfo::none())?.into();
                            Ok(Box::new(Expr { kind: ExprKind::IsTouchingEntity { entity }, info }))
                        }
                    }

                    "reportRPCError" => self.parse_0_args(expr, s).map(|info| Box::new(Expr { kind: ExprKind::RpcError, info })),

                    "getScale" => self.parse_0_args(expr, s).map(|info| Box::new(Expr { kind: ExprKind::Size, info })),
                    "reportShown" => self.parse_0_args(expr, s).map(|info| Box::new(Expr { kind: ExprKind::IsVisible, info })),

                    "xPosition" => self.parse_0_args(expr, s).map(|info| Box::new(Expr { kind: ExprKind::XPos, info })),
                    "yPosition" => self.parse_0_args(expr, s).map(|info| Box::new(Expr { kind: ExprKind::YPos, info })),
                    "direction" => self.parse_0_args(expr, s).map(|info| Box::new(Expr { kind: ExprKind::Heading, info })),

                    "getPenDown" => self.parse_0_args(expr, s).map(|info| Box::new(Expr { kind: ExprKind::PenDown, info })),

                    "getLastAnswer" => self.parse_0_args(expr, s).map(|info| Box::new(Expr { kind: ExprKind::Answer, info })),

                    "getTimer" => self.parse_0_args(expr, s).map(|info| Box::new(Expr { kind: ExprKind::Timer, info })),

                    "reportMap" => self.parse_2_args(expr, s).map(|(f, list, info)| Box::new(Expr { kind: ExprKind::Map { f, list }, info })),
                    "reportKeep" => self.parse_2_args(expr, s).map(|(f, list, info)| Box::new(Expr { kind: ExprKind::Keep { f, list }, info })),
                    "reportFindFirst" => self.parse_2_args(expr, s).map(|(f, list, info)| Box::new(Expr { kind: ExprKind::FindFirst { f, list }, info })),
                    "reportCombine" => self.parse_2_args(expr, s).map(|(list, f, info)| Box::new(Expr { kind: ExprKind::Combine { list, f }, info })),

                    "reifyScript" | "reifyReporter" | "reifyPredicate" => {
                        let is_script = s == "reifyScript";
                        let info = self.check_children_get_info(expr, s, 2)?;

                        let mut params = SymbolTable::new(self.parser);
                        fn define_param(script_info: &ScriptInfo, params: &mut SymbolTable, name: String) -> Result<(), Box<Error>> {
                            match params.define(name, 0.0.into()) {
                                Ok(None) => Ok(()),
                                Ok(Some(prev)) => Err(Box::new(Error::InputsWithSameName { role: script_info.role.name.clone(), name: prev.def.name, entity: Some(script_info.entity.name.clone()) })),
                                Err(SymbolError::ConflictingTrans { trans_name, names }) => Err(Box::new(Error::LocalsWithSameTransName { role: script_info.role.name.clone(), entity: script_info.entity.name.clone(), trans_name, names })),
                                Err(SymbolError::NameTransformError { name }) => Err(Box::new(Error::NameTransformError { name, role: Some(script_info.role.name.clone()), entity: Some(script_info.entity.name.clone()) })),
                            }
                        }
                        for input in expr.children[1].children.iter() {
                            define_param(self, &mut params, input.text.clone())?;
                        }

                        let prev_in_autofill_mode = self.in_autofill_mode;
                        let prev_autofill_args_len = self.autofill_args.len();
                        self.in_autofill_mode = params.is_empty();

                        self.locals.push((params.clone(), Default::default()));
                        let locals_len = self.locals.len();
                        let stmts = match is_script {
                            true => self.parse(&expr.children[0])?.stmts,
                            false => {
                                let _ = self.check_children_get_info(&expr.children[0], s, 1)?;
                                let value = self.parse_expr(&expr.children[0].children[0])?;
                                vec![Stmt { kind: StmtKind::Return { value }, info: BlockInfo::none() }]
                            }
                        };
                        assert_eq!(locals_len, self.locals.len());
                        let (_, captures) = self.locals.pop().unwrap();
                        for var in captures.iter() {
                            self.reference_var(&var.name).unwrap();
                        }

                        for autofill_arg in &self.autofill_args[prev_autofill_args_len..] {
                            define_param(self, &mut params, autofill_arg.clone())?;
                        }

                        self.in_autofill_mode = prev_in_autofill_mode;
                        if !self.in_autofill_mode {
                            self.autofill_args.clear();
                        }

                        Ok(Box::new(Expr { kind: ExprKind::Closure { params: params.into_defs(), captures, stmts }, info }))
                    }
                    "evaluate" => {
                        let info = self.check_children_get_info(expr, s, 2)?;
                        let closure = self.parse_expr(&expr.children[0])?;
                        let mut args = Vec::with_capacity(expr.children[1].children.len());
                        for input in expr.children[1].children.iter() {
                            args.push(*self.parse_expr(input)?);
                        }
                        Ok(Box::new(Expr { kind: ExprKind::CallClosure { new_entity: None, closure, args }, info }))
                    }
                    "reportAskFor" => {
                        let info = self.check_children_get_info(expr, s, 3)?;
                        let entity = self.grab_entity(&expr.children[0], BlockInfo::none())?;
                        let closure = self.parse_expr(&expr.children[1])?;
                        let mut args = Vec::with_capacity(expr.children[2].children.len());
                        for input in expr.children[2].children.iter() {
                            args.push(*self.parse_expr(input)?);
                        }
                        Ok(Box::new(Expr { kind: ExprKind::CallClosure { new_entity: Some(entity), closure, args }, info }))
                    }
                    "doSocketRequest" => {
                        let NetworkMessage { target, msg_type, values, info } = self.parse_send_message_common(expr, s)?;
                        Ok(Box::new(Expr { kind: ExprKind::NetworkMessageReply { target, msg_type, values }, info }))
                    }
                    "nativeCallSyscall" => {
                        let Syscall { name, args, info } = self.parse_syscall(expr, s)?;
                        Ok(Box::new(Expr { kind: ExprKind::Syscall { name, args }, info }))
                    }
                    "nativeSyscallError" => self.parse_0_args(expr, s).map(|info| Box::new(Expr { kind: ExprKind::SyscallError, info })),
                    "getEffect" => {
                        let info = self.check_children_get_info(expr, s, 1)?;
                        let effect = self.parse_effect(&expr.children[0], s)?;
                        Ok(Box::new(Expr { kind: ExprKind::Effect { kind: effect }, info }))
                    }
                    "getPenAttribute" => {
                        let info = self.check_children_get_info(expr, s, 1)?;
                        let attr = self.parse_pen_attr(&expr.children[0], s)?;
                        Ok(Box::new(Expr { kind: ExprKind::PenAttr { attr }, info }))
                    }
                    "reportGet" => {
                        let info = self.check_children_get_info(expr, s, 1)?;
                        match self.grab_option(s, &expr.children[0])? {
                            "costumes" => Ok(Box::new(Expr { kind: ExprKind::CostumeList, info })),
                            "costume" => Ok(Box::new(Expr { kind: ExprKind::Costume, info })),
                            m => Err(Box::new(Error::BlockCurrentlyUnsupported { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.into(), what: format!("the {s} block with option {m} is currently not supported") })),
                        }
                    }
                    "reportObject" => {
                        let info = self.check_children_get_info(expr, s, 1)?;
                        self.grab_entity(&expr.children[0], info)
                    }
                    "getCostumeIdx" => self.parse_0_args(expr, s).map(|info| Box::new(Expr { kind: ExprKind::CostumeNumber, info })),
                    _ => Err(Box::new(Error::UnknownBlockType { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: s.to_owned() })),
                }
            }
            x => Err(Box::new(Error::UnknownBlockType { role: self.role.name.clone(), entity: self.entity.name.clone(), block_type: x.into() })),
        }
    }
}

struct EntityInfo<'a, 'b> {
    parser: &'a Parser,
    role: &'b RoleInfo<'a>,
    name: String,
    trans_name: String,
    fields: SymbolTable<'a>,
    funcs: SymbolTable<'a>,
    costumes: SymbolTable<'a>,
}
impl<'a, 'b> EntityInfo<'a, 'b> {
    fn new(role: &'b RoleInfo<'a>, name: VariableRef) -> Self {
        Self {
            parser: role.parser,
            role,
            name: name.name,
            trans_name: name.trans_name,
            fields: SymbolTable::new(role.parser),
            funcs: SymbolTable::new(role.parser),
            costumes: SymbolTable::new(role.parser),
        }
    }
    fn parse(mut self, entity: &'a Xml) -> Result<Entity, Box<Error>> {
        for costume in entity.get(&["costumes", "list"]).map(|c| c.children.as_slice()).unwrap_or(&[]) {
            if let Some(ident) = costume.get(&["ref"]).map(|r| r.attr("mediaID")).flatten() {
                let ident = ident.value.as_str();
                if !ident.starts_with(&self.name) || !ident[self.name.len()..].starts_with("_cst_") {
                    return Err(Box::new(Error::InvalidProject { error: ProjectError::CostumeIdFmt { role: self.role.name.clone(), entity: self.name, id: ident.into() } }));
                }
                let name = &ident[self.name.len() + 5..];

                let content = match self.role.images.get(ident) {
                    Some(x) => x.clone(),
                    None => return Err(Box::new(Error::InvalidProject { error: ProjectError::CostumeUndefinedRef { role: self.role.name.clone(), entity: self.name, id: ident.into() } })),
                };

                match self.costumes.define(name.into(), Value::Image(content)) {
                    Ok(None) => (),
                    Ok(Some(prev)) => return Err(Box::new(Error::InvalidProject { error: ProjectError::CostumesWithSameName { role: self.role.name.clone(), entity: self.name, name: prev.def.name } })),
                    Err(SymbolError::NameTransformError { name }) => return Err(Box::new(Error::NameTransformError { name, role: Some(self.role.name.clone()), entity: Some(self.name) })),
                    Err(SymbolError::ConflictingTrans { trans_name, names }) => return Err(Box::new(Error::CostumesWithSameTransName { role: self.role.name.clone(), entity: self.name, trans_name, names })),
                }
            }
        }

        let blocks = entity.get(&["blocks"]).map(|v| v.children.as_slice()).unwrap_or(&[]);
        for block in blocks {
            parse_block_header(block, &mut self.funcs, &self.role.name, Some(&self.name))?;
        }
        let mut funcs = vec![];
        for block in blocks {
            funcs.push(parse_block(block, &self.funcs, self.role, Some(&self))?);
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
                    None => return Err(Box::new(Error::InvalidProject { error: ProjectError::UnnamedField { role: self.role.name.clone(), entity: self.name } })),
                    Some(x) => x.value.clone(),
                };
                let value = match def.children.get(0) {
                    None => return Err(Box::new(Error::InvalidProject { error: ProjectError::FieldNoValue { role: self.role.name.clone(), entity: self.name, name } })),
                    Some(x) => match dummy_script.parse_expr(x)?.kind {
                        ExprKind::Value(v) => v,
                        _ => return Err(Box::new(Error::InvalidProject { error: ProjectError::ValueNotEvaluated { role: self.role.name.clone(), entity: Some(self.name) } })),
                    }
                };
                defs.push((name, value));
            }

            for (name, value) in defs {
                match self.fields.define(name.clone(), value) {
                    Ok(None) => (),
                    Ok(Some(prev)) => return Err(Box::new(Error::InvalidProject { error: ProjectError::FieldsWithSameName { role: self.role.name.clone(), entity: self.name.clone(), name: prev.def.name } })),
                    Err(SymbolError::NameTransformError { name }) => return Err(Box::new(Error::NameTransformError { name, role: Some(self.role.name.clone()), entity: Some(self.name.clone()) })),
                    Err(SymbolError::ConflictingTrans { trans_name, names }) => return Err(Box::new(Error::FieldsWithSameTransName { role: self.role.name.clone(), entity: self.name.clone(), trans_name, names })),
                }
            }
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

// returns the signature and returns flag of the block header value
fn get_block_info(value: &Value) -> (&str, bool) {
    match value {
        Value::List(vals, _) => {
            assert_eq!(vals.len(), 2);
            let s = match &vals[0] { Value::String(v) => v, _ => panic!() };
            let returns = match vals[1] { Value::Bool(v) => v, _ => panic!() };
            (s, returns)
        }
        _ => panic!(), // header parser would never do this
    }
}

fn block_name_from_def(s: &str) -> String {
    replace_ranges(s, ParamIter::new(s), "\t") // tabs leave a marker for args which disappears after ident renaming
}
fn block_name_from_ref(s: &str) -> String {
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

fn parse_block_header<'a>(block: &'a Xml, funcs: &mut SymbolTable<'a>, role: &str, entity: Option<&str>) -> Result<(), Box<Error>> {
    let entity = || entity.map(|v| v.to_owned());

    let s = match block.attr("s") {
        Some(v) => v.value.as_str(),
        None => return Err(Box::new(Error::InvalidProject { error: ProjectError::CustomBlockWithoutName { role: role.into(), entity: entity() } })),
    };
    let returns = match block.attr("type") {
        Some(v) => match v.value.as_str() {
            "command" => false,
            "reporter" | "predicate" => true,
            x => return Err(Box::new(Error::InvalidProject { error: ProjectError::CustomBlockUnknownType { role: role.into(), entity: entity(), sig: s.into(), ty: x.into() } })),
        }
        None => return Err(Box::new(Error::InvalidProject { error: ProjectError::CustomBlockWithoutType { role: role.into(), entity: entity(), sig: s.into() } })),
    };

    let name = block_name_from_def(s);
    match funcs.define(name, Value::List(vec![Value::from(s), Value::from(returns)], None)) {
        Ok(None) => Ok(()),
        Ok(Some(prev)) => Err(Box::new(Error::BlocksWithSameName { role: role.into(), entity: entity(), name: prev.def.name, sigs: (get_block_info(&prev.init).0.into(), s.into()) })),
        Err(SymbolError::NameTransformError { name }) => Err(Box::new(Error::NameTransformError { name, role: Some(role.into()), entity: entity() })),
        Err(SymbolError::ConflictingTrans { trans_name, names }) => Err(Box::new(Error::BlocksWithSameTransName { role: role.into(), entity: entity(), trans_name, names })),
    }
}
fn parse_block<'a>(block: &'a Xml, funcs: &SymbolTable<'a>, role: &RoleInfo, entity: Option<&EntityInfo>) -> Result<Function, Box<Error>> {
    let s = block.attr("s").unwrap().value.as_str(); // unwrap ok because we assume parse_block_header() was called before
    let entry = funcs.get(&block_name_from_def(s)).unwrap();
    let (s2, returns) = get_block_info(&entry.init); // unwrap ok because header parser
    assert_eq!(s, s2);

    let finalize = |entity_info: &EntityInfo| {
        let mut script_info = ScriptInfo::new(entity_info);
        for param in ParamIter::new(s).map(|(a, b)| s[a+2..b-1].to_owned()) {
            script_info.decl_local(param, 0f64.into())?;
        }
        debug_assert_eq!(script_info.locals.len(), 1);
        debug_assert_eq!(script_info.locals[0].1.len(), 0);
        let params = script_info.locals[0].0.clone().into_defs();
        let stmts = match block.get(&["script"]) {
            Some(script) => script_info.parse(script)?.stmts,
            None => vec![],
        };

        Ok(Function {
            name: entry.def.name.clone(),
            trans_name: entry.def.trans_name.clone(),
            params,
            returns,
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
    name: String,
    globals: SymbolTable<'a>,
    entities: SymbolTable<'a>,
    funcs: SymbolTable<'a>,
    images: VecMap<&'a str, Rc<Vec<u8>>>,
    msg_types: VecMap<&'a str, Vec<&'a str>>,
}
impl<'a> RoleInfo<'a> {
    fn new(parser: &'a Parser, name: String) -> Self {
        Self {
            parser,
            name,
            globals: SymbolTable::new(parser),
            entities: SymbolTable::new(parser),
            funcs: SymbolTable::new(parser),
            images: Default::default(),
            msg_types: Default::default(),
        }
    }
    fn parse(mut self, role_root: &'a Xml) -> Result<Role, Box<Error>> {
        assert_eq!(role_root.name, "role");
        let role = match role_root.attr("name") {
            None => return Err(Box::new(Error::InvalidProject { error: ProjectError::UnnamedRole })),
            Some(x) => x.value.clone(),
        };
        let content = match role_root.get(&["project"]) {
            None => return Err(Box::new(Error::InvalidProject { error: ProjectError::NoRoleContent { role } })),
            Some(x) => x,
        };
        let notes = content.get(&["notes"]).map(|v| v.text.as_str()).unwrap_or("").to_owned();
        let stage = match content.get(&["stage"]) {
            None => return Err(Box::new(Error::InvalidProject { error: ProjectError::NoStageDef { role } })),
            Some(x) => x,
        };
        let stage_width = stage.attr("width").and_then(|x| x.value.parse::<usize>().ok()).unwrap_or(480);
        let stage_height = stage.attr("height").and_then(|x| x.value.parse::<usize>().ok()).unwrap_or(360);

        let msg_types = stage.get(&["messageTypes"]).map(|x| x.children.as_slice()).unwrap_or(&[]);
        for msg_type in msg_types {
            let name = match msg_type.get(&["name"]) {
                None => return Err(Box::new(Error::InvalidProject { error: ProjectError::MessageTypeMissingName { role } })),
                Some(x) => match x.text.as_str() {
                    "" => return Err(Box::new(Error::InvalidProject { error: ProjectError::MessageTypeNameEmpty { role } })),
                    x => x,
                }
            };
            let fields = match msg_type.get(&["fields"]) {
                None => return Err(Box::new(Error::InvalidProject { error: ProjectError::MessageTypeMissingFields { role, msg_type: name.into() } })),
                Some(x) => {
                    let mut res = vec![];
                    for field in x.children.iter() {
                        if field.name != "field" { continue }
                        res.push(match field.text.as_str() {
                            "" => return Err(Box::new(Error::InvalidProject { error: ProjectError::MessageTypeFieldEmpty { role, msg_type: name.into() } })),
                            x => x,
                        });
                    }
                    res
                }
            };

            if self.msg_types.insert(name, fields).is_some() {
                return Err(Box::new(Error::InvalidProject { error: ProjectError::MessageTypeMultiplyDefined { role, msg_type: name.into() } }));
            }
        }

        for entry in role_root.get(&["media"]).map(|v| v.children.as_slice()).unwrap_or(&[]) {
            if entry.name != "costume" { continue }
            let id = match entry.attr("mediaID") {
                Some(x) => x.value.as_str(),
                None => return Err(Box::new(Error::InvalidProject { error: ProjectError::ImageWithoutId { role } })),
            };

            let content = match entry.attr("image") {
                Some(x) => match x.value.as_str() {
                    x if x.starts_with("data:image/png;base64,") => base64_decode(&x[22..])?,
                    x => return Err(Box::new(Error::InvalidProject { error: ProjectError::ImageUnknownFormat { role, id: id.into(), content: x.into() } })),
                }
                None => return Err(Box::new(Error::InvalidProject { error: ProjectError::ImageWithoutContent { role, id: id.into() } })),
            };

            if self.images.insert(id, Rc::new(content)).is_some() {
                return Err(Box::new(Error::InvalidProject { error: ProjectError::ImagesWithSameId { role, id: id.into() } }));
            }
        }

        if let Some(globals) = content.get(&["variables"]) {
            let dummy_name = VariableRef { name: "global".into(), trans_name: "global".into(), location: VarLocation::Global };
            let dummy_entity = EntityInfo::new(&self, dummy_name); // fine to do before entities/blocks/etc. since globals are just values (not stmts or exprs)
            let mut dummy_script = ScriptInfo::new(&dummy_entity);

            let mut defs = vec![];
            for def in globals.children.iter().filter(|v| v.name == "variable") {
                let name = match def.attr("name") {
                    None => return Err(Box::new(Error::InvalidProject { error: ProjectError::UnnamedGlobal { role } })),
                    Some(x) => x.value.clone(),
                };
                let value = match def.children.get(0) {
                    None => Value::Number(0.0),
                    Some(x) => match dummy_script.parse_expr(x)?.kind {
                        ExprKind::Value(v) => v,
                        _ => return Err(Box::new(Error::InvalidProject { error: ProjectError::ValueNotEvaluated { role, entity: None } })),
                    }
                };
                defs.push((name, value));
            }

            for (name, value) in defs {
                match self.globals.define(name.clone(), value) {
                    Ok(None) => (),
                    Ok(Some(prev)) => return Err(Box::new(Error::InvalidProject { error: ProjectError::GlobalsWithSameName { role: self.name.clone(), name: prev.def.name } })),
                    Err(SymbolError::NameTransformError { name }) => return Err(Box::new(Error::NameTransformError { name, role: Some(self.name.clone()), entity: None })),
                    Err(SymbolError::ConflictingTrans { trans_name, names }) => return Err(Box::new(Error::GlobalsWithSameTransName { role: self.name.clone(), trans_name, names })),
                }
            }
        }

        let mut entities_raw = vec![];
        if let Some(entities_xml) = stage.get(&["sprites"]) {
            for entity in iter::once(stage).chain(entities_xml.children.iter().filter(|s| s.name == "sprite")) {
                let name = match entity.attr("name") {
                    None => return Err(Box::new(Error::InvalidProject { error: ProjectError::UnnamedEntity { role } })),
                    Some(x) => match self.entities.define(x.value.clone(), 0f64.into()) {
                        Ok(None) => self.entities.get(&x.value).unwrap().def.ref_at(VarLocation::Global),
                        Ok(Some(prev)) => return Err(Box::new(Error::InvalidProject { error: ProjectError::EntitiesWithSameName { role, name: prev.def.name } })),
                        Err(SymbolError::NameTransformError { name }) => return Err(Box::new(Error::NameTransformError { role: Some(role), entity: Some(name.clone()), name })),
                        Err(SymbolError::ConflictingTrans { trans_name, names }) => return Err(Box::new(Error::EntitiesWithSameTransName { role, trans_name, names })),
                    }
                };
                entities_raw.push((entity, name));
            }
        }

        let blocks = content.get(&["blocks"]).map(|v| v.children.as_slice()).unwrap_or(&[]);
        for block in blocks {
            parse_block_header(block, &mut self.funcs, &self.name, None)?;
        }

        // ----------------------------------------------------------------------------------- //
        // -- we now have all the necessary items defined to parse exprs, stmts, and entity -- //
        // ----------------------------------------------------------------------------------- //

        let funcs = blocks.iter().map(|block| parse_block(block, &self.funcs, &self, None)).collect::<Result<Vec<_>,_>>()?;
        let entities = entities_raw.into_iter().map(|(entity, name)| EntityInfo::new(&self, name).parse(entity)).collect::<Result<Vec<_>,_>>()?;

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
    /// If `true`, the emitted syntax tree will be processed by static optimizations.
    /// Defaults to `false`.
    pub optimize: bool,

    /// If `true`, the parser will skip script blocks that lack a hat block.
    /// This is typically desirable since free floating blocks are never automatically executed,
    /// and thus are typically not needed for translation efforts.
    /// Defaults to `true`.
    pub omit_nonhat_scripts: bool,

    /// If `true`, the emitted syntax tree will be automatically adjusted to support
    /// convenient translation into languages with zero-based indexing.
    /// For instance, with this enabled, an `item X of _` block will emit `X-1` as the index rather than `X`, and similar for other list-based blocks.
    /// Defaults to `false`.
    pub adjust_to_zero_index: bool,

    /// All symbol names in the program will be passed through this function,
    /// allowing easy conversion of Snap! names to, e.g., valid C-like identifiers.
    /// The default operation performs no conversion.
    /// Note that non-default transform strategies may also require a custom [`ParserBuilder::autofill_generator`].
    pub name_transformer: Rc<dyn Fn(&str) -> Result<String, ()>>,

    /// A generator used to produce symbol names for auto-fill closure arguments.
    /// The function receives a number that can be used to differentiate different generated arguments.
    /// It is expected that multiple calls to this function with the same input will produce the same output symbol name.
    /// The default is to produce a string of format `%n` where `n` is the input number.
    /// Note that, after generation, symbol names are still passed through [`ParserBuilder::name_transformer`] as usual.
    pub autofill_generator: Rc<dyn Fn(usize) -> Result<String, ()>>,
}
impl Default for Parser {
    fn default() -> Self {
        Self {
            optimize: false,
            omit_nonhat_scripts: true,
            adjust_to_zero_index: false,
            name_transformer: Rc::new(|v| Ok(v.into())),
            autofill_generator: Rc::new(|v| Ok(format!("%{}", v))),
        }
    }
}
impl Parser {
    fn opt(&self, project: Project) -> Result<Project, Box<Error>> {
        Ok(project)
    }
    pub fn parse(&self, xml: &str) -> Result<Project, Box<Error>> {
        let mut xml = xmlparser::Tokenizer::from(xml);
        while let Some(Ok(e)) = xml.next() {
            if let xmlparser::Token::ElementStart { local, .. } = e {
                let (proj_name, roles) = match local.as_str() {
                    "room" => {
                        let project_xml = parse_xml_root(&mut xml, local.as_str())?;
                        let proj_name = project_xml.attr("name").map(|v| v.value.as_str()).unwrap_or("untitled").to_owned();

                        let mut roles = Vec::with_capacity(project_xml.children.len());
                        for child in project_xml.children.iter() {
                            if child.name == "role" {
                                let role_name = match child.attr("name") {
                                    None => return Err(Box::new(Error::InvalidProject { error: ProjectError::UnnamedRole })),
                                    Some(x) => x.value.clone(),
                                };
                                roles.push(RoleInfo::new(self, role_name).parse(child)?);
                            }
                        }

                        (proj_name, roles)
                    }
                    "role" => {
                        let role_xml = parse_xml_root(&mut xml, local.as_str())?;
                        let proj_name = role_xml.attr("name").map(|v| v.value.as_str()).unwrap_or("untitled").to_owned();

                        let role = RoleInfo::new(self, proj_name.clone()).parse(&role_xml)?;

                        (proj_name, vec![role])
                    }
                    "project" => {
                        let project_xml = parse_xml_root(&mut xml, local.as_str())?;
                        let proj_name = project_xml.attr("name").map(|v| v.value.as_str()).unwrap_or("untitled").to_owned();

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

                let mut project = Some(Project { name: proj_name, roles });
                if self.optimize { project = Some(self.opt(mem::take(&mut project).unwrap())?) }
                return Ok(project.unwrap())
            }
        }
        Err(Box::new(Error::InvalidProject { error: ProjectError::NoRoot }))
    }
}

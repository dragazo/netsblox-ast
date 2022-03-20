use std::io::Read;
use std::convert::TryFrom;
use std::rc::Rc;
use std::mem;
use std::iter;
use std::fmt;

use linked_hash_map::LinkedHashMap;
use derive_builder::Builder;

use xml::reader::{EventReader, XmlEvent};
use xml::name::OwnedName;
use xml::attribute::OwnedAttribute;
use xml::common::Position;

use serde_json::Value as JsonValue;

use regex::Regex;

use crate::rpcs::*;

#[cfg(feature = "serde")]
use serde::Serialize;

lazy_static! {
    static ref NUMBER_REGEX: Regex = Regex::new(r"^-?[0-9]+(\.[0-9]*)?([eE][+-]?[0-9]+)?$").unwrap();
    static ref PARAM_FINDER: Regex = Regex::new(r"%'([^']+)'").unwrap();
    static ref ARG_FINDER: Regex = Regex::new(r"%(\S+)").unwrap();
    static ref NEW_LINE: Regex = Regex::new("\r\n|\r|\n").unwrap();
}

fn clean_newlines(s: &str) -> String {
    NEW_LINE.replace_all(s, "\n").into_owned()
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
fn parse_xml_root<R: Read>(xml: &mut EventReader<R>, root_name: OwnedName, root_attrs: Vec<OwnedAttribute>) -> Result<Xml, Error> {
    let mut text = String::new();
    let mut children = vec![];
    loop {
        match xml.next() {
            Ok(XmlEvent::StartElement { name, attributes, .. }) => {
                children.push(parse_xml_root(xml, name, attributes)?);
            }
            Ok(XmlEvent::EndElement { name }) => {
                assert_eq!(name, root_name);
                let attrs = root_attrs.into_iter().map(|a| XmlAttr {
                    name: a.name.local_name,
                    value: a.value,
                }).collect();
                return Ok(Xml { name: root_name.local_name, attrs, children, text: clean_newlines(&text) });
            }
            Ok(XmlEvent::Characters(s)) | Ok(XmlEvent::CData(s)) => text += &s,
            Ok(XmlEvent::Comment(_)) | Ok(XmlEvent::Whitespace(_)) | Ok(XmlEvent::ProcessingInstruction { .. }) => (),
            Ok(x @ XmlEvent::StartDocument { .. }) | Ok(x @ XmlEvent::EndDocument) => panic!("{:?} at pos {:?}", x, xml.position()),
            Err(error) => return Err(Error::InvalidXml { error }),
        }
    }
}

#[derive(Debug)]
pub enum ProjectError {
    NoRoot,
    UnnamedRole,
    ValueNotEvaluated { role: String, sprite: Option<String> },
    InvalidJson { reason: String },
    NoRoleContent { role: String },
    NoStageDef { role: String },

    CustomBlockWithoutName { role: String, sprite: Option<String> },
    CustomBlockWithoutType { role: String, sprite: Option<String>, sig: String },
    CustomBlockUnknownType { role: String, sprite: Option<String>, sig: String, ty: String },
    CustomBlockWithoutCode { role: String, sprite: Option<String>, sig: String },

    ImageWithoutId { role: String },
    ImagesWithSameId { role: String, id: String },
    ImageWithoutContent { role: String, id: String },
    ImageUnknownFormat { role: String, id: String, content: String },

    SpritesWithSameName { role: String, name: String },

    CostumeIdFmt { role: String, sprite: String, id: String },
    CostumeUndefinedRef { role: String, sprite: String, id: String },
    CostumesWithSameName { role: String, sprite: String, name: String },

    UnnamedGlobal { role: String },
    GlobalNoValue { role: String, name: String },
    GlobalsWithSameName { role: String, name: String },

    UnnamedField { role: String, sprite: String },
    FieldNoValue { role: String, sprite: String, name: String },
    FieldsWithSameName { role: String, sprite: String, name: String },

    ListItemNoValue { role: String, sprite: String },
    BoolNoValue { role: String, sprite: String },
    BoolUnknownValue { role: String, sprite: String, value: String },
    UnnamedSprite { role: String },

    UnknownBlockMetaType { role: String, sprite: String, meta_type: String },
    BlockWithoutType { role: String, sprite: String },
    BlockChildCount { role: String, sprite: String, block_type: String, needed: usize, got: usize },

    BlockMissingOption { role: String, sprite: String, block_type: String },
    BlockOptionUnknown { role: String, sprite: String, block_type: String, got: String },

    InvalidBoolLiteral { role: String, sprite: String },
    NonConstantUpvar { role: String, sprite: String, block_type: String },

    FailedToParseColor { role: String, sprite: String, color: String },

    MessageTypeMissingName { role: String },
    MessageTypeNameEmpty { role: String },
    MessageTypeMissingFields { role: String, msg_type: String },
    MessageTypeFieldEmpty { role: String, msg_type: String },
    MessageTypeMultiplyDefined { role: String, msg_type: String },
}
#[derive(Debug)]
pub enum Error {
    InvalidXml { error: xml::reader::Error },
    InvalidProject { error: ProjectError },
    NameTransformError { name: String, role: Option<String>, sprite: Option<String> },
    UnknownBlockType { role: String, sprite: String, block_type: String },
    DerefAssignment { role: String, sprite: String },
    UndefinedVariable { role: String, sprite: String, name: String },
    UndefinedFn { role: String, sprite: String, name: String },
    BlockOptionNotConst { role: String, sprite: String, block_type: String },
    BlockOptionNotSelected { role: String, sprite: String, block_type: String },
    UnknownEntity { role: String, sprite: String, entity: String },

    UnknownMessageType { role: String, sprite: String, msg_type: String },
    MessageTypeWrongNumberArgs { role: String, sprite: String, msg_type: String, block_type: String, got: usize, expected: usize },

    UnknownService { role: String, sprite: String, block_type: String, service: String },
    UnknownRPC { role: String, sprite: String, block_type: String, service: String, rpc: String },

    GlobalsWithSameTransName { role: String, trans_name: String, names: (String, String) },
    SpritesWithSameTransName { role: String, trans_name: String, names: (String, String) },
    FieldsWithSameTransName { role: String, sprite: String, trans_name: String, names: (String, String) },
    LocalsWithSameTransName { role: String, sprite: String, trans_name: String, names: (String, String) },
    CostumesWithSameTransName { role: String, sprite: String, trans_name: String, names: (String, String) },
    BlocksWithSameTransName { role: String, sprite: Option<String>, trans_name: String, names: (String, String) },

    BlocksWithSameName { role: String, sprite: Option<String>, name: String, sigs: (String, String) },

    // TODO: get rid of these cases when new features are added
    BlockCurrentlyUnsupported { role: String, sprite: String, block_type: String, what: String },
}

#[derive(Debug)]
pub enum SymbolError {
    NameTransformError { name: String },
    ConflictingTrans { trans_name: String, names: (String, String) }
}

#[derive(Clone)]
struct SymbolTable<'a> {
    parser: &'a Parser,
    orig_to_def: LinkedHashMap<String, VariableDef>,
    trans_to_orig: LinkedHashMap<String, String>,
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
    fn define(&mut self, name: String, value: Value) -> Result<Option<VariableDef>, SymbolError> {
        let trans_name = self.transform_name(&name)?;
        if let Some(orig) = self.trans_to_orig.get(&trans_name) {
            let def = self.orig_to_def.get(orig).unwrap();
            return Err(SymbolError::ConflictingTrans { trans_name, names: (def.name.clone(), name) });
        }

        let entry = VariableDef { name: name.clone(), trans_name: trans_name.clone(), value };
        self.trans_to_orig.insert(trans_name, name.clone());
        Ok(self.orig_to_def.insert(name, entry))
    }
    /// Returns the definition of the given variable if it exists.
    fn get(&self, name: &str) -> Option<&VariableDef> {
        self.orig_to_def.get(name)
    }
    /// Gets the list of all defined variables.
    /// This is guaranteed to be in order of definition.
    fn into_defs(self) -> Vec<VariableDef> {
        self.orig_to_def.into_iter().map(|x| x.1).collect()
    }
    fn len(&self) -> usize {
        self.orig_to_def.len()
    }
}
#[test]
fn test_sym_tab() {
    let parser = ParserBuilder::default().name_transformer(Rc::new(crate::util::c_ident)).build().unwrap();
    let mut sym = SymbolTable::new(&parser);
    assert!(sym.orig_to_def.is_empty());
    assert!(sym.trans_to_orig.is_empty());
    assert!(sym.define("hello world!".into(), 0f64.into()).unwrap().is_none());
    assert_eq!(sym.orig_to_def["hello world!"].name, "hello world!");
    assert_eq!(sym.orig_to_def["hello world!"].trans_name, "hello_world");
    assert_eq!(sym.trans_to_orig["hello_world"], "hello world!");
}

#[derive(Debug)]
struct Rpc {
    service: String,
    rpc: String,
    args: Vec<(String, Expr)>,
    comment: Option<String>,
}
#[derive(Debug)]
struct FnCall {
    function: FnRef,
    args: Vec<Expr>,
    comment: Option<String>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Project {
    pub name: String,
    pub roles: Vec<Role>,
}
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Role {
    pub name: String,
    pub notes: String,
    pub stage_size: (usize, usize),
    pub globals: Vec<VariableDef>,
    pub funcs: Vec<Function>,
    pub sprites: Vec<Sprite>,
}
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Function {
    pub name: String,
    pub trans_name: String,
    pub params: Vec<VariableDef>,
    pub returns: bool,
    pub stmts: Vec<Stmt>,
}
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Sprite {
    pub name: String,
    pub trans_name: String,
    pub fields: Vec<VariableDef>,
    pub costumes: Vec<VariableDef>,
    pub funcs: Vec<Function>,
    pub scripts: Vec<Script>,

    pub active_costume: Option<usize>,
    pub visible: bool,
    pub color: (u8, u8, u8),
    pub pos: (f64, f64),
    pub heading: f64,
    pub scale: f64,
}
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct VariableDef {
    pub name: String,
    pub trans_name: String,
    pub value: Value,
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
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct VariableRef {
    pub name: String,
    pub trans_name: String,
    pub location: VarLocation,
}
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct FnRef {
    pub name: String,
    pub trans_name: String,
    pub location: FnLocation,
}
#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum VarLocation {
    Global, Field, Local,
}
#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum FnLocation {
    Global, Method,
}
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Script {
    pub hat: Option<Hat>,
    pub stmts: Vec<Stmt>,
}
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Hat {
    OnFlag { comment: Option<String> },
    OnKey { key: String, comment: Option<String> },
    MouseDown { comment: Option<String> },
    MouseUp { comment: Option<String> },
    MouseEnter { comment: Option<String> },
    MouseLeave { comment: Option<String> },
    ScrollUp { comment: Option<String> },
    ScrollDown { comment: Option<String> },
    Dropped { comment: Option<String> },
    Stopped { comment: Option<String> },
    When { condition: Expr, comment: Option<String> },
    LocalMessage { msg_type: String, comment: Option<String> },
    NetworkMessage { msg_type: String, fields: Vec<VariableRef>, comment: Option<String> },
}
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Stmt {
    /// Assign the given value to each of the listed variables (afterwards, they should all be ref-eq).
    Assign { vars: Vec<VariableRef>, value: Expr, comment: Option<String> },
    AddAssign { var: VariableRef, value: Expr, comment: Option<String> },

    Warp { stmts: Vec<Stmt>, comment: Option<String> },

    InfLoop { stmts: Vec<Stmt>, comment: Option<String> },
    ForeachLoop { var: VariableRef, items: Expr, stmts: Vec<Stmt>, comment: Option<String> },
    ForLoop { var: VariableRef, start: Expr, stop: Expr, stmts: Vec<Stmt>, comment: Option<String> },
    UntilLoop { condition: Expr, stmts: Vec<Stmt>, comment: Option<String> },
    Repeat { times: Expr, stmts: Vec<Stmt>, comment: Option<String> },

    If { condition: Expr, then: Vec<Stmt>, comment: Option<String> },
    IfElse { condition: Expr, then: Vec<Stmt>, otherwise: Vec<Stmt>, comment: Option<String> },

    Push { list: Expr, value: Expr, comment: Option<String> },
    InsertAt { list: Expr, value: Expr, index: Expr, comment: Option<String> },
    InsertAtRand { list: Expr, value: Expr, comment: Option<String> },

    Pop { list: Expr, comment: Option<String> },
    RemoveAt { list: Expr, index: Expr, comment: Option<String> },
    RemoveAll { list: Expr, comment: Option<String> },

    IndexAssign { list: Expr, value: Expr, index: Expr, comment: Option<String> },
    RandIndexAssign { list: Expr, value: Expr, comment: Option<String> },
    LastIndexAssign { list: Expr, value: Expr, comment: Option<String> },

    Return { value: Expr, comment: Option<String> },

    Sleep { seconds: Expr, comment: Option<String> },
    WaitUntil { condition: Expr, comment: Option<String> },

    SwitchCostume { costume: Option<Expr>, comment: Option<String> },

    Forward { distance: Expr, comment: Option<String> },
    ChangePos { dx: Option<Expr>, dy: Option<Expr>, comment: Option<String> },
    SetPos { x: Option<Expr>, y: Option<Expr>, comment: Option<String> },
    /// Similar to `SetPos` except that the target can be either a list of `[x, y]` coordinates or a sprite.
    Goto { target: Expr, comment: Option<String> },

    TurnRight { angle: Expr, comment: Option<String> },
    TurnLeft { angle: Expr, comment: Option<String> },
    SetHeading { value: Expr, comment: Option<String> },

    BounceOffEdge { comment: Option<String> },

    PenDown { comment: Option<String> },
    PenUp { comment: Option<String> },
    PenClear { comment: Option<String> },
    Stamp { comment: Option<String> },
    Write { content: Expr, font_size: Expr, comment: Option<String> },
    SetPenColor { color: (u8, u8, u8), comment: Option<String> },

    Say { content: Expr, duration: Option<Expr>, comment: Option<String> },
    Think { content: Expr, duration: Option<Expr>, comment: Option<String> },

    SetVisible { value: bool, comment: Option<String> },
    ChangeScalePercent { amount: Expr, comment: Option<String> },
    SetScalePercent { value: Expr, comment: Option<String> },

    ChangePenSize { amount: Expr, comment: Option<String> },
    SetPenSize { value: Expr, comment: Option<String> },

    RunRpc { service: String, rpc: String, args: Vec<(String, Expr)>, comment: Option<String> },
    RunFn { function: FnRef, args: Vec<Expr>, comment: Option<String> },

    /// Sends a message to local entities (not over the network).
    /// If `target` is `None`, this should broadcast to all entities.
    /// Otherwise `target` is either a single target or a list of targets to send to.
    /// The `wait` flag determines if the broadcast should be blocking (wait for receivers to terminate).
    SendLocalMessage { target: Option<Expr>, msg_type: Expr, wait: bool, comment: Option<String> },
    SendNetworkMessage { target: Expr, msg_type: String, values: Vec<(String, Expr)>, comment: Option<String> },
}

impl From<Rpc> for Stmt {
    fn from(rpc: Rpc) -> Stmt {
        let Rpc { service, rpc, args, comment } = rpc;
        Stmt::RunRpc { service, rpc, args, comment }
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Value {
    Bool(bool),
    Number(f64),
    String(String),
    List(Vec<Value>),
    Constant(Constant),
}

impl From<f64> for Value { fn from(v: f64) -> Value { Value::Number(v) } }
impl From<&str> for Value { fn from(v: &str) -> Value { Value::String(v.into()) } }
impl From<bool> for Value { fn from(v: bool) -> Value { Value::Bool(v) } }
impl From<String> for Value { fn from(v: String) -> Value { Value::String(v) } }
impl From<Constant> for Value { fn from(v: Constant) -> Value { Value::Constant(v) } }
impl From<Vec<Value>> for Value { fn from(v: Vec<Value>) -> Value { Value::List(v) } }

impl TryFrom<JsonValue> for Value {
    type Error = Error;
    fn try_from(val: JsonValue) -> Result<Value, Self::Error> {
        Ok(match val {
            JsonValue::String(v) => Value::String(v),
            JsonValue::Bool(v) => Value::Bool(v),
            JsonValue::Array(vals) => {
                let mut res = Vec::with_capacity(vals.len());
                for val in vals { res.push(Value::try_from(val)?) }
                Value::List(res)
            }
            JsonValue::Number(v) => match v.as_f64() {
                Some(v) => Value::Number(v),
                None => return Err(Error::InvalidProject { error: ProjectError::InvalidJson { reason: format!("failed to convert {} to f64", v) } }),
            }
            JsonValue::Object(_) => return Err(Error::InvalidProject { error: ProjectError::InvalidJson { reason: format!("got object: {}", val) } }),
            JsonValue::Null => return Err(Error::InvalidProject { error: ProjectError::InvalidJson { reason: "got null".into() } }),
        })
    }
}
#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Constant {
    E, Pi,
}
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Expr {
    Value(Value),
    Variable { var: VariableRef, comment: Option<String> },

    Add { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
    Sub { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
    Mul { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
    Div { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
    /// Mathematical modulus (not remainder!). For instance, `-1 mod 7 == 6`.
    Mod { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },

    Pow { base: Box<Expr>, power: Box<Expr>, comment: Option<String> },
    Log { value: Box<Expr>, base: Box<Expr>, comment: Option<String> },

    /// Short-circuiting logical `or`.
    And { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
    /// Short-circuiting logical `and`.
    Or { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
    /// Lazily-evaluated conditional expression. Returns `then` if `condition` is true, otherwise `otherwise`.
    Conditional { condition: Box<Expr>, then: Box<Expr>, otherwise: Box<Expr>, comment: Option<String> },

    /// If both values are lists, returns true of they are references to the same list.
    /// If both values are non-lists, returns true if the values are equal.
    /// Otherwise returns `false`.
    Identical { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
    Eq { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
    Less { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
    Greater { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },

    /// Get a random number between `a` and `b` (inclusive).
    /// There are no ordering guarantees (swapping `a` and `b` is equivalent).
    /// If both values are integers, the result is an integer, otherwise continuous floats are returned.
    RandInclusive { a: Box<Expr>, b: Box<Expr>, comment: Option<String> },
    /// Get a list of all the numbers starting at `start` and stepping towards `stop` (by `+1` or `-1`), but not going past `stop`.
    RangeInclusive { start: Box<Expr>, stop: Box<Expr>, comment: Option<String> },

    MakeList { values: Vec<Expr>, comment: Option<String> },
    Listcat { lists: Vec<Expr>, comment: Option<String> },
    Listlen { value: Box<Expr>, comment: Option<String> },
    /// Given a list, returns a new (shallow copy) of all the items except the first.
    /// If the list is empty, an empty list is returned.
    ListAllButFirst { value: Box<Expr>, comment: Option<String> },
    /// Returns the (1-based) index of value in the list, or 0 if not present.
    ListFind { list: Box<Expr>, value: Box<Expr>, comment: Option<String> },

    ListIndex { list: Box<Expr>, index: Box<Expr>, comment: Option<String> },
    ListRandIndex { list: Box<Expr>, comment: Option<String> },
    ListLastIndex { list: Box<Expr>, comment: Option<String> },

    Strcat { values: Vec<Expr>, comment: Option<String> },
    /// String length in terms of unicode code points (not bytes or grapheme clusters!).
    Strlen { value: Box<Expr>, comment: Option<String> },

    /// Convert a unicode code point into a 1-character string.
    UnicodeToChar { value: Box<Expr>, comment: Option<String> },
    /// Convert a 1-character string into its unicode code point.
    CharToUnicode { value: Box<Expr>, comment: Option<String> },

    Not { value: Box<Expr>, comment: Option<String> },
    Neg { value: Box<Expr>, comment: Option<String> },
    Abs { value: Box<Expr>, comment: Option<String> },
    Sqrt { value: Box<Expr>, comment: Option<String> },

    Floor { value: Box<Expr>, comment: Option<String> },
    Ceil { value: Box<Expr>, comment: Option<String> },
    Round { value: Box<Expr>, comment: Option<String> },

    Sin { value: Box<Expr>, comment: Option<String> },
    Cos { value: Box<Expr>, comment: Option<String> },
    Tan { value: Box<Expr>, comment: Option<String> },

    Asin { value: Box<Expr>, comment: Option<String> },
    Acos { value: Box<Expr>, comment: Option<String> },
    Atan { value: Box<Expr>, comment: Option<String> },

    CallRpc { service: String, rpc: String, args: Vec<(String, Expr)>, comment: Option<String> },
    CallFn { function: FnRef, args: Vec<Expr>, comment: Option<String> },

    StageWidth { comment: Option<String> },
    StageHeight { comment: Option<String> },

    MouseX { comment: Option<String> },
    MouseY { comment: Option<String> },

    Latitude { comment: Option<String> },
    Longitude { comment: Option<String> },

    YPos { comment: Option<String> },
    XPos { comment: Option<String> },
    Heading { comment: Option<String> },

    PenDown { comment: Option<String> },

    Scale { comment: Option<String> },
    IsVisible { comment: Option<String> },

    This { comment: Option<String> },
    Entity { name: String, trans_name: String, comment: Option<String> },

    ImageOfEntity { entity: Box<Expr>, comment: Option<String> },
    ImageOfDrawings { comment: Option<String> },

    IsTouchingEntity { entity: Box<Expr>, comment: Option<String> },
    IsTouchingMouse { comment: Option<String> },
    IsTouchingEdge { comment: Option<String> },
    IsTouchingDrawings { comment: Option<String> },

    RpcError { comment: Option<String> },
}
impl<T: Into<Value>> From<T> for Expr { fn from(v: T) -> Expr { Expr::Value(v.into()) } }

impl From<Rpc> for Expr {
    fn from(rpc: Rpc) -> Expr {
        let Rpc { service, rpc, args, comment } = rpc;
        Expr::CallRpc { service, rpc, args, comment }
    }
}

macro_rules! define_local_and_ref {
    ($self:ident, $name:expr, $value:expr) => {{
        let name = $name;
        match $self.locals.define(name.clone(), $value) {
            Ok(_) => (), // redefining locals is fine
            Err(SymbolError::NameTransformError { name }) => return Err(Error::NameTransformError { name, role: Some($self.role.name.clone()), sprite: Some($self.sprite.name.clone()) }),
            Err(SymbolError::ConflictingTrans { trans_name, names }) => return Err(Error::LocalsWithSameTransName { role: $self.role.name.clone(), sprite: $self.sprite.name.clone(), trans_name, names }),
        }
        $self.locals.get(&name).unwrap().ref_at(VarLocation::Local)
    }}
}
macro_rules! check_children_get_comment {
    ($self:ident, $expr:ident, $s:expr => $req:expr) => {{
        let s = $s;
        let req = $req;
        #[allow(unused_comparisons)]
        if $expr.children.len() < req {
            return Err(Error::InvalidProject { error: ProjectError::BlockChildCount { role: $self.role.name.clone(), sprite: $self.sprite.name.clone(), block_type: s.into(), needed: req, got: $expr.children.len() } });
        }
        match $expr.children.get(req) {
            Some(comment) => if comment.name == "comment" { Some(clean_newlines(&comment.text)) } else { None },
            None => None,
        }
    }}
}
macro_rules! binary_op {
    ($self:ident, $expr:ident, $s:expr => $res:path $({ $($field:ident : $value:expr),*$(,)? })? : $left:ident, $right:ident) => {{
        let comment = check_children_get_comment!($self, $expr, $s => 2);
        let $left = $self.parse_expr(&$expr.children[0])?.into();
        let $right = $self.parse_expr(&$expr.children[1])?.into();
        $res { $left, $right, comment, $( $($field : $value),* )? }
    }};
    ($self:ident, $expr:ident, $s:expr => $res:path $({ $($field:ident : $value:expr),*$(,)? })?) => {
        binary_op! { $self, $expr, $s => $res $({ $($field : $value),* })? : left, right }
    }
}
macro_rules! unary_op {
    ($self:ident, $expr:ident, $s:expr => $res:path $({ $($field:ident : $value:expr),*$(,)? })? : $val:ident) => {{
        let comment = check_children_get_comment!($self, $expr, $s => 1);
        let $val = $self.parse_expr(&$expr.children[0])?.into();
        $res { $val, comment, $( $($field : $value),* )? }
    }};
    ($self:ident, $expr:ident, $s:expr => $res:path $({ $($field:ident : $value:expr),*$(,)? })? ) => {
        unary_op! { $self, $expr, $s => $res $({ $($field : $value),* })? : value }
    }
}
macro_rules! noarg_op {
    ($self:ident, $expr:ident, $s:expr => $res:path $({ $($field:ident : $value:expr),*$(,)? })?) => {{
        let comment = check_children_get_comment!($self, $expr, $s => 0);
        $res { comment, $( $($field : $value),* )? }
    }}
}
macro_rules! variadic_op {
    ($self:ident, $expr:ident, $s:expr => $res:path $({ $($field:ident : $value:expr),*$(,)? })? : $val:ident) => {{
        let comment = check_children_get_comment!($self, $expr, $s => 1);
        let mut $val = vec![];
        for item in $expr.children[0].children.iter() {
            $val.push($self.parse_expr(item)?);
        }
        $res { $val, comment, $( $($field : $value),* )? }
    }};
    ($self:ident, $expr:ident, $s:expr => $res:path $({ $($field:ident : $value:expr),*$(,)? })?) => {
        variadic_op! { $self, $expr, $s => $res $({ $($field : $value),* })? : values }
    }
}
macro_rules! grab_option {
    ($self:ident, $s:ident, $child:expr) => {{
        let res = match $child.get(&["option"]) {
            None => return Err(Error::InvalidProject { error: ProjectError::BlockMissingOption { role: $self.role.name.clone(), sprite: $self.sprite.name.clone(), block_type: $s.into() } }),
            Some(f) => {
                if f.children.len() != 0 { return Err(Error::BlockOptionNotConst { role: $self.role.name.clone(), sprite: $self.sprite.name.clone(), block_type: $s.into() }) }
                f.text.as_str()
            }
        };
        if res == "" { return Err(Error::BlockOptionNotSelected { role: $self.role.name.clone(), sprite: $self.sprite.name.clone(), block_type: $s.into() }) }
        res
    }}
}
macro_rules! grab_entity {
    ($self:ident, $s:ident, $child:expr, $comment:ident) => {
        match $child.text.as_str() {
            "" => match $child.children.is_empty() {
                true => return Err(Error::BlockOptionNotSelected { role: $self.role.name.clone(), sprite: $self.sprite.name.clone(), block_type: $s.into() }),
                false => $self.parse_expr($child)?,
            },
            "myself" => Expr::This { comment: $comment },
            name => match $self.role.sprites.get(name) {
                None => return Err(Error::UnknownEntity { role: $self.role.name.clone(), sprite: $self.sprite.name.clone(), entity: name.into() }),
                Some(entity) => Expr::Entity { name: entity.name.clone(), trans_name: entity.trans_name.clone(), comment: $comment },
            }
        }
    }
}

fn parse_color(value: &str) -> Option<(u8, u8, u8)> {
    let rgb: Vec<_> = value.split(',').take(3).map(|v| v.parse::<f64>().ok()).flatten().collect();
    if rgb.len() == 3 && rgb.iter().all(|&v| (0.0..256.0).contains(&v)) {
        Some((rgb[0] as u8, rgb[1] as u8, rgb[2] as u8))
    } else {
        None
    }
}

struct ScriptInfo<'a, 'b, 'c> {
    parser: &'a Parser,
    role: &'c RoleInfo<'a>,
    sprite: &'c SpriteInfo<'a, 'b>,
    locals: SymbolTable<'a>,
}
impl<'a, 'b, 'c> ScriptInfo<'a, 'b, 'c> {
    fn new(sprite: &'c SpriteInfo<'a, 'b>) -> Self {
        Self {
            parser: sprite.parser,
            role: sprite.role,
            sprite,
            locals: SymbolTable::new(sprite.parser)
        }
    }
    fn parse(&mut self, script: &Xml) -> Result<Script, Error> {
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
                    let FnCall { function, args, comment } = self.parse_fn_call(stmt)?;
                    stmts.push(Stmt::RunFn { function, args, comment });
                }
                x => return Err(Error::InvalidProject { error: ProjectError::UnknownBlockMetaType { role: self.role.name.clone(), sprite: self.sprite.name.clone(), meta_type: x.to_owned() } }),
            }
        }
        Ok(Script { hat, stmts })
    }
    fn parse_hat(&mut self, stmt: &Xml) -> Result<Option<Hat>, Error> {
        let s = match stmt.attr("s") {
            None => return Err(Error::InvalidProject { error: ProjectError::BlockWithoutType { role: self.role.name.clone(), sprite: self.sprite.name.clone() } }),
            Some(v) => v.value.as_str(),
        };
        Ok(Some(match s {
            "receiveGo" => {
                let comment = check_children_get_comment!(self, stmt, s => 0);
                Hat::OnFlag { comment }
            }
            "receiveCondition" => {
                let comment = check_children_get_comment!(self, stmt, s => 1);
                let condition = self.parse_expr(&stmt.children[0])?;
                Hat::When { condition, comment }
            }
            "receiveKey" => {
                let comment = check_children_get_comment!(self, stmt, s => 1);
                let key = grab_option!(self, s, stmt.children[0]);
                Hat::OnKey { key: key.into(), comment }
            }
            "receiveInteraction" => {
                let comment = check_children_get_comment!(self, stmt, s => 1);
                match grab_option!(self, s, stmt.children[0]) {
                    "pressed" => Hat::MouseDown { comment },
                    "clicked" => Hat::MouseUp { comment },
                    "mouse-entered" => Hat::MouseEnter { comment },
                    "mouse-departed" => Hat::MouseLeave { comment },
                    "scrolled-up" => Hat::ScrollUp { comment },
                    "scrolled-down" => Hat::ScrollDown { comment },
                    "dropped" => Hat::Dropped { comment },
                    "stopped" => Hat::Stopped { comment },
                    x => return Err(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into(), got: x.into() } }),
                }
            }
            "receiveMessage" => {
                let comment = check_children_get_comment!(self, stmt, s => 1);
                let child = &stmt.children[0];
                if child.name != "l" { return Err(Error::BlockOptionNotConst { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into() }) }
                let msg_type = match child.text.as_str() {
                    "" => return Err(Error::BlockOptionNotSelected { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into() }),
                    x => x.to_owned(),
                };
                Hat::LocalMessage { msg_type, comment }
            }
            "receiveSocketMessage" => {
                if stmt.children.is_empty() { return Err(Error::InvalidProject { error: ProjectError::BlockChildCount { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into(), needed: 1, got: 0 } }) }
                if stmt.children[0].name != "l" { return Err(Error::BlockOptionNotConst { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into() }) }

                let msg_type = match stmt.children[0].text.as_str() {
                    "" => return Err(Error::BlockOptionNotSelected { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into() }),
                    x => x.to_owned(),
                };

                let mut fields = vec![];
                let mut comment = None;
                for child in stmt.children[1..].iter() {
                    if child.name == "comment" {
                        comment = Some(clean_newlines(&child.text));
                    }
                    if child.name != "l" { break }
                    let var = define_local_and_ref!(self, child.text.clone(), 0f64.into());
                    fields.push(var);
                }
                Hat::NetworkMessage { msg_type, fields, comment }
            }
            x if x.starts_with("receive") => return Err(Error::UnknownBlockType { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: x.into() }),
            _ => return Ok(None),
        }))
    }
    fn parse_rpc(&self, stmt: &Xml, block_type: &str) -> Result<Rpc, Error> {
        if stmt.children.len() < 2 { return Err(Error::InvalidProject { error: ProjectError::BlockChildCount { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: block_type.into(), needed: 2, got: stmt.children.len() } }) }
        for i in 0..=1 { if stmt.children[i].name != "l" { return Err(Error::BlockOptionNotConst { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: block_type.into() }) } }
        for i in 0..=1 { if stmt.children[i].name.is_empty() { return Err(Error::BlockOptionNotSelected { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: block_type.into() }) } }

        let service = stmt.children[0].text.clone();
        let rpc = stmt.children[1].text.clone();

        let arg_names = match SERVICE_INFO.get(service.as_str()) {
            None => return Err(Error::UnknownService { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: block_type.into(), service }),
            Some(x) => match x.get(rpc.as_str()) {
                None => return Err(Error::UnknownRPC { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: block_type.into(), service, rpc }),
                Some(&x) => x,
            }
        };

        let comment = check_children_get_comment!(self, stmt, block_type => 2 + arg_names.len());
        let mut args = Vec::with_capacity(arg_names.len());
        for (&arg_name, child) in arg_names.iter().zip(&stmt.children[2 .. 2 + arg_names.len()]) {
            let val = self.parse_expr(child)?;
            args.push((arg_name.to_owned(), val));
        }
        Ok(Rpc { service, rpc, args, comment })
    }
    fn parse_fn_call(&self, stmt: &Xml) -> Result<FnCall, Error> {
        let s = match stmt.attr("s") {
            Some(v) => v.value.as_str(),
            None => return Err(Error::InvalidProject { error: ProjectError::CustomBlockWithoutName { role: self.role.name.clone(), sprite: Some(self.sprite.name.clone()) } }),
        };

        let name = block_name_from_ref(s);
        let argc = ARG_FINDER.find_iter(s).count();
        let function = self.reference_fn(&name)?;
        let comment = check_children_get_comment!(self, stmt, s => argc);

        let mut args = Vec::with_capacity(argc);
        for expr in stmt.children[..argc].iter() {
            args.push(self.parse_expr(expr)?);
        }

        Ok(FnCall { function, args, comment })
    }
    fn parse_block(&mut self, stmt: &Xml) -> Result<Stmt, Error> {
        let s = match stmt.attr("s") {
            None => return Err(Error::InvalidProject { error: ProjectError::BlockWithoutType { role: self.role.name.clone(), sprite: self.sprite.name.clone() } }),
            Some(v) => v.value.as_str(),
        };
        Ok(match s {
            "doDeclareVariables" => {
                let comment = check_children_get_comment!(self, stmt, s => 1);
                let mut vars = vec![];
                for var in stmt.children[0].children.iter() {
                    vars.push(define_local_and_ref!(self, var.text.clone(), 0f64.into()));
                }
                Stmt::Assign { vars, value: 0f64.into(), comment }
            }
            "doSetVar" | "doChangeVar" => {
                let comment = check_children_get_comment!(self, stmt, s => 2);
                let var = match stmt.children[0].name.as_str() {
                    "l" => self.reference_var(&stmt.children[0].text)?,
                    _ => return Err(Error::DerefAssignment { role: self.role.name.clone(), sprite: self.sprite.name.clone() }),
                };
                let value = self.parse_expr(&stmt.children[1])?;
                match s {
                    "doSetVar" => Stmt::Assign { vars: vec![var], value, comment },
                    "doChangeVar" => Stmt::AddAssign { var, value, comment },
                    _ => unreachable!(),
                }
            }
            "doFor" => {
                let comment = check_children_get_comment!(self, stmt, s => 4);

                let var = match stmt.children[0].name.as_str() {
                    "l" => stmt.children[0].text.as_str(),
                    _ => return Err(Error::InvalidProject { error: ProjectError::NonConstantUpvar { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into() } }),
                };
                let start = self.parse_expr(&stmt.children[1])?;
                let stop = self.parse_expr(&stmt.children[2])?;
                let var = define_local_and_ref!(self, var.to_owned(), 0f64.into()); // define after bounds, but before loop body
                let stmts = self.parse(&stmt.children[3])?.stmts;

                Stmt::ForLoop { var, start, stop, stmts, comment }
            }
            "doForEach" => {
                let comment = check_children_get_comment!(self, stmt, s => 3);

                let var = match stmt.children[0].name.as_str() {
                    "l" => stmt.children[0].text.as_str(),
                    _ => return Err(Error::InvalidProject { error: ProjectError::NonConstantUpvar { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into() } }),
                };
                let items = self.parse_expr(&stmt.children[1])?;
                let var = define_local_and_ref!(self, var.to_owned(), 0f64.into()); // define after bounds, but before loop body
                let stmts = self.parse(&stmt.children[2])?.stmts;

                Stmt::ForeachLoop { var, items, stmts, comment }
            }
            "doRepeat" | "doUntil" | "doIf" => {
                let comment = check_children_get_comment!(self, stmt, s => 2);
                let expr = self.parse_expr(&stmt.children[0])?;
                let stmts = self.parse(&stmt.children[1])?.stmts;
                match s {
                    "doRepeat" => Stmt::Repeat { times: expr, stmts, comment },
                    "doUntil" => Stmt::UntilLoop { condition: expr, stmts, comment },
                    "doIf" => Stmt::If { condition: expr, then: stmts, comment },
                    _ => unreachable!(),
                }
            }
            "doForever" => {
                let comment = check_children_get_comment!(self, stmt, s => 1);
                let stmts = self.parse(&stmt.children[0])?.stmts;
                Stmt::InfLoop { stmts, comment }
            }
            "doIfElse" => {
                let comment = check_children_get_comment!(self, stmt, s => 3);
                let condition = self.parse_expr(&stmt.children[0])?;
                let then = self.parse(&stmt.children[1])?.stmts;
                let otherwise = self.parse(&stmt.children[2])?.stmts;
                Stmt::IfElse { condition, then, otherwise, comment }
            }
            "doWarp" => {
                let comment = check_children_get_comment!(self, stmt, s => 1);
                let stmts = self.parse(&stmt.children[0])?.stmts;
                Stmt::Warp { stmts, comment }
            }
            "doDeleteFromList" => {
                let comment = check_children_get_comment!(self, stmt, s => 2);
                let list = self.parse_expr(&stmt.children[1])?;
                match stmt.children[0].get(&["option"]) {
                    Some(opt) => match opt.text.as_str() {
                        "last" => Stmt::Pop { list, comment },
                        "all" => Stmt::RemoveAll { list, comment },
                        "" => return Err(Error::BlockOptionNotSelected { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into() }),
                        x => return Err(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into(), got: x.into() } }),
                    }
                    None => {
                        let index = self.parse_expr(&stmt.children[0])?;
                        Stmt::RemoveAt { list, index, comment }
                    }
                }
            }
            "doInsertInList" => {
                let comment = check_children_get_comment!(self, stmt, s => 3);
                let value = self.parse_expr(&stmt.children[0])?;
                let list = self.parse_expr(&stmt.children[2])?;
                match stmt.children[1].get(&["option"]) {
                    Some(opt) => match opt.text.as_str() {
                        "last" => Stmt::Push { list, value, comment },
                        "random" | "any" => Stmt::InsertAtRand { list, value, comment },
                        "" => return Err(Error::BlockOptionNotSelected { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into() }),
                        x => return Err(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into(), got: x.into() } }),
                    }
                    None => {
                        let index = self.parse_expr(&stmt.children[0])?;
                        Stmt::InsertAt { list, value, index, comment }
                    }
                }
            }
            "doReplaceInList" => {
                let comment = check_children_get_comment!(self, stmt, s => 3);
                let value = self.parse_expr(&stmt.children[2])?;
                let list = self.parse_expr(&stmt.children[1])?;
                match stmt.children[0].get(&["option"]) {
                    Some(opt) => match opt.text.as_str() {
                        "last" => Stmt::LastIndexAssign { list, value, comment },
                        "random" | "any" => Stmt::RandIndexAssign { list, value, comment },
                        "" => return Err(Error::BlockOptionNotSelected { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into() }),
                        x => return Err(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into(), got: x.into() } }),
                    }
                    None => {
                        let index = self.parse_expr(&stmt.children[0])?;
                        Stmt::IndexAssign { list, value, index, comment }
                    }
                }
            }
            "doSwitchToCostume" => {
                let comment = check_children_get_comment!(self, stmt, s => 1);

                let costume = {
                    let val = &stmt.children[0];
                    if val.name == "l" && val.children.is_empty() && val.text.is_empty() {
                        None
                    }
                    else if val.name == "l" && val.get(&["option"]).is_some() {
                        let opt = grab_option!(self, s, val);
                        match opt {
                            "Turtle" => None,
                            x => return Err(Error::BlockCurrentlyUnsupported { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into(), what: format!("{} with project costume ({}) currently not supported", s, x) }),
                        }
                    }
                    else {
                        Some(self.parse_expr(val)?)
                    }
                };

                Stmt::SwitchCostume { costume, comment }
            }
            "setHeading" => {
                let comment = check_children_get_comment!(self, stmt, s => 1);

                let child = &stmt.children[0];
                let value = if child.name == "l" && child.get(&["option"]).is_some() {
                    let opt = grab_option!(self, s, child);
                    match opt {
                        "random" => Expr::RandInclusive { a: Box::new(0f64.into()), b: Box::new(360f64.into()), comment: None },
                        _ => return Err(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into(), got: opt.into() } }),
                    }
                } else { self.parse_expr(child)? };

                Stmt::SetHeading { value, comment }
            }
            "doGotoObject" => {
                let comment = check_children_get_comment!(self, stmt, s => 1);

                let child = &stmt.children[0];
                if child.name == "l" && child.get(&["option"]).is_some() {
                    let opt = grab_option!(self, s, child);
                    match opt {
                        "random position" => {
                            let half_width = Expr::Div { left: Box::new(Expr::StageWidth { comment: None }), right: Box::new(2f64.into()), comment: None };
                            let half_height = Expr::Div { left: Box::new(Expr::StageHeight { comment: None }), right: Box::new(2f64.into()), comment: None };
                            Stmt::SetPos {
                                x: Some(Expr::RandInclusive { a: Box::new(Expr::Neg { value: Box::new(half_width.clone()), comment: None }), b: Box::new(half_width), comment: None }),
                                y: Some(Expr::RandInclusive { a: Box::new(Expr::Neg { value: Box::new(half_height.clone()), comment: None }), b: Box::new(half_height), comment: None }),
                                comment
                            }
                        }
                        "mouse-pointer" => Stmt::SetPos { x: Some(Expr::MouseX { comment: None }), y: Some(Expr::MouseY { comment: None }), comment },
                        "center" => Stmt::SetPos { x: Some(0f64.into()), y: Some(0f64.into()), comment },
                        _ => return Err(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into(), got: opt.into() } }),
                    }
                }
                else {
                    Stmt::Goto { target: self.parse_expr(child)?, comment }
                }
            }
            "doBroadcast" => {
                let comment = check_children_get_comment!(self, stmt, s => 1);
                let msg_type = self.parse_expr(&stmt.children[0])?;
                Stmt::SendLocalMessage { target: None, msg_type, wait: false, comment }
            }
            "setColor" => {
                let comment = check_children_get_comment!(self, stmt, s => 1);
                let color = match stmt.get(&["color"]) {
                    Some(color) => match parse_color(&color.text) {
                        Some(color) => color,
                        None => return Err(Error::InvalidProject { error: ProjectError::FailedToParseColor { role: self.role.name.clone(), sprite: self.sprite.name.clone(), color: color.text.clone() } }),
                    }
                    None => return Err(Error::BlockOptionNotConst { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into() }),
                };
                Stmt::SetPenColor { color, comment }
            }
            "write" => {
                let comment = check_children_get_comment!(self, stmt, s => 2);
                let content = self.parse_expr(&stmt.children[0])?;
                let font_size = self.parse_expr(&stmt.children[1])?;
                Stmt::Write { content, font_size, comment }
            }
            "doSocketMessage" => {
                let msg_type = match stmt.children.get(0) {
                    Some(value) if value.name != "comment" => value.text.as_str(),
                    _ => return Err(Error::InvalidProject { error: ProjectError::BlockMissingOption { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into() } }),
                };
                let fields = match self.role.msg_types.get(msg_type) {
                    None => return Err(Error::UnknownMessageType { role: self.role.name.clone(), sprite: self.sprite.name.clone(), msg_type: msg_type.into() }),
                    Some(x) => x,
                };

                let (argc, comment) = stmt.children.iter().enumerate().find(|(_, x)| x.name == "comment").map(|(i, x)| (i, Some(x.text.as_str()))).unwrap_or((stmt.children.len(), None));
                assert!(argc >= 1); // due to msg_type from above

                let values = stmt.children[1..argc - 1].iter().map(|x| self.parse_expr(x)).collect::<Result<Vec<_>,_>>()?;
                if fields.len() != values.len() {
                    return Err(Error::MessageTypeWrongNumberArgs { role: self.role.name.clone(), sprite: self.sprite.name.clone(), msg_type: msg_type.into(), block_type: s.into(), got: values.len(), expected: fields.len() });
                }

                let target_xml = &stmt.children[argc - 1];
                let target = match target_xml.get(&["option"]) {
                    Some(x) => match x.text.as_str() {
                        "" => return Err(Error::BlockOptionNotSelected { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into() }),
                        x => x.into(),
                    }
                    None => self.parse_expr(target_xml)?,
                };

                Stmt::SendNetworkMessage { target, msg_type: msg_type.into(), values: fields.iter().map(|&x| x.to_owned()).zip(values).collect(), comment: comment.map(|x| x.to_owned()) }
            }
            "changeScale" => unary_op!(self, stmt, s => Stmt::ChangeScalePercent : amount),
            "setScale" => unary_op!(self, stmt, s => Stmt::SetScalePercent),
            "doSayFor" => binary_op!(self, stmt, s => Stmt::Say : content, duration),
            "doThinkFor" => binary_op!(self, stmt, s => Stmt::Think : content, duration),
            "bubble" => unary_op!(self, stmt, s => Stmt::Say { duration: None } : content),
            "doThink" => unary_op!(self, stmt, s => Stmt::Think { duration: None } : content),
            "hide" => noarg_op!(self, stmt, s => Stmt::SetVisible { value: false }),
            "show" => noarg_op!(self, stmt, s => Stmt::SetVisible { value: true }),
            "doWaitUntil" => unary_op!(self, stmt, s => Stmt::WaitUntil : condition),
            "changeSize" => unary_op!(self, stmt, s => Stmt::ChangePenSize : amount),
            "setSize" => unary_op!(self, stmt, s => Stmt::SetPenSize),
            "doAddToList" => binary_op!(self, stmt, s => Stmt::Push : value, list),
            "doReport" => unary_op!(self, stmt, s => Stmt::Return),
            "doStamp" => noarg_op!(self, stmt, s => Stmt::Stamp),
            "doWait" => unary_op!(self, stmt, s => Stmt::Sleep : seconds),
            "forward" => unary_op!(self, stmt, s => Stmt::Forward : distance),
            "turn" => unary_op!(self, stmt, s => Stmt::TurnRight : angle),
            "turnLeft" => unary_op!(self, stmt, s => Stmt::TurnLeft : angle),
            "setXPosition" => unary_op!(self, stmt, s => Stmt::SetPos { y: None } : x),
            "setYPosition" => unary_op!(self, stmt, s => Stmt::SetPos { x: None } : y),
            "changeXPosition" => unary_op!(self, stmt, s => Stmt::ChangePos { dy: None } : dx),
            "changeYPosition" => unary_op!(self, stmt, s => Stmt::ChangePos { dx: None } : dy),
            "gotoXY" => binary_op!(self, stmt, s => Stmt::SetPos : x, y),
            "bounceOffEdge" => noarg_op!(self, stmt, s => Stmt::BounceOffEdge),
            "down" => noarg_op!(self, stmt, s => Stmt::PenDown),
            "up" => noarg_op!(self, stmt, s => Stmt::PenUp),
            "clear" => noarg_op!(self, stmt, s => Stmt::PenClear),
            "doRunRPC" => self.parse_rpc(stmt, s)?.into(),
            _ => return Err(Error::UnknownBlockType { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.to_owned() }),
        })
    }
    fn reference_var(&self, name: &str) -> Result<VariableRef, Error> {
        let locs = [(&self.locals, VarLocation::Local), (&self.sprite.fields, VarLocation::Field), (&self.role.globals, VarLocation::Global)];
        match locs.iter().find_map(|v| v.0.get(name).map(|x| x.ref_at(v.1))) {
            Some(v) => Ok(v),
            None => Err(Error::UndefinedVariable { role: self.role.name.clone(), sprite: self.sprite.name.clone(), name: name.into() })
        }
    }
    fn reference_fn(&self, name: &str) -> Result<FnRef, Error> {
        let locs = [(&self.sprite.funcs, FnLocation::Method), (&self.role.funcs, FnLocation::Global)];
        match locs.iter().find_map(|v| v.0.get(name).map(|x| x.fn_ref_at(v.1))) {
            Some(v) => Ok(v),
            None => Err(Error::UndefinedFn { role: self.role.name.clone(), sprite: self.sprite.name.clone(), name: name.into() })
        }
    }
    fn cnd_adjust_index(&self, index: Expr, condition: bool, delta: f64) -> Expr {
        match condition {
            true => Expr::Add { left: index.into(), right: Box::new(delta.into()), comment: None },
            false => index,
        }
    }
    fn parse_expr(&self, expr: &Xml) -> Result<Expr, Error> {
        let parse_bool = |val: &str| -> Result<Expr, Error> {
            match val {
                "true" => Ok(true.into()),
                "false" => Ok(false.into()),
                _ => Err(Error::InvalidProject { error: ProjectError::BoolUnknownValue { role: self.role.name.clone(), sprite: self.sprite.name.clone(), value: val.into() } })
            }
        };
        match expr.name.as_str() {
            "l" => match expr.children.first() {
                Some(child) if child.name == "bool" => parse_bool(&child.text),
                _ => match expr.text.parse::<f64>() {
                    Ok(v) => Ok(v.into()),
                    Err(_) => Ok(expr.text.clone().into()),
                }
            }
            "bool" => parse_bool(&expr.text),
            "list" => match expr.attr("struct") {
                Some(v) if v.value == "atomic" => match serde_json::from_str::<JsonValue>(&format!("[{}]", expr.text)) {
                    Err(_) => return Err(Error::InvalidProject { error: ProjectError::InvalidJson { reason: format!("content was not json: [{}]", expr.text) } }),
                    Ok(json) => Ok(Value::try_from(json)?.into()),
                }
                _ => {
                    let mut values = Vec::with_capacity(expr.children.len());
                    for item in expr.children.iter() {
                        match item.children.get(0) {
                            None => return Err(Error::InvalidProject { error: ProjectError::ListItemNoValue { role: self.role.name.clone(), sprite: self.sprite.name.clone() } }),
                            Some(x) => match self.parse_expr(x)? {
                                Expr::Value(v) => values.push(v),
                                _ => return Err(Error::InvalidProject { error: ProjectError::ValueNotEvaluated { role: self.role.name.clone(), sprite: Some(self.sprite.name.clone()) } }),
                            }
                        }
                    }
                    Ok(values.into())
                }
            }
            "custom-block" => {
                let FnCall { function, args, comment } = self.parse_fn_call(expr)?;
                Ok(Expr::CallFn { function, args, comment })
            }
            "block" => {
                if let Some(var) = expr.attr("var") {
                    let comment = check_children_get_comment!(self, expr, "var" => 0);
                    let var = self.reference_var(&var.value)?;
                    return Ok(Expr::Variable { var, comment });
                }
                let s = match expr.attr("s") {
                    None => return Err(Error::InvalidProject { error: ProjectError::BlockWithoutType { role: self.role.name.clone(), sprite: self.sprite.name.clone() } }),
                    Some(v) => v.value.as_str(),
                };
                Ok(match s {
                    "reportSum" => binary_op!(self, expr, s => Expr::Add),
                    "reportDifference" => binary_op!(self, expr, s => Expr::Sub),
                    "reportProduct" => binary_op!(self, expr, s => Expr::Mul),
                    "reportQuotient" => binary_op!(self, expr, s => Expr::Div),
                    "reportModulus" => binary_op!(self, expr, s => Expr::Mod),
                    "reportPower" => binary_op!(self, expr, s => Expr::Pow : base, power),

                    "reportAnd" => binary_op!(self, expr, s => Expr::And),
                    "reportOr" => binary_op!(self, expr, s => Expr::Or),

                    "reportIsIdentical" => binary_op!(self, expr, s => Expr::Identical),
                    "reportEquals" => binary_op!(self, expr, s => Expr::Eq),
                    "reportLessThan" => binary_op!(self, expr, s => Expr::Less),
                    "reportGreaterThan" => binary_op!(self, expr, s => Expr::Greater),

                    "reportRandom" => binary_op!(self, expr, s => Expr::RandInclusive : a, b),
                    "reportNumbers" => binary_op!(self, expr, s => Expr::RangeInclusive : start, stop),

                    "reportNot" => unary_op!(self, expr, s => Expr::Not),
                    "reportRound" => unary_op!(self, expr, s => Expr::Round),

                    "reportListLength" => unary_op!(self, expr, s => Expr::Listlen),
                    "reportListIsEmpty" => {
                        let comment = check_children_get_comment!(self, expr, s => 1);
                        let value = self.parse_expr(&expr.children[0])?.into();
                        Expr::Greater { left: Box::new(Expr::Listlen { value, comment: None }), right: Box::new(0.0f64.into()), comment }
                    }

                    "reportListIndex" => self.cnd_adjust_index(binary_op!(self, expr, s => Expr::ListFind : value, list), self.parser.adjust_to_zero_index, 1.0),
                    "reportListContainsItem" => {
                        let comment = check_children_get_comment!(self, expr, s => 2);
                        let value = self.parse_expr(&expr.children[0])?.into();
                        let list = self.parse_expr(&expr.children[1])?.into();
                        Expr::Greater { left: Box::new(Expr::ListFind { value, list, comment: None }), right: Box::new(if self.parser.adjust_to_zero_index { -1.0 } else { 0.0 }.into()), comment }
                    }
                    "reportListItem" => {
                        let comment = check_children_get_comment!(self, expr, s => 2);
                        let list = self.parse_expr(&expr.children[1])?.into();
                        match expr.children[0].get(&["option"]) {
                            Some(opt) => match opt.text.as_str() {
                                "last" => Expr::ListLastIndex { list, comment },
                                "any" => Expr::ListRandIndex { list, comment },
                                "" => return Err(Error::BlockOptionNotSelected { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into() }),
                                x => return Err(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into(), got: x.into() } }),
                            }
                            None => {
                                let index = self.cnd_adjust_index(self.parse_expr(&expr.children[0])?, self.parser.adjust_to_zero_index, -1.0).into();
                                Expr::ListIndex { list, index, comment }
                            }
                        }
                    }

                    "reportStringSize" => unary_op!(self, expr, s => Expr::Strlen),
                    "reportUnicodeAsLetter" => unary_op!(self, expr, s => Expr::UnicodeToChar),
                    "reportUnicode" => unary_op!(self, expr, s => Expr::CharToUnicode),

                    "reportCDR" => unary_op!(self, expr, s => Expr::ListAllButFirst),
                    "reportCONS" => {
                        let comment = check_children_get_comment!(self, expr, s => 2);
                        let val = self.parse_expr(&expr.children[0])?;
                        let list = self.parse_expr(&expr.children[0])?;
                        Expr::Listcat { lists: vec![val, list], comment}
                    }

                    "reportJoinWords" => variadic_op!(self, expr, s => Expr::Strcat),
                    "reportConcatenatedLists" => variadic_op!(self, expr, s => Expr::Listcat : lists),
                    "reportNewList" => variadic_op!(self, expr, s => Expr::MakeList),

                    "reportBoolean" => match expr.get(&["l", "bool"]) {
                        Some(v) if v.text == "true" => true.into(),
                        Some(v) if v.text == "false" => false.into(),
                        _ => return Err(Error::InvalidProject { error: ProjectError::InvalidBoolLiteral { role: self.role.name.clone(), sprite: self.sprite.name.clone() } }),
                    }
                    "reportMonadic" => {
                        let comment = check_children_get_comment!(self, expr, s => 2);
                        let func = grab_option!(self, s, expr.children[0]);
                        let value = Box::new(self.parse_expr(&expr.children[1])?);
                        match func {
                            "id" => *value,

                            "neg" => Expr::Neg { value, comment },
                            "abs" => Expr::Abs { value, comment },
                            "sqrt" => Expr::Sqrt { value, comment },
                            "floor" => Expr::Floor { value, comment },
                            "ceiling" => Expr::Ceil { value, comment },

                            "sin" => Expr::Sin { value, comment },
                            "cos" => Expr::Cos { value, comment },
                            "tan" => Expr::Tan { value, comment },

                            "asin" => Expr::Asin { value, comment },
                            "acos" => Expr::Acos { value, comment },
                            "atan" => Expr::Atan { value, comment },

                            "ln" => Expr::Log { value, base: Box::new(Constant::E.into()), comment },
                            "lg" => Expr::Log { value, base: Box::new(2f64.into()), comment },
                            "log" => Expr::Log { value, base: Box::new(10f64.into()), comment },

                            "e^" => Expr::Pow { base: Box::new(Constant::E.into()), power: value, comment },
                            "2^" => Expr::Pow { base: Box::new(2f64.into()), power: value, comment },
                            "10^" => Expr::Pow { base: Box::new(10f64.into()), power: value, comment },

                            _ => return Err(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into(), got: func.into() } }),
                        }
                    }
                    "reportIfElse" => {
                        let comment = check_children_get_comment!(self, expr, s => 3);
                        let condition = Box::new(self.parse_expr(&expr.children[0])?);
                        let then = Box::new(self.parse_expr(&expr.children[1])?);
                        let otherwise = Box::new(self.parse_expr(&expr.children[2])?);
                        Expr::Conditional { condition, then, otherwise, comment }
                    }
                    "getJSFromRPCStruct" => self.parse_rpc(expr, s)?.into(),

                    "reportStageWidth" => noarg_op!(self, expr, s => Expr::StageWidth),
                    "reportStageHeight" => noarg_op!(self, expr, s => Expr::StageHeight),

                    "reportMouseX" => noarg_op!(self, expr, s => Expr::MouseX),
                    "reportMouseY" => noarg_op!(self, expr, s => Expr::MouseY),

                    "reportLatitude" => noarg_op!(self, expr, s => Expr::Latitude),
                    "reportLongitude" => noarg_op!(self, expr, s => Expr::Longitude),

                    "reportPenTrailsAsCostume" => noarg_op!(self, expr, s => Expr::ImageOfDrawings),
                    "reportImageOfObject" => {
                        let comment = check_children_get_comment!(self, expr, s => 1);
                        let entity = grab_entity!(self, s, &expr.children[0], None).into();
                        Expr::ImageOfEntity { entity, comment }
                    }
                    "reportTouchingObject" => {
                        let comment = check_children_get_comment!(self, expr, s => 1);
                        let child = &expr.children[0];
                        if child.name == "l" && child.get(&["option"]).is_some() {
                            match grab_option!(self, s, child) {
                                "mouse-pointer" => Expr::IsTouchingMouse { comment },
                                "pen trails" => Expr::IsTouchingDrawings { comment },
                                "edge" => Expr::IsTouchingEdge { comment },
                                x => return Err(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into(), got: x.into() } }),
                            }
                        }
                        else {
                            let entity = grab_entity!(self, s, child, None).into();
                            Expr::IsTouchingEntity { entity, comment }
                        }
                    }

                    "reportRPCError" => noarg_op!(self, expr, s => Expr::RpcError),

                    "getScale" => noarg_op!(self, expr, s => Expr::Scale),
                    "reportShown" => noarg_op!(self, expr, s => Expr::IsVisible),

                    "xPosition" => noarg_op!(self, expr, s => Expr::XPos),
                    "yPosition" => noarg_op!(self, expr, s => Expr::YPos),
                    "direction" => noarg_op!(self, expr, s => Expr::Heading),

                    "getPenDown" => noarg_op!(self, expr, s => Expr::PenDown),

                    _ => return Err(Error::UnknownBlockType { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.to_owned() }),
                })
            }
            x => Err(Error::UnknownBlockType { role: self.role.name.clone(), sprite: self.sprite.name.clone(), block_type: x.into() }),
        }
    }
}

struct SpriteInfo<'a, 'b> {
    parser: &'a Parser,
    role: &'b RoleInfo<'a>,
    name: String,
    trans_name: String,
    fields: SymbolTable<'a>,
    funcs: SymbolTable<'a>,
    costumes: SymbolTable<'a>,
}
impl<'a, 'b> SpriteInfo<'a, 'b> {
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
    fn parse(mut self, sprite: &'a Xml) -> Result<Sprite, Error> {
        for costume in sprite.get(&["costumes", "list"]).map(|c| c.children.as_slice()).unwrap_or(&[]) {
            if let Some(ident) = costume.get(&["ref"]).map(|r| r.attr("mediaID")).flatten() {
                let ident = ident.value.as_str();
                if !ident.starts_with(&self.name) || !ident[self.name.len()..].starts_with("_cst_") {
                    return Err(Error::InvalidProject { error: ProjectError::CostumeIdFmt { role: self.role.name.clone(), sprite: self.name, id: ident.into() } });
                }
                let name = &ident[self.name.len() + 5..];

                let content = match self.role.images.get(ident) {
                    Some(&x) => x,
                    None => return Err(Error::InvalidProject { error: ProjectError::CostumeUndefinedRef { role: self.role.name.clone(), sprite: self.name, id: ident.into() } }),
                };

                match self.costumes.define(name.into(), content.into()) {
                    Ok(None) => (),
                    Ok(Some(prev)) => return Err(Error::InvalidProject { error: ProjectError::CostumesWithSameName { role: self.role.name.clone(), sprite: self.name, name: prev.name } }),
                    Err(SymbolError::NameTransformError { name }) => return Err(Error::NameTransformError { name, role: Some(self.role.name.clone()), sprite: Some(self.name) }),
                    Err(SymbolError::ConflictingTrans { trans_name, names }) => return Err(Error::CostumesWithSameTransName { role: self.role.name.clone(), sprite: self.name, trans_name, names }),
                }
            }
        }

        let blocks = sprite.get(&["blocks"]).map(|v| v.children.as_slice()).unwrap_or(&[]);
        for block in blocks {
            parse_block_header(block, &mut self.funcs, &self.role.name, Some(&self.name))?;
        }
        let mut funcs = vec![];
        for block in blocks {
            funcs.push(parse_block(block, &self.funcs, self.role, Some(&self))?);
        }

        let active_costume = match sprite.attr("costume").map(|v| v.value.parse::<usize>().ok()).flatten() {
            Some(idx) if idx >= 1 && idx <= self.costumes.len() => Some(idx - 1),
            _ => None,
        };
        let color = sprite.attr("color").map(|v| parse_color(&v.value)).flatten().unwrap_or((0, 0, 0));
        let visible = !sprite.attr("hidden").and_then(|s| s.value.parse::<bool>().ok()).unwrap_or(false);

        let float_attr = |attr: &str| sprite.attr(attr).map(|v| v.value.parse::<f64>().ok().filter(|v| v.is_finite())).flatten();
        let pos = (float_attr("x").unwrap_or(0.0), float_attr("y").unwrap_or(0.0));
        let heading = float_attr("heading").unwrap_or(0.0);
        let scale = float_attr("scale").unwrap_or(0.0);

        if let Some(fields) = sprite.get(&["variables"]) {
            let dummy_script = ScriptInfo::new(&self);

            let mut defs = vec![];
            for def in fields.children.iter().filter(|v| v.name == "variable") {
                let name = match def.attr("name") {
                    None => return Err(Error::InvalidProject { error: ProjectError::UnnamedField { role: self.role.name.clone(), sprite: self.name } }),
                    Some(x) => x.value.clone(),
                };
                let value = match def.children.get(0) {
                    None => return Err(Error::InvalidProject { error: ProjectError::FieldNoValue { role: self.role.name.clone(), sprite: self.name, name } }),
                    Some(x) => match dummy_script.parse_expr(x)? {
                        Expr::Value(v) => v,
                        _ => return Err(Error::InvalidProject { error: ProjectError::ValueNotEvaluated { role: self.role.name.clone(), sprite: Some(self.name) } }),
                    }
                };
                defs.push((name, value));
            }

            for (name, value) in defs {
                match self.fields.define(name.clone(), value) {
                    Ok(None) => (),
                    Ok(Some(prev)) => return Err(Error::InvalidProject { error: ProjectError::FieldsWithSameName { role: self.role.name.clone(), sprite: self.name.clone(), name: prev.name } }),
                    Err(SymbolError::NameTransformError { name }) => return Err(Error::NameTransformError { name, role: Some(self.role.name.clone()), sprite: Some(self.name.clone()) }),
                    Err(SymbolError::ConflictingTrans { trans_name, names }) => return Err(Error::FieldsWithSameTransName { role: self.role.name.clone(), sprite: self.name.clone(), trans_name, names }),
                }
            }
        }

        let mut scripts = vec![];
        if let Some(scripts_xml) = sprite.get(&["scripts"]) {
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

        Ok(Sprite {
            name: self.name,
            trans_name: self.trans_name,
            fields: self.fields.into_defs(),
            costumes: self.costumes.into_defs(),
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
        Value::List(vals) => {
            assert_eq!(vals.len(), 2);
            let s = match &vals[0] { Value::String(v) => v, _ => panic!() };
            let returns = match vals[1] { Value::Bool(v) => v, _ => panic!() };
            (s, returns)
        }
        _ => panic!(), // header parser would never do this
    }
}
fn block_name_from_def(s: &str) -> String {
    PARAM_FINDER.replace_all(s, "\t").into_owned() // tabs leave a marker for args which disappears after ident renaming
}
fn block_name_from_ref(s: &str) -> String {
    ARG_FINDER.replace_all(s, "\t").into_owned()
}
fn parse_block_header<'a>(block: &'a Xml, funcs: &mut SymbolTable<'a>, role: &str, sprite: Option<&str>) -> Result<(), Error> {
    let sprite = || sprite.map(|v| v.to_owned());

    let s = match block.attr("s") {
        Some(v) => v.value.as_str(),
        None => return Err(Error::InvalidProject { error: ProjectError::CustomBlockWithoutName { role: role.into(), sprite: sprite() } }),
    };
    let returns = match block.attr("type") {
        Some(v) => match v.value.as_str() {
            "command" => false,
            "reporter" | "predicate" => true,
            x => return Err(Error::InvalidProject { error: ProjectError::CustomBlockUnknownType { role: role.into(), sprite: sprite(), sig: s.into(), ty: x.into() } }),
        }
        None => return Err(Error::InvalidProject { error: ProjectError::CustomBlockWithoutType { role: role.into(), sprite: sprite(), sig: s.into() } }),
    };

    let name = block_name_from_def(s);
    match funcs.define(name, vec![Value::from(s), Value::from(returns)].into()) {
        Ok(None) => Ok(()),
        Ok(Some(prev)) => Err(Error::BlocksWithSameName { role: role.into(), sprite: sprite(), name: prev.name, sigs: (get_block_info(&prev.value).0.into(), s.into()) }),
        Err(SymbolError::NameTransformError { name }) => Err(Error::NameTransformError { name, role: Some(role.into()), sprite: sprite() }),
        Err(SymbolError::ConflictingTrans { trans_name, names }) => Err(Error::BlocksWithSameTransName { role: role.into(), sprite: sprite(), trans_name, names }),
    }
}
fn parse_block<'a>(block: &'a Xml, funcs: &SymbolTable<'a>, role: &RoleInfo, sprite: Option<&SpriteInfo>) -> Result<Function, Error> {
    let s = block.attr("s").unwrap().value.as_str(); // unwrap ok because we assume parse_block_header() was called before
    let entry = funcs.get(&block_name_from_def(s)).unwrap();
    let (s2, returns) = get_block_info(&entry.value); // unwrap ok because header parser
    assert_eq!(s, s2);

    let code = match block.get(&["script"]) {
        Some(v) => v,
        None => return Err(Error::InvalidProject { error: ProjectError::CustomBlockWithoutCode { role: role.name.clone(), sprite: sprite.map(|v| v.name.clone()), sig: s.into() } }),
    };
    let finalize = |sprite_info: &SpriteInfo| {
        let mut script_info = ScriptInfo::new(sprite_info);
        for param in PARAM_FINDER.captures_iter(s).map(|v| v.get(1).unwrap().as_str().to_owned()) {
            define_local_and_ref!(script_info, param, 0f64.into());
        }
        let params = script_info.locals.clone().into_defs();
        let stmts = script_info.parse(code)?.stmts;

        Ok(Function {
            name: entry.name.clone(),
            trans_name: entry.trans_name.clone(),
            params,
            returns,
            stmts,
        })
    };
    match sprite {
        Some(v) => finalize(v),
        None => {
            let sprite = SpriteInfo::new(role, VariableRef { name: "global".into(), trans_name: "global".into(), location: VarLocation::Global });
            finalize(&sprite)
        }
    }
}

struct RoleInfo<'a> {
    parser: &'a Parser,
    name: String,
    globals: SymbolTable<'a>,
    sprites: SymbolTable<'a>,
    funcs: SymbolTable<'a>,
    images: LinkedHashMap<&'a str, &'a str>,
    msg_types: LinkedHashMap<&'a str, Vec<&'a str>>,
}
impl<'a> RoleInfo<'a> {
    fn new(parser: &'a Parser, name: String) -> Self {
        Self {
            parser,
            name,
            globals: SymbolTable::new(parser),
            sprites: SymbolTable::new(parser),
            funcs: SymbolTable::new(parser),
            images: Default::default(),
            msg_types: Default::default(),
        }
    }
    fn parse(mut self, role_root: &'a Xml) -> Result<Role, Error> {
        assert_eq!(role_root.name, "role");
        let role = match role_root.attr("name") {
            None => return Err(Error::InvalidProject { error: ProjectError::UnnamedRole }),
            Some(x) => x.value.clone(),
        };
        let content = match role_root.get(&["project"]) {
            None => return Err(Error::InvalidProject { error: ProjectError::NoRoleContent { role } }),
            Some(x) => x,
        };
        let notes = content.get(&["notes"]).map(|v| v.text.as_str()).unwrap_or("").to_owned();
        let stage = match content.get(&["stage"]) {
            None => return Err(Error::InvalidProject { error: ProjectError::NoStageDef { role } }),
            Some(x) => x,
        };
        let stage_width = stage.attr("width").and_then(|x| x.value.parse::<usize>().ok()).unwrap_or(480);
        let stage_height = stage.attr("height").and_then(|x| x.value.parse::<usize>().ok()).unwrap_or(360);

        let msg_types = stage.get(&["messageTypes"]).map(|x| x.children.as_slice()).unwrap_or(&[]);
        for msg_type in msg_types {
            let name = match msg_type.get(&["name"]) {
                None => return Err(Error::InvalidProject { error: ProjectError::MessageTypeMissingName { role } }),
                Some(x) => match x.text.as_str() {
                    "" => return Err(Error::InvalidProject { error: ProjectError::MessageTypeNameEmpty { role } }),
                    x => x,
                }
            };
            let fields = match msg_type.get(&["fields"]) {
                None => return Err(Error::InvalidProject { error: ProjectError::MessageTypeMissingFields { role, msg_type: name.into() } }),
                Some(x) => {
                    let mut res = vec![];
                    for field in x.children.iter() {
                        if field.name != "field" { continue }
                        res.push(match field.text.as_str() {
                            "" => return Err(Error::InvalidProject { error: ProjectError::MessageTypeFieldEmpty { role, msg_type: name.into() } }),
                            x => x,
                        });
                    }
                    res
                }
            };

            if self.msg_types.insert(name, fields).is_some() {
                return Err(Error::InvalidProject { error: ProjectError::MessageTypeMultiplyDefined { role, msg_type: name.into() } });
            }
        }

        for entry in role_root.get(&["media"]).map(|v| v.children.as_slice()).unwrap_or(&[]) {
            if entry.name != "costume" { continue }
            let id = match entry.attr("mediaID") {
                Some(x) => x.value.as_str(),
                None => return Err(Error::InvalidProject { error: ProjectError::ImageWithoutId { role } }),
            };

            let content = match entry.attr("image") {
                Some(x) => match x.value.as_str() {
                    x if x.starts_with("data:image/png;base64,") => &x[22..],
                    x => return Err(Error::InvalidProject { error: ProjectError::ImageUnknownFormat { role, id: id.into(), content: x.into() } }),
                }
                None => return Err(Error::InvalidProject { error: ProjectError::ImageWithoutContent { role, id: id.into() } }),
            };

            if self.images.insert(id, content).is_some() {
                return Err(Error::InvalidProject { error: ProjectError::ImagesWithSameId { role, id: id.into() } });
            }
        }

        if let Some(globals) = content.get(&["variables"]) {
            let dummy_name = VariableRef { name: "global".into(), trans_name: "global".into(), location: VarLocation::Global };
            let dummy_sprite = SpriteInfo::new(&self, dummy_name); // fine to do before sprites/blocks/etc. since globals are just values (not stmts or exprs)
            let dummy_script = ScriptInfo::new(&dummy_sprite);

            let mut defs = vec![];
            for def in globals.children.iter().filter(|v| v.name == "variable") {
                let name = match def.attr("name") {
                    None => return Err(Error::InvalidProject { error: ProjectError::UnnamedGlobal { role } }),
                    Some(x) => x.value.clone(),
                };
                let value = match def.children.get(0) {
                    None => return Err(Error::InvalidProject { error: ProjectError::GlobalNoValue { role, name } }),
                    Some(x) => match dummy_script.parse_expr(x)? {
                        Expr::Value(v) => v,
                        _ => return Err(Error::InvalidProject { error: ProjectError::ValueNotEvaluated { role, sprite: None } }),
                    }
                };
                defs.push((name, value));
            }

            for (name, value) in defs {
                match self.globals.define(name.clone(), value) {
                    Ok(None) => (),
                    Ok(Some(prev)) => return Err(Error::InvalidProject { error: ProjectError::GlobalsWithSameName { role: self.name.clone(), name: prev.name } }),
                    Err(SymbolError::NameTransformError { name }) => return Err(Error::NameTransformError { name, role: Some(self.name.clone()), sprite: None }),
                    Err(SymbolError::ConflictingTrans { trans_name, names }) => return Err(Error::GlobalsWithSameTransName { role: self.name.clone(), trans_name, names }),
                }
            }
        }

        let mut sprites_raw = vec![];
        if let Some(sprites_xml) = stage.get(&["sprites"]) {
            for sprite in iter::once(stage).chain(sprites_xml.children.iter().filter(|s| s.name == "sprite")) {
                let name = match sprite.attr("name") {
                    None => return Err(Error::InvalidProject { error: ProjectError::UnnamedSprite { role } }),
                    Some(x) => match self.sprites.define(x.value.clone(), 0f64.into()) {
                        Ok(None) => self.sprites.get(&x.value).unwrap().ref_at(VarLocation::Global),
                        Ok(Some(prev)) => return Err(Error::InvalidProject { error: ProjectError::SpritesWithSameName { role, name: prev.name } }),
                        Err(SymbolError::NameTransformError { name }) => return Err(Error::NameTransformError { role: Some(role), sprite: Some(name.clone()), name }),
                        Err(SymbolError::ConflictingTrans { trans_name, names }) => return Err(Error::SpritesWithSameTransName { role, trans_name, names }),
                    }
                };
                sprites_raw.push((sprite, name));
            }
        }

        let blocks = content.get(&["blocks"]).map(|v| v.children.as_slice()).unwrap_or(&[]);
        for block in blocks {
            parse_block_header(block, &mut self.funcs, &self.name, None)?;
        }

        // ------------------------------------------------------------------------------------ //
        // -- we now have all the necessary items defined to parse exprs, stmts, and sprites -- //
        // ------------------------------------------------------------------------------------ //

        let funcs = blocks.iter().map(|block| parse_block(block, &self.funcs, &self, None)).collect::<Result<Vec<_>,_>>()?;
        let sprites = sprites_raw.into_iter().map(|(sprite, name)| SpriteInfo::new(&self, name).parse(sprite)).collect::<Result<Vec<_>,_>>()?;

        Ok(Role {
            name: role,
            notes,
            stage_size: (stage_width, stage_height),
            globals: self.globals.into_defs(),
            funcs,
            sprites,
        })
    }
}

#[derive(Builder)]
pub struct Parser {
    /// If `true`, the emitted syntax tree will be processed by static optimizations.
    /// Defaults to `false`.
    #[builder(default = "false")]
    optimize: bool,

    /// If `true`, the parser will skip script blocks that lack a hat block.
    /// This is typically desirable since free floating blocks are never automatically executed,
    /// and thus are typically not needed for translation efforts.
    /// Defaults to `true`.
    #[builder(default = "true")]
    omit_nonhat_scripts: bool,

    /// If `true`, the emitted syntax tree will be automatically adjusted to support
    /// convenient translation into languages with zero-based indexing.
    /// For instance, with this enabled, an `item X of _` block will emit `X-1` as the index rather than `X`, and similar for other list-based blocks.
    /// Defaults to `false`.
    #[builder(default = "false")]
    adjust_to_zero_index: bool,

    /// All symbol names in the program will be passed through this function,
    /// allowing easy conversion of Snap! names to, e.g., valid C-like identifiers.
    /// The default operation performs no conversion.
    #[builder(default = "Rc::new(|v| Ok(v.into()))")]
    name_transformer: Rc<dyn Fn(&str) -> Result<String, ()>>,
}
impl Parser {
    fn opt(&self, project: Project) -> Result<Project, Error> {
        Ok(project)
    }
    pub fn parse<R: Read>(&self, xml: R) -> Result<Project, Error> {
        let mut xml = EventReader::new(xml);
        while let Ok(e) = xml.next() {
            if let XmlEvent::StartElement { name, attributes, .. } = e {
                if name.local_name != "room" { continue }
                let project = parse_xml_root(&mut xml, name, attributes)?;
                let proj_name = project.attr("name").map(|v| v.value.as_str()).unwrap_or("untitled").to_owned();

                let mut roles = Vec::with_capacity(project.children.len());
                for child in project.children.iter() {
                    if child.name == "role" {
                        let role_name = match child.attr("name") {
                            None => return Err(Error::InvalidProject { error: ProjectError::UnnamedRole }),
                            Some(x) => x.value.clone(),
                        };
                        roles.push(RoleInfo::new(self, role_name).parse(child)?);
                    }
                }

                let mut project = Some(Project { name: proj_name, roles });
                if self.optimize { project = Some(self.opt(mem::take(&mut project).unwrap())?) }
                return Ok(project.unwrap())
            }
        }
        Err(Error::InvalidProject { error: ProjectError::NoRoot })
    }
}

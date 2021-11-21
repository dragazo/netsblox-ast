use std::io::Read;
use std::iter;

use linked_hash_map::LinkedHashMap;

use xml::reader::{EventReader, XmlEvent};
use xml::name::OwnedName;
use xml::attribute::OwnedAttribute;
use xml::common::Position;

use regex::Regex;

#[cfg(feature = "serde")]
use serde::{Serialize, Deserialize};

lazy_static! {
    static ref NUMBER_REGEX: Regex = Regex::new(r"^-?[0-9]+(\.[0-9]*)?([eE][+-]?[0-9]+)?$").unwrap();
    static ref PARAM_FINDER: Regex = Regex::new(r"%'([^']+)'").unwrap();
    static ref NEW_LINE: Regex = Regex::new("\r\n|\r|\n").unwrap();
}

fn clean_newlines(s: &str) -> String {
    NEW_LINE.replace_all(s, "\n").into_owned()
}

#[derive(Debug, Clone)]
struct XmlAttr {
    name: String,
    value: String,
}
#[derive(Debug, Clone)]
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
    NoRoleContent { role: String },
    NoStageDef { role: String },

    UnnamedGlobal { role: String },
    GlobalNoValue { role: String, name: String },
    GlobalNotEvaluated { role: String, name: String },
    GlobalsWithSameName { role: String, name: String },

    UnnamedField { role: String, sprite: String },
    FieldNoValue { role: String, sprite: String, name: String },
    FieldNotEvaluated { role: String, sprite: String, name: String },
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
}
#[derive(Debug)]
pub enum Error {
    InvalidXml { error: xml::reader::Error },
    InvalidProject { error: ProjectError },
    UnknownBlockType { role: String, sprite: String, block_type: String },
    DerefAssignment { role: String, sprite: String },
    UndefinedVariable { role: String, sprite: String, name: String },
    BlockOptionNotConst { role: String, sprite: String, block_type: String },
    BlockOptionNotSelected { role: String, sprite: String, block_type: String },
}

#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Project {
    pub name: String,
    pub roles: Vec<Role>,
}
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Role {
    pub name: String,
    pub notes: String,
    pub globals: Vec<VariableDef>,
    pub sprites: Vec<Sprite>,
}
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Sprite {
    pub name: String,
    pub fields: Vec<VariableDef>,
    pub scripts: Vec<Script>,
}
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct VariableDef {
    pub name: String,
    pub value: Expr,
}
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct VariableRef {
    pub name: String,
    pub location: VarLocation,
}
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum VarLocation {
    Global, Field, Local,
}
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Script {
    pub stmts: Vec<Stmt>,
}
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Stmt {
    Assign { vars: Vec<VariableRef>, value: Expr, comment: Option<String> },
    AddAssign { var: VariableRef, value: Expr, comment: Option<String> },
}
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Value {
    Literal(String),
    Bool(bool),
    List(Vec<Expr>),
}
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Expr {
    Value(Value),
    Variable { var: VariableRef, comment: Option<String> },

    Add { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
    Sub { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
    Mul { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
    Div { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
    Mod { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
    Pow { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },

    And { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
    Or { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },

    RefEq { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
    Eq { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
    Less { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
    Greater { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },

    RandInt { lower: Box<Expr>, upper: Box<Expr>, comment: Option<String> },

    Not { value: Box<Expr>, comment: Option<String> },
    Round { value: Box<Expr>, comment: Option<String> },

    Strlen { value: Box<Expr>, comment: Option<String> },
    UnicodeToChar { value: Box<Expr>, comment: Option<String> },
    CharToUnicode { value: Box<Expr>, comment: Option<String> },

    Neg { value: Box<Expr>, comment: Option<String> },
    Abs { value: Box<Expr>, comment: Option<String> },
    Sqrt { value: Box<Expr>, comment: Option<String> },
    Floor { value: Box<Expr>, comment: Option<String> },
    Ceil { value: Box<Expr>, comment: Option<String> },
    
    Sin { value: Box<Expr>, comment: Option<String> },
    Cos { value: Box<Expr>, comment: Option<String> },
    Tan { value: Box<Expr>, comment: Option<String> },
    
    Asin { value: Box<Expr>, comment: Option<String> },
    Acos { value: Box<Expr>, comment: Option<String> },
    Atan { value: Box<Expr>, comment: Option<String> },

    LogE { value: Box<Expr>, comment: Option<String> },
    Log2 { value: Box<Expr>, comment: Option<String> },
    Log10 { value: Box<Expr>, comment: Option<String> },
    
    ExpE { value: Box<Expr>, comment: Option<String> },
    Exp2 { value: Box<Expr>, comment: Option<String> },
    Exp10 { value: Box<Expr>, comment: Option<String> },
}
impl Expr {
    fn is_evaluated(&self) -> bool {
        match self {
            Expr::Value(value) => match value {
                Value::Literal(_) => true,
                Value::Bool(_) => true,
                Value::List(values) => values.iter().all(Expr::is_evaluated),
            }
            _ => false,
        }
    }
}

macro_rules! check_children_get_comment {
    ($self:ident, $root:ident, $s:expr => $req:literal) => {{
        let s = $s;
        #[allow(unused_comparisons)]
        if $root.children.len() < $req {
            return Err(Error::InvalidProject { error: ProjectError::BlockChildCount { role: $self.sprite.role.name.clone(), sprite: $self.sprite.name.clone(), block_type: s.into(), needed: $req, got: $root.children.len() } });
        }
        match $root.children.get($req) {
            Some(comment) => if comment.name == "comment" { Some(clean_newlines(&comment.text)) } else { None },
            None => None,
        }
    }}
}
struct ScriptInfo<'a> {
    sprite: &'a SpriteInfo<'a>,
    locals: LinkedHashMap<String, VariableDef>,
}
impl<'a> ScriptInfo<'a> {
    fn new(sprite: &'a SpriteInfo) -> Self {
        Self { sprite, locals: Default::default() }
    }
    fn parse(mut self, script: &Xml) -> Result<Script, Error> {
        let mut stmts = vec![];
        for stmt_xml in script.children.iter() {
            match stmt_xml.name.as_str() {
                "block" => stmts.push(self.parse_block(stmt_xml)?),
                x => return Err(Error::InvalidProject { error: ProjectError::UnknownBlockMetaType { role: self.sprite.role.name.clone(), sprite: self.sprite.name.clone(), meta_type: x.to_owned() } }),
            }
        }

        Ok(Script { stmts })
    }
    fn parse_block(&mut self, stmt_xml: &Xml) -> Result<Stmt, Error> {
        let s = match stmt_xml.attr("s") {
            None => return Err(Error::InvalidProject { error: ProjectError::BlockWithoutType { role: self.sprite.role.name.clone(), sprite: self.sprite.name.clone() } }),
            Some(v) => v.value.as_str(),
        };
        Ok(match s {
            "doDeclareVariables" => {
                let comment = check_children_get_comment!(self, stmt_xml, s => 1);
                let mut vars = vec![];
                for var in stmt_xml.children[0].children.iter() {
                    let name = var.text.clone();
                    let var = VariableDef { name: name.clone(), value: Expr::Value(Value::Literal("0".into())) };
                    vars.push(VariableRef { name: name.clone(), location: VarLocation::Local });
                    self.locals.insert(name, var);
                }
                Stmt::Assign { vars, value: Expr::Value(Value::Literal("0".into())), comment }
            }
            "doSetVar" | "doChangeVar" => {
                let comment = check_children_get_comment!(self, stmt_xml, s => 2);
                let var = match stmt_xml.children[0].name.as_str() {
                    "l" => self.reference_var(stmt_xml.children[0].text.clone())?,
                    _ => return Err(Error::DerefAssignment { role: self.sprite.role.name.clone(), sprite: self.sprite.name.clone() }),
                };
                let value = self.parse_expr(&stmt_xml.children[1])?;

                if s == "doSetVar" { Stmt::Assign { vars: vec![var], value, comment } }
                else { Stmt::AddAssign { var, value, comment } }
            }
            _ => return Err(Error::UnknownBlockType { role: self.sprite.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.to_owned() }),
        })
    }
    fn reference_var(&self, name: String) -> Result<VariableRef, Error> {
        let locations = [(&self.locals, VarLocation::Local), (&self.sprite.fields, VarLocation::Field), (&self.sprite.role.globals, VarLocation::Global)];
        for (sym_table, location) in locations {
            if sym_table.contains_key(&name) {
                return Ok(VariableRef { name, location })
            }
        }
        Err(Error::UndefinedVariable { role: self.sprite.role.name.clone(), sprite: self.sprite.name.clone(), name })
    }
    fn parse_expr(&self, expr: &Xml) -> Result<Expr, Error> {
        match expr.name.as_str() {
            "l" => Ok(Expr::Value(Value::Literal(expr.text.clone()))),
            "bool" => match expr.text.as_str() {
                "true" => Ok(Expr::Value(Value::Bool(true))),
                "false" => Ok(Expr::Value(Value::Bool(false))),
                x => return Err(Error::InvalidProject { error: ProjectError::BoolUnknownValue { role: self.sprite.role.name.clone(), sprite: self.sprite.name.clone(), value: x.into() } })
            }
            "list" => {
                let mut values = Vec::with_capacity(expr.children.len());
                for item in expr.children.iter() {
                    let value = match item.children.get(0) {
                        None => return Err(Error::InvalidProject { error: ProjectError::ListItemNoValue { role: self.sprite.role.name.clone(), sprite: self.sprite.name.clone() } }),
                        Some(x) => x,
                    };
                    values.push(self.parse_expr(value)?);
                }
                Ok(Expr::Value(Value::List(values)))
            }
            "block" => {
                if let Some(var) = expr.attr("var") {
                    let comment = check_children_get_comment!(self, expr, "var" => 0);
                    let var = self.reference_var(var.value.clone())?;
                    return Ok(Expr::Variable { var, comment });
                }
                let s = match expr.attr("s") {
                    None => return Err(Error::InvalidProject { error: ProjectError::BlockWithoutType { role: self.sprite.role.name.clone(), sprite: self.sprite.name.clone() } }),
                    Some(v) => v.value.as_str(),
                };
                macro_rules! binary_op {
                    ($self:ident, $root:ident, $s:expr => $res:path : $left:ident, $right:ident) => {{
                        let comment = check_children_get_comment!(self, expr, s => 2);
                        let $left = Box::new(self.parse_expr(&expr.children[0])?);
                        let $right = Box::new(self.parse_expr(&expr.children[1])?);
                        $res { $left, $right, comment }
                    }};
                    ($self:ident, $root:ident, $s:expr => $res:path) => { binary_op! { $self, $root, $s => $res : left, right } }
                }
                macro_rules! unary_op {
                    ($self:ident, $root:ident, $s:expr => $res:path : $val:ident) => {{
                        let comment = check_children_get_comment!(self, expr, s => 1);
                        let $val = Box::new(self.parse_expr(&expr.children[0])?);
                        $res { $val, comment }
                    }};
                    ($self:ident, $root:ident, $s:expr => $res:path) => { unary_op! { $self, $root, $s => $res : value } }
                }
                Ok(match s {
                    "reportSum" => binary_op!(self, expr, s => Expr::Add),
                    "reportDifference" => binary_op!(self, expr, s => Expr::Sub),
                    "reportProduct" => binary_op!(self, expr, s => Expr::Mul),
                    "reportQuotient" => binary_op!(self, expr, s => Expr::Div),
                    "reportModulus" => binary_op!(self, expr, s => Expr::Mod),
                    "reportPower" => binary_op!(self, expr, s => Expr::Pow),

                    "reportAnd" => binary_op!(self, expr, s => Expr::And),
                    "reportOr" => binary_op!(self, expr, s => Expr::Or),
                    
                    "reportIsIdentical" => binary_op!(self, expr, s => Expr::RefEq),
                    "reportEquals" => binary_op!(self, expr, s => Expr::Eq),
                    "reportLessThan" => binary_op!(self, expr, s => Expr::Less),
                    "reportGreaterThan" => binary_op!(self, expr, s => Expr::Greater),
                    
                    "reportRandom" => binary_op!(self, expr, s => Expr::RandInt : lower, upper),
                    
                    "reportNot" => unary_op!(self, expr, s => Expr::Not),
                    "reportRound" => unary_op!(self, expr, s => Expr::Round),

                    "reportStringSize" => unary_op!(self, expr, s => Expr::Strlen),
                    "reportUnicodeAsLetter" => unary_op!(self, expr, s => Expr::UnicodeToChar),
                    "reportUnicode" => unary_op!(self, expr, s => Expr::CharToUnicode),

                    "reportBoolean" => match expr.get(&["l", "bool"]) {
                        Some(v) if v.text == "true" => Expr::Value(Value::Bool(true)),
                        Some(v) if v.text == "false" => Expr::Value(Value::Bool(false)),
                        _ => return Err(Error::InvalidProject { error: ProjectError::InvalidBoolLiteral { role: self.sprite.role.name.clone(), sprite: self.sprite.name.clone() } }),
                    }
                    "reportMonadic" => {
                        let comment = check_children_get_comment!(self, expr, s => 2);
                        let func = match expr.children[0].get(&["option"]) {
                            None => return Err(Error::InvalidProject { error: ProjectError::BlockMissingOption { role: self.sprite.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into() } }),
                            Some(f) => {
                                if f.children.len() != 0 { return Err(Error::BlockOptionNotConst { role: self.sprite.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into() }) }
                                f.text.as_str()
                            }
                        };
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

                            "ln" => Expr::LogE { value, comment },
                            "lg" => Expr::Log2 { value, comment },
                            "log" => Expr::Log10 { value, comment },

                            "e^" => Expr::ExpE { value, comment },
                            "2^" => Expr::Exp2 { value, comment },
                            "10^" => Expr::Exp10 { value, comment },

                            "" => return Err(Error::BlockOptionNotSelected { role: self.sprite.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into() }),
                            _ => return Err(Error::InvalidProject { error: ProjectError::BlockOptionUnknown { role: self.sprite.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.into(), got: func.into() } }),
                        }
                    }

                    _ => return Err(Error::UnknownBlockType { role: self.sprite.role.name.clone(), sprite: self.sprite.name.clone(), block_type: s.to_owned() }),
                })
            }
            x => return Err(Error::UnknownBlockType { role: self.sprite.role.name.clone(), sprite: self.sprite.name.clone(), block_type: x.into() }),
        }
    }
}

struct SpriteInfo<'a> {
    role: &'a RoleInfo,
    name: String,
    fields: LinkedHashMap<String, VariableDef>,
}
impl<'a> SpriteInfo<'a> {
    fn new(role: &'a RoleInfo, name: String) -> Self {
        Self { role, name, fields: Default::default() }
    }
    fn parse(mut self, sprite: &Xml) -> Result<Sprite, Error> {
        if let Some(fields) = sprite.get(&["variables"]) {
            let dummy_script = ScriptInfo::new(&self);

            let mut defs = vec![];
            for def in fields.children.iter().filter(|v| v.name == "variable") {
                let name = match def.attr("name") {
                    None => return Err(Error::InvalidProject { error: ProjectError::UnnamedField { role: self.role.name.clone(), sprite: self.name } }),
                    Some(x) => x.value.to_owned(),
                };
                let value = match def.children.get(0) {
                    None => return Err(Error::InvalidProject { error: ProjectError::FieldNoValue { role: self.role.name.clone(), sprite: self.name, name } }),
                    Some(x) => dummy_script.parse_expr(x)?,
                };
                if !value.is_evaluated() {
                    return Err(Error::InvalidProject { error: ProjectError::FieldNotEvaluated { role: self.role.name.clone(), sprite: self.name, name } });
                }
                defs.push((name, value));
            }

            for (name, value) in defs {
                if let Some(prev) = self.fields.insert(name.clone(), VariableDef { name, value }) {
                    return Err(Error::InvalidProject { error: ProjectError::FieldsWithSameName { role: self.role.name.clone(), sprite: self.name, name: prev.name } });
                }
            }
        }

        let mut scripts = vec![];
        if let Some(scripts_xml) = sprite.get(&["scripts"]) {
            for script_xml in scripts_xml.children.iter() {
                match script_xml.children.as_slice() {
                    [] => continue,
                    [stmt] => {
                        if stmt.attr("var").is_some() { continue }
                        if let Some(s) = stmt.attr("s") {
                            if s.value.starts_with("report") { continue }
                        }
                    }
                    _ => (),
                }
                scripts.push(ScriptInfo::new(&self).parse(script_xml)?);
            }
        }

        let fields = self.fields.into_iter().map(|(_, v)| v).collect();
        Ok(Sprite { name: self.name, fields, scripts })
    }
}

struct RoleInfo {
    name: String,
    globals: LinkedHashMap<String, VariableDef>,
}
impl RoleInfo {
    fn new(name: String) -> Self {
        Self { name, globals: Default::default() }
    }
    fn parse(mut self, role_root: &Xml) -> Result<Role, Error> {
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

        if let Some(globals) = content.get(&["variables"]) {
            let dummy_sprite = SpriteInfo::new(&self, "global".into());
            let dummy_script = ScriptInfo::new(&dummy_sprite);

            let mut defs = vec![];
            for def in globals.children.iter().filter(|v| v.name == "variable") {
                let name = match def.attr("name") {
                    None => return Err(Error::InvalidProject { error: ProjectError::UnnamedGlobal { role } }),
                    Some(x) => x.value.to_owned(),
                };
                let value = match def.children.get(0) {
                    None => return Err(Error::InvalidProject { error: ProjectError::GlobalNoValue { role, name } }),
                    Some(x) => dummy_script.parse_expr(x)?,
                };
                if !value.is_evaluated() {
                    return Err(Error::InvalidProject { error: ProjectError::GlobalNotEvaluated { role, name } });
                }
                defs.push((name, value));
            }

            for (name, value) in defs {
                if let Some(prev) = self.globals.insert(name.clone(), VariableDef { name, value }) {
                    return Err(Error::InvalidProject { error: ProjectError::GlobalsWithSameName { role, name: prev.name } });
                }
            }
        }

        let mut sprites = vec![];
        if let Some(sprites_xml) = stage.get(&["sprites"]) {
            for sprite in iter::once(stage).chain(sprites_xml.children.iter().filter(|s| s.name == "sprite")) {
                let name = match sprite.attr("name") {
                    None => return Err(Error::InvalidProject { error: ProjectError::UnnamedSprite { role } }),
                    Some(x) => x.value.clone(),
                };
                sprites.push(SpriteInfo::new(&self, name).parse(sprite)?);
            }
        }

        let globals = self.globals.into_iter().map(|(_, v)| v).collect();
        Ok(Role { name: role, notes, globals, sprites })
    }
}

pub fn parse<R: Read>(xml: R) -> Result<Project, Error> {
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
                    roles.push(RoleInfo::new(role_name).parse(child)?);
                }
            }
            return Ok(Project { name: proj_name, roles });
        }
    }
    Err(Error::InvalidProject { error: ProjectError::NoRoot })
}

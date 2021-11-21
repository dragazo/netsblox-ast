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
}
#[derive(Debug)]
pub enum Error {
    InvalidXml { error: xml::reader::Error },
    InvalidProject { error: ProjectError },
    UnknownBlockType { role: String, sprite: String, block_type: String },
    DerefAssignment { role: String, sprite: String },
    UndefinedVariable { role: String, sprite: String, name: String },
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
    Assign { var: VariableRef, value: Expr, comment: Option<String> },
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
    Mul { left: Box<Expr>, right: Box<Expr>, comment: Option<String> },
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
            None => { println!("here {:#?}", stmt_xml); return Err(Error::InvalidProject { error: ProjectError::BlockWithoutType { role: self.sprite.role.name.clone(), sprite: self.sprite.name.clone() } }) }
            Some(v) => v.value.as_str(),
        };
        Ok(match s {
            "doSetVar" => {
                let comment = check_children_get_comment!(self, stmt_xml, s => 2);
                let var = match stmt_xml.children[0].name.as_str() {
                    "l" => self.reference_var(stmt_xml.children[0].text.clone())?,
                    _ => return Err(Error::DerefAssignment { role: self.sprite.role.name.clone(), sprite: self.sprite.name.clone() }),
                };
                let value = self.parse_expr(&stmt_xml.children[1])?;
                Stmt::Assign { var, value, comment }
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
                Ok(match s {
                    "reportProduct" => {
                        let comment = check_children_get_comment!(self, expr, s => 2);
                        let left = Box::new(self.parse_expr(&expr.children[0])?);
                        let right = Box::new(self.parse_expr(&expr.children[1])?);
                        Expr::Mul { left, right, comment }
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

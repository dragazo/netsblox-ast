use std::io::Read;

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
}
#[derive(Debug)]
pub enum Error {
    InvalidXml { error: xml::reader::Error },
    InvalidProject { error: ProjectError },
    UnknownBlockType { role: String, sprite: String, block_type: String },
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Project {
    pub name: String,
    pub roles: Vec<Role>,
}
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Role {
    pub name: String,
    pub notes: String,
    pub globals: Vec<VariableDef>,
    pub sprites: Vec<Sprite>,
}
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Sprite {
    pub name: String,
    pub fields: Vec<VariableDef>,
}
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct VariableDef {
    pub name: String,
    pub init_value: Expr,
}
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Value {
    Literal(String),
    Bool(bool),
    List(Vec<Expr>),
}
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Expr {
    Value(Value),
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

struct BlockInfo<'a> {
    sprite: &'a SpriteInfo<'a>,
    name: String,
    locals: LinkedHashMap<String, VariableDef>,
}
impl<'a> BlockInfo<'a> {
    fn new(sprite: &'a SpriteInfo, name: String) -> Self {
        Self { sprite, name, locals: Default::default() }
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
            let dummy_block = BlockInfo::new(&self, "fields".into());

            let mut defs = vec![];
            for def in fields.children.iter().filter(|v| v.name == "variable") {
                let name = match def.attr("name") {
                    None => return Err(Error::InvalidProject { error: ProjectError::UnnamedField { role: self.role.name.clone(), sprite: self.name } }),
                    Some(x) => x.value.to_owned(),
                };
                let init_value = match def.children.get(0) {
                    None => return Err(Error::InvalidProject { error: ProjectError::FieldNoValue { role: self.role.name.clone(), sprite: self.name, name } }),
                    Some(x) => dummy_block.parse_expr(x)?,
                };
                if !init_value.is_evaluated() {
                    return Err(Error::InvalidProject { error: ProjectError::FieldNotEvaluated { role: self.role.name.clone(), sprite: self.name, name } });
                }
                defs.push((name, init_value));
            }

            for (name, init_value) in defs {
                if let Some(prev) = self.fields.insert(name.clone(), VariableDef { name, init_value }) {
                    return Err(Error::InvalidProject { error: ProjectError::FieldsWithSameName { role: self.role.name.clone(), sprite: self.name, name: prev.name } });
                }
            }
        }

        let fields = self.fields.into_iter().map(|(_, v)| v).collect();
        Ok(Sprite { name: self.name, fields })
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
            let dummy_block = BlockInfo::new(&dummy_sprite, "global".into());

            let mut defs = vec![];
            for def in globals.children.iter().filter(|v| v.name == "variable") {
                let name = match def.attr("name") {
                    None => return Err(Error::InvalidProject { error: ProjectError::UnnamedGlobal { role } }),
                    Some(x) => x.value.to_owned(),
                };
                let init_value = match def.children.get(0) {
                    None => return Err(Error::InvalidProject { error: ProjectError::GlobalNoValue { role, name } }),
                    Some(x) => dummy_block.parse_expr(x)?,
                };
                if !init_value.is_evaluated() {
                    return Err(Error::InvalidProject { error: ProjectError::GlobalNotEvaluated { role, name } });
                }
                defs.push((name, init_value));
            }

            for (name, init_value) in defs {
                if let Some(prev) = self.globals.insert(name.clone(), VariableDef { name, init_value }) {
                    return Err(Error::InvalidProject { error: ProjectError::GlobalsWithSameName { role, name: prev.name } });
                }
            }
        }

        let mut sprites = vec![];
        if let Some(sprites_xml) = stage.get(&["sprites"]) {
            for sprite in sprites_xml.children.iter().filter(|s| s.name == "sprite") {
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

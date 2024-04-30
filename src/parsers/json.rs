use crate::choice;
use crate::core::*;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Object {
    attributes: HashMap<String, Value>,
}

impl Object {
    pub fn new() -> Self {
        Self {
            attributes: HashMap::new(),
        }
    }

    pub fn from_vec(attributes: Vec<(String, Value)>) -> Self {
        Self::from_map(HashMap::from_iter(attributes))
    }

    pub fn from_map(attributes: HashMap<String, Value>) -> Self {
        Self { attributes }
    }

    pub fn attr(&self, name: &str) -> Option<&Value> {
        self.attributes.get(name)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Object(Object),
    Array(Vec<Value>),
    String(String),
    Number(i64),
    Boolean(bool),
    Undefined,
    Null,
}

impl Value {
    fn from_str(val: &str) -> Self {
        Self::String(String::from(val))
    }

    fn from_attrs(attrs: Vec<(String, Value)>) -> Self {
        Self::Object(Object::from_vec(attrs))
    }

    fn as_str(&self) -> &str {
        if let Value::String(val) = self {
            &val
        } else {
            panic!()
        }
    }
}

// --------------------------------------------------

fn string<'a>() -> impl Parser<'a, Value> {
    quoted_string().map(|val| Value::String(val))
}

#[test]
fn test_string() {
    assert_eq!(
        Ok(("", Value::from_str("test"))),
        string().parse("\"test\"")
    );
    assert_eq!(Err("not-quoted"), string().parse("not-quoted"));
}

// --------------------------------------------------

fn digit<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_digit(10))
}

fn number<'a>() -> impl Parser<'a, Value> {
    // this isnt great, as we expect the conversion
    // to int to succeed, but that might not be the
    // case if the number is too large.
    //
    one_or_more(digit()).map(|d| {
        d.into_iter()
            .collect::<String>()
            .parse::<i64>()
            .map(|v| Value::Number(v))
            .expect("Digits should convert to number")
    })
}

#[test]
fn test_number() {
    assert_eq!(Ok(("", Value::Number(123))), number().parse("123"));
}

// --------------------------------------------------

fn bool<'a>() -> impl Parser<'a, Value> {
    or(literal("true"), literal("false")).box_map(|val| Value::Boolean(val == "true"))
}

#[test]
fn test_bool() {
    let p = bool();
    assert_eq!(Ok(("", Value::Boolean(true))), p.parse("true"));
    assert_eq!(Ok(("", Value::Boolean(false))), p.parse("false"));
    assert_eq!(Err("not-a-bool"), p.parse("not-a-bool"));
}

// --------------------------------------------------

fn undefined<'a>() -> impl Parser<'a, Value> {
    literal("undefined").map(|_| Value::Undefined)
}

fn null<'a>() -> impl Parser<'a, Value> {
    literal("null").map(|_| Value::Null)
}

// --------------------------------------------------

fn object<'a>() -> impl Parser<'a, Value> {
    inner("{", attributes, "}").box_map(|val| Value::from_attrs(val))
}

#[test]
fn test_object() {
    let parsed = object().parse(r#"{ "name": "stephen" }"#).unwrap().1;

    if let Value::Object(obj) = parsed {
        assert_eq!("stephen", obj.attr("name").unwrap().as_str());
    } else {
        assert!(false)
    }
}

// --------------------------------------------------

fn array_items<'a>(input: &'a str) -> ParseResult<'a, Vec<Value>> {
    trim(sep(value(), trim(","))).parse(input)
}

fn array<'a>() -> impl Parser<'a, Value> {
    inner("[", array_items, "]").map(|val| Value::Array(val))
}

#[test]
fn test_array() {
    let expected = Ok((
        "",
        Value::Array(vec![Value::Number(1), Value::Number(2), Value::Number(3)]),
    ));

    assert_eq!(expected, array().parse("[ 1, 2, 3 ]"));
    assert_eq!(expected, array().parse("[1,2,3]"));
    assert_eq!(expected, array().parse("[1, 2, 3, ]"));
}

// --------------------------------------------------

fn value<'a>() -> impl Parser<'a, Value> {
    choice!(
        object(),
        array(),
        number(),
        string(),
        bool(),
        undefined(),
        null()
    )
}

fn attribute<'a>() -> impl Parser<'a, (String, Value)> {
    outer(quoted_string(), trim(":"), value())
}

#[test]
fn test_attribute() {
    let key = String::from("name");
    let value = Value::from_str("stephen");

    let expected = Ok(("", (key, value)));

    assert_eq!(expected, attribute().parse(r#""name":"stephen""#));
    assert_eq!(expected, attribute().parse(r#""name" :"stephen""#));
    assert_eq!(expected, attribute().parse(r#""name": "stephen""#));
    assert_eq!(expected, attribute().parse(r#""name"  :  "stephen""#));
}

fn attributes<'a>(input: &'a str) -> ParseResult<'a, Vec<(String, Value)>> {
    trim(sep(attribute(), trim(","))).parse(input)
}

// --------------------------------------------------

pub fn parse_json(doc: &str) -> Result<Object, &str> {
    trim(object()).parse(doc).map(|res| {
        if let Value::Object(o) = res.1 {
            o
        } else {
            panic!("Invalid JSON document")
        }
    })
}

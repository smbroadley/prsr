mod core;
mod parsers;

use crate::parsers::json::*;
use crate::parsers::xml::*;

fn main() {
    // let doc = r#"
    //     <Users>
    //         <User name="stephen" job="socialite" />
    //         <User name="paige" job="ledger controller" />
    //     </Users>
    // "#;

    // if let Ok(el) = parse_xml(doc) {
    //     println!(
    //         "Element {} with name {}",
    //         el.name, el.children[0].attributes[0].1
    //     );
    // }

    let doc = r#"
        {
            "name": "Stephen",
            "age": 41,
            "isEmployed": false,
            "favThings": [ "programming", "raindrops on roses" ],
        }
    "#;

    if let Ok(obj) = parse_json(doc) {
        if let Some(Value::String(name)) = obj.attr("name") {
            println!("Object with 'name' == '{}'", name);
        }
    } else {
        println!("Failed to parse doc");
    }
}

use std::fs::File;
use std::io::BufReader;
use netsblox_ast::*;

fn main() {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("usage: {} [input]", args[0]);
        std::process::exit(1);
    }

    let input = &args[1];
    if input.ends_with(".xml") {
        let xml = BufReader::new(File::open(input).expect("failed to open file"));
        let parser = ParserBuilder::default().optimize(true).build().unwrap();
        let res = parser.parse(xml).expect("failed to translate");
        println!("{:?}", res);
    }
    else {
        eprintln!("unknown input file type");
        std::process::exit(1);
    }
}

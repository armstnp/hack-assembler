use std::{env, fs::File, io::Read};

extern crate nom;

mod commands;
mod parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    assert!(args.len() > 1, "Usage: {} <codefile.asm>", args[0]);
    let filename = &args[1];
    let mut file = File::open(filename).expect(&format!("File not found: {}", filename));
    let mut data = String::new();
    file.read_to_string(&mut data)
        .expect(&format!("Error while reading file: {}", filename));
    let ast = parser::parse(&data);
    dbg!("{}", ast);
}

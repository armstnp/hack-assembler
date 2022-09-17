use std::{
    env,
    fs::File,
    io::{Read, Write},
};

extern crate nom;

mod commands;
mod parser;
mod translator;

fn main() {
    let args: Vec<String> = env::args().collect();
    assert!(args.len() > 1, "Usage: {} <codefile.asm>", args[0]);
    let filename = &args[1];
    let mut file = File::open(filename).expect(&format!("File not found: {}", filename));
    let mut data = String::new();
    file.read_to_string(&mut data)
        .expect(&format!("Error while reading file: {}", filename));

    let ast = parser::parse(&data);
    let translation = translator::translate(&ast);

    let outfilename = filename
        .rsplit_once('.')
        .map(|(fname, _)| fname)
        .unwrap_or(filename)
        .to_string()
        + ".hack";
    let mut outfile = File::create(outfilename).unwrap();

    for instruction in translation {
        writeln!(outfile, "{:0>16b}", instruction).unwrap();
    }
}

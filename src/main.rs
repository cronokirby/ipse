use std::fs::File;
use std::io;
use std::io::prelude::*;
mod parser;

fn main() -> io::Result<()> {
    let mut args = std::env::args();
    args.next();
    let file_name = args.next().unwrap();
    let mut file = File::open(file_name)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let expr = parser::parse(&contents);
    println!("{:?}", expr);
    Ok(())
}

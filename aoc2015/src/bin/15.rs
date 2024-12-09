use std::error::Error;
use std::{env, fs::read_to_string};

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let lines = input.lines().collect::<Vec<_>>();

    println!("Part 1: {}", "todo");

    println!("Part 2: {}", "todo");

    Ok(())
}

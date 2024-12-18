use std::error::Error;
use std::{env, fs::read_to_string};

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let key = input.lines().next().expect("No input");
    for num in 1.. {
        let block = format!("{key}{num}");
        let hash = format!("{:x}", md5::compute(block));
        if hash.starts_with("00000") {
            println!("Part1: {num}");
            break;
        }
    }

    for num in 1.. {
        let block = format!("{key}{num}");
        let hash = format!("{:x}", md5::compute(block));
        if hash.starts_with("000000") {
            println!("Part1: {num}");
            break;
        }
    }

    Ok(())
}

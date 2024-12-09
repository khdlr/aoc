use std::error::Error;
use std::{env, fs::read_to_string};

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let line = input.lines().next().expect("No line in input");

    let part1: i32 = line
        .chars()
        .map(|c| match c {
            ')' => -1,
            '(' => 1,
            c => panic!("Unexpected character in input! `{c}`"),
        })
        .sum();

    println!("Part 1: {part1}");

    let mut agg = 0;
    for (i, c) in line.chars().enumerate() {
        match c {
            ')' => agg -= 1,
            '(' => agg += 1,
            c => panic!("Unexpected character in input! `{c}`"),
        };
        if agg == -1 {
            println!("Part 2: {}", i + 1);
            break;
        }
    }

    Ok(())
}

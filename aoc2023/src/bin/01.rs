use std::{env, fs::read_to_string};
use std::error::Error;

fn process_line(inp_line: &str, parse_numbers: bool) -> u32 {
    let mut line = inp_line.to_owned();
    if parse_numbers {
        // Is this cheating? Probably, but I don't want to invoke regex on day 1 =)
        line = line
            .replace("one", "one1one")
            .replace("two", "two2two")
            .replace("three", "three3three")
            .replace("four", "four4four")
            .replace("five", "five5five")
            .replace("six", "six6six")
            .replace("seven", "seven7seven")
            .replace("eight", "eight8eight")
            .replace("nine", "nine9nine")
            .replace("zero", "zero0zero");
    }
    let digits: Vec<u32> = line
        .chars()
        .filter_map(|c| c.to_digit(10))
        .collect();
    let res = 10 * digits.first().unwrap_or(&0) + digits.last().unwrap_or(&0);
    // println!("{inp_line} -> {line} -> {res}");
    res
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;

    println!("Task 1: {}", input.lines().map(|l| process_line(l, false)).sum::<u32>());
    println!("Task 2: {}", input.lines().map(|l| process_line(l, true)).sum::<u32>());

    Ok(())
}

use std::error::Error;
use std::{env, fs::read_to_string};

pub fn dfs(combination: &Vec<char>, towels: &Vec<Vec<char>>, matched: usize) -> Option<()> {
    if matched == combination.len() {
        return Some(());
    } else if matched > combination.len() {
        return None;
    }
    for towel in towels.iter() {
        if towel
            .iter()
            .zip(combination.iter().skip(matched))
            .all(|(a, b)| a == b)
        {
            // Towel matches!
            if let Some(_) = dfs(combination, towels, matched + towel.len()) {
                return Some(());
            }
        }
    }
    None
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let mut lines = input.lines();
    let towels: Vec<Vec<char>> = lines
        .next()
        .expect("No towels")
        .split(", ")
        .map(|s| s.chars().collect())
        .collect();
    assert!(lines.next().expect("No second line") == "", "No empty line");
    let combinations = lines.map(|c| c.chars().collect()).collect::<Vec<_>>();

    let part1 = combinations
        .iter()
        .filter_map(|c| dfs(c, &towels, 0))
        .count();

    println!("Part 1: {}", part1);

    println!("Part 2: {}", "todo");

    Ok(())
}

use std::collections::HashMap;
use std::error::Error;
use std::{env, fs::read_to_string};

pub fn dfs(combination: &str, towels: &Vec<String>, matched: usize) -> Option<()> {
    if matched == combination.len() {
        return Some(());
    } else if matched > combination.len() {
        return None;
    }
    for towel in towels.iter() {
        if towel
            .chars()
            .zip(combination.chars().skip(matched))
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

pub fn num_combinations(
    combination: &str,
    towels: &Vec<String>,
    memo: &mut HashMap<String, usize>,
) -> usize {
    if combination == "" {
        return 1; // Only one way to do nothing :)
    }
    if let Some(count) = memo.get(combination) {
        *count
    } else {
        let mut count = 0;
        for towel in towels {
            if let Some(rest) = combination.strip_prefix(towel) {
                let num = num_combinations(rest, towels, memo);
                count += num;
            }
        }
        memo.insert(combination.to_owned(), count);
        count
    }
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let mut lines = input.lines();
    let towels: Vec<String> = lines
        .next()
        .expect("No towels")
        .split(", ")
        .map(|s| s.to_owned())
        .collect();
    assert!(lines.next().expect("No second line") == "", "No empty line");
    let combinations = lines.collect::<Vec<_>>();

    let part1 = combinations
        .iter()
        .filter_map(|c| dfs(c, &towels, 0))
        .count();

    println!("Part 1: {}", part1);

    let mut memo: HashMap<String, usize> = HashMap::new();
    let mut part2 = 0;
    for comb in combinations {
        let count = num_combinations(&comb, &towels, &mut memo);
        part2 += count;
    }
    println!("Part 2: {part2}");

    Ok(())
}

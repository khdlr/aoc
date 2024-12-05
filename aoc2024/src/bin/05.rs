use std::collections::HashSet;
use std::error::Error;
use std::{env, fs::read_to_string};

pub fn parse(input: &str) -> i32 {
    input.parse().expect("Couldn't parse int")
}

pub fn parse_rule(input: &str) -> (i32, i32) {
    let mut nums = input.split('|').map(parse);
    (nums.next().unwrap(), nums.next().unwrap())
}

pub fn quicksort<T: IntoIterator<Item = i32>>(vec: T, rules: &HashSet<(i32, i32)>) -> Vec<i32> {
    let mut numbers = vec.into_iter();
    match numbers.next() {
        Some(pivot) => {
            let (left, right) = numbers.partition::<Vec<i32>, _>(|a| {
                rules.contains(&(*a, pivot)) && !rules.contains(&(pivot, *a))
            });
            return quicksort(left, rules)
                .into_iter()
                .chain([pivot].into_iter())
                .chain(quicksort(right, rules))
                .collect();
        }
        None => {
            return vec![];
        }
    }
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let mut lines = input.lines().peekable();
    let rules: HashSet<(i32, i32)> = (&mut lines)
        .take_while(|c| c.contains('|'))
        .map(parse_rule)
        .collect();

    let input: Vec<Vec<i32>> = lines
        .map(|line| line.split(',').map(parse).collect::<Vec<i32>>())
        .collect();

    let mut part1 = 0;
    let mut part2 = 0;
    for line in input.iter() {
        let sorted = quicksort(line.clone(), &rules);
        let median = sorted[sorted.len() / 2];
        if sorted == *line {
            part1 += median;
        } else {
            part2 += median;
        }
    }
    println!("Part 1: {part1}");

    println!("Part 2: {part2}");

    Ok(())
}

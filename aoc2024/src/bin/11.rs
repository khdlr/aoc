use std::collections::HashMap;
use std::error::Error;
use std::{env, fs::read_to_string};

type Num = u64;

pub fn parse(input: &str) -> Num {
    input.parse().expect("Couldn't parse int")
}

pub fn step(num: Num) -> (Num, Option<Num>) {
    if num == 0 {
        return (1, None);
    }
    let n_digits = 1 + num.ilog10();
    if n_digits % 2 == 0 {
        // Split
        let modulus = (10u64).pow(n_digits / 2);
        let left = num / modulus;
        let right = num % modulus;
        return (left, Some(right));
    } else {
        return (num * 2024, None);
    }
}

pub fn step_n(counts: &HashMap<Num, usize>, steps: usize) -> HashMap<Num, usize> {
    let mut counts = counts.clone();
    for i in 0..steps {
        let mut next_counts = HashMap::new();
        for (num, count) in counts {
            let (num, opt) = step(num);
            *next_counts.entry(num).or_default() += count;
            if let Some(num) = opt {
                *next_counts.entry(num).or_default() += count
            }
        }
        counts = next_counts;
    }
    counts
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let numbers: Vec<Num> = input
        .lines()
        .next()
        .expect("No input line")
        .split(' ')
        .map(parse)
        .collect();
    let mut counts: HashMap<Num, usize> = HashMap::new();
    for num in numbers {
        *counts.entry(num).or_default() += 1;
    }
    let part1 = step_n(&counts, 25);

    println!("Part 1: {:?}", part1.values().sum::<usize>());

    // Turns out this works without any fast exponentiation... :)
    let part2 = step_n(&counts, 75);
    println!("Part 2: {}", part2.values().sum::<usize>());

    Ok(())
}

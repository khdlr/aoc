use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::iter::successors;
use std::{env, fs::read_to_string};

use itertools::iterate;

fn mix(a: u64, b: u64) -> u64 {
    a ^ b
}

fn prune(a: u64) -> u64 {
    a % 16777216
}

fn step(a: &u64) -> u64 {
    let a = *a;
    let b = prune(mix(a, a * 64));
    let c = prune(mix(b, b / 32));
    let d = prune(mix(c, c * 2048));
    d
}

fn step_and_diffs(a: &(u64, (i32, i32, i32, i32))) -> (u64, (i32, i32, i32, i32)) {
    let (_, d2, d3, d4) = a.1;
    let a = a.0;
    let b = prune(mix(a, a * 64));
    let c = prune(mix(b, b / 32));
    let d = prune(mix(c, c * 2048));

    let d5 = (d % 10) as i32 - (a % 10) as i32;
    (d, (d2, d3, d4, d5))
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let inputs = input
        .lines()
        .map(|num| num.parse::<u64>().expect("Parsing error"))
        .collect::<Vec<_>>();

    assert!(mix(42, 15) == 37, "Mixing error");
    assert!(prune(100000000) == 16113920, "Pruning error");

    let mut sum = 0;
    for input in &inputs {
        let nums = iterate(*input, step);
        let secret = nums.take(2001).last().expect("Couldn't iterate 2000 times");
        sum += secret;
    }

    println!("Part 1: {sum}");

    let mut seqyield: HashMap<(i32, i32, i32, i32), u64> = HashMap::new();
    for input in &inputs {
        let mut monkey_sold = HashSet::new();
        for (num, diffs) in iterate((*input, (127, 127, 127, 127)), step_and_diffs)
            .skip(1)
            .take(2000)
        {
            if diffs.0 != 127 && !monkey_sold.contains(&diffs) {
                monkey_sold.insert(diffs);
                *seqyield.entry(diffs).or_default() += num % 10;
            }
        }
    }

    let part2 = seqyield
        .into_iter()
        .max_by_key(|(k, s)| *s)
        .expect("No seqyields!")
        .1;
    println!("Part 2: {}", part2);

    Ok(())
}

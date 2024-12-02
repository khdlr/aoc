use counter::Counter;
use itertools::Itertools;
use std::error::Error;
use std::{env, fs::read_to_string}; // 0.8.0

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let pairs = input
        .lines()
        .map(|l| {
            let s: Result<Vec<i32>, _> = l.split("   ").map(str::parse::<i32>).collect();
            if let Ok(v) = s {
                return (v[0], v[1]);
            } else {
                panic!("{l}");
            }
        })
        .collect::<Vec<_>>();

    let left: Vec<i32> = pairs.iter().map(|(l, _)| *l).sorted().collect();
    let right: Vec<i32> = pairs.iter().map(|(_, r)| *r).sorted().collect();

    let res1: i32 = left
        .iter()
        .zip(right.iter())
        .map(|(l, r)| (l - r).abs())
        .sum();
    println!("Part 1: {res1}");

    let counter: Counter<i32> = right.into_iter().collect();
    let res2: i32 = left.iter().map(|l| l * counter[l] as i32).sum();
    println!("Part 2: {res2}");

    Ok(())
}

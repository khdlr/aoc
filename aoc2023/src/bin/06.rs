use std::{env, fs::read_to_string};
use std::error::Error;
use itertools::Itertools;

fn parse_line(line: &str) -> Vec<i64> {
    line.split_whitespace().skip(1).map(|c| c.parse::<i64>().unwrap()).collect()
}

fn join_list(list: Vec<i64>) -> i64 {
    list.iter().map(|c| c.to_string()).join("").parse().unwrap()
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let lines: Vec<&str> = input.lines().collect();
    let times: Vec<i64> = parse_line(lines[0]);
    let dists: Vec<i64> = parse_line(lines[1]);

    println!("{times:?}");
    let mut counts = vec![];
    for (&time, &dist) in times.iter().zip(dists.iter()) {
        let count = (1..time).filter(|t| (time-t)*t > dist).count();
        counts.push(count);
    }
    println!("Task 1: {}", counts.iter().product::<usize>());

    let final_time = join_list(times);
    let final_dist = join_list(dists);

    let count = (1..final_time).filter(|t| (final_time-t)*t > final_dist).count();
    println!("Task 2: {count}");

    Ok(())
}

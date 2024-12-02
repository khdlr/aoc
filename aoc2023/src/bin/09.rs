use std::{env, fs::read_to_string};
use std::error::Error;
use itertools::Itertools;

fn diff(seq: &Vec<i64>) -> Vec<i64> {
    seq.iter().tuple_windows().map(|(a, b)| b - a).collect()
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let lines: Vec<_> = input.lines().collect();
    let seqs: Vec<Vec<i64>> = lines.iter().map(|line| line.split(' ').map(|c| c.parse().unwrap()).collect()).collect();

    let mut lasts = Vec::new();
    for seq in &seqs {
        let mut stack = vec![seq.clone()];
        while !stack.last().unwrap().iter().all(|&c| c == 0) {
            stack.push(diff(stack.last().unwrap()));
        }
        let mut diff = 0;
        for i in (0..stack.len()).rev() {
            diff += stack[i].last().unwrap();
        }
        lasts.push(diff);
    }

    let sum: i64 = lasts.iter().sum();
    println!("Task 1: {sum}");

    let mut firsts = Vec::new();
    for seq in &seqs {
        let mut stack = vec![seq.clone()];
        while !stack.last().unwrap().iter().all(|&c| c == 0) {
            stack.push(diff(stack.last().unwrap()));
        }
        let mut diff = 0;
        for i in (0..stack.len()).rev() {
            diff = stack[i][0] - diff;
        }
        firsts.push(diff);
    }

    let sum: i64 = firsts.iter().sum();
    println!("Task 2: {sum}");


    Ok(())
}

use std::collections::HashSet;
use std::error::Error;
use std::{env, fs::read_to_string};

#[derive(PartialEq, Eq, Clone, Copy)]
enum Op {
    Add,
    Mul,
    Cat,
}

impl Op {
    fn apply(&self, left: i64, right: i64) -> i64 {
        match self {
            Op::Add => left + right,
            Op::Mul => left * right,
            Op::Cat => left * 10i64.pow(1 + right.ilog10()) + right,
        }
    }
}

pub fn parse(input: &str) -> i64 {
    input.parse().expect("Couldn't parse int")
}

struct Eqn {
    result: i64,
    operands: Vec<i64>,
}

impl Eqn {
    fn from_string(input: &str) -> Self {
        let mut parts = input.split(": ");
        let result: i64 = parse(parts.next().unwrap());
        let operands = parts.next().unwrap().split(' ').map(parse).collect();
        Self { result, operands }
    }

    fn fulfillable(&self, ops: Vec<Op>) -> bool {
        let mut possible = HashSet::from([self.operands[0]]);
        for &num in &self.operands[1..] {
            possible = possible
                .iter()
                .flat_map(|&n| ops.iter().map(|op| op.apply(n, num)).collect::<Vec<_>>())
                .collect();
        }
        possible.contains(&self.result)
    }
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let eqns = input.lines().map(Eqn::from_string).collect::<Vec<_>>();

    let part1: i64 = eqns
        .iter()
        .filter(|e| e.fulfillable(vec![Op::Add, Op::Mul]))
        .map(|e| e.result)
        .sum();

    println!("Part 1: {part1}");

    let part2: i64 = eqns
        .iter()
        .filter(|e| e.fulfillable(vec![Op::Add, Op::Mul, Op::Cat]))
        .map(|e| e.result)
        .sum();

    println!("Part 2: {}", part2);

    Ok(())
}

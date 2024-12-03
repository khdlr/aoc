use regex::{Captures, Regex};
use std::error::Error;
use std::{env, fs::read_to_string};

#[derive(Clone, PartialEq, Copy)]
enum Op {
    Do,
    Dont,
    Mul(i32),
}

impl Op {
    fn from_captures(captures: Captures<'_>) -> Self {
        match captures.extract() {
            ("do()", _) => Op::Do,
            ("don't()", _) => Op::Dont,
            (_, [l, r]) => {
                let l: i32 = l.parse().unwrap();
                let r: i32 = r.parse().unwrap();
                Op::Mul(l * r)
            }
        }
    }

    fn to_num(&self) -> Option<i32> {
        if let Op::Mul(num) = self {
            Some(*num)
        } else {
            None
        }
    }
}

fn part2_reducer(acc: (bool, i32), op: &Op) -> (bool, i32) {
    let (active, value) = acc;
    match op {
        Op::Do => (true, value),
        Op::Dont => (false, value),
        Op::Mul(add) => {
            if active {
                (active, value + add)
            } else {
                (active, value)
            }
        }
    }
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let re = Regex::new(r"mul\(([0-9]{1,3}),([0-9]{1,3})\)|do\(\)()()|don't\(\)()()")?;
    let ops: Vec<Op> = re.captures_iter(&input).map(Op::from_captures).collect();

    let res1: i32 = ops.iter().filter_map(Op::to_num).sum();
    println!("Part 1: {res1}");

    let (_, res2) = ops.iter().fold((true, 0), part2_reducer);
    println!("Part 2: {res2}");

    Ok(())
}

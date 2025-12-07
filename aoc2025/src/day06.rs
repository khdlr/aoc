use std::fs::read_to_string;

use crate::utils::grid::Grid;

#[allow(unused)]
pub fn solve(path: &str) {
    let mut lines: Vec<_> = read_to_string(path)
        .unwrap()
        .lines()
        .map(|l| {
            l.split_whitespace()
                .map(String::from)
                .collect::<Vec<String>>()
        })
        .collect();

    let operations = lines.pop().expect("No operations");
    let operands = lines
        .into_iter()
        .map(|line| {
            line.into_iter()
                .map(|s| s.parse::<i64>().unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let mut sum = 0;
    for (i, op) in operations.into_iter().enumerate() {
        let op = match op.as_str() {
            "+" => |a: i64, b: i64| a + b,
            "*" => |a: i64, b: i64| a * b,
            a => panic!("Incompatible operator: `{a}`"),
        };

        let result = operands
            .iter()
            .map(|r| r[i])
            .reduce(op)
            .expect("Empty operands");

        sum += result;
    }

    println!("Part 1: {sum}");

    // ==== PART 2 ====
    let grid = Grid::from_str(read_to_string(path).expect("Couldn't read input"));

    grid.print();

    let mut operands: Vec<i64> = Vec::new();
    let mut results: Vec<i64> = Vec::new();
    for x in (0..grid.width()).rev() {
        let mut operand = 0;
        let mut anynum = false;
        for y in (0..grid.height() - 1) {
            match grid[(y, x)] {
                c @ '0'..='9' => {
                    operand = 10 * operand + (c as u32 - '0' as u32) as i64;
                    anynum = true;
                }
                ' ' => { /* noop */ }
                c => {
                    panic!("Invalid operand char: {c}")
                }
            }
        }
        if anynum {
            operands.push(operand);
        }
        match grid[(grid.height() - 1, x)] {
            '+' => {
                results.push(operands.iter().sum());
                operands.clear();
            }
            '*' => {
                results.push(operands.iter().product());
                operands.clear();
            }
            _ => { /* noop */ }
        }
    }

    println!("Part 2: {}", results.iter().sum::<i64>());
}

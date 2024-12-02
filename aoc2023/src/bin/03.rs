use std::{env, fs::read_to_string};
use std::error::Error;
use regex::Regex;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Number {
    pub num: i32,
    pub row: i32,
    pub start: i32,
    pub end: i32,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Symbol {
    pub chr: String,
    pub row: i32,
    pub col: i32,
}

fn parse_numbers(row_inp: (usize, &str)) -> Vec<Number> {
    let re = Regex::new(r"[0-9]+").unwrap();
    let (row, inp) = row_inp;
    re.find_iter(inp)
        .map(|n| Number { num: n.as_str().parse().unwrap(), row: row as i32, start: n.start() as i32, end: n.end() as i32 - 1 })
        .collect()
}

fn parse_symbols(row_inp: (usize, &str)) -> Vec<Symbol> {
    let re = Regex::new(r"[^.0-9]+").unwrap();
    let (row, inp) = row_inp;
    re.find_iter(inp)
        .map(|n| Symbol { chr: n.as_str().to_owned(), row: row as i32, col: n.start() as i32 })
        .collect()
}

fn touches(sym: &Symbol, num: &Number) -> bool {
    sym.col >= num.start - 1 &&
    sym.col <= num.end + 1 &&
    sym.row >= num.row - 1 &&
    sym.row <= num.row + 1
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let nums: Vec<Number> = input.lines().enumerate().flat_map(parse_numbers).collect();
    let symbols: Vec<Symbol> = input.lines().enumerate().flat_map(parse_symbols).collect();

    let sum: i32 = nums.iter()
        .filter(|num| symbols.iter().any(|sym| touches(sym, num)))
        .map(|num| num.num)
        .sum();
    println!("Task 1: {sum}");

    let gear_sum: i32 = symbols.iter()
        .filter(|sym| sym.chr == "*")
        .filter_map(|sym| {
            let adj: Vec<&Number> = nums.iter().filter(|num| touches(sym, num)).collect();
            (adj.len() == 2).then(|| adj[0].num * adj[1].num)
        })
        .sum();
    println!("Task 2: {gear_sum}");

    Ok(())
}

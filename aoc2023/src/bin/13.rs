use std::error::Error;
use std::{env, fs::read_to_string};

use counter::Counter;

type Grid = Vec<Vec<char>>;

pub fn parse_grid(inp: &str) -> Grid {
    inp.lines().map(|line| line.chars().collect()).collect()
}

pub fn score_grid(grid: &Grid) -> (usize, usize) {
    let h = grid.len();
    let w = grid[0].len();
    println!("Scoring {h}x{w} grid");
    let mut row_counts: Counter<usize, usize> = Counter::new();
    // Check rows
    for y in 0..(h-1) {
        for x in 0..w {
            if (0..=y).all(|y2| {
                let ymir = 2 * y + 1 - y2;
                (ymir >= h) || (grid[y2][x] == grid[ymir][x])
            }) {
                row_counts.update([(y+1)].iter().cloned());
            }
        }
    }

    // Check columns
    let mut col_counts: Counter<usize, usize> = Counter::new();
    for x in 0..(w-1) {
        for y in 0..h {
            if (0..=x).all(|x2| {
                let xmir = 2 * x + 1 - x2;
                (xmir >= w) || (grid[y][x2] == grid[y][xmir])
            }) {
                col_counts.update([x+1].iter().cloned());
            }
        }
    }

    println!("Rows: {row_counts:?}");
    println!("Cols: {col_counts:?}");

    let perfect_symmetries: Vec<_> =
        row_counts.iter().filter(|(_, &c)| c == w).map(|(y, _)| 100 * y).chain(
        col_counts.iter().filter(|(_, &c)| c == h).map(|(x, _)| x.clone())).collect();

    let smudge_symmetries: Vec<_> =
        row_counts.iter().filter(|(_, &c)| c == w-1).map(|(y, _)| 100 * y).chain(
        col_counts.iter().filter(|(_, &c)| c == h-1).map(|(x, _)| x.clone())).collect();

    assert!(perfect_symmetries.len() == 1, "too many perfect symmetries!");
    assert!(smudge_symmetries.len() == 1, "too many smudge symmetries!");

    return (perfect_symmetries[0], smudge_symmetries[0]);
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let grids: Vec<Grid> = input.split("\n\n").map(parse_grid).collect();

    let scores = grids.iter().map(score_grid).collect::<Vec<_>>();
    println!("Task 1: {}", scores.iter().map(|a| a.0).sum::<usize>());
    println!("Task 2: {}", scores.iter().map(|a| a.1).sum::<usize>());

    Ok(())
}

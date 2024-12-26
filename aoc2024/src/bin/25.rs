use std::error::Error;
use std::{env, fs::read_to_string};

use aoc2024::grid::Grid;

fn is_lock(grid: &Grid<bool>) -> bool {
    let w = grid.width();
    (0..w).all(|x| grid[(0, x)])
}

fn parse_grid(grid: Grid<bool>) -> Vec<i32> {
    let h = grid.height();
    let w = grid.width();
    (0..w)
        .map(|x| (0..h).map(|y| grid[(y, x)] as i32).sum::<i32>())
        .collect()
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let (locks, keys): (Vec<Grid<bool>>, _) = input
        .split("\n\n")
        .map(Grid::from_str)
        .map(|grid| grid.map(|c| *c == '#'))
        .partition(is_lock);

    let locks: Vec<Vec<i32>> = locks.into_iter().map(parse_grid).collect();
    let keys: Vec<Vec<i32>> = keys.into_iter().map(parse_grid).collect();

    let l = locks.len();
    let k = keys.len();
    println!("{l}x{k} = {}", l * k);

    let mut matches = 0;
    for lock in &locks {
        for key in &keys {
            if lock.iter().zip(key.iter()).all(|(l, k)| l + k <= 7) {
                matches += 1;
            }
        }
    }

    println!("Part 1: {matches}");

    Ok(())
}

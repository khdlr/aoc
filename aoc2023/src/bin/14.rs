use std::collections::HashMap;
use std::error::Error;
use std::{env, fs::read_to_string};
use tqdm::tqdm;
use rayon::prelude::*;

type Grid = Vec<Vec<char>>;

pub fn parse_grid(inp: &str) -> Grid {
    inp.lines().map(|line| line.chars().collect()).collect()
}

pub fn print(grid: &Grid) {
    for row in grid {
        let line: String = row.iter().collect::<String>();
        println!("{}", line);
    }
}

pub fn shift_y(grid: &mut Grid, up: bool) {
    let h = grid.len();
    let w = grid[0].len();
    for x in 0..w {
        let mut slices = vec![];
        let mut start = 0;
        for y in 0..h {
            if grid[y][x] == '#' {
                if start != y {
                    slices.push((start, y));
                }
                start = y+1;
            }
        }
        if start != h-1 {
            slices.push((start, h));
        }
        for (a, b) in slices {
            let mut count = 0;
            // print!("Slice ({a}, {b}): ");
            for y in a..b {
                assert!(grid[y][x] != '#', "Broke down for slice {a}-{b} in col {x}");
                // print!("{}", grid[y][x]);
                if grid[y][x] == 'O' {
                    count += 1;
                    grid[y][x] = '.';
                }
            }
            // println!();
            if up {
                for y in a..(a+count) {
                    grid[y][x] = 'O';
                }
            } else {
                for y in (b-count)..b {
                    grid[y][x] = 'O';
                }
            }
        }
    }
}

pub fn shift_x(grid: &mut Grid, up: bool) {
    let h = grid.len();
    let w = grid[0].len();
    for y in 0..h {
        let mut slices = vec![];
        let mut start = 0;
        for x in 0..w {
            if grid[y][x] == '#' {
                if start != x {
                    slices.push((start, x));
                }
                start = x+1;
            }
        }
        if start != w-1 {
            slices.push((start, w));
        }
        for (a, b) in slices {
            let mut count = 0;
            // print!("Slice ({a}, {b}): ");
            for x in a..b {
                assert!(grid[y][x] != '#', "Broke down for slice {a}-{b} in col {x}");
                // print!("{}", grid[y][x]);
                if grid[y][x] == 'O' {
                    count += 1;
                    grid[y][x] = '.';
                }
            }
            // println!();
            if up {
                for x in a..(a+count) {
                    grid[y][x] = 'O';
                }
            } else {
                for x in (b-count)..b {
                    grid[y][x] = 'O';
                }
            }
        }
    }
}


pub fn score(grid: &Grid) -> usize {
    let mut score = 0;
    for (idx, row) in grid.iter().enumerate() {
        let mult = grid.len() - idx;
        score += mult * row.iter().filter(|&&c| c == 'O').count();
    }
    return score;
}

pub fn cycle(grid: &mut Grid) {
    // north, then west, then south, then east
    shift_y(grid, true);
    shift_x(grid, true);
    shift_y(grid, false);
    shift_x(grid, false);
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let init: Grid = parse_grid(&input);
    let mut grid = init.clone();

    shift_y(&mut grid, true);
    println!("Task 1: {}", score(&grid));

    let mut grid = init.clone();
    let mut states: HashMap<Grid, usize> = HashMap::new();
    let mut iters = 0;

    const N: usize = 1000000000;
    loop {
        iters += 1;
        cycle(&mut grid);
        match states.get(&grid) {
            Some(last_idx) => {
                println!("Loop detected! {last_idx} -> {iters}");
                let cycle_len = iters - last_idx;
                while iters <= N-cycle_len {
                    iters += cycle_len;
                }
                println!("Skipped to {iters}");
            },
            None => {
                states.insert(grid.clone(), iters);
            },
        }
        if iters == N {
            break;
        }
    }
    println!("Task 2: Score after {N} iterations: {}", score(&grid));

    Ok(())
}

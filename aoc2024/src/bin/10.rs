use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::{env, fs::read_to_string};

use aoc2024::grid::Grid;

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let grid: Grid<i32> = input
        .lines()
        .enumerate()
        .flat_map(|(iter_y, l)| {
            l.chars()
                .enumerate()
                .map(|(iter_x, c)| {
                    (
                        (iter_y as i32, iter_x as i32),
                        c.to_digit(10).unwrap() as i32,
                    )
                })
                .collect::<Vec<_>>()
        })
        .collect();

    let mut num2locs: HashMap<i32, Vec<(i32, i32)>> = HashMap::new();
    for (loc, num) in grid.iter().with_pos() {
        num2locs.entry(*num).or_default().push(loc);
    }

    let mut sources: Grid<HashSet<(i32, i32)>> = Grid::empty(grid.height(), grid.width());
    for loc in &num2locs[&9] {
        sources[*loc].insert(loc.clone());
    }
    for num in (0..=8).rev() {
        for (y, x) in num2locs.entry(num).or_default() {
            let y = *y;
            let x = *x;
            for (dy, dx) in [(-1, 0), (1, 0), (0, 1), (0, -1)] {
                if let Some(srcs) = sources.get((y + dy, x + dx)) {
                    if grid[(y + dy, x + dx)] == grid[(y, x)] + 1 {
                        sources[(y, x)] = &sources[(y, x)] | &srcs;
                    }
                }
            }
        }
    }

    let part1: i32 = num2locs[&0]
        .iter()
        .map(|loc| sources[*loc].len() as i32)
        .sum();

    println!("Part 2: {}", part1);

    let mut rating: Grid<i32> = Grid::empty(grid.height(), grid.width());
    for loc in &num2locs[&9] {
        rating[*loc] = 1;
    }
    for num in (0..=8).rev() {
        for (y, x) in num2locs.entry(num).or_default() {
            let y = *y;
            let x = *x;
            for (dy, dx) in [(-1, 0), (1, 0), (0, 1), (0, -1)] {
                if let Some(r) = rating.get((y + dy, x + dx)) {
                    if grid[(y + dy, x + dx)] == grid[(y, x)] + 1 {
                        rating[(y, x)] = rating[(y, x)] + r;
                    }
                }
            }
        }
    }

    let part2: i32 = num2locs[&0].iter().map(|loc| rating[*loc]).sum();
    println!("Part 2: {}", part2);

    Ok(())
}

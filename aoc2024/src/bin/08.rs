use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::{env, fs::read_to_string};

use aoc2024::grid::Grid;

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let mut positions: HashMap<char, Vec<(i32, i32)>> = HashMap::new();
    let mut xmax = 0;
    let mut ymax = 0;
    for (y, line) in read_to_string(&args[1])?.lines().enumerate() {
        for (x, char) in line.chars().enumerate() {
            if char != '.' {
                positions
                    .entry(char)
                    .or_default()
                    .push((y as i32, x as i32));
            }
            xmax = xmax.max(x as i32);
            ymax = ymax.max(y as i32);
        }
    }

    let mut grid1: Grid<bool> = Grid::empty(ymax + 1, xmax + 1);
    let mut grid2: Grid<bool> = Grid::empty(ymax + 1, xmax + 1);

    let mut antinode_locs: HashSet<(i32, i32)> = HashSet::new();
    let mut antinode_locs2: HashSet<(i32, i32)> = HashSet::new();
    for locs in positions.values() {
        for comb in locs.iter().combinations(2) {
            let (ya, xa) = comb[0];
            let (yb, xb) = comb[1];

            // Part 1
            let y1 = 2 * ya - yb;
            let x1 = 2 * xa - xb;
            let y2 = 2 * yb - ya;
            let x2 = 2 * xb - xa;

            if grid1.inside((y1, x1)) {
                antinode_locs.insert((y1, x1));
                grid1[(y1, x1)] = true;
            }

            if grid1.inside((y2, x2)) {
                antinode_locs.insert((y2, x2));
                grid1[(y2, x2)] = true;
            }

            // Part 2
            let dx = xb - xa;
            let dy = yb - ya;

            let dumb_extent = grid2.height().max(grid2.width());
            for t in (-dumb_extent)..=(dumb_extent) {
                let x = xa + t * dx;
                let y = ya + t * dy;
                if grid2.inside((y, x)) {
                    grid2[(y, x)] = true;
                    antinode_locs2.insert((y, x));
                }
            }
        }
    }

    // grid2.map(|b| if *b { '#' } else { '.' }).print();

    println!("Part 1: {}", antinode_locs.len());

    println!("Part 2: {}", antinode_locs2.len());

    Ok(())
}

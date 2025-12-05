use std::fs::read_to_string;

use crate::utils::grid::Grid;

pub fn solve(path: &str) {
    let grid = Grid::from_str(&read_to_string(path).unwrap());

    let free = grid.map_with_eight_neighbors(|&n, neighbors| {
        n == '@' && neighbors.iter().filter(|&&n| n == '@').count() < 4
    });
    // free.print_map(|c| if *c { 'x' } else { '.' });
    let p1 = free.iter().filter(|c| **c).count();

    println!("Part 1: {}", p1);

    let mut iter_grid = grid.clone();

    let mut changed = true;
    while changed {
        changed = false;
        iter_grid = iter_grid.map_with_eight_neighbors(|&n, neighbors| {
            if n == '.' {
                '.'
            } else if neighbors.iter().filter(|&&n| n == '@').count() < 4 {
                changed = true;
                '.'
            } else {
                '@'
            }
        });
    }

    let rolls_before = grid.iter().filter(|&&c| c == '@').count();
    let rolls_after = iter_grid.iter().filter(|&&c| c == '@').count();
    println!("Part 2: {}", rolls_before - rolls_after);
}

use std::collections::{BinaryHeap, HashSet};
use std::error::Error;
use std::{env, fs::read_to_string};

use aoc2024::grid::Grid;
use aoc2024::utils::Dir;

type State = (usize, (i32, i32), Dir);

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let grid: Grid<char> = input.lines().collect();
    let start = grid.find('S').expect("No start field");
    let end = grid.find('E').expect("No end field");

    let start_state = (0, start, Dir::E);
    let mut heap = BinaryHeap::new();
    heap.push(start_state);
    let grid = grid.map(|c| *c == '#');
    grid.print_braille();

    let mut visited = HashSet::new();
    let mut solution = 0;
    // Basic Dijkstra
    while let Some((cost, pos, dir)) = heap.pop() {
        if pos == end {
            solution = -cost;
            break;
        }

        let step = dir.apply(pos);
        if !grid[step] && !visited.contains(&(step, dir)) {
            visited.insert((step, dir));
            heap.push((cost - 1, step, dir));
        }
        if !visited.contains(&(pos, dir.left())) {
            visited.insert((pos, dir.left()));
            heap.push((cost - 1000, pos, dir.left()));
        }
        if !visited.contains(&(pos, dir.right())) {
            visited.insert((pos, dir.right()));
            heap.push((cost - 1000, pos, dir.right()));
        }
    }
    println!("Part 1: {solution}");

    Ok(())
}

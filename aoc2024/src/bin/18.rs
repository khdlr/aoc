use std::collections::{HashSet, VecDeque};
use std::error::Error;
use std::{env, fs::read_to_string};

use aoc2024::grid::Grid;
use aoc2024::utils::DIRECTIONS;

pub fn bfs(grid: &Grid<bool>) -> Option<usize> {
    let start = (0, 0);
    let goal = (grid.height() - 1, grid.width() - 1);
    let mut visited = HashSet::from([start]);
    let mut queue = VecDeque::from([(start, 0)]);

    while let Some((pos, t)) = queue.pop_front() {
        for dir in DIRECTIONS {
            let chk = dir.apply(pos);
            if grid.get(chk) == Some(false) && !visited.contains(&chk) {
                if chk == goal {
                    return Some(t + 1);
                }
                visited.insert(chk);
                queue.push_back((chk, t + 1));
            }
        }
    }
    None
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let coords = input
        .lines()
        .map(|row| {
            let mut nums = row
                .split(',')
                .map(|c| c.parse::<i32>().expect("Parse error"));
            let x = nums.next().unwrap();
            let y = nums.next().unwrap();
            (y, x)
        })
        .collect::<Vec<_>>();

    let (H, T) = if args[1].ends_with(".sample") {
        (7, 12)
    } else {
        (71, 1024)
    };
    let grid = Grid::filled(false, H, H);
    let mut grids = vec![grid.clone()];
    grids.extend(coords.iter().scan(grid, |g, pos| {
        g[*pos] = true;
        Some(g.clone())
    }));

    let part1 = bfs(&grids[T]);

    println!("Part 1: {}", part1.unwrap());

    // Quick binary search
    let mut min = 0;
    let mut max = grids.len() - 1;
    while min + 1 < max {
        let t = (min + max) / 2;
        match bfs(&grids[t]) {
            Some(_) => min = t,
            None => max = t,
        }
    }
    let (y, x) = coords[min];
    println!("Part 2: {x},{y}");

    Ok(())
}

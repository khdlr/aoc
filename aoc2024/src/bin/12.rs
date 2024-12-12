use std::collections::{BTreeSet, HashMap, HashSet};
use std::error::Error;
use std::{env, fs::read_to_string};

use aoc2024::grid::Grid;
use aoc2024::utils::{Dir, DIRECTIONS};

fn area(region: &HashSet<(i32, i32)>) -> i32 {
    region.len() as i32
}

fn perimeter(region: &HashSet<(i32, i32)>) -> i32 {
    let mut perimeter = 0;
    for (i, j) in region.iter().cloned() {
        if !region.contains(&(i + 1, j)) {
            perimeter += 1;
        }
        if !region.contains(&(i - 1, j)) {
            perimeter += 1;
        }
        if !region.contains(&(i, j + 1)) {
            perimeter += 1;
        }
        if !region.contains(&(i, j - 1)) {
            perimeter += 1;
        }
    }

    perimeter
}

fn discount_perimeter(region: &HashSet<(i32, i32)>) -> i32 {
    let mut fence_pieces: BTreeSet<(i32, i32, Dir)> = BTreeSet::new();
    for pos in region.iter().cloned() {
        for dir in DIRECTIONS {
            let (i2, j2) = dir.apply(pos);
            if !region.contains(&(i2, j2)) {
                fence_pieces.insert((i2, j2, dir));
            }
        }
    }
    let mut deduped: BTreeSet<(i32, i32, Dir)> = BTreeSet::new();
    while !fence_pieces.is_empty() {
        let fence = fence_pieces.iter().next().unwrap().clone(); // Will be the min
        deduped.insert(fence);

        let mut removed = true;
        let (mut i, mut j, dir) = fence;
        while removed {
            removed = false;
            if fence_pieces.contains(&(i, j, dir)) {
                removed = true;
                fence_pieces.remove(&(i, j, dir));
                if dir == Dir::W || dir == Dir::E {
                    i += 1;
                } else {
                    j += 1;
                }
            }
        }
    }

    deduped.len() as i32
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let grid: Grid<char> = Grid::from_str(&input);
    let mut region_ptr: Grid<(i32, i32)> = grid
        .iter()
        .with_pos()
        .map(|((i, j), _)| ((i, j), (i, j)))
        .collect();

    let mut changed = true;
    let mut steps = 0;
    while changed {
        changed = false;
        region_ptr.update_with_neighbors(|(i, j), nbs| {
            let mut out = (*i, *j);
            for nb in nbs {
                if grid[out] == grid[nb] && nb < out {
                    out = nb;
                    changed = true;
                }
            }
            out
        });
        steps += 1;
    }
    println!("{steps} floodfill steps...");
    let mut regions: HashMap<(i32, i32), HashSet<(i32, i32)>> = HashMap::new();
    for (pos, key) in region_ptr.iter().with_pos() {
        regions.entry(*key).or_default().insert(pos);
    }

    let part1: i32 = regions.values().map(|r| area(r) * perimeter(r)).sum();
    println!("Part 1: {:?}", part1);

    let part2: i32 = regions
        .values()
        .map(|r| area(r) * discount_perimeter(r))
        .sum();
    println!("Part 2: {}", part2);

    Ok(())
}

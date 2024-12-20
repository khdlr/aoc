use std::collections::{HashMap, VecDeque};
use std::error::Error;
use std::{env, fs::read_to_string};

use aoc2024::grid::Grid;
use aoc2024::utils::DIRECTIONS;

type Point = (i32, i32);

pub fn solve(
    grid: &Grid<bool>,
    start: Point,
    end: Point,
    cheat_allowed: i32,
) -> HashMap<(Point, Point), i32> {
    // Distance from start
    let mut d_start: HashMap<Point, i32> = HashMap::new();
    let mut q = VecDeque::from([(start, 0)]);
    while let Some((pos, cost)) = q.pop_front() {
        assert!(d_start.insert(pos, cost) == None);
        for dir in DIRECTIONS {
            let chk = pos + dir;
            if grid.get(chk) == Some(false) && !d_start.contains_key(&chk) {
                q.push_back((chk, cost + 1));
            }
        }
    }

    // Distance to end
    let mut d_end: HashMap<Point, i32> = HashMap::new();
    let mut q = VecDeque::from([(end, 0)]);
    while let Some((pos, cost)) = q.pop_front() {
        assert!(d_end.insert(pos, cost) == None);
        for dir in DIRECTIONS {
            let chk = pos + dir;
            if grid.get(chk) == Some(false) && !d_end.contains_key(&chk) {
                q.push_back((chk, cost + 1));
            }
        }
    }

    let shortest_path = *d_start.get(&end).expect("No path from start to end!");

    /*
        let mut cheats: HashMap<(Point, Point), i32> = HashMap::new();
        let walls = grid
            .iter()
            .with_pos()
            .filter_map(|(pos, b)| b.then_some(pos));
        for wall in walls {
            for pre_dir in DIRECTIONS {
                for post_dir in DIRECTIONS {
                    if pre_dir == post_dir {
                        continue;
                    }
                    if let Some(from_start) = d_start.get(&(wall + pre_dir)) {
                        if let Some(to_end) = d_end.get(&(wall + post_dir)) {
                            let cheat = (wall + pre_dir, wall + post_dir);
                            let cheated_path = from_start + to_end + 2;
                            if cheated_path < shortest_path {
                                let saving = shortest_path - cheated_path;
                                let previous_saving = *cheats.get(&cheat).unwrap_or(&saving);
                                cheats.insert(cheat, saving.max(previous_saving));
                            }
                        }
                    }
                }
            }
        }
    */

    let mut cheats: HashMap<(Point, Point), i32> = HashMap::new();
    for (pre, &_) in grid.iter().with_pos() {
        for (post, &wall_post) in grid.iter().with_pos() {
            let cheated_dist = (pre.0 - post.0).abs() + (pre.1 - post.1).abs();
            if cheated_dist > cheat_allowed {
                continue; // Cannot cheat for longer than `cheat_allowed`
            }
            if wall_post {
                continue; // Cannot end on a wall
            }
            let Some(d_pre) = d_start.get(&pre) else {
                continue;
            };
            let Some(d_post) = d_end.get(&post) else {
                continue;
            };

            let total_dist = d_pre + d_post + cheated_dist;
            if total_dist < shortest_path {
                cheats.insert((pre, post), shortest_path - total_dist);
            }
        }
    }

    cheats
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let chrgrid: Grid<char> = input.lines().collect();
    let start = chrgrid.find('S').expect("No start field");
    let end = chrgrid.find('E').expect("No end field");
    let grid = chrgrid.map(|c| *c == '#');

    let cheats = solve(&grid, start, end, 2);
    let part1 = cheats.iter().filter(|(_, saving)| **saving >= 100).count();
    println!("Part 1: {:?}", part1);

    let cheats = solve(&grid, start, end, 20);
    let part2 = cheats.iter().filter(|(_, saving)| **saving >= 100).count();
    println!("Part 2: {}", part2);

    Ok(())
}

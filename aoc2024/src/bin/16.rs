use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::error::Error;
use std::{env, fs::read_to_string};

use aoc2024::grid::Grid;
use aoc2024::utils::{Dir, DIRECTIONS};

fn cheapest(costmap: &HashMap<((i32, i32), Dir), i32>, pos: (i32, i32)) -> Option<i32> {
    let mut cheapest = None;
    for dir in &DIRECTIONS {
        if let Some(&c2) = costmap.get(&(pos, *dir)) {
            cheapest = Some(cheapest.map_or(c2, |c1: i32| c1.max(c2)))
        }
    }
    cheapest
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let chrgrid: Grid<char> = input.lines().collect();
    let start = chrgrid.find('S').expect("No start field");
    let end = chrgrid.find('E').expect("No end field");

    let start_state = (0, start, Dir::E);
    let mut heap = BinaryHeap::new();
    heap.push(start_state);
    let grid = chrgrid.map(|c| *c == '#');

    let mut parents: HashMap<((i32, i32), Dir), Vec<((i32, i32), Dir)>> = HashMap::new();
    let mut costmap: HashMap<((i32, i32), Dir), i32> = HashMap::new();
    // Dijkstra
    while let Some((cost, pos, dir)) = heap.pop() {
        if let Some(c) = cheapest(&costmap, end) {
            if c < cost {
                break;
            }
        }
        for check in [
            (cost - 1, dir.apply(pos), dir),
            (cost - 1000, pos, dir.left()),
            (cost - 1000, pos, dir.right()),
        ] {
            let (nextcost, nextpos, nextdir) = check;
            if !grid[nextpos] {
                match costmap.get(&(nextpos, nextdir)) {
                    None => {
                        costmap.insert((nextpos, nextdir), nextcost);
                        heap.push(check);
                        parents.insert((nextpos, nextdir), vec![(pos, dir)]);
                    }
                    Some(c) => match c.cmp(&nextcost) {
                        Ordering::Less => {
                            costmap.insert((nextpos, nextdir), nextcost);
                            heap.push(check);
                            parents.insert((nextpos, nextdir), vec![(pos, dir)]);
                        }
                        Ordering::Equal => {
                            parents
                                .get_mut(&(nextpos, nextdir))
                                .unwrap()
                                .push((pos, dir));
                        }
                        Ordering::Greater => {}
                    },
                }
            }
        }
    }

    let neg_cost = cheapest(&costmap, end).unwrap();
    println!("Part 1: {}", -neg_cost);

    let mut q = vec![];
    for &dir in &DIRECTIONS {
        if costmap.get(&(end, dir)) == Some(&neg_cost) {
            q.push((end, dir));
        }
    }

    let mut visited: HashSet<((i32, i32), Dir)> = HashSet::new();
    while let Some(step) = q.pop() {
        if !visited.contains(&step) {
            for parent in &parents[&step] {
                q.push(*parent);
            }
            visited.insert(step);
        }
    }
    let visited: HashSet<(i32, i32)> = visited.into_iter().map(|(pos, _)| pos).collect();

    // chrgrid.print_overlay(|pos| visited.contains(&pos).then_some('O'));
    // chrgrid.print_overlay(|pos| {
    //     if let Some(c) = cheapest(&costmap, pos) {
    //         if visited.contains(&pos) {
    //             format!("{c}").chars().last()
    //         } else {
    //             Some(char::from_u32(0x2080u32 + ((-c % 10) as u32)).unwrap())
    //         }
    //     } else {
    //         Some(' ')
    //     }
    // });

    println!("Part 2: {}", visited.len());

    Ok(())
}

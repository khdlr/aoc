use aoc2024::grid::Grid;
use indicatif::ProgressIterator;
use std::collections::HashSet;
use std::error::Error;
use std::thread::sleep;
use std::time::Duration;
use std::{env, fs::read_to_string};

const DEBUG: bool = true;

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
enum Dir {
    N,
    W,
    E,
    S,
}

#[derive(PartialEq, Eq, Hash, Copy, Clone)]
struct GuardState {
    y: i32,
    x: i32,
    dir: Dir,
}

impl GuardState {
    fn step(&self) -> Self {
        let mut st = self.clone();
        match self.dir {
            Dir::N => st.y -= 1,
            Dir::W => st.x -= 1,
            Dir::E => st.x += 1,
            Dir::S => st.y += 1,
        };
        st
    }

    fn turn_right(&self) -> Self {
        let mut st = self.clone();
        match self.dir {
            Dir::N => st.dir = Dir::E,
            Dir::W => st.dir = Dir::N,
            Dir::E => st.dir = Dir::S,
            Dir::S => st.dir = Dir::W,
        };
        st
    }

    fn pos(&self) -> (i32, i32) {
        (self.y, self.x)
    }
}

fn simulate(grid: &Grid<bool>, state: GuardState) -> (HashSet<GuardState>, bool) {
    let mut state = state;

    let mut visited = HashSet::from([state]);
    let mut has_loop = false;

    'outer: loop {
        let next = state.step();
        match grid.get((next.y, next.x)) {
            Some(true) => {
                state = state.turn_right();
            }
            Some(false) => {
                state = next;
                if visited.contains(&state) {
                    has_loop = true;
                    break 'outer;
                }
                visited.insert(state);

                if DEBUG {
                    let mut g = grid.clone();
                    g[(state.y, state.x)] = true;
                    g.print_braille();
                    sleep(Duration::from_millis(20));
                }
            }
            None => break 'outer,
        }
    }

    (visited, has_loop)
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;

    let mut init_state = GuardState {
        y: 0,
        x: 0,
        dir: Dir::N,
    };
    let grid: Grid<bool> = input
        .lines()
        .enumerate()
        .flat_map(|(iter_y, l)| {
            l.chars()
                .enumerate()
                .map(|(iter_x, c)| {
                    if c == '^' {
                        init_state.x = iter_x as i32;
                        init_state.y = iter_y as i32;
                    }
                    return ((iter_y as i32, iter_x as i32), c == '#');
                })
                .collect::<Vec<_>>()
        })
        .collect();
    let init_state = init_state; // Remove `mut`

    let disp = grid.map(|b| if *b { '#' } else { '.' });
    // Part 1
    let (visited, _has_loop) = simulate(&grid, init_state);
    let visited_locations: HashSet<(i32, i32)> =
        visited.iter().map(|state| (state.y, state.x)).collect();
    println!("Part 1: {:?}", visited_locations.len());

    let mut possible_obstacle_locations: HashSet<(i32, i32)> = HashSet::new();
    for state in visited {
        if state.pos() != init_state.pos() {
            possible_obstacle_locations.insert(state.pos());
        }
    }

    println!(
        "Need to check {} locations for part 2",
        possible_obstacle_locations.len()
    );

    let loop_count = possible_obstacle_locations
        .into_iter()
        .progress()
        .filter(|(y, x)| {
            let mut grid = grid.clone();
            grid[(*y, *x)] = true;
            let (_, has_loop) = simulate(&grid, init_state);
            has_loop
        })
        .count();

    println!("Part 2: {loop_count}");

    Ok(())
}

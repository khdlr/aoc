use std::collections::HashSet;
use std::error::Error;
use std::{env, fs::read_to_string};
use tqdm::tqdm;

type Grid = Vec<Vec<char>>;

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
enum Dir {N, E, S, W}
const DIRS: [Dir; 4] = [Dir::N, Dir::E, Dir::S, Dir::W];

pub fn parse_grid(inp: &str) -> Grid {
    inp.lines().map(|line| line.chars().collect()).collect()
}

struct LightGrid<'a> {
    grid: &'a Grid,
    visited: HashSet<(i32, i32, Dir)>
}

impl<'a> LightGrid<'a> {
    pub fn new(grid: &'a Grid) -> Self {
        Self { grid, visited: HashSet::new() }
    }

    pub fn print(&self) {
        for row in self.grid {
            let line: String = row.iter().collect::<String>();
            println!("{}", line);
        }
    }

    pub fn print_visited(&self) {
        for y in 0..self.grid.len() {
            let y = y as i32;
            for x in 0..self.grid[0].len() {
                let x = x as i32;
                let h = self.visited.contains(&(y, x, Dir::W)) || self.visited.contains(&(y, x, Dir::E));
                let v = self.visited.contains(&(y, x, Dir::N)) || self.visited.contains(&(y, x, Dir::S));
                match (h, v) {
                    (true, true) => print!("┼"),
                    (true, false) => print!("─"),
                    (false, true) => print!("│"),
                    (false, false) => print!(" "),
                }
            }
            println!();
        }
    }

    pub fn total_energy(&self) -> usize {
        let mut energy = 0;
        for y in 0..self.grid.len() {
            for x in 0..self.grid[0].len() {
                let visited = DIRS.iter().any(|&d| self.visited.contains(&(y as i32,x as i32,d)));
                if visited {
                    energy += 1;
                }
            }
        }
        energy
    }

    fn get(&self, y: i32, x: i32) -> Option<char> {
        if y < 0 || x < 0 {
            None
        } else {
            self.grid.get(y as usize).cloned()?.get(x as usize).cloned()
        }
    }

    pub fn trace_beam(&mut self, y: i32, x: i32, dir: Dir) {
        let Some(element) = self.get(y, x) else {
            return;
        };
        let tag = (y, x, dir);
        if self.visited.contains(&tag) {
            // Been here, done that;
            return;
        }
        self.visited.insert(tag);
        match (element, dir) {
            ('/', Dir::N) => { self.trace_beam(y, x+1, Dir::E) },
            ('/', Dir::E) => { self.trace_beam(y-1, x, Dir::N) },
            ('/', Dir::S) => { self.trace_beam(y, x-1, Dir::W) },
            ('/', Dir::W) => { self.trace_beam(y+1, x, Dir::S) },
            ('\\', Dir::N) => { self.trace_beam(y, x-1, Dir::W) },
            ('\\', Dir::W) => { self.trace_beam(y-1, x, Dir::N) },
            ('\\', Dir::S) => { self.trace_beam(y, x+1, Dir::E) },
            ('\\', Dir::E) => { self.trace_beam(y+1, x, Dir::S) },
            ('|', Dir::E) | ('|', Dir::W) => { self.trace_beam(y-1, x, Dir::N); self.trace_beam(y+1, x, Dir::S) },
            ('-', Dir::N) | ('-', Dir::S) => { self.trace_beam(y, x+1, Dir::E); self.trace_beam(y, x-1, Dir::W) },
            (_, Dir::N) => { self.trace_beam(y-1, x, Dir::N) }, // NOOP
            (_, Dir::E) => { self.trace_beam(y, x+1, Dir::E) }, // NOOP
            (_, Dir::S) => { self.trace_beam(y+1, x, Dir::S) }, // NOOP
            (_, Dir::W) => { self.trace_beam(y, x-1, Dir::W) }, // NOOP
        }
    }
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let base_grid: Grid = parse_grid(&input);
    let mut grid = LightGrid::new(&base_grid);

    grid.trace_beam(0, 0, Dir::E);
    grid.print();
    grid.print_visited();
    println!("Task 1: {}", grid.total_energy());

    println!("Task 2...");
    let mut best = 0;
    let w = base_grid[0].len() as i32;
    let h = base_grid.len() as i32;

    for y in tqdm(0..h-1) {
        let mut grid = LightGrid::new(&base_grid);
        grid.trace_beam(y, 0, Dir::E);
        best = best.max(grid.total_energy());

        let mut grid = LightGrid::new(&base_grid);
        grid.trace_beam(y, w-1, Dir::W);
        best = best.max(grid.total_energy());
    }

    for x in tqdm(0..w-1) {
        let mut grid = LightGrid::new(&base_grid);
        grid.trace_beam(0, x, Dir::S);
        best = best.max(grid.total_energy());

        let mut grid = LightGrid::new(&base_grid);
        grid.trace_beam(h-1, x, Dir::N);
        best = best.max(grid.total_energy());
    }

    println!("{best}");

    Ok(())
}

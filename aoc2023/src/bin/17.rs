use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};
use std::error::Error;
use std::{env, fs::read_to_string};

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
enum Dir {N, E, S, W}
const DIRS: [Dir; 4] = [Dir::N, Dir::E, Dir::S, Dir::W];

impl Dir {
    pub fn opposite(&self) -> Dir {
        match &self {
            Dir::N => Dir::S,
            Dir::S => Dir::N,
            Dir::E => Dir::W,
            Dir::W => Dir::E,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct State {
    pos: (i32, i32),
    dir: Dir,
    rep: usize,
    cost: usize
}

impl State {
    pub fn tag(&self) -> (i32, i32, Dir, usize) {
        (self.pos.0, self.pos.1, self.dir, self.rep)
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost).then_with(||
            (self.pos.0 + self.pos.1).cmp(&(other.pos.0 + other.pos.1)))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub struct Grid {
    grid: Vec<Vec<usize>>,

}

impl Grid {
    pub fn parse(str: &str) -> Self {
        let grid = str.lines().map(|line| line.chars().map(
                |c| c.to_string().parse::<usize>().unwrap()
        ).collect()).collect();
        Self { grid }
    }

    pub fn print(&self) {
        for row in &self.grid {
            let line: String = row.iter().map(|c| c.to_string()).collect::<String>();
            println!("{}", line);
        }
    }

    fn get(&self, pos: (i32, i32)) -> Option<usize> {
        let (y, x) = pos;
        if y < 0 || x < 0 {
            None
        } else {
            self.grid.get(y as usize).cloned()?.get(x as usize).cloned()
        }
    }

    pub fn height(&self) -> i32 {
        self.grid.len() as i32
    }

    pub fn width(&self) -> i32 {
        self.grid[0].len() as i32
    }

    pub fn print_overlay(&self, fun: impl Fn((i32, i32)) -> Option<char>) {
        for i in 0..self.height() {
            for j in 0..self.width() {
                match fun((i, j)) {
                    Some(chr) => print!("{}", chr),
                    None => print!("{}", self.get((i, j)).unwrap()),
                }
            }
            println!();
        }
    }
}

pub fn solve(grid: &Grid, min_reps: usize, max_reps: usize) {
    let mut queue = BinaryHeap::new();
    let mut dist: HashMap<(i32, i32, Dir, usize), usize> = HashMap::new();
    let mut parent: HashMap<(i32, i32, Dir, usize), (i32, i32, Dir, usize)> = HashMap::new();

    let start1 = State { dir: Dir::S, rep: 0, cost: 0, pos: (0, 0) };
    let start2 = State { dir: Dir::E, rep: 0, cost: 0, pos: (0, 0) };
    let goal = (grid.height() - 1, grid.width() - 1);
    dist.insert(start1.tag(), 0);
    dist.insert(start2.tag(), 0);
    queue.push(start1);
    queue.push(start2);

    let mut final_state = start1;
    while let Some(state) = queue.pop() {
        let State { pos, rep, dir, cost } = state;
        let (y, x) = pos;
        if pos == goal { 
            final_state = state;
            break;
        }
        if cost > dist.get(&state.tag()).cloned().unwrap_or(usize::MAX) { continue; }
        for next_dir in DIRS.clone() {
            if next_dir == dir.opposite() { continue; }
            if next_dir == dir && rep >= max_reps { continue; }
            if next_dir != dir && rep < min_reps { continue; }
            let next_pos = match &next_dir {
                Dir::N => (y-1, x),
                Dir::S => (y+1, x),
                Dir::E => (y, x+1),
                Dir::W => (y, x-1),
            };
            let Some(cell_cost) = grid.get(next_pos) else { continue; };
            let next_rep = if next_dir == dir {
                rep + 1
            } else {
                0
            };
            if next_rep == 0 && rep < min_reps { continue; }
            if next_rep >= max_reps { continue; }
            let next = State {
                cost: cost + cell_cost,
                dir: next_dir,
                pos: next_pos,
                rep: next_rep,
            };
            if next.cost < dist.get(&next.tag()).cloned().unwrap_or(usize::MAX) {
                queue.push(next);
                dist.insert(next.tag(), next.cost);
                parent.insert(next.tag(), state.tag());
            }
        }
    }
    let mut chain = Vec::new();
    let mut current = final_state.tag();
    while parent.contains_key(&current) {
        chain.push((current.0, current.1));
        current = parent[&current];
    }
    grid.print_overlay(|pos| chain.contains(&pos).then_some('#'));
    println!("Finished!");
    println!("Found: {}", final_state.cost);
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let grid: Grid = Grid::parse(&input);

    solve(&grid, 0, 3);
    solve(&grid, 3, 10);

    Ok(())
}

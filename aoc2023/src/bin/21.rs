use std::error::Error;
use std::{env, fs::read_to_string};

use aoc2023::grid::Grid;
use itertools::iterate;


#[derive(Eq, PartialEq, Clone, Copy)]
struct Cell {
    wall: bool,
    n: usize,
    w: usize,
    e: usize,
    s: usize,
    nw: usize,
    ne: usize,
    sw: usize,
    se: usize,
    i: usize,
}

impl Cell {
    fn empty() -> Cell {
        Cell { wall: false, n: 0, w: 0, e: 0, s: 0, nw: 0, ne: 0, sw: 0, se: 0, i: 0 }
    }

    fn wall() -> Cell {
        Cell { wall: true, n: 0, w: 0, e: 0, s: 0, nw: 0, ne: 0, sw: 0, se: 0, i: 0 }
    }

    fn start() -> Cell {
        Cell { wall: false, n: 1, w: 1, e: 1, s: 1, nw: 1, ne: 1, sw: 1, se: 1, i: 0 }
    }

    fn count(&self) -> usize {
        self.n + self.w + self.e + self.s + self.nw + self.ne + self.sw + self.se + self.i
    }

    fn disp(&self) -> char {
        if self.wall {
            '█'
        } else {
            let sum = self.count();
            let str = format!("{}", sum).chars().collect::<Vec<char>>();
            if sum == 0 {
                ' '
            } else if str.len() == 1 {
                str[0]
            } else {
                '∞'
            }
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum State {
    Wall, Visit, Empty
}

impl State {
    fn disp(&self) -> char {
        match *self {
            State::Wall => '█',
            State::Visit => '*',
            State::Empty => ' ',
        }
    }

    fn map(&self, fun: impl Fn(State) -> State) -> State {
        match *self {
            State::Wall => State::Wall,
            e => fun(e)
        }
    }
}

fn step(grid: &Grid<State>) -> Grid<State> {
    grid.map_with_neighbors(|el, nb| {
        let any = nb.iter().any(|c| *c == State::Visit);
        match (el, any) {
            (State::Wall, _) => State::Wall,
            (_, true) => State::Visit,
            _ => State::Empty,
        }
    })
}

fn crossed_tiles(square_steps: usize) -> (usize, usize) {
    let n = square_steps;

    let nh_c = n/2;
    let tiles_0 = nh_c*nh_c;

    let nh = (n-1)/2;
    let tiles_1 = nh*(nh+1);
    (tiles_0, tiles_1)
}

fn count_quadrant(grid: &Grid<State>, n_steps: usize, t: usize, full_counts: (usize, usize), starting_point: (i32, i32)) -> usize {
    let h = grid.height();
    let r = (n_steps - t) / h as usize;
    let tiles_crossed = crossed_tiles(r);

    let steps_left = (n_steps - t) % h as usize;
    // Now, we have r+1 tiles with steps_left steps taken
    // and r tiles with h+steps_left steps taken
    let mut partial = grid.map(|s| s.map(|_| State::Empty));
    let partial_visited = {
        partial[starting_point] = State::Visit;
        let mut res = (0, 0);
        if steps_left > 0 {
            for _ in 0..steps_left {
                partial = step(&partial);
            }
            res.0 = partial.iter().filter(|&&el| el == State::Visit).count();
        }
        for _ in 0..h {
            partial = step(&partial);
        }
        res.1 = partial.iter().filter(|&&el| el == State::Visit).count();
        res.0 * (r+1) + res.1 * r
    };

    partial_visited + tiles_crossed.0 * full_counts.0 + tiles_crossed.1 * full_counts.1

}

fn count_line(grid: &Grid<State>, n_steps: usize, t: usize, full_counts: (usize, usize)) {

}

fn count(input: &Grid<State>, n_steps: usize) -> usize {
    let empty = input.map(|s| s.map(|_| State::Empty));
    let mut grid = input.clone();
    let mut t = 0;
    let (mut t_nw, mut t_ne, mut t_sw, mut t_se) = (0, 0, 0, 0);
    let h = grid.height();
    let w = grid.width();

    while [t_nw, t_ne, t_sw, t_se].iter().any(|i| *i == 0) {
        grid = step(&grid);
        t += 1;
        if t_nw == 0 && grid[(0, 0)] == State::Visit {
            t_nw = t;
        }
        if t_ne == 0 && grid[(0, w-1)] == State::Visit {
            t_ne = t;
        }
        if t_sw == 0 && grid[(h-1, 0)] == State::Visit {
            t_sw = t;
        }
        if t_se == 0 && grid[(h-1, w-1)] == State::Visit {
            t_se = t;
        }
    }
    let mut count = (0, 0);
    if (n_steps - t) % 2 == 0 {
        count.0 = grid.iter().filter(|&&e| e == State::Visit).count();
    } else {
        count.1 = grid.iter().filter(|&&e| e == State::Visit).count();
    }
    grid = step(&grid);
    t += 1;
    if (n_steps - t) % 2 == 0 {
        count.0 = grid.iter().filter(|&&e| e == State::Visit).count();
    } else {
        count.1 = grid.iter().filter(|&&e| e == State::Visit).count();
    }

    assert!(h == w);
    // Quadrants
    let nw = count_quadrant(&input, n_steps, t_nw, count, (h-1, w-1));
    let ne = count_quadrant(&input, n_steps, t_ne, count, (h-1,   0));
    let sw = count_quadrant(&input, n_steps, t_sw, count, (  0, w-1));
    let se = count_quadrant(&input, n_steps, t_se, count, (  0,   0));

    // Lines
    let n = count_line(&input, n_steps, t_nw.min(t_ne), count);

    nw + ne + sw + se + count.0
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let input: Grid<char> = Grid::from_str(&input);
    let input = input.map(|c| match *c {
        '#' => State::Wall,
        '.' => State::Empty,
        'S' => State::Visit,
        _ => panic!(),
    });
    input.print_map(|c| c.disp());

    let shouldbe = [
        // (6, 16),
        // (10, 50),
        (50, 1594),
        (100, 6536),
        (500, 167004),
        (1000, 668697),
        (5000, 16733044)];

    for (i, tgt) in shouldbe {
        let num = count(&input, i);
        let mark = if num == tgt { '✓' } else { '✗' };
        println!("{mark} After {i:>4} steps, can reach {num:>8} tiles (should be {tgt:>8})");
    }

    Ok(())
}

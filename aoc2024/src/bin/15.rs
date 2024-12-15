use std::error::Error;
use std::fmt::Display;
use std::thread::sleep;
use std::time::Duration;
use std::{env, fs::read_to_string};

use aoc2024::grid::Grid;
use aoc2024::utils::Dir;

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
enum Field {
    Empty,
    Wall,
    Robo,
    Box,
    BoxLeft,
    BoxRight,
}

impl Field {
    fn from_char(chr: &char) -> Self {
        match chr {
            '.' => Field::Empty,
            '#' => Field::Wall,
            '@' => Field::Robo,
            'O' => Field::Box,
            '[' => Field::BoxLeft,
            ']' => Field::BoxRight,
            _ => panic!(""),
        }
    }
}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Field::Empty => " ",
                Field::Wall => "█",
                Field::Robo => "🯅",
                Field::Box => "🯰",
                Field::BoxLeft => "📦",
                Field::BoxRight => "",
            }
        )
    }
}

#[derive(Clone)]
struct State {
    grid: Grid<Field>,
    robo: (i32, i32),
}

impl State {
    fn new(grid: Grid<Field>) -> Self {
        let mut robo = (0, 0);
        for (loc, c) in grid.iter().with_pos() {
            if *c == Field::Robo {
                robo = loc;
            }
        }

        Self { grid, robo }
    }

    fn try_push(&self, pos: (i32, i32), dir: Dir) -> Option<State> {
        // try to push object at `pos` in direction `dir`
        let obj = self.grid[pos];
        match obj {
            Field::Empty | Field::Wall => return None,
            _ => {}
        };

        let check = dir.apply(pos);
        let (y, x) = check;
        let pre_push = match self.grid[check] {
            Field::Empty => Some(self.clone()),
            Field::Wall => None,
            Field::Robo => panic!(""),
            Field::Box => self.try_push(check, dir),
            Field::BoxLeft => self.try_push(check, dir).and_then(|st| {
                if dir == Dir::E {
                    Some(st)
                } else {
                    st.try_push((y, x + 1), dir)
                }
            }),
            Field::BoxRight => self.try_push(check, dir).and_then(|st| {
                if dir == Dir::W {
                    Some(st)
                } else {
                    st.try_push((y, x - 1), dir)
                }
            }),
        };
        pre_push.and_then(|mut st| {
            st.grid[pos] = Field::Empty;
            st.grid[check] = obj;
            if obj == Field::Robo {
                st.robo = check;
            }
            Some(st)
        })
    }

    fn step(&self, dir: Dir) -> State {
        self.try_push(self.robo, dir)
            .unwrap_or_else(|| self.clone())
        /*
                match self.grid[trypos] {
                    Field::Empty => {
                        let mut grid = self.grid.clone();
                        grid[self.robo] = Field::Empty;
                        grid[trypos] = Field::Robo;
                        Self { grid, robo: trypos }
                    }
                    Field::Wall => self.clone(),
                    Field::Robo => panic!("Robo shouldn't be able to move into itself!"),
                    Field::Box => {
                        let mut num_boxes = 1;
                        let mut checkpos = trypos;
                        while self.grid[checkpos] == Field::Box {
                            checkpos = dir.apply(checkpos);
                            num_boxes += 1;
                        }
                        match self.grid[checkpos] {
                            Field::Empty => {
                                let mut grid = self.grid.clone();
                                grid[self.robo] = Field::Empty;
                                grid[trypos] = Field::Robo;
                                let mut setpos = trypos;
                                for _ in 1..num_boxes {
                                    setpos = dir.apply(setpos);
                                    grid[setpos] = Field::Box
                                }

                                Self { grid, robo: trypos }
                            }
                            Field::Wall => self.clone(),
                            Field::Robo => panic!("Robo shouldn't be able to push boxes into itself!"),
                            Field::Box | Field::BoxLeft | Field::BoxRight => panic!("Box loop is broken"),
                        }
                    }
                    Field::BoxLeft => todo!(),
                    Field::BoxRight => todo!(),
                }
        */
    }

    fn show(&self) {
        self.grid.print()
    }

    fn score(&self) -> i32 {
        self.grid
            .iter()
            .with_pos()
            .map(|((y, x), f)| match f {
                Field::Box | Field::BoxLeft => 100 * y + x,
                _ => 0,
            })
            .sum()
    }
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;

    let mut lines = input.lines().peekable();

    let grid_lines: Vec<String> = (&mut lines)
        .take_while(|l| *l != "")
        .map(|s| s.to_owned())
        .collect();
    let grid: Grid<Field> = grid_lines.iter().collect::<Grid<_>>().map(Field::from_char);
    let grid2: Grid<Field> = grid_lines
        .iter()
        .map(|row| {
            row.chars()
                .map(|c| match c {
                    '#' => "##",
                    '@' => "@.",
                    'O' => "[]",
                    '.' => "..",
                    _ => panic!(),
                })
                .collect::<String>()
        })
        .collect::<Grid<_>>()
        .map(Field::from_char);

    let mut state = State::new(grid);

    let moves: Vec<Dir> = lines
        .collect::<String>()
        .replace("\n", "")
        .chars()
        .map(Dir::from_arrow)
        .collect();

    state.show();
    for mov in &moves {
        state = state.step(*mov);
        // println!("Move: {mov}");
        // state.show();
        // sleep(Duration::from_millis(10));
    }

    println!("Part 1: {}", state.score());

    let mut state2 = State::new(grid2);
    for mov in &moves {
        state2 = state2.step(*mov);
    }
    state2.show();

    println!("Part 2: {}", state2.score());

    Ok(())
}

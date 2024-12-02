use std::error::Error;
use std::{env, fs::read_to_string};
use std::fmt::{Debug, Display};
use tqdm::tqdm;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum State {
    Empty, Filled, Any
}

impl Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            State::Empty => '.',
            State::Filled => '#',
            State::Any => '?',
        })
    }
}


impl From<char> for State {
    fn from(value: char) -> Self {
        match value {
            '.' => State::Empty,
            '#' => State::Filled,
            '?' => State::Any,
            _ => panic!("Illegal state: `{value}`")
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Entry {
    states: Vec<State>,
    constraints: Vec<usize>,
}

impl From<&str> for Entry {
    fn from(value: &str) -> Self {
        let mut parts = value.split(' ');
        let states: Vec<State> = parts.next().expect("No states part in line").chars().map(State::from).collect();
        let constraints: Vec<usize> = parts.next().expect("No constraints part in line").split(',').map(|c| c.parse().unwrap()).collect();

        Self { states, constraints }
    }
}

impl Entry {
    fn unfold(&self) -> Entry {
        let states: Vec<State> = (0..5).map(|_| &self.states).cloned().reduce(|acc, snd| {
            let mut acc = acc.clone();
            let mut snd = snd.clone();
            acc.push(State::Any);
            acc.append(&mut snd);
            acc
        }).unwrap().to_vec();
        let constraints: Vec<usize> = (0..5).map(|_| &self.constraints).cloned().reduce(|acc, snd| {
            let mut acc = acc.clone();
            let mut snd = snd.clone();
            acc.append(&mut snd);
            acc
        }).unwrap().to_vec();

        Entry { states, constraints }
    }
}

struct Trellis {
    lens: Vec<usize>,
    states: Vec<State>,
    entries: Vec<Vec<usize>>,
}

impl From<Entry> for Trellis {
    fn from(value: Entry) -> Self {
        Self::new(value.constraints, value.states)
    }
}

impl Trellis {
    pub fn new(lens: Vec<usize>, states: Vec<State>) -> Self {
        let entries = vec![vec![0; states.len()]; lens.len()];
        Self { lens, states, entries }
    }

    pub fn get_relative(&self, y: usize, x: usize, dy: i64, dx: i64) -> usize {
        let y = (y as i64) + dy;
        let x = (x as i64) + dx;
        if y < 0 {
            for tx in 0..=x {
                if self.states[tx as usize] == State::Filled {
                    return 0;
                }
            }
            return 1;
        } else if x < 0 {
            return 0;
        } else {
            let y: usize = y.try_into().expect(&format!("Error indexing at ({y}, {x})"));
            let x: usize = x.try_into().expect(&format!("Error indexing at ({y}, {x})"));
            self.entries[y][x]
        }
    }

    fn can_place(&self, end: usize, len: usize) -> bool {
        if end+1 < len {
            return false;
        }
        if end > self.states.len() {
            return false;
        }
        for tx in end+1-len..=end {
            if self.states[tx] == State::Empty {
                return false;
            }
        }
        if end >= len && self.states[end-len] == State::Filled {
            return false;
        }
        if end+1 < self.states.len() && self.states[end+1] == State::Filled {
            return false;
        }
        return true;
    }

    pub fn solve(&mut self) -> usize {
        for (x, state) in self.states.iter().cloned().enumerate() {
            for (y, len) in self.lens.iter().cloned().enumerate() {
                // self.entries[y][x] is the number of ways to end up in a state
                // where range <y> has been placed by the end of token <x>,
                // but range <y+1> has not been placed yet.
                self.entries[y][x] = 0;
                if state == State::Filled || state == State::Any {
                    if self.can_place(x, len) {
                        self.entries[y][x] += self.get_relative(y, x, -1, -(len as i64)-1);
                    }
                }
                if state == State::Empty || state == State::Any {
                    self.entries[y][x] += self.get_relative(y, x, 0, -1);
                }
            }
        }

        let res = self.entries[self.lens.len()-1][self.states.len()-1];
        assert!(res != 0);
        res
    }
}

impl Display for Trellis {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "     ")?;
        for x in 0..self.states.len() {
            write!(f, "{:>4} ", format!("[{}]", self.states[x]))?;
        }
        writeln!(f)?;
        write!(f, "     ")?;
        for x in 0..self.states.len() {
            write!(f, " ({:}) ", self.get_relative(0, x, -1, 0))?;
        }
        writeln!(f, "")?;
        for y in 0..self.lens.len() {
            write!(f, "{:>4} ", format!("[{}]", self.lens[y]))?;
            for x in 0..self.states.len() {
                write!(f, "{:>4} ", self.entries[y][x])?;
            }
            writeln!(f, "")?;
        }
        Ok(())
    }
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let entries: Vec<Entry> = input.lines().map(Entry::from).collect();

    let sum: usize = entries.iter().cloned()
        .map(|c| Trellis::from(c).solve()).sum();
    println!("Task 1: {sum}");

    let sum: usize = entries.iter().cloned()
        .map(|c| Trellis::from(c.unfold()).solve()).sum();
    println!("Task 2: {sum}");

    // for sol in entries.iter().cloned().map(|c| Trellis::from(c).solve()) {
    //     println!("{sol}");
    // }
    // let mut trellis = Trellis::from(entries[998].clone());
    // trellis.solve();
    // println!("{trellis}");


    Ok(())
}

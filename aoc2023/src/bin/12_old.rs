use std::error::Error;
use std::{env, fs::read_to_string};
use std::fmt::Debug;
use tqdm::tqdm;

#[derive(Copy, Clone, Eq, PartialEq)]
enum State {
    Empty, Filled, Any
}

impl Debug for State {
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
    fn iter(&self) -> EntryIter {
        let mut starts = Vec::new();
        let mut lens = Vec::new();
        let mut i = 0;
        for len in &self.constraints {
            starts.push(i);
            lens.push(*len);
            i += 1 + len;
        }

        let longest_segment = *self.constraints.iter().max().unwrap();
        let mut possible_starts: Vec<Vec<usize>> = vec![vec![]; longest_segment+1];
        for i in 0..self.states.len() {
            if i > 0 && self.states[i-1] == State::Filled {
                continue;
            }
            for j in i..(i+longest_segment).min(self.states.len()) {
                if self.states[j] == State::Empty {
                    break;
                }
                let len = j-i+1;
                possible_starts[len].push(i);
            }
        }
        EntryIter { entry: self, starts, lens, min_len: i-1, len: self.states.len() as usize, first: true, possible_starts }
    }

    fn valid_assignment(&self, assignment: &Vec<State>) -> bool {
        self.states.iter().zip(assignment.iter()).all(|(s, a)| *s == *a || *s == State::Any)
    }

    fn iter_valid(&self) -> impl Iterator + '_ {
        self.iter().filter(|a| self.valid_assignment(a))
    }

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

#[derive(Debug)]
struct EntryIter<'a> {
    entry: &'a Entry,
    starts: Vec<usize>,
    lens: Vec<usize>,
    min_len: usize,
    len: usize,
    first: bool,
    possible_starts: Vec<Vec<usize>>
}

impl Iterator for EntryIter<'_> {
    type Item = Vec<State>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.first {
            self.first = false;
            return Some(self.make_state());
        }

        let mut push_idx = self.starts.len() - 1;
        let mut current_end = self.starts[push_idx] + self.lens[push_idx];
        while push_idx > 0 && ((current_end >= self.len) ||
            ((push_idx < self.starts.len() - 1) && (current_end + 1 >= self.starts[push_idx+1]))) {
                push_idx -= 1;
                current_end = self.starts[push_idx] + self.lens[push_idx];
        }

        if push_idx == 0 && self.starts[0] + self.min_len >= self.len {
            return None;
        }

        let mut last_stop: usize = 0;
        for (i, s) in self.starts.iter_mut().enumerate() {
            if i == push_idx {
                *s += 1;
            } else if i >= push_idx {
                *s = last_stop;
            }
            last_stop = *s + self.lens[i] + 1;
        }

        return Some(self.make_state());
    }
}

impl EntryIter<'_> {
    fn make_state(&self) -> Vec<State> {
        let mut state = self.entry.states.iter().enumerate().map(|_| State::Empty).collect::<Vec<_>>();
        for (start, len) in self.starts.iter().zip(self.lens.iter()) {
            state[*start..*start+*len].fill(State::Filled);
        }
        state
    }
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let entries: Vec<Entry> = input.lines().map(Entry::from).collect();

    let task1 = entries.iter().map(|e| e.iter_valid().count()).sum::<usize>();
    println!("Task 1: {task1}");

    for sol in entries.iter().map(|e| e.iter_valid().count()) {
        println!("{sol}");
    }


    // let task2 = tqdm(entries.iter()).map(|e| e.unfold().iter_valid().count()).sum::<usize>();
    // println!("Task 2: {task2}");

    Ok(())
}

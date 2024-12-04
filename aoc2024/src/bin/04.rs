use std::collections::HashMap;
use std::error::Error;
use std::{env, fs::read_to_string};

const DIRECTIONS: [(i32, i32); 8] = [
    (0, 1),
    (0, -1),
    (1, 0),
    (-1, 0),
    (1, 1),
    (1, -1),
    (-1, 1),
    (-1, -1),
];
const LETTERS: [char; 4] = ['X', 'M', 'A', 'S'];

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;

    let mut starts = HashMap::<char, Vec<(i32, i32)>>::new();
    let chars: HashMap<(i32, i32), char> = input
        .lines()
        .enumerate()
        .flat_map(|(y, l)| {
            let y = y as i32;
            l.chars()
                .enumerate()
                .map(|(x, c)| {
                    let x = x as i32;
                    starts.entry(c).or_default().push((y, x));
                    ((y, x), c)
                })
                .collect::<Vec<_>>()
        })
        .collect();

    let mut count = 0;
    for (start_y, start_x) in &starts[&'X'] {
        for (dy, dx) in &DIRECTIONS {
            let (mut y, mut x) = (*start_y, *start_x);
            for i in 1..=3 {
                y += dy;
                x += dx;
                match chars.get(&(y, x)) {
                    Some(val) if *val == LETTERS[i] => {}
                    _ => break,
                }
                if i == 3 {
                    count += 1;
                }
            }
        }
    }

    println!("Part 1: {count}");

    // Part 2
    let mut count = 0;
    for (y, x) in &starts[&'A'] {
        let nw = chars.get(&(y - 1, x - 1));
        let ne = chars.get(&(y - 1, x + 1));
        let sw = chars.get(&(y + 1, x - 1));
        let se = chars.get(&(y + 1, x + 1));
        let diag1 = match (nw, se) {
            (Some('M'), Some('S')) => true,
            (Some('S'), Some('M')) => true,
            _ => false,
        };
        let diag2 = match (sw, ne) {
            (Some('M'), Some('S')) => true,
            (Some('S'), Some('M')) => true,
            _ => false,
        };
        if diag1 && diag2 {
            count += 1
        }
    }

    println!("Part 2: {}", count);

    Ok(())
}

use std::collections::HashSet;
use std::error::Error;
use std::{env, fs::read_to_string};

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let line = input.lines().next().expect("No lines in input");
    let mut y = 0;
    let mut x = 0;
    let mut visited: HashSet<(i32, i32)> = HashSet::from([(0, 0)]);

    for mov in line.chars() {
        let (dy, dx) = match mov {
            'v' => (1, 0),
            '^' => (-1, 0),
            '>' => (0, 1),
            '<' => (0, -1),
            c => panic!("Unknown char: `{c}`"),
        };
        y += dy;
        x += dx;
        visited.insert((y, x));
    }

    println!("Part 1: {}", visited.len());

    let mut r_y = 0;
    let mut r_x = 0;
    let mut s_y = 0;
    let mut s_x = 0;
    let mut visited: HashSet<(i32, i32)> = HashSet::from([(0, 0)]);
    for (i, mov) in line.chars().enumerate() {
        let (dy, dx) = match mov {
            'v' => (1, 0),
            '^' => (-1, 0),
            '>' => (0, 1),
            '<' => (0, -1),
            c => panic!("Unknown char: `{c}`"),
        };
        if i % 2 == 0 {
            s_y += dy;
            s_x += dx;
            visited.insert((s_y, s_x));
        } else {
            r_y += dy;
            r_x += dx;
            visited.insert((r_y, r_x));
        }
    }
    println!("Part 2: {}", visited.len());

    Ok(())
}

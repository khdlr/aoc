use std::collections::HashMap;
use std::error::Error;
use std::{env, fs::read_to_string};

type Point = (i32, i32);
type Network = HashMap<Point, (Point, Point)>;

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let rows = input.lines().map(|line| line.chars().collect::<Vec<_>>()).collect::<Vec<_>>();
    let cells = input.lines().enumerate().flat_map(|(row, line)| {
        line.chars()
            .enumerate()
            .map(move |(col, c)| (row as i32, col as i32, c.clone()))
    }).collect::<Vec<_>>();

    let conn = cells.iter().cloned().filter_map(|(y, x, c)| {
        let nb = match c {
            '-' => Some(((y, x-1), (y, x+1))),
            '|' => Some(((y-1, x), (y+1, x))),
            'L' => Some(((y-1, x), (y, x+1))),
            'F' => Some(((y, x+1), (y+1, x))),
            'J' => Some(((y, x-1), (y-1, x))),
            '7' => Some(((y, x-1), (y+1, x))),
            _ => None,
        };
        nb.map(|nb| ((y, x), nb))
    }).collect::<Network>();

    let start = cells.iter().find(|(_, _, c)| *c == 'S').unwrap();
    let start = (start.0, start.1);
    println!("start: {start:?}");

    let mut last = start.clone();
    let mut current = [
        (last.0+1, last.1),
        (last.0-1, last.1),
        (last.0, last.1-1),
        (last.0, last.1+1)
    ].into_iter().find(|c| conn[&c].0 == last || conn[&c].1 == last).unwrap();

    let mut grid: Vec<Vec<char>> = rows.iter().map(|row| row.iter().map(|c| match *c {
        _ => '.',
    }).collect()).collect();

    let mut steps = 1;
    grid[start.0 as usize][start.1 as usize] = 'P';
    while current != start {
        grid[current.0 as usize][current.1 as usize] = 'P';
        let nb = conn[&current];
        if nb.0 == last {
            last = current;
            current = nb.1;
        } else {
            last = current;
            current = nb.0;
        }
        steps += 1;
    }
    println!("Steps: {steps}");
    println!("=> Task 1: {}", steps/2);

    let mut inner_points = 0;
    for (y, row) in grid.iter_mut().enumerate() {
        let mut crossings = 0;
        for (x, chr) in row.iter_mut().enumerate() {
            let xing = rows[y][x];
            if *chr == 'P' {
                if xing == '|' || xing == 'L' || xing == 'J' {
                    crossings += 1
                }
            } else if *chr == '.' {
                if crossings % 2 == 0 {
                    *chr = 'O';
                } else {
                    *chr = 'I';
                    inner_points += 1;
                }
            }
        }
    }
    println!("Task 2: {inner_points}");

    Ok(())
}

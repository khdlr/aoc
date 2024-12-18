use std::error::Error;
use std::{env, fs::read_to_string};

use aoc2015::grid::Grid;
use regex::Regex;

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let re = Regex::new(r"(turn on|toggle|turn off) (\d+),(\d+) through (\d+),(\d+)")
        .expect("Regex building error");

    let mut grid = Grid::filled(false, 1000, 1000);
    let mut grid2 = Grid::filled(0, 1000, 1000);
    for line in input.lines() {
        let (_, [command, x1, y1, x2, y2]) =
            re.captures(line).expect("Line doesn't match").extract();
        let [x1, y1, x2, y2] = [x1, y1, x2, y2]
            .into_iter()
            .map(|num| num.parse::<i32>().expect("Int parsing error"))
            .collect::<Vec<_>>()
            .try_into()
            .expect("Array unpacking error");

        for y in y1..=y2 {
            for x in x1..=x2 {
                match command {
                    "turn on" => {
                        grid[(y, x)] = true;
                        grid2[(y, x)] += 1;
                    }
                    "turn off" => {
                        grid[(y, x)] = false;
                        grid2[(y, x)] = (grid2[(y, x)] - 1).max(0);
                    }
                    "toggle" => {
                        grid[(y, x)] ^= true;
                        grid2[(y, x)] += 2;
                    }
                    _ => panic!("Unknown command!"),
                }
            }
        }
    }

    let part1 = grid.iter().filter(|c| **c).count();
    println!("Part 1: {}", part1);

    let part2: i64 = grid2.iter().sum();
    println!("Part 2: {}", part2);

    Ok(())
}

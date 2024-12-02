use std::error::Error;
use std::{env, fs::read_to_string};

use aoc2023::grid::Grid;

use regex::Regex;

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
enum Dir {N, E, S, W}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct Instruction {
    dir: Dir,
    num: usize,
    hex_dir: Dir,
    hex_num: usize,
}

impl Instruction {
    pub fn parse(str: &str) -> Instruction {
        let re = Regex::new(r"([RDLU]) (\d+) \(#([a-f0-9]{5})([a-f0-9])\)").unwrap();
        let caps = re.captures(str).expect(&format!("Malformed string: '{str}'"));
        let dir = match caps.get(1).unwrap().as_str() {
            "U" => Dir::N,
            "L" => Dir::W,
            "R" => Dir::E,
            "D" => Dir::S,
            e => panic!("Invalid dir: {}", e)
        };
        let num = caps.get(2).unwrap().as_str().parse::<usize>().unwrap();
        let hex_num = usize::from_str_radix(caps.get(3).unwrap().as_str(), 16).unwrap();
        let hex_dir = match caps.get(4).unwrap().as_str() {
            "3" => Dir::N,
            "2" => Dir::W,
            "0" => Dir::E,
            "1" => Dir::S,
            _ => panic!(),
        };

        Instruction { dir, num, hex_dir, hex_num }
    }

    pub fn offset(&self) -> (i32, i32) {
        match self.dir {
            Dir::N => (-(self.num as i32), 0),
            Dir::S => ((self.num as i32), 0),
            Dir::E => (0, (self.num as i32)),
            Dir::W => (0, -(self.num as i32)),
        }
    }
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let instructions: Vec<Instruction> = input.lines().map(Instruction::parse).collect();
    let mut miny = 0;
    let mut maxy = 0;
    let mut minx = 0;
    let mut maxx = 0;
    let mut y = 0;
    let mut x = 0;
    for i in &instructions {
        let (dy, dx) = i.offset();
        y += dy;
        x += dx;
        maxy = maxy.max(y);
        maxx = maxx.max(x);
        miny = miny.min(y);
        minx = minx.min(x);
    }
    let w = maxx - minx + 1;
    let h = maxy - miny + 1;
    let mut grid = Grid::<bool>::empty(h, w);
    let mut y = -miny;
    let mut x = -minx;
    println!("Y: {miny} -> {maxy}");
    println!("X: {minx} -> {maxx}");
    for i in &instructions {
        for _ in 0..i.num {
            match i.dir {
                Dir::N => y -= 1,
                Dir::S => y += 1,
                Dir::E => x += 1,
                Dir::W => x -= 1,
            }
            grid.set((y, x), true);
        }
    }
    grid.save_img("pre-fill.png",
                    |&f| if f { (0, 0, 0) } else { (255, 255, 255) });

    let mut count = 0;
    let mut true_area = 0;
    let mut true_border = 0;
    for y in 0..grid.height() {
        let mut inside = false;
        for x in 0..grid.width() {
            let original = grid.get((y, x)).unwrap();
            if y < grid.height() - 1 {
                let cell = grid.get((y, x)).unwrap();
                let next = grid.get((y+1, x)).unwrap();
                if cell && next {
                    inside = !inside;
                }
                if !cell && inside {
                    grid.set((y, x), true);
                }
            }
            if grid.get((y, x)).unwrap() {
                count += 1;
                if original {
                    true_border += 1;
                } else {
                    true_area += 1;
                }
            }
        }
    }
    grid.save_img("maze.png", |&f| if f { (0, 0, 0) } else { (255, 255, 255) });

    let mut area: i64 = 0;
    let mut border: i64 = 0;
    let mut x: i64 = 0;
    for i in &instructions {
        let num = i.num as i64;
        border += i.num as i64;
        match i.dir {
            Dir::N => {
                area -= x * num;
            }
            Dir::S => {
                area += x * num;
            }
            Dir::E => {
                x += num;
                // area += num;
            }
            Dir::W => {
                x -= num;
                // area += num;
            }
        };
    }
    let area = area + border/2 + 1;

    println!("Task 1:  {true_area} + {true_border} = {count}");
    println!("Task 1': {area}");

    let mut area: i64 = 0;
    let mut border: i64 = 0;
    let mut x: i64 = 0;
    for i in &instructions {
        let num = i.hex_num as i64;
        border += i.hex_num as i64;
        match i.hex_dir {
            Dir::N => {
                area -= x * num;
            }
            Dir::S => {
                area += x * num;
            }
            Dir::E => {
                x += num;
                // area += num;
            }
            Dir::W => {
                x -= num;
                // area += num;
            }
        };
    }
    let area = area + border/2 + 1;
    println!("Task 2: {area}");

    Ok(())
}

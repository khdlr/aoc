use std::error::Error;
use std::thread::sleep;
use std::time::Duration;
use std::{env, fs::read_to_string};

use aoc2024::grid::Grid;
use regex::Regex;

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
struct State {
    px: i32,
    py: i32,
    vx: i32,
    vy: i32,
}

impl State {}

fn parse(inp: &str) -> i32 {
    inp.parse::<i32>().expect("Parsing error")
}

fn parse_input(input: &str) -> Vec<State> {
    let re =
        Regex::new(r"p=([0-9]+),([0-9]+) v=(-?[0-9]+),(-?[0-9]+)").expect("Regex compile error");
    re.captures_iter(input)
        .map(|c| c.extract())
        .map(|(_, [px, py, vx, vy])| State {
            px: parse(px),
            py: parse(py),
            vx: parse(vx),
            vy: parse(vy),
        })
        .collect()
}

fn step_robots(states: &Vec<State>, n: i32, w: i32, h: i32) -> Vec<State> {
    states
        .iter()
        .map(|s| State {
            px: ((s.px + n * s.vx) % w + w) % w,
            py: ((s.py + n * s.vy) % h + h) % h,
            vy: s.vy,
            vx: s.vx,
        })
        .collect()
}

fn safety_factor(robos: &Vec<State>, w: i32, h: i32) -> i32 {
    let mut nw = 0;
    let mut ne = 0;
    let mut se = 0;
    let mut sw = 0;
    for r in robos {
        let sgn_x = (r.px - (w / 2)).signum();
        let sgn_y = (r.py - (h / 2)).signum();
        match (sgn_x, sgn_y) {
            (-1, -1) => nw += 1,
            (1, -1) => ne += 1,
            (-1, 1) => sw += 1,
            (1, 1) => se += 1,
            _ => {}
        }
    }
    nw * ne * sw * se
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let states = parse_input(&input);

    let w = 101;
    let h = 103;

    let robos1 = step_robots(&states, 100, w, h);

    // let g: Grid<char> = Grid::filled('.', 7, 11);
    // g.print_overlay(|(y, x)| {
    //     let count = robos1.iter().filter(|r| r.px == x && r.py == y).count();
    //     (count > 0)
    //         .then_some(count)
    //         .and_then(|c| format!("{c}").chars().next())
    // });

    let part1 = safety_factor(&robos1, w, h);

    println!("Part 1: {}", part1);

    // for time in (25..101 * 103).step_by(103) {
    for time in (1..101 * 103) {
        println!("{time}");
        let st = step_robots(&states, time, w, h);
        let mut g: Grid<bool> = Grid::filled(false, h, w);
        for r in &st {
            // if r.py < crop || r.py > h - crop || r.px < crop || r.px > w - crop {
            //     // continue 'outer;
            // }
            g[(r.py, r.px)] = true;
        }
        // g.print_braille();
        g.print_map(|&c| if c { '#' } else { '.' })
        // sleep(Duration::from_millis(50));
    }

    println!("Part 2: {}", "todo");
    Ok(())
}

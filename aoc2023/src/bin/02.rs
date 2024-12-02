use std::{env, fs::read_to_string};
use std::error::Error;
use derive_more::{Add,Sum};
use regex::Regex;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Add, Sum)]
struct Draw {
    red: u32,
    green: u32,
    blue: u32,
}

impl From<&str> for Draw {
    fn from(value: &str) -> Self {
        let mut res = Draw::default();
        for component in value.trim().split(", ") {
            let words: Vec<&str> = component.split(" ").collect();
            let [count, color] = words[..] else {
                panic!("Malformed component: {}", component);
            };
            let count = count.parse::<u32>().unwrap();
            match color {
                "blue" => res.blue = count,
                "red" => res.red = count,
                "green" => res.green = count,
                _ => panic!("Unknown color: {}", color)
            }
        }
        res
    }
}

impl Draw {
    pub fn power(&self) -> u32 {
        self.blue * self.red * self.green
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Game {
    id: u32,
    draws: Vec<Draw>,
}

fn parse_game(inp_line: &str) -> Game {
    let inp: Vec<&str> = inp_line.split(&[':', ';']).collect();
    let id = inp[0].split(' ').collect::<Vec<&str>>()[1].parse::<u32>()
        .expect(&format!("Couldn't parse game_id `{}`: {}", &inp[0], inp_line));
    let draws = inp[1..].iter().map(|&s| Draw::from(s)).collect();

    Game { id, draws }
}

fn valid_game(game: &&Game) -> bool {
    game.draws
        .iter()
        .all(|game|
            game.red <= 12 && game.green <= 13 && game.blue <= 14
        )
}

fn draw_max(mut acc: Draw, next: &Draw) -> Draw {
    acc.blue = u32::max(next.blue, acc.blue);
    acc.red = u32::max(next.red, acc.red);
    acc.green = u32::max(next.green, acc.green);
    acc
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let games: Vec<Game> = input.lines().map(parse_game).collect();

    let res1: u32 = games
        .iter()
        .filter(valid_game)
        .map(|game| game.id)
        .sum();
    println!("Task 1: {res1:?}");
    let res2: u32 = games
        .iter()
        .map(|game| game.draws.iter().fold(Draw::default(), draw_max).power())
        .sum();
    println!("Task 2: {res2:?}");
    // println!("Task 1: {}", input.lines().map(|l| process_line(l, false)).sum::<u32>());
    // println!("Task 2: {}", input.lines().map(|l| process_line(l, true)).sum::<u32>());

    Ok(())
}

use std::{env, fs::read_to_string};
use std::error::Error;
use regex::Regex;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Card {
    pub winning: Vec<i32>,
    pub have: Vec<i32>,
}

impl Card {
    pub fn num_winners(&self) -> usize {
        self.have
            .iter()
            .filter(|n| self.winning.contains(n))
            .count()
    }

    pub fn score(&self) -> usize {
        match self.num_winners() {
            0 => 0,
            n => 1 << (n - 1)
        }
    }
}

fn parse_card(inp: &str) -> Card {
    let re = Regex::new(r"^Card\s+([0-9]+): ([0-9 ]+) \| ([0-9 ]+)$").unwrap();
    let cap = re.captures(inp).unwrap();
    let winning: Vec<i32> = cap[2].split_whitespace().filter_map(|i| str::parse(i).ok()).collect();
    let have: Vec<i32> = cap[3].split_whitespace().filter_map(|i| str::parse(i).ok()).collect();
    Card { winning, have }
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let cards: Vec<Card> = input.lines().map(parse_card).collect();

    let sum: usize = cards.iter()
        .map(|card| card.score())
        .sum();
    println!("Task 1: {sum}");

    let mut counts: Vec<usize> = cards.iter().map(|_| 1).collect();
    for i in 0..cards.len() {
        let score = cards[i].num_winners();
        println!("Have {} of card {}, which scores {}", counts[i], i+1, score);
        println!("Iterating from {} to {}", i+1+1, i+score+1);
        for j in i+1..=i+score {
            print!("{j}, ");
            counts[j] += counts[i];
        }
        println!();
    }
    println!("Task 2: {}", counts.iter().sum::<usize>());

    Ok(())
}

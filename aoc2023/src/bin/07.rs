use std::cmp::Ordering;
use std::fmt;
use std::{env, fs::read_to_string};
use std::error::Error;
use itertools::Itertools;
use counter::Counter;

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug, Copy, Clone, Hash)]
enum Card {
    D2,
    D3,
    D4,
    D5,
    D6,
    D7,
    D8,
    D9,
    T,
    J,
    Q,
    K,
    A
}

impl Card {
    pub fn from_char(chr: char) -> Self {
        match chr {
            '2' => Card::D2,
            '3' => Card::D3,
            '4' => Card::D4,
            '5' => Card::D5,
            '6' => Card::D6,
            '7' => Card::D7,
            '8' => Card::D8,
            '9' => Card::D9,
            'T' => Card::T,
            'J' => Card::J,
            'Q' => Card::Q,
            'K' => Card::K,
            'A' => Card::A,
            c => panic!("Invalid char: {c}")
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug)]
enum WinCondition {
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind
}

#[derive(Debug)]
struct Hand {
    pub cards: [Card; 5],
    pub bid: usize,
}

impl Hand {
    pub fn condition(&self) -> WinCondition {
        let counts = self.cards.iter().collect::<Counter<_>>();
        let count_counts = counts.values().collect::<Counter<_>>();
        if count_counts[&5] == 1 {
            WinCondition::FiveOfAKind
        } else if count_counts[&4] == 1 {
            WinCondition::FourOfAKind
        } else if count_counts[&3] == 1 {
            if count_counts[&2] == 1 {
                WinCondition::FullHouse
            } else {
                WinCondition::ThreeOfAKind
            }
        } else if count_counts[&2] == 2 {
            WinCondition::TwoPair
        } else if count_counts[&2] == 1 {
            WinCondition::OnePair
        } else {
            WinCondition::HighCard
        }
    }

    pub fn from_str(inp: &str) -> Self {
        let parts: Vec<_> = inp.split(' ').collect();
        let cards: [Card; 5] = parts[0].chars().map(Card::from_char).collect::<Vec<_>>().try_into().unwrap();
        let bid = parts[1].parse::<usize>().unwrap();

        Hand { cards, bid }
    }
}

fn cmp_hands(hand1: &Hand, hand2: &Hand) -> std::cmp::Ordering {
    match hand1.condition().cmp(&hand2.condition()) {
        Ordering::Equal => {
            hand1.cards.cmp(&hand2.cards)
        },
        gt_or_lt => gt_or_lt
    }
}

impl PartialEq for Hand {
    fn eq(&self, other: &Self) -> bool {
       self.cards == other.cards
    }
}

impl Eq for Hand {}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(cmp_hands(self, other))
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        cmp_hands(self, other)
    }
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let mut hands: Vec<Hand> = input.lines().map(Hand::from_str).collect();
    hands.sort();
    let mut lines: Vec<&str> = input.lines().collect();
    lines.sort_by_key(|line| Hand::from_str(line));

    for (i, (line, hand)) in lines.iter().zip(hands.iter()).enumerate() {
        let cond = format!("{:?}", hand.condition());
        let hand_str = format!("{:?}", hand);
        println!("{line:<10} -> {hand_str:<50} -> {cond:15} (rk: {:>4} bid: {:>3})", hands.len() - i, hand.bid);
    }

    let winnings: Vec<usize> = hands.iter().enumerate().map(|(i, c)| (1 + i) * c.bid).collect();
    println!("{}", winnings.iter().sum::<usize>());


    Ok(())
}

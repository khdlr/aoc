use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::ops::{Add, Mul};
use std::{env, fs::read_to_string};
use std::error::Error;
use regex::Regex;

type Node = [char; 3];
type Network = HashMap<Node, (Node, Node)>;

fn parse_entry(line: &&str) -> (Node, (Node, Node)) {
    let re = Regex::new(r"(...) = \((...), (...)\)").unwrap();
    let cap = re.captures_iter(line).next().unwrap();

    let src = cap[1].chars().collect::<Vec<char>>();
    let src = [src[0], src[1], src[2]];
    let left = cap[1].chars().collect::<Vec<char>>();
    let left = [left[0], left[1], left[2]];
    let right = cap[1].chars().collect::<Vec<char>>();
    let right = [right[0], right[1], right[2]];

    (src, (left, right))
}

fn step(network: &Network, cur: &Node, dir: &char) -> Node {
    match dir {
        'L' => &network[cur].0,
        'R' => &network[cur].1,
        _ => panic!()
    }.clone()
}

struct Cycle {
    mapping: HashMap<Node, Node>,
    hits: HashMap<Node, Vec<usize>>,
    length: usize,
}

impl Cycle {
    pub fn initial(network: &Network, instructions: &String) -> Self {
        let mut mapping = HashMap::new();
        let mut hits: HashMap<Node, Vec<usize>> =
            network.keys().map(|c| (c.to_owned(), vec![])).collect();

        for start in network.keys() {
            let mut idx = 0;
            let mut current = start.clone();
            if current[2] == 'Z' {
                let mut hit = hits.get(start).cloned().unwrap_or_else(|| Vec::new()).clone();
                hit.push(idx);
                hits.insert(start.clone(), hit);
            }
            for dir in instructions.chars() {
                idx += 1;
                current = step(network, &current, &dir);
                if current[2] == 'Z' {
                    let mut hit = hits.get(start).cloned().unwrap_or_else(|| Vec::new()).clone();
                    hit.push(idx);
                    hits.insert(start.clone(), hit);
                }
            }
            mapping.insert(start.to_owned(), current);
        }

        Cycle { mapping, hits, length: instructions.len() }
    }

    pub fn fold(&mut self) {
        for entry in self.mapping.keys() {
            let next = &self.mapping[entry];
            let mut hit2 = self.hits[next].iter()
                .map(|c| c + self.length)
                .filter(|c| *c != 0)
                .collect::<Vec<_>>();
            let hit = self.hits.get_mut(entry).unwrap();
            hit.append(&mut hit2);
        }

        self.mapping = self.mapping.keys().map(|c| {
            let tgt = &self.mapping[&self.mapping[c]];
            (c.to_owned(), tgt.to_owned())
        }).collect();

        self.length *= 2;
    }

    pub fn fmt_entry(&self, start: &Node) -> String {
        let end = &self.mapping[start];
        let hits = &self.hits[start];
        let mut paren = String::from("");
        if hits.len() > 0 {
            let hitstr = hits.iter()
                .map(|i| format!("{i}")).collect::<Vec<_>>().join(",");
            paren = format!(" (finishes at t={})", hitstr);
        }

        format!("{start:?} -> {end:?}{paren}")
    }
}

impl Mul<&Cycle> for &Cycle {
    type Output = Cycle;

    fn mul(self, rhs: &Cycle) -> Self::Output {
        let length = self.length + rhs.length;
        let mapping = self.mapping.keys().map(|c| {
            let tgt = &rhs.mapping[&self.mapping[c]];
            (c.to_owned(), tgt.to_owned())
        }).collect();
        let hits = self.hits.keys().map(|c| {
            let mut hits1 = self.hits[c].clone();
            let mut hits2 = rhs.hits[&self.mapping[c]].iter()
                .map(|i| i + self.length)
                .filter(|i| *i > 0).collect();
            hits1.append(&mut hits2);
            (c.to_owned(), hits1)
        }).collect();

        Cycle { mapping, hits, length }
    }
}

impl Display for Cycle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Cycle (len = {}):\n", self.length)?;
        for start in self.mapping.keys() {
            writeln!(f, "\t{:?}", self.fmt_entry(start))?;
        }
        Ok(())
    }
}


pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let lines: Vec<_> = input.lines().collect();
    let instructions = lines[0].to_owned();
    let network: HashMap<_, _> = lines.iter().skip(2).map(parse_entry).collect();

    // let mut current = "AAA".to_owned();
    // let mut steps = 0;
    // for dir in instructions.chars().cycle() {
    //     steps += 1;
    //     let next = step(&network, c, &dir);
    //     if current == "ZZZ" {
    //         break;
    //     }
    // }
    // println!("Task 1: {steps} steps");

    let starts: Vec<Node> =
        network.keys().filter(|c| c[2] == 'A').cloned().collect();
    let mut cycle = Cycle::initial(&network, &instructions);
    loop {
        cycle = cycle * cycle;
        println!("Folded cycle to length {}", cycle.length);
        println!("{cycle}");

        // let mut intersection: Option<Vec<usize>> = None;
        // for start in &starts {
        //     // println!("\t{}", cycle.fmt_entry(&start));
        //     let hits = cycle.hits[start].clone();
        //     intersection = match intersection {
        //         None => Some(hits),
        //         Some(i) => Some(i.iter().filter(|f| hits.contains(f)).cloned().collect()),
        //     }
        // }
        // println!("Intersection: {intersection:?}");
        if cycle.length > 1000000000 {
            break;
        }
    }

    Ok(())
}

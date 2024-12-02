use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;
use std::ops::{Index, IndexMut};
use std::{env, fs::read_to_string};

use regex::Regex;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Operator { GT, LT }

type RuleSet = HashMap<String, Rule>;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct Element {
    x: i32,
    m: i32,
    a: i32,
    s: i32
}

impl Element {
    pub fn new(x: i32, m: i32, a: i32, s: i32) -> Self { 
        Self {x, m, a, s}
    }

    pub fn get(&self, chr: char) -> i32 {
        match chr {
            'x' => self.x,
            'm' => self.m,
            'a' => self.a,
            's' => self.s,
            _ => panic!()
        }
    }

    pub fn sum(&self) -> i32 {
        self.x + self.m + self.a + self.s
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Hypercube {
    x: (i32, i32), // x.0 <= x < x.1
    m: (i32, i32),
    a: (i32, i32),
    s: (i32, i32),
}

impl Index<char> for Hypercube {
    type Output = (i32, i32);

    fn index(&self, index: char) -> &Self::Output {
        match index {
            'x' => &self.x,
            'm' => &self.m,
            'a' => &self.a,
            's' => &self.s,
            _ => panic!()
        }
    }
}

impl IndexMut<char> for Hypercube {
    fn index_mut(&mut self, index: char) -> &mut Self::Output {
        match index {
            'x' => &mut self.x,
            'm' => &mut self.m,
            'a' => &mut self.a,
            's' => &mut self.s,
            _ => panic!()
        }
    }
}

impl Display for Hypercube {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[x:{}<{}, m:{}<{}, a:{}<{}, s:{}<{}]",
               self.x.0, self.x.1, self.m.0, self.m.1,
               self.a.0, self.a.1, self.s.0, self.s.1)
    }
}

impl Hypercube {
    pub fn new(x: (i32, i32), m: (i32, i32), a: (i32, i32), s: (i32, i32)) -> Self { 
        Self {x, m, a, s}
    }

    pub fn volume(&self) -> i64 {
        (self.x.1 - self.x.0) as i64 *
        (self.m.1 - self.m.0) as i64 *
        (self.a.1 - self.a.0) as i64 *
        (self.s.1 - self.s.0) as i64
    }

    pub fn split(&self, op: &Operation) -> (Option<Hypercube>, Option<Hypercube>) {
        let mut left = self.clone();
        let mut right = self.clone();

        let v = op.variable;
        match op.operator {
            Operator::LT => {
                let (a, b) = self[op.variable];
                left[v] = (a, op.value);
                right[v] = (op.value, b);
                let left = (left[v].0 < left[v].1).then_some(left);
                let right = (right[v].0 < right[v].1).then_some(right);
                (left, right)
            },
            Operator::GT => {
                let (a, b) = self[op.variable];
                left[v] = (op.value+1, b);
                right[v] = (a, op.value+1);
                let left = (left[v].0 < left[v].1).then_some(left);
                let right = (right[v].0 < right[v].1).then_some(right);
                (left, right)
            },
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct Operation {
    variable: char,
    operator: Operator,
    value: i32,
}

impl Operation {
    pub fn from_string(str: &str) -> Self {
        for splitter in &['>', '<'] {
            let operator = match splitter {
                '>' => Operator::GT,
                '<' => Operator::LT,
                _ => panic!("Invalid operator: {str}"),
            };
            if str.contains(*splitter) {
                let mut splits = str.split(*splitter);
                let variable = splits.next().unwrap().chars().next().unwrap();
                let value = splits.next().unwrap().parse::<i32>().unwrap();
                return Operation { variable, operator, value };
            }
        }
        panic!("Couldn't parse string: {str}");
    }

    pub fn eval(&self, el: &Element) -> bool {
        let comp = el.get(self.variable);
        match self.operator {
            Operator::GT => comp > self.value,
            Operator::LT => comp < self.value,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Target {
    NextRule(String),
    Finish(bool),
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct Instruction {
    pub target: Target,
    pub operation: Option<Operation>,
}

impl Instruction {
    pub fn from_string(str: &str) -> Self {
        let mut parts = str.rsplit(':');
        let target = match parts.next().unwrap() {
            "A" => Target::Finish(true),
            "R" => Target::Finish(false),
            other => Target::NextRule(other.to_owned()),
        };
        let operation = parts.next().map(|str| Operation::from_string(str));
        Self { target, operation }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Rule {
    raw: String,
    name: String,
    instructions: Vec<Instruction>,
}

impl Rule {
    pub fn from_string(str: &str) -> Self {
        let re = Regex::new(r"^([a-z]+)\{(.+)\}$").unwrap();
        let caps = re.captures(str).unwrap();
        let name = caps[1].to_owned();
        let instructions: Vec<Instruction> = caps[2].split(',').map(Instruction::from_string).collect();
        Self { name, instructions, raw: str.to_owned() }

    }

    pub fn eval(&self, el: &Element) -> Target {
        for i in &self.instructions {
            match i.operation {
                Some(op) => if op.eval(el) { return i.target.clone(); },
                None => return i.target.clone(),
            }
        }
        panic!("No target found!");
    }
}

pub fn eval(el: &Element, ruleset: &RuleSet) -> bool {
    let mut rule = &ruleset["in"];
    loop {
        match rule.eval(el) {
            Target::NextRule(name) => rule = &ruleset[&name],
            Target::Finish(b) => return b,
        }
    }
}

pub fn parse_element(str: &str) -> Element {
    let re = Regex::new(r"^\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}$").unwrap();
    let caps = re.captures(str).unwrap();
    let vals = (1..=4).map(|i| caps[i].to_string().parse::<i32>().unwrap()).collect::<Vec<_>>();
    Element::new(vals[0], vals[1], vals[2], vals[3])
}

pub fn recurse_hypercube(input: &Hypercube, name: &str, ruleset: &RuleSet, depth: i32) -> i64 {
    let rule = &ruleset[name];
    let mut current = Some(input.clone());
    let mut total_volume: i64 = 0;
    for i in &rule.instructions {
        if current == None {
            break;
        }
        let filtered;
        (filtered, current) = match i.operation {
            Some(op) => current.unwrap().split(&op),
            None => (current.clone(), None),
        };
        if let Some(cube) = filtered {
            match &i.target {
                Target::NextRule(name) => total_volume += recurse_hypercube(&cube, name, ruleset, depth + 1),
                Target::Finish(b) => if *b { total_volume += cube.volume() },
            }
        }
    }
    for _ in 0..depth {
        print!("> ");
    }
    println!("Matching {input} against {} => {total_volume}", rule.raw);
    total_volume
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let parts: Vec<&str> = input.split("\n\n").collect();
    let rules: RuleSet = parts[0].lines().map(Rule::from_string).map(|rule| (rule.name.clone(), rule)).collect();
    let elements: Vec<Element> = parts[1].lines().map(parse_element).collect();

    let score = elements.iter()
        .filter(|el| eval(el, &rules))
        .map(|el| el.sum())
        .sum::<i32>();
    println!("Task 1: {score}");

    let full_cube = Hypercube::new((1, 4001), (1, 4001), (1, 4001), (1, 4001));
    let score = recurse_hypercube(&full_cube, "in", &rules, 0);
    println!("Task 2: {score}");

    Ok(())
}

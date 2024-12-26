use rayon::prelude::*;
use std::collections::{BTreeMap, BTreeSet};
use std::error::Error;
use std::fmt::Display;
use std::{env, fs::read_to_string};

fn parse_state(inp: &str) -> (String, bool) {
    let mut parts = inp.split(": ");
    let name = parts.next().expect("No node name!");
    let state = parts.next().expect("No node state!");
    (name.to_string(), state == "1")
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Op {
    And,
    Xor,
    Or,
}

impl Op {
    fn parse(input: &str) -> Self {
        match input {
            "AND" => Op::And,
            "XOR" => Op::Xor,
            "OR" => Op::Or,
            other => panic!("Unknown Op: {other}"),
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::And => "AND",
                Op::Xor => "XOR",
                Op::Or => "OR",
            }
        )
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Input {
    X(u8),
    Y(u8),
    Intermediate(usize),
}

impl Input {
    fn is_initial(&self) -> bool {
        match self {
            Input::X(_) => true,
            Input::Y(_) => true,
            Input::Intermediate(_) => false,
        }
    }

    fn as_output(&self) -> Option<Output> {
        match self {
            Input::X(_) => None,
            Input::Y(_) => None,
            Input::Intermediate(idx) => Some(Output::Intermediate(*idx)),
        }
    }

    fn get(&self, x: u64, y: u64, intermediates: &Vec<Option<bool>>) -> Option<bool> {
        match self {
            Input::X(bit) => Some((x >> bit) & 1 != 0),
            Input::Y(bit) => Some((y >> bit) & 1 != 0),
            Input::Intermediate(ix) => intermediates[*ix],
        }
    }

    fn from_token(token: &str, mapping: &mut TokenMapping) -> Self {
        if let Some(num) = token
            .strip_prefix("x")
            .and_then(|num| num.parse::<u8>().ok())
        {
            return Self::X(num);
        }
        if let Some(num) = token
            .strip_prefix("y")
            .and_then(|num| num.parse::<u8>().ok())
        {
            return Self::Y(num);
        }

        let idx = mapping.get_or_insert(token);
        return Self::Intermediate(idx);
    }

    fn to_token(&self, intermediates: &TokenMapping) -> String {
        match self {
            Input::X(i) => format!("x{i:0>2}"),
            Input::Y(i) => format!("y{i:0>2}"),
            Input::Intermediate(i) => intermediates.list[*i].clone(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
enum Output {
    Z(u8),
    Intermediate(usize),
}

impl Output {
    fn apply(&self, val: bool, intermediates: &mut Vec<Option<bool>>, z: &mut u64) {
        match self {
            Output::Z(bit) => {
                if val {
                    *z |= 1 << bit; // Set bit to 1
                } else {
                    *z &= !(1 << bit); // Set bit to 0
                }
            }
            Output::Intermediate(idx) => intermediates[*idx] = Some(val),
        }
    }

    fn from_token(token: &str, token_map: &mut TokenMapping) -> Self {
        if let Some(num) = token
            .strip_prefix("z")
            .and_then(|num| num.parse::<u8>().ok())
        {
            return Self::Z(num);
        }

        let idx = token_map.get_or_insert(token);
        return Self::Intermediate(idx);
    }

    fn to_token(&self, intermediates: &TokenMapping) -> String {
        match self {
            Output::Z(i) => format!("z{i:0>2}"),
            Output::Intermediate(i) => intermediates.list[*i].clone(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
struct Swap(Output, Output);

impl Swap {
    fn from(fst: Output, snd: Output) -> Self {
        assert!(fst != snd, "Cannot swap with self!");
        if fst < snd {
            Self(fst, snd)
        } else {
            Self(snd, fst)
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Rule {
    left: Input,
    right: Input,
    op: Op,
    out: Output,
}

impl Rule {
    fn parse(inp: &str, token_map: &mut TokenMapping) -> Self {
        let mut parts = inp.split(' ');
        let left = Input::from_token(parts.next().expect("No left input!"), token_map);
        let op = Op::parse(parts.next().expect("No operator!"));
        let right = Input::from_token(parts.next().expect("No right input!"), token_map);
        assert!(
            parts.next().expect("No '->'") == "->",
            "Wrong operation formatting"
        );
        let out = Output::from_token(parts.next().expect("No output node!"), token_map);

        Self {
            left,
            op,
            right,
            out,
        }
    }

    fn apply(&self, x: u64, y: u64, intermediates: &mut Vec<Option<bool>>, z: &mut u64) -> bool {
        let mut ran = false;
        if let Some(left) = self.left.get(x, y, intermediates) {
            if let Some(right) = self.right.get(x, y, intermediates) {
                let val = match self.op {
                    Op::And => left && right,
                    Op::Xor => left ^ right,
                    Op::Or => left || right,
                };
                self.out.apply(val, intermediates, z);
                ran = true
            }
        }
        ran
    }

    fn aoc_format(&self, token_map: &TokenMapping) -> String {
        format!(
            "{} {} {} -> {}",
            self.left.to_token(token_map),
            self.op,
            self.right.to_token(token_map),
            self.out.to_token(token_map)
        )
    }
}

impl Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} = {:?} {:?} {:?}",
            self.out, self.left, self.op, self.right
        )
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug, Ord, PartialOrd)]
enum Fix {
    TurnOff,
    TurnOn,
}

struct TokenMapping {
    list: Vec<String>,
    lookup: BTreeMap<String, usize>,
}

impl TokenMapping {
    fn new() -> Self {
        Self {
            list: vec![],
            lookup: BTreeMap::new(),
        }
    }

    fn len(&self) -> usize {
        self.list.len()
    }

    fn get_or_insert(&mut self, token: &str) -> usize {
        if self.lookup.contains_key(token) {
            self.lookup[token]
        } else {
            let len = self.lookup.len();
            self.lookup.insert(token.to_owned(), len);
            self.list.push(token.to_owned());
            len
        }
    }
}

#[derive(Clone)]
struct Network<'a> {
    initial_rules: Vec<Rule>,
    other_rules: Vec<Rule>,
    swaps: Vec<Swap>,
    token_map: &'a TokenMapping,
    rule_map: BTreeMap<Output, usize>,
}

impl<'a> Network<'a> {
    fn apply(&self, x: u64, y: u64) -> u64 {
        let mut intermediates: Vec<Option<bool>> = vec![None; self.token_map.len()];
        let mut z = 0;
        for rule in &self.initial_rules {
            let rule_ran = rule.apply(x, y, &mut intermediates, &mut z);
            assert!(rule_ran, "Initial rule did not run!");
        }

        let (final_rules, intermediate_rules): (Vec<&Rule>, Vec<&Rule>) = self
            .other_rules
            .iter()
            .partition(|rule| matches!(rule.out, Output::Z(_)));

        let mut all_ran = true;
        for _ in 1..=intermediate_rules.len() {
            all_ran = true;
            for rule in &self.other_rules {
                let rule_ran = rule.apply(x, y, &mut intermediates, &mut z);
                all_ran &= rule_ran;
            }
            if all_ran {
                break;
            }
        }
        if !all_ran {
            return !0;
            // panic!("Not all ran!");
        } else {
            for rule in final_rules {
                let rule_ran = rule.apply(x, y, &mut intermediates, &mut z);
                assert!(rule_ran, "Final rule did not run!");
            }
        }

        z
    }

    fn swap(&self, swap: Swap) -> Self {
        let Swap(a, b) = swap;
        let mut net = self.clone();
        let out1 = net.get_rule(a).out;
        let out2 = net.get_rule(b).out;
        // Set swapped outputs
        net.get_rule_mut(a).out = out2;
        net.get_rule_mut(b).out = out1;

        net.swaps.push(swap);
        net
    }

    fn find_swaps(&self, x: u64, y: u64, tgt_z: u64) -> Option<(BTreeSet<Swap>, usize)> {
        let mut intermediates: Vec<Option<bool>> = vec![None; self.token_map.len()];
        let mut z = 0;
        for rule in &self.initial_rules {
            let rule_ran = rule.apply(x, y, &mut intermediates, &mut z);
            assert!(rule_ran, "Initial rule did not run!");
        }

        let (final_rules, intermediate_rules): (Vec<&Rule>, Vec<&Rule>) = self
            .other_rules
            .iter()
            .partition(|rule| matches!(rule.out, Output::Z(_)));

        let mut all_ran = true;
        for _ in 1..=intermediate_rules.len() {
            all_ran = true;
            for rule in &self.other_rules {
                let rule_ran = rule.apply(x, y, &mut intermediates, &mut z);
                all_ran &= rule_ran;
            }
            if all_ran {
                break;
            }
        }
        if !all_ran {
            return None;
            // panic!("Not all ran!");
        } else {
            for rule in final_rules {
                let rule_ran = rule.apply(x, y, &mut intermediates, &mut z);
                assert!(rule_ran, "Final rule did not run!");
            }
        }

        let mut queue = vec![];
        let mut fixes = BTreeSet::new();
        let mut mistakes = 0;
        for bit in 0..=45 {
            let pred = (z >> bit) & 1 != 0;
            let tgt = (tgt_z >> bit) & 1 != 0;

            if pred != tgt {
                let fix = if pred && !tgt {
                    Fix::TurnOff
                } else {
                    Fix::TurnOn
                };
                queue.push((Output::Z(bit), fix));
                mistakes += 1;
            }
        }

        if intermediates.iter().any(|i| i.is_none()) {
            return None;
        }

        let out2node: BTreeMap<Output, Rule> = self
            .initial_rules
            .iter()
            .chain(self.other_rules.iter())
            .map(|rule| (rule.out, rule.clone()))
            .collect();

        while let Some((node, fix)) = queue.pop() {
            if fixes.insert((node, fix)) {
                let Some(rule) = out2node.get(&node) else {
                    println!("No entry for {node:?}");
                    return None;
                };
                match (rule.op, fix) {
                    (Op::And, Fix::TurnOff) => {
                        // Both parents are on, and one of them should be off
                        for node in [rule.left, rule.right]
                            .iter()
                            .filter_map(|inp| inp.as_output())
                        {
                            queue.push((node, Fix::TurnOff));
                        }
                    }
                    (Op::And, Fix::TurnOn) => {
                        // Both outputs need to be on, but at least one isn't
                        let val_l = rule.left.get(x, y, &intermediates).expect("Not computed");
                        let val_r = rule.right.get(x, y, &intermediates).expect("Not computed");
                        if !val_l && val_r {
                            if let Some(node) = rule.left.as_output() {
                                queue.push((node, Fix::TurnOn));
                            }
                        }
                        if val_l && !val_r {
                            if let Some(node) = rule.right.as_output() {
                                queue.push((node, Fix::TurnOn));
                            }
                        }
                    }
                    (Op::Xor, _) => {
                        // Either one needs to flip
                        for input in [rule.left, rule.right] {
                            if let Some(node) = input.as_output() {
                                let fix = if input.get(x, y, &intermediates).expect("Not computed")
                                {
                                    Fix::TurnOff
                                } else {
                                    Fix::TurnOn
                                };
                                queue.push((node, fix));
                            }
                        }
                    }
                    (Op::Or, Fix::TurnOff) => {
                        // Both need to be turned off
                        let val_l = rule.left.get(x, y, &intermediates).expect("Not computed");
                        let val_r = rule.right.get(x, y, &intermediates).expect("Not computed");
                        if val_l {
                            if let Some(node) = rule.left.as_output() {
                                queue.push((node, Fix::TurnOff));
                            }
                        }
                        if val_r {
                            if let Some(node) = rule.right.as_output() {
                                queue.push((node, Fix::TurnOff));
                            }
                        }
                    }
                    (Op::Or, Fix::TurnOn) => {
                        // Both are off, but one needs to be on
                        for node in [rule.left, rule.right]
                            .iter()
                            .filter_map(|inp| inp.as_output())
                        {
                            queue.push((node, Fix::TurnOn));
                        }
                    }
                }
            }
        }

        let mut swaps: BTreeSet<Swap> = BTreeSet::new();
        let (fix_on, fix_off): (Vec<_>, Vec<_>) =
            fixes.into_iter().partition(|(_, fix)| *fix == Fix::TurnOn);
        for &(on, _) in &fix_on {
            for &(off, _) in &fix_off {
                swaps.insert(Swap::from(on, off));
                // println!("Swap {on:?} and {off:?}");
            }
        }

        Some((swaps, mistakes))
    }

    fn get_rule(&self, node: Output) -> &Rule {
        let idx = self.rule_map[&node];
        let len = self.initial_rules.len();
        if idx >= len {
            &self.other_rules[idx - len]
        } else {
            &self.initial_rules[idx]
        }
    }

    fn get_rule_mut(&mut self, node: Output) -> &mut Rule {
        let idx = self.rule_map[&node];
        let len = self.initial_rules.len();
        if idx >= len {
            &mut self.other_rules[idx - len]
        } else {
            &mut self.initial_rules[idx]
        }
    }

    fn swap_set(&self) -> BTreeSet<Swap> {
        self.swaps.iter().cloned().collect()
    }

    fn fmt_swaps(&self) -> String {
        self.swaps
            .iter()
            .map(|swap| {
                format!(
                    "{}↔{}",
                    self.get_rule(swap.0).out.to_token(&self.token_map),
                    self.get_rule(swap.1).out.to_token(&self.token_map),
                )
            })
            .collect::<Vec<_>>()
            .join(", ")
    }

    fn answer_fmt(&self) -> String {
        let mut swaps = self
            .swaps
            .iter()
            .flat_map(|swap| {
                [
                    self.get_rule(swap.0).out.to_token(&self.token_map),
                    self.get_rule(swap.1).out.to_token(&self.token_map),
                ]
            })
            .collect::<Vec<_>>();
        swaps.sort();
        swaps.join(",")
    }

    fn rules(&self) -> impl Iterator<Item = &Rule> {
        self.initial_rules.iter().chain(self.other_rules.iter())
    }

    fn aoc_print(&self) {
        for rule in self.initial_rules.iter().chain(self.other_rules.iter()) {
            println!("{}", rule.aoc_format(&self.token_map));
        }
    }

    fn num_errors(&self) -> usize {
        let mut wrong = 0;
        for test_bit in 0..=45 {
            let tgt_z = 1 << test_bit;
            if self.apply(tgt_z / 2, tgt_z / 2) != tgt_z {
                wrong += 1;
            }
            if self.apply(tgt_z - 1, 1) != tgt_z {
                wrong += 1;
            }
            if self.apply(1, tgt_z - 1) != tgt_z {
                wrong += 1;
            }
        }
        wrong
    }
}

fn swap_search<'a, 'b>(
    net: Network<'a>,
    visited: &'b mut BTreeSet<BTreeSet<Swap>>,
) -> Option<Network<'a>> {
    if !visited.insert(net.swap_set()) {
        // already visited
        return None;
    }
    if net.swaps.len() > 4 {
        return None;
    }
    let mut swap_counts: BTreeMap<Swap, usize> = BTreeMap::new();
    let mut mistakes = 0;
    for b in (0..=44).map(|s| 1 << s) {
        for (x, y) in [(b, b), (b, 0), (0, b)] {
            if let Some((swaps, mistake)) = net.find_swaps(x, y, x + y) {
                mistakes += mistake;
                for swap in swaps {
                    *swap_counts.entry(swap).or_default() += 1;
                }
            } else {
                return None;
            }
        }
    }
    println!("Searching from {} ({mistakes} wrong bits)", net.fmt_swaps());
    if mistakes == 0 {
        return Some(net);
    }

    let mut swaps: Vec<(Swap, usize)> = swap_counts.into_iter().collect();
    swaps.sort_by_key(|(_, cnt)| usize::MAX - cnt);
    for (swap, _) in swaps {
        if let Some(res) = swap_search(net.swap(swap), visited) {
            return Some(res);
        }
    }

    None
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;

    let mut token_map = TokenMapping::new();
    let mut lines = input.lines().peekable();

    let mut x = 0;
    let mut y = 0;
    for (token, value) in (&mut lines).take_while(|l| *l != "").map(parse_state) {
        if value {
            match Input::from_token(&token, &mut token_map) {
                Input::X(bit) => {
                    x += 1 << bit;
                }
                Input::Y(bit) => {
                    y += 1 << bit;
                }
                Input::Intermediate(idx) => {
                    panic!("Intermediate {idx} set from the start!");
                }
            }
        }
    }

    let rules: Vec<Rule> = (&mut lines)
        .map(|l| Rule::parse(l, &mut token_map))
        .collect();

    let (base_rules, other_rules): (Vec<_>, Vec<_>) = rules
        .into_iter()
        .partition(|rule| rule.left.is_initial() && rule.right.is_initial());

    let mut rule_map = BTreeMap::new();
    for (i, r) in base_rules.iter().enumerate() {
        rule_map.insert(r.out, i);
    }
    for (i, r) in other_rules.iter().enumerate() {
        rule_map.insert(r.out, i + base_rules.len());
    }

    let net = Network {
        initial_rules: base_rules,
        other_rules,
        token_map: &token_map,
        rule_map,
        swaps: vec![],
    };

    let part1 = net.apply(x, y);
    println!("Part 1: {part1}");

    // Identify suspicious wires
    let mut suspicious_xors = vec![];
    for rule in net.rules() {
        if rule.op == Op::Xor {
            let is_input_xor = match (rule.left, rule.right) {
                (Input::X(_), Input::Y(_)) => true,
                (Input::Y(_), Input::X(_)) => true,
                _ => false,
            };
            let is_output_xor = match rule.out {
                Output::Z(_) => true,
                _ => false,
            };
            if !is_input_xor && !is_output_xor {
                suspicious_xors.push(rule.out);
                println!(
                    "XOR node {:?} should probably be connected to some Z! {}",
                    rule.out,
                    rule.aoc_format(&token_map)
                );
            }
        }
    }

    let intermediate_swaps = (0..token_map.list.len())
        .flat_map(|n1| {
            (0..n1)
                .map(|n2| Swap::from(Output::Intermediate(n1), Output::Intermediate(n2)))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let swap_scores = intermediate_swaps
        .into_par_iter()
        .map(|swap| {
            let swap_net = net.swap(swap);
            let swapped_score = swap_net.num_errors();
            (swap, swapped_score)
        })
        .collect::<Vec<_>>();
    let (best_swap, score) = swap_scores
        .iter()
        .min_by_key(|(swap, score)| score)
        .expect("No swaps");
    println!(
        "{best_swap:?} improves from {} to {score}",
        net.num_errors()
    );

    let mut test_net = net.swap(*best_swap);
    for xor_node in suspicious_xors {
        let score = test_net.num_errors();
        for z_bit in 0..=45 {
            let swap = Swap::from(xor_node, Output::Z(z_bit));
            let swap_net = test_net.swap(swap);
            let swapped_score = swap_net.num_errors();
            if swapped_score < score {
                println!("{swap:?} improves from {score} to {swapped_score}");
                test_net = swap_net;
                break;
            }
        }
    }

    println!("{}", test_net.answer_fmt());

    Ok(())
}

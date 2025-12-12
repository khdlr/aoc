use std::{collections::BTreeMap, fs::read_to_string};

fn get_num_paths_to<'a>(
    mut cache: &mut BTreeMap<&'a str, usize>,
    incoming: &BTreeMap<&'a str, Vec<&'a str>>,
    node: &'a str,
) -> usize {
    if node == "you" {
        return 1;
    }
    if !incoming.contains_key(node) {
        return 0;
    }
    if !cache.contains_key(node) {
        let cost: usize = incoming[&node]
            .iter()
            .map(|&parent| get_num_paths_to(&mut cache, incoming, parent))
            .sum();
        cache.insert(node, cost);
    }
    cache[node]
}

fn get_num_paths_to_v2<'a>(
    mut cache: &mut BTreeMap<(&'a str, bool, bool), usize>,
    incoming: &BTreeMap<&'a str, Vec<&'a str>>,
    state: (&'a str, bool, bool),
) -> usize {
    let (node, mut has_fft, mut has_dac) = state;
    if node == "svr" && has_fft && has_dac {
        return 1;
    }
    if node == "fft" {
        has_fft = true;
    }
    if node == "dac" {
        has_dac = true;
    }
    if !incoming.contains_key(node) {
        return 0;
    }
    let lookup = (node, has_fft, has_dac);
    if !cache.contains_key(&lookup) {
        let cost: usize = incoming[node]
            .iter()
            .map(|&parent| get_num_paths_to_v2(&mut cache, incoming, (parent, has_fft, has_dac)))
            .sum();
        cache.insert(lookup, cost);
    }
    cache[&lookup]
}

#[allow(unused)]
pub fn solve(path: &str) {
    let mut outgoing: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    let mut incoming: BTreeMap<&str, Vec<&str>> = BTreeMap::new();

    let input = read_to_string(path).unwrap();
    for line in input.lines() {
        let mut parts = line.split(": ");
        let src = parts.next().expect("No source");
        let tgts = parts.next().expect("No targets");
        for tgt in tgts.split(' ') {
            outgoing.entry(src).or_default().push(tgt);
            incoming.entry(tgt).or_default().push(src);
        }
    }

    let mut cache: BTreeMap<&str, usize> = BTreeMap::new();
    let part1 = get_num_paths_to(&mut cache, &incoming, "out");
    println!("Part 1: {part1}");

    let mut outgoing: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    let mut incoming: BTreeMap<&str, Vec<&str>> = BTreeMap::new();

    let input = if path.ends_with(".test") {
        read_to_string(format!("{path}2")).unwrap()
    } else {
        input
    };
    for line in input.lines() {
        let mut parts = line.split(": ");
        let src = parts.next().expect("No source");
        let tgts = parts.next().expect("No targets");
        for tgt in tgts.split(' ') {
            outgoing.entry(src).or_default().push(tgt);
            incoming.entry(tgt).or_default().push(src);
        }
    }

    let mut cache: BTreeMap<(&str, bool, bool), usize> = BTreeMap::new();
    let part2 = get_num_paths_to_v2(&mut cache, &incoming, ("out", false, false));
    println!("Part 2: {part2}");
}

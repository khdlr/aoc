use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs::File;
use std::io::Write;
use std::{env, fs::read_to_string};
use counter::Counter;
use itertools::Itertools;
use rayon::prelude::*;
use tqdm::{tqdm,Iter};

type Graph = HashMap<String, Vec<String>>;
type IGraph = Vec<Counter<usize>>;
type Adjacency = Vec<Vec<i32>>;

fn parse(str: &str) -> Graph {
    let mut edges: Graph = HashMap::new();
    for line in str.lines() {
        let Some((src, dests)) = line.split(": ").collect_tuple() else { panic!("Couldn't parse line: {line}") };
        let dests: Vec<String> = dests.split(" ").map(|d| d.to_owned()).collect();
        edges.entry(src.to_owned()).or_default().extend_from_slice(&dests);
        for d in dests {
            edges.entry(d).or_default().push(src.to_owned());
        }
    }
    edges
}

fn to_adjacency(g: &Graph) -> Adjacency {
    let idx: HashMap<String, usize> = g.keys().enumerate()
        .map(|(i, k)| (k.clone(), i)).collect();
    let mut adj = vec![vec![0; g.len()]; g.len()];

    for (a, bs) in g.iter() {
        let a = idx[a];
        for b in bs {
            let b = idx[b];
            adj[a][b] = 1;
            adj[b][a] = 1;
        }
    }

    adj
}

fn to_multigraph(g: &Graph) -> IGraph {
    let idx: HashMap<String, usize> = g.keys().enumerate()
        .map(|(i, k)| (k.clone(), i)).collect();
    let mut counters: IGraph = vec![Counter::default(); g.len()];

    for (a, bs) in g.iter() {
        let a = idx[a];
        counters[a] += bs.iter().map(|b| idx[b]);
    }

    counters
}

fn minimum_cut_phase_old(g: &mut Graph) -> usize {
    let start = g.keys().min().expect("Empty Graph").to_owned();
    let mut set = HashSet::from([start]);
    let all = g.keys().cloned().collect::<HashSet<_>>();
    let mut last = String::new();
    let mut current = String::new();
    while set.len() < g.len() {
        // Find most tightly connected node
        for n in g.keys() {
            assert!(!g[n].iter().any(|m| n == m), "{n} vs. {:?}", g[n])
        }

        let next = (&all - &set).par_iter()
            .map(|node|
                 (node.clone(),
                 g[node].iter().filter(|&n| set.contains(n)).count()))
            .max_by_key(|(n, c)| (*c, n.clone()))
            .map(|(n, _)| n)
            .expect("Empty Graph!").to_owned();
        last = current.clone();
        current = next.clone();
        set.insert(next);
    }
    // println!("Gonna merge {last} and {current}");
    // println!("{:?}", g[&current]);
    let cut_of_the_phase = g[&current].len().min(g[&last].len());
    // println!("cut of the phase: {cut_of_the_phase}");
    let merged = format!("{last}_{current}");
    let n1 = g.remove(&last).expect("last not found");
    let n2 = g.remove(&current).expect("current not found");
    let merged_neighbors: Vec<String> = n1.iter().chain(n2.iter())
        .filter(|&n| *n != last && *n != current).cloned().collect();
    g.insert(merged.clone(), merged_neighbors);
    for vals in g.values_mut() {
        for val in vals.iter_mut() {
            if *val == last || *val == current {
                *val = merged.clone();
            }
        }
    }
    cut_of_the_phase
}

fn minimum_cut_old(g: &mut Graph) -> usize {
    let n = g.len();
    let mut best = usize::MAX;
    let mut result = usize::MAX;
    for i in (1..g.len()-1).tqdm() {
        println!("-- Phase {i} --");
        let cotp = minimum_cut_phase_old(g);
        if cotp < best {
            best = cotp;
            // visualize(&g, "25_best.dot").expect("Couldn't write");
            result = i * (n - i);
            println!("{result} = {i} * {}", n - i);
        }
        // visualize(&g, format!("25_{i}.dot")).expect("Couldn't write");
    }
    result
}

fn minimum_cut(g: &mut IGraph) -> usize {
    let n = g.len();
    let mut best = usize::MAX;
    let mut result = usize::MAX;
    for i in (1..g.len()-1).tqdm() {
        println!("-- Phase {i} --");

        // minimum_cut_phase
        let mut set = HashSet::from([0]);
        let all = (0..g.len()).filter(|i| g[*i].len() > 0).collect::<Vec<usize>>();
        let mut last = usize::MAX;
        let mut current = usize::MAX;
        while set.len() < all.len() {
            // Find most tightly connected node
            let next = all.par_iter().cloned()
                .filter(|c| !set.contains(c))
                .max_by_key(|i| g[*i].iter().map(|(i, cnt)| if set.contains(i) { *cnt } else { 0 }).sum::<usize>())
                .expect("Empty Graph!");
            last = current;
            current = next;
            set.insert(next);
        }
        println!("Gonna merge {last} and {current}");
        println!("{:?}", g[current]);
        let cut_of_the_phase = g[current].iter().map(|(_, c)| *c).sum();
        println!("cut of the phase: {cut_of_the_phase}");
        let merged = format!("{last}_{current}");

        let min = last.min(current);
        let max = last.max(current);

        g[min].remove(&max);
        g[max].remove(&min);

        g[min] = g[min].clone() + g[max].clone();
        g[max] = Default::default();

        for vals in g.iter_mut() {
            let total: usize = vals.get(&max).cloned().unwrap_or(0) +
                vals.get(&min).cloned().unwrap_or(0);
            if total > 0 {
                vals.insert(min, total);
            }
            vals.remove(&max);
        }

        if cut_of_the_phase < best {
            best = cut_of_the_phase;
            // visualize(&g, "25_best.dot").expect("Couldn't write");
            result = i * (n - i);
            println!("{result} = {i} * {}", n - i);
        }
        // visualize(&g, format!("25_{i}.dot")).expect("Couldn't write");
    }
    result
}

fn visualize(g: &IGraph, filename: impl Into<String>) -> Result<(), Box<dyn Error>> {
    let mut file = File::create(filename.into())?;
    writeln!(file, "graph {{")?;
    let all = (0..g.len()).filter(|i| g[*i].len() > 0).collect::<Vec<usize>>();
    for &node in all.iter() {
        writeln!(file, "n{node} [style=filled, fillcolor=\"#fff\"]")?;
    }
    for &node in all.iter() {
        for (&n, &cnt) in g[node].iter() {
            if n < node {
                for _ in 1..=cnt {
                    writeln!(file, "n{node} -- n{n}")?;
                }
            }
        }
    }
    writeln!(file, "}}")?;

    Ok(())
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let graph = parse(&input);
    let mut graph = to_multigraph(&graph);
    // visualize(&graph, "25.dot")?;
    let res = minimum_cut(&mut graph);
    println!("Task 1: {res}");

    Ok(())
}

use std::collections::{BTreeMap, BTreeSet};
use std::error::Error;
use std::{env, fs::read_to_string};

pub fn grow_clique(
    clique: BTreeSet<String>,
    adj: &BTreeMap<String, BTreeSet<String>>,
) -> BTreeSet<String> {
    let last = clique.last().expect("No last element");
    let growth_candidates = clique
        .iter()
        .map(|n| {
            adj[n]
                .range::<String, _>(last..)
                .cloned()
                .collect::<BTreeSet<String>>()
        })
        .reduce(|acc, set| {
            acc.intersection(&set)
                .cloned()
                .collect::<BTreeSet<String>>()
        });

    let mut largest_found = clique.clone();
    match growth_candidates {
        Some(candidates) => {
            for candidate in candidates {
                let mut grown_clique = clique.clone();
                grown_clique.insert(candidate);
                let found = grow_clique(grown_clique, adj);
                if found.len() > largest_found.len() {
                    largest_found = found;
                }
            }
        }
        None => {}
    }
    largest_found
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;

    let mut adj: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let mut ts: BTreeSet<String> = BTreeSet::new();
    for ln in input.lines() {
        let [left, right] = ln
            .split('-')
            .map(ToOwned::to_owned)
            .collect::<Vec<_>>()
            .try_into()
            .expect("Couldn't parse line");
        adj.entry(left.clone()).or_default().insert(right.clone());
        adj.entry(right.clone()).or_default().insert(left.clone());
        if left.starts_with("t") {
            ts.insert(left.clone());
        }
        if right.starts_with("t") {
            ts.insert(right.clone());
        }
    }

    let mut t_cliques = BTreeSet::new();
    for t in ts {
        let t_neighbors = &adj[&t];
        for n in t_neighbors {
            for third in adj[n].intersection(t_neighbors) {
                let mut triple = [t.to_owned(), n.to_owned(), third.to_owned()];
                triple.sort();
                t_cliques.insert(triple);
            }
        }
    }

    let part1 = t_cliques.len();
    println!("Part 1: {part1}");

    let mut deg_count: BTreeMap<usize, usize> = BTreeMap::new();
    let mut largest_clique: BTreeSet<String> = BTreeSet::new();
    for (node, neighbors) in adj.iter() {
        *deg_count.entry(neighbors.len()).or_default() += 1;
        let clique = grow_clique(BTreeSet::from([node.clone()]), &adj);
        if clique.len() > largest_clique.len() {
            largest_clique = clique;
        }
    }

    let part2 = largest_clique.into_iter().collect::<Vec<_>>().join(",");
    println!("Part 2: {part2}");

    Ok(())
}

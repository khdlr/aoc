use std::collections::HashMap;
use std::error::Error;
use std::{env, fs::read_to_string};

use aoc2024::grid::Grid;

type PathCache = HashMap<(char, char, usize), (String, usize)>;
type InputPad = HashMap<char, (i32, i32)>;
type PairCounts = HashMap<(char, char), usize>;

fn pairs<'a>(seq: &'a str) -> impl Iterator<Item = (char, char)> + 'a {
    let iter1 = ['A'].into_iter().chain(seq.chars());
    let iter2 = seq.chars();
    iter1.zip(iter2)
}

fn paths(start: (i32, i32), end: (i32, i32), blocked: (i32, i32)) -> Vec<String> {
    if start == blocked {
        return vec![];
    } else if start == end {
        return vec!["A".to_string()];
    }
    let (sy, sx) = start;
    let dy = end.0 - start.0;
    let dx = end.1 - start.1;

    let mut res = vec![];
    if dy > 0 {
        res.extend(
            paths((sy + 1, sx), end, blocked)
                .iter()
                .map(|p| format!("v{p}")),
        );
    } else if dy < 0 {
        res.extend(
            paths((sy - 1, sx), end, blocked)
                .iter()
                .map(|p| format!("^{p}")),
        );
    }

    if dx > 0 {
        res.extend(
            paths((sy, sx + 1), end, blocked)
                .iter()
                .map(|p| format!(">{p}")),
        );
    } else if dx < 0 {
        res.extend(
            paths((sy, sx - 1), end, blocked)
                .iter()
                .map(|p| format!("<{p}")),
        );
    }
    res
}

fn optimal_arrows(
    pre: char,
    post: char,
    coords: &HashMap<char, (i32, i32)>,
    search_depth: usize,
    cache: &mut PathCache,
) -> (String, usize) {
    let cache_key = (pre, post, search_depth);
    if let Some(res) = cache.get(&cache_key) {
        return res.to_owned();
    }
    let start = coords[&pre];
    let end = coords[&post];
    let blocked = coords[&' '];
    let paths = paths(start, end, blocked);
    if search_depth == 0 {
        let res = paths
            .iter()
            .min_by_key(|s| s.chars().count())
            .expect("No paths");
        let res = (res.to_string(), res.len());
        cache.insert(cache_key, res.clone());
        return res;
    }
    let mut best = "".to_string();
    let mut best_cost = usize::MAX;

    for path in paths {
        let mut cost = 0;
        for (a, b) in pairs(&path) {
            let (_, len) = optimal_arrows(a, b, coords, search_depth - 1, cache);
            cost += len;
        }
        if cost < best_cost {
            best_cost = cost;
            best = path;
        }
    }

    cache.insert(cache_key, (best.clone(), best_cost));
    (best, best_cost)
}

fn optimal_input_seq(
    seq: String,
    grid: &InputPad,
    arr_grid: &InputPad,
    cache: &mut PathCache,
) -> String {
    let mut overall = vec![];
    for (a, b) in pairs(&seq) {
        let mut best = "".to_string();
        let mut best_cost = usize::MAX;

        for path in paths(grid[&a], grid[&b], grid[&' ']) {
            let mut cost = 0;
            for (a, b) in pairs(&path) {
                let (_, len) = optimal_arrows(a, b, arr_grid, 4, cache);
                cost += len;
            }
            if cost < best_cost {
                best_cost = cost;
                best = path;
            }
        }
        assert!(best != "");
        overall.push(best);
    }
    overall.join("")
}

fn inflate(
    count: PairCounts,
    arrows: &InputPad,
    cache: &mut PathCache,
) -> HashMap<(char, char), usize> {
    let mut out: HashMap<(char, char), usize> = HashMap::new();
    for (&(a, b), cnt) in count.iter() {
        let (arrows, _) = optimal_arrows(a, b, arrows, 4, cache);
        for pair in pairs(&arrows) {
            *out.entry(pair).or_default() += cnt;
        }
    }
    out
}

fn complexity(
    code: &str,
    iterations: usize,
    pin: &InputPad,
    arrows: &InputPad,
    cache: &mut PathCache,
) -> usize {
    let numeric = code
        .strip_suffix("A")
        .expect("Code doesn't end with A")
        .parse::<usize>()
        .expect("Parse error");

    let mut counts: HashMap<(char, char), usize> = HashMap::new();
    let seq = optimal_input_seq(code.to_string(), &pin, &arrows, cache);
    for pair in pairs(&seq) {
        *counts.entry(pair).or_default() += 1;
    }
    for _ in 1..=iterations {
        counts = inflate(counts, &arrows, cache);
    }
    counts.values().sum::<usize>() * numeric
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let lines = input.lines().collect::<Vec<_>>();

    let pin_grid = Grid::from_str("789\n456\n123\n 0A");
    let arr_grid = Grid::from_str(" ^A\n<v>");

    let pin_coords: InputPad = pin_grid.iter().with_pos().map(|(p, c)| (*c, p)).collect();
    let arr_coords: InputPad = arr_grid.iter().with_pos().map(|(p, c)| (*c, p)).collect();
    let mut cache: PathCache = HashMap::new();

    let mut sum1 = 0;
    let mut sum2 = 0;
    for code in lines {
        sum1 += complexity(code, 2, &pin_coords, &arr_coords, &mut cache);
        sum2 += complexity(code, 25, &pin_coords, &arr_coords, &mut cache);
    }

    println!("Part 1: {sum1}");

    println!("Part 2: {sum2}");

    Ok(())
}

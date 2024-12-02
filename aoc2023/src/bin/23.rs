use std::collections::{VecDeque, HashSet, HashMap};
use std::error::Error;
use std::fs::File;
use std::io::Write;
use std::{env, fs::read_to_string};

use aoc2023::grid::Grid;

type Point = (i32, i32);

pub fn solve(grid: &Grid<char>) -> HashSet<Point> {
    let mut queue = VecDeque::<(Point, HashSet<Point>)>::new();
    queue.push_back(((0, 1), HashSet::new()));
    let mut max_visited = HashSet::new();
    while let Some((pos, visited)) = queue.pop_front() {
        let (y, x) = pos;
        let tile = grid.get((y, x));
        let neighbors = match tile {
            Some('.') => vec![(y-1, x), (y, x-1), (y, x+1), (y+1, x)],
            Some('>') => vec![(y, x+1)],
            Some('<') => vec![(y, x-1)],
            Some('v') => vec![(y+1, x)],
            Some('^') => vec![(y-1, x)],
            _ => panic!(),
        };
        for n in neighbors {
            if visited.contains(&n) { continue; }
            if let Some(tile) = grid.get(n) {
                if tile != '#' {
                    let mut inserted = visited.clone();
                    inserted.insert(pos);
                    queue.push_back((n, inserted));
                }
            }
            if n == (grid.height()-1, grid.width()-1) && visited.len() > max_visited.len() {
                max_visited = visited.clone();
            }
        }
    }
    max_visited
}

pub fn is_node(grid: &Grid<char>, pos: Point) -> bool {
    if pos == (grid.height()-1, grid.width()-2) {
        return true;
    }
    let mut num_neighbors = 0;
    let (y, x) = pos;
    for neighbor in [(y-1, x), (y, x-1), (y, x+1), (y+1, x)] {
        if let Some(c) = grid.get(neighbor) {
            if c != '#' {
                num_neighbors += 1;
            }
        }
    }
    num_neighbors > 2
}

pub fn next_nodes(grid: &Grid<char>, node: Point, ignore_arrows: bool) -> Vec<(Point, usize)> {
    let mut queue = VecDeque::<(Point, HashSet<Point>)>::new();
    queue.push_back((node, HashSet::new()));
    let mut nodes = Vec::new();
    while let Some((pos, visited)) = queue.pop_front() {
        let (y, x) = pos;
        let tile = grid.get((y, x));
        let neighbors = if ignore_arrows {
            vec![(y-1, x), (y, x-1), (y, x+1), (y+1, x)]
        } else {
            match tile {
                Some('.') => vec![(y-1, x), (y, x-1), (y, x+1), (y+1, x)],
                Some('>') => vec![(y, x+1)],
                Some('<') => vec![(y, x-1)],
                Some('v') => vec![(y+1, x)],
                Some('^') => vec![(y-1, x)],
                _ => panic!(),
            }
        };
        for n in neighbors {
            if visited.contains(&n) { continue; }
            if let Some(tile) = grid.get(n) {
                if tile != '#' {
                    if is_node(grid, n) {
                        nodes.push((n, 1+visited.len()));
                    } else {
                        let mut inserted = visited.clone();
                        inserted.insert(pos);
                        queue.push_back((n, inserted));
                    }
                }
            }
        }
    }
    nodes
}

pub fn graph(grid: &Grid<char>, ignore_arrows: bool) -> HashMap<Point, Vec<(Point, usize)>> {
    let mut out = File::create("23.dot").expect("Unable to create file");
    let _ = writeln!(out, "digraph {{").expect("Couldn't write");
    let mut queue = VecDeque::<Point>::new();
    let mut visited = HashSet::<Point>::new();

    let mut edges = HashMap::<Point, Vec<(Point, usize)>>::new();

    queue.push_back((0, 1));
    while let Some(pos) = queue.pop_front() {
        visited.insert(pos.clone());
        let next = next_nodes(grid, pos, ignore_arrows);
        for (node, dist) in &next {
            writeln!(out, "{} -> {} [label={}]",
                     format!("n{}_{}", pos.0, pos.1),
                     format!("n{}_{}", node.0, node.1),
                     dist
                     ).expect("Couldn't write");
            if !visited.contains(node) {
                queue.push_back(node.clone());
                visited.insert(node.clone());
            }
        }
        edges.insert(pos, next);
    }

    let _ = writeln!(out, "}}").expect("Couldn't write");

    edges
}

pub fn solve_graph(graph: &HashMap<Point, Vec<(Point, usize)>>, start: Point, end: Point) -> usize {
    let mut queue = VecDeque::<(Point, usize, HashSet<Point>)>::new();
    queue.push_back((start, 0, HashSet::new()));
    let mut max_dist = 0;
    while let Some((node, dist, visited)) = queue.pop_front() {
        for (neighbor, d) in &graph[&node] {
            if visited.contains(&neighbor) { continue; }
            let dist = dist + d;
            if *neighbor == end {
                max_dist = max_dist.max(dist);
            } else {
                let mut inserted = visited.clone();
                inserted.insert(node);
                queue.push_back((*neighbor, dist, inserted))
            }
        }
    }
    max_dist
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let grid: Grid<char> = Grid::from_str(&input);

    // let visited = solve(&grid);
    // grid.print_overlay(|pos| visited.contains(&pos).then_some('O'));
    // println!("Task 1: {}", visited.len());

    let start = (0, 1);
    let end = (grid.height()-1, grid.width()-2);
    let soln = solve_graph(&graph(&grid, false), start, end);
    println!("Task 1': {}", soln);

    // let grid2 = grid.map(|c| if ['v', '^', '<', '>'].contains(c) { '.' } else { *c });
    // let visited = solve(&grid2);
    // grid2.print_overlay(|pos| visited.contains(&pos).then_some('O'));
    // println!("Task 2: {}", visited.len());

    let start = (0, 1);
    let end = (grid.height()-1, grid.width()-2);
    let soln = solve_graph(&graph(&grid, true), start, end);
    println!("Task 2': {}", soln);

    Ok(())
}

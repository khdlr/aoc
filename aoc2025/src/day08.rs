use std::{
    collections::{BTreeMap, BTreeSet},
    fs::read_to_string,
};

#[derive(PartialEq, Eq, Copy, Clone, Debug, PartialOrd, Ord)]
struct Point(i64, i64, i64);

fn to_point(val: &str) -> Point {
    let mut vals = val
        .split(',')
        .map(|num| num.parse::<i64>().expect("Parse error"));
    Point(
        vals.next().expect("Missing X"),
        vals.next().expect("Missing Y"),
        vals.next().expect("Missing Z"),
    )
}

fn dist(p1: &Point, p2: &Point) -> i64 {
    (p1.0 - p2.0) * (p1.0 - p2.0) + (p1.1 - p2.1) * (p1.1 - p2.1) + (p1.2 - p2.2) * (p1.2 - p2.2)
}

#[allow(unused)]
pub fn solve(path: &str) {
    let points: Vec<Point> = read_to_string(path)
        .unwrap()
        .lines()
        .map(to_point)
        .collect();

    let mut cost = BTreeSet::new();
    let mut parent = BTreeMap::new();
    let mut children: BTreeMap<&Point, Vec<&Point>> = BTreeMap::new();
    for p1 in &points {
        parent.insert(p1, p1);
        children.insert(p1, vec![p1]);
        for p2 in &points {
            if p1 < p2 {
                cost.insert((dist(p1, p2), p1, p2));
            }
        }
    }

    // Part 1
    let num_connections = if path.ends_with(".full") { 1000 } else { 10 };
    for _ in 0..num_connections {
        let Some((cost, p1, p2)) = cost.pop_first() else {
            eprintln!("Everything is connected already...");
            break;
        };
        if parent[p1] != parent[p2] {
            let new_parent = parent[p1];
            let mut p2_children = children
                .remove(parent[p2])
                .expect("Couldn't pop existing value!?");
            for child in &p2_children {
                parent.insert(child, new_parent);
            }
            children
                .get_mut(parent[p1])
                .expect("p1 has no parent!?")
                .append(&mut p2_children);
        }
    }

    let mut counts = children
        .values()
        .map(|v| v.len() as i64)
        .collect::<Vec<i64>>();
    counts.sort_by_key(|c| -c);
    println!("Part 1: {}", counts[0] * counts[1] * counts[2]);

    loop {
        let Some((cost, p1, p2)) = cost.pop_first() else {
            eprintln!("Everything is connected already...");
            break;
        };
        if parent[p1] != parent[p2] {
            let new_parent = parent[p1];
            let mut p2_children = children
                .remove(parent[p2])
                .expect("Couldn't pop existing value!?");
            for child in &p2_children {
                parent.insert(child, new_parent);
            }
            children
                .get_mut(parent[p1])
                .expect("p1 has no parent!?")
                .append(&mut p2_children);
            if children.len() == 1 {
                println!("Final connection: {p1:?} <-> {p2:?}");
                println!("Part 2: {}", p1.0 * p2.0);
                break;
            }
        }
    }
}

use std::{collections::BTreeSet, fs::read_to_string};

#[derive(Clone, Copy, Eq, PartialEq)]
struct Point(i64, i64);

#[derive(Clone, Copy, Eq, PartialEq)]
struct Edge(Point, Point);

#[allow(unused)]
pub fn solve(path: &str) {
    let pts: Vec<Point> = read_to_string(path)
        .unwrap()
        .lines()
        .map(|l| {
            let mut parts = l
                .split(',')
                .map(|n| n.parse::<i64>().expect("Couldn't parse"));
            (Point(
                parts.next().expect("Couldn't parse list"),
                parts.next().expect("Couldn't parse list"),
            ))
        })
        .collect();

    let mut max_size = 0;
    for i in 0..pts.len() {
        for j in 0..i {
            let p = pts[i];
            let q = pts[j];
            let rect_size = ((p.0 - q.0).abs() + 1) * ((p.1 - q.1).abs() + 1);
            max_size = max_size.max(rect_size);
        }
    }

    println!("Part 1: {max_size}");

    let mut splits_x = BTreeSet::new();
    let mut splits_y = BTreeSet::new();

    for pt in &pts {
        splits_x.insert(pt.0);
        splits_y.insert(pt.1);
    }
    println!("{} x splits", splits_x.len());
    println!("{} y splits", splits_y.len());

    println!("Part 2: {}", 0);
}

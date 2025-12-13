use std::{collections::BTreeSet, fs::read_to_string};

use crate::utils::grid::Grid;

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
struct Point(i64, i64);

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
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

    let mut split_y = BTreeSet::new();
    let mut split_x = BTreeSet::new();
    let mut edges: BTreeSet<Edge> = BTreeSet::new();

    for i in 0..pts.len() {
        let pt = pts[i];
        let pt2 = pts[(i + 1) % pts.len()];
        split_y.insert(pt.0);
        split_x.insert(pt.1);
        if pt < pt2 {
            edges.insert(Edge(pt, pt2));
        } else {
            edges.insert(Edge(pt2, pt));
        }
    }

    fn grid_yx(pt: Point, split_y: &BTreeSet<i64>, split_x: &BTreeSet<i64>) -> (i32, i32) {
        let mut y = -1;
        for &sy in split_y {
            if sy > pt.0 {
                break;
            }
            y += 1;
            if sy == pt.0 {
                break;
            }
            y += 1;
        }

        let mut x = -1;
        for &sx in split_x {
            if sx > pt.1 {
                break;
            }
            x += 1;
            if sx == pt.1 {
                break;
            }
            x += 1;
        }

        (y, x)
    }

    let h = 2 * split_y.len() as i32 - 1;
    let w = 2 * split_x.len() as i32 - 1;
    let mut area: Grid<i64> = Grid::empty(h, w);

    let mut y_stretches = vec![];
    let split_y_ary = split_y.iter().collect::<Vec<_>>();
    for i in 0..(split_y.len() - 1) {
        y_stretches.push(1);
        y_stretches.push(split_y_ary[i + 1] - split_y_ary[i] - 1);
    }
    y_stretches.push(1);

    let mut x_stretches = vec![];
    let split_x_ary = split_x.iter().collect::<Vec<_>>();
    for i in 0..(split_x.len() - 1) {
        x_stretches.push(1);
        x_stretches.push(split_x_ary[i + 1] - split_x_ary[i] - 1);
    }
    x_stretches.push(1);

    for (y, area_y) in y_stretches.iter().enumerate() {
        for (x, area_x) in x_stretches.iter().enumerate() {
            area[(y as i32, x as i32)] = area_y * area_x;
        }
    }

    let mut color: Grid<u8> = Grid::empty(h, w);
    let mut allowed: Grid<bool> = Grid::empty(h, w);

    for &Edge(a, b) in &edges {
        let (y0, x0) = grid_yx(a, &split_y, &split_x);
        let (y1, x1) = grid_yx(b, &split_y, &split_x);
    }

    let mut active_vertical = BTreeSet::new();
    let mut frontier_y = 0;
    for Edge(a, b) in edges {
        let (y0, x0) = grid_yx(a, &split_y, &split_x);
        let (y1, x1) = grid_yx(b, &split_y, &split_x);

        for y in frontier_y..y0 {
            let mut inside = false;
            let mut onhorizontal = false;
            let mut last_x = 0;
            active_vertical = active_vertical
                .into_iter()
                .filter(|&(x, ya, yb)| {
                    if inside {
                        for xx in last_x..=x {
                            allowed[(y, xx)] = true;
                        }
                    }
                    last_x = x;

                    let old_horizontal = onhorizontal;
                    onhorizontal = y == ya || y == yb;

                    // Pretend we're
                    // Line stops here
                    if (yb != y) {
                        inside = !inside;
                    }
                    yb > y
                })
                .collect();
        }
        frontier_y = y0;

        if x0 == x1 {
            // vertical
            for y in y0..=y1 {
                allowed[(y, x0)] = true;
                color[(y, x0)] = 2;
            }
            active_vertical.insert((x0, y0, y1));
        } else {
            // horizontal
            for x in x0..=x1 {
                allowed[(y0, x)] = true;
                color[(y0, x)] = 2;
            }
        }
        color[(y0, x0)] = 1;
        color[(y1, x1)] = 1;
    }

    color.save_img("redgreen.png", |&i| match i {
        0 => (0, 0, 0),
        1 => (255, 0, 0),
        2 => (0, 255, 0),
        _ => panic!(),
    });

    allowed.save_img("allowed_pre_zeros.png", |&b| {
        if b { (255, 255, 255) } else { (0, 0, 0) }
    });

    for (idx, &area) in area.iter().with_pos() {
        // Zero area is always green... :)
        if area == 0 {
            allowed[idx] = true;
        }
    }

    allowed.save_img("allowed_post_zeros.png", |&b| {
        if b { (255, 255, 255) } else { (0, 0, 0) }
    });

    let maxarea = *area.iter().max().expect("No maximum");
    area.save_img("area.png", |&area| {
        let val = ((area as f64 / maxarea as f64) * 255.0) as u8;
        (val, val, val)
    });

    // allowed.print_map(|&b| if b { 'x' } else { ' ' });
    // area.print_map(|&c| format!("{c:02} "));

    let mut max_area = 0;
    let mut max_rect = (0, 0, 0, 0);
    for i in 0..pts.len() {
        for j in 0..i {
            let (y0, x0) = grid_yx(pts[i], &split_y, &split_x);
            let (y1, x1) = grid_yx(pts[j], &split_y, &split_x);

            let (y0, y1) = (y0.min(y1), y0.max(y1));
            let (x0, x1) = (x0.min(x1), x0.max(x1));

            let mut allow = true;
            let mut ar = 0;
            for y in y0..=y1 {
                for x in x0..=x1 {
                    allow &= allowed[(y, x)];
                    ar += area[(y, x)];
                }
            }
            if allow && ar > max_area {
                max_rect = (y0, x0, y1, x1);
                max_area = ar;
            }
        }
    }

    println!("Best rect: {max_rect:?}");
    println!("Part 2: {max_area}");
}

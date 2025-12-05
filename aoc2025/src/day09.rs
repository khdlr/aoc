use std::fs::read_to_string;

#[allow(unused)]
pub fn solve(path: &str) {
    let lines: Vec<String> = read_to_string(path)
        .unwrap()
        .lines()
        .map(String::from)
        .collect();

    println!("Part 1: {}", 0);
    println!("Part 2: {}", 0);
}

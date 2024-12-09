use std::error::Error;
use std::{env, fs::read_to_string};

fn parse_present(input: &str) -> (i32, i32, i32) {
    let mut nums = input
        .split('x')
        .map(|num| num.parse::<i32>().expect("Couldn't parse"));
    let l = nums.next().expect("No length in line");
    let w = nums.next().expect("No width in line");
    let h = nums.next().expect("No height in line");
    (l, w, h)
}

fn required_paper(lwh: &(i32, i32, i32)) -> i32 {
    let (l, w, h) = lwh;
    let slack = (l * w).min(l * h).min(w * h);
    2 * (l * w + l * h + w * h) + slack
}

fn required_ribbon(lwh: &(i32, i32, i32)) -> i32 {
    let (l, w, h) = lwh;
    let bow = l * w * h;
    let wrap = 2 * (l + w).min(l + h).min(w + h);
    bow + wrap
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let lines = input.lines().map(parse_present).collect::<Vec<_>>();
    let part1: i32 = lines.iter().map(required_paper).sum();
    println!("Part 1: {}", part1);

    let part2: i32 = lines.iter().map(required_ribbon).sum();
    println!("Part 2: {}", part2);

    Ok(())
}

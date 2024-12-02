use std::error::Error;
use std::{env, fs::read_to_string};

pub fn is_safe(row: &&Vec<i32>) -> bool {
    let diffs: Vec<_> = row
        .iter()
        .zip(row.iter().skip(1))
        .map(|(a, b)| b - a)
        .collect();
    let same_sign = diffs.iter().all(|d| d.signum() == diffs[0].signum());
    let right_range = diffs.iter().all(|d| d.abs() >= 1 && d.abs() <= 3);
    return same_sign && right_range;
}

pub fn is_safe_one_removed(row: &&Vec<i32>) -> bool {
    for remove_idx in 0..row.len() {
        let mut check: Vec<i32> = (**row).clone();
        check.remove(remove_idx);
        if is_safe(&&check) {
            return true;
        }
    }
    return false;
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let rows = input
        .lines()
        .map(|l| {
            let s: Result<Vec<i32>, _> = l.split(' ').map(str::parse::<i32>).collect();
            if let Ok(v) = s {
                return v;
            } else {
                panic!("{l}");
            }
        })
        .collect::<Vec<_>>();

    let num_safe = rows.iter().filter(is_safe).count();
    println!("Part 1: {num_safe}");

    let num_safe = rows.iter().filter(is_safe_one_removed).count();
    println!("Part 2: {num_safe}");

    Ok(())
}

use std::error::Error;
use std::{env, fs::read_to_string};
use itertools::Itertools;

type Point = (usize, usize);

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let filled = input.lines().map(|line| line.chars().map(|c| c == '#').collect::<Vec<_>>()).collect::<Vec<_>>();
    let observed_galaxies = filled.iter().enumerate().flat_map(|(row, line)| {
        line.iter()
            .enumerate()
            .filter(|(_, c)| **c)
            .map(move |(col, _)| (row, col))
    }).collect::<Vec<Point>>();

    let height = filled.len();
    let width = filled[0].len();

    let empty_rows = (0..height).filter(|y| {
        (0..width).all(|x| !filled[*y][x])
    }).collect::<Vec<_>>();

    let empty_cols = (0..width).filter(|x| {
        (0..height).all(|y| !filled[y][*x])
    }).collect::<Vec<_>>();

    let dists1: i64 = observed_galaxies.iter().map(|(y, x)| {
        ((y + empty_rows.iter().filter(|y2| *y2 < y).count()) as i64,
         (x + empty_cols.iter().filter(|x2| *x2 < x).count()) as i64)
    }).combinations(2).map(|tuple| {
        let v = tuple[0];
        let w = tuple[1];

        (v.0 - w.0).abs() + (v.1 - w.1).abs()
    }).sum();
    println!("Dists: {dists1:?}");


    let dists2: i64 = observed_galaxies.iter().map(|(y, x)| {
        ((y + 999999 * empty_rows.iter().filter(|y2| *y2 < y).count()) as i64,
         (x + 999999 * empty_cols.iter().filter(|x2| *x2 < x).count()) as i64)
    }).combinations(2).map(|tuple| {
        let v = tuple[0];
        let w = tuple[1];

        (v.0 - w.0).abs() + (v.1 - w.1).abs()
    }).sum();
    println!("Dists: {dists2:?}");

    Ok(())
}

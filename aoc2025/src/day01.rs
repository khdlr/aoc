use std::fs::read_to_string;

pub fn solve(path: &str) {
    let lines: Vec<String> = read_to_string(path)
        .unwrap()
        .lines()
        .map(String::from)
        .collect();
    let mut dial = 50;
    let mut count_zero = 0;

    for line in &lines {
        if let Some(num) = line.strip_prefix('L') {
            let num: i32 = num.parse().expect("Couldn't parse num");
            dial = ((dial - num) % 100 + 100) % 100;
        } else if let Some(num) = line.strip_prefix('R') {
            let num: i32 = num.parse().expect("Couldn't parse num");
            dial = (dial + num) % 100;
        }
        if dial == 0 {
            count_zero += 1;
        }
    }

    println!("Part 1: {count_zero}");

    let mut count_clicks = 0;
    dial = 50;
    for line in &lines {
        let mut num = if let Some(num) = line.strip_prefix('L') {
            -num.parse::<i32>().expect("Couldn't parse num")
        } else if let Some(num) = line.strip_prefix('R') {
            num.parse::<i32>().expect("Couldn't parse num")
        } else {
            panic!("Line didn't start with L or R!")
        };

        let mut flipped = false;
        if num < 0 {
            flipped = true;
            num = -num;
            dial = (100 - dial) % 100;
        }
        // num guaranteed > 0

        dial = dial + num;
        let rotations = dial / 100;
        dial = dial - 100 * rotations;
        count_clicks += rotations.abs();

        if flipped {
            dial = (100 - dial) % 100;
        }
    }
    println!("Part 2: {count_clicks}");
}

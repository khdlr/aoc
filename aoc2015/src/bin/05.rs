use std::error::Error;
use std::{env, fs::read_to_string};

fn is_nice(inp: &&&str) -> bool {
    let vowel_count = inp
        .chars()
        .filter(|&c| matches!(c, 'a' | 'e' | 'i' | 'o' | 'u'))
        .count();
    let has_repeat = inp.chars().zip(inp.chars().skip(1)).any(|(a, b)| a == b);
    let has_forbidden = ["ab", "cd", "pq", "xy"]
        .into_iter()
        .any(|seq| inp.contains(seq));

    return (vowel_count >= 3) && has_repeat && !has_forbidden;
}

fn is_nice_v2(inp: &&&str) -> bool {
    let mut xyxy = false;
    let mut xyx = false;
    let chrs: Vec<_> = inp.chars().collect();
    for i in 0..inp.len() - 2 {
        xyx |= chrs[i] == chrs[i + 2];
        for j in i + 2..inp.len() - 1 {
            xyxy |= (chrs[i] == chrs[j]) && (chrs[i + 1] == chrs[j + 1])
        }
    }

    xyxy && xyx
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let lines: Vec<&str> = input.lines().collect::<Vec<_>>();

    let part1 = lines.iter().filter(is_nice).count();
    println!("Part 1: {}", part1);

    let part2 = lines.iter().filter(is_nice_v2).count();
    println!("Part 2: {}", part2);

    Ok(())
}

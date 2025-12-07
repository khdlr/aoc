use std::collections::BTreeMap;
use std::fs::read_to_string;

#[allow(unused)]
pub fn solve(path: &str) {
    let mut lines: Vec<String> = read_to_string(path)
        .unwrap()
        .lines()
        .map(String::from)
        .collect();

    let mut beams = BTreeMap::new();
    for (i, chr) in lines[0].chars().enumerate() {
        if chr == 'S' {
            beams.insert(i, 1);
        }
    }

    let mut split_count: u64 = 0;
    for line in &lines[1..] {
        let mut next_beams = BTreeMap::new();
        let row: Vec<_> = line.chars().collect();
        for (beam, n_timelines) in beams {
            match row[beam] {
                '.' => {
                    *next_beams.entry(beam).or_default() += n_timelines;
                }
                '^' => {
                    split_count += 1;
                    *next_beams.entry(beam - 1).or_default() += n_timelines;
                    *next_beams.entry(beam + 1).or_default() += n_timelines;
                }
                _ => {
                    panic!()
                }
            }
        }
        beams = next_beams;
    }

    println!("Part 1: {split_count}");
    println!("Part 2: {}", beams.values().sum::<u64>());
}

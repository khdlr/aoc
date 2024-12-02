use std::error::Error;
use std::{env, fs::read_to_string};

type Grid = Vec<Vec<char>>;

pub fn parse_grid(inp: &str) -> Grid {
    inp.lines().map(|line| line.chars().collect()).collect()
}

pub fn print(grid: &Grid) {
    for row in grid {
        let line: String = row.iter().collect::<String>();
        println!("{}", line);
    }
}

pub fn hash(inp: &str) -> usize {
    inp.chars().map(|c| c as usize).fold(0, |acc, i| ((acc + i) * 17) % 256)
}

type Entry = (String, usize);

#[derive(Debug)]
struct HashMap {
    buckets: [Vec<Entry>; 256]
}

const ARRAY_INIT_VAL: Vec<Entry> = Vec::new();
impl HashMap {
    pub fn empty() -> HashMap {
        HashMap { buckets: [ARRAY_INIT_VAL; 256] }
    }

    pub fn apply(&mut self, instruction: &str) {
        if instruction.ends_with("-") {
            let label = instruction[..(instruction.len()-1)].to_owned();
            let bucket = &mut self.buckets[hash(&label)];
            let mut delete_idx = None;
            for (i, (key, _)) in bucket.iter().enumerate() {
                if *key == label {
                    delete_idx = Some(i);
                }
            }
            if let Some(idx) = delete_idx {
                bucket.remove(idx);
            }
        } else {
            let mut vals = instruction.split('=');
            let label = vals.next().expect("No label in instruction").to_owned();
            let val = vals.next().expect("No value in instruction");
            let val = val.parse::<usize>().expect("Couldn't parse value");
            let bucket = &mut self.buckets[hash(&label)];
            let mut present_idx = None;
            for (i, (key, _)) in bucket.iter().enumerate() {
                if *key == label {
                    present_idx = Some(i);
                }
            }
            match present_idx {
                Some(i) => bucket[i] = (label, val),
                None => bucket.push((label, val))
            }

        }
    }

    pub fn score(&self) -> usize {
        let mut score = 0;
        for (i, bucket) in self.buckets.iter().enumerate() {
            let i = i+1;
            for (j, (_, val)) in bucket.iter().enumerate() {
                let j = j+1;
                let lens_score = i * j * val;
                score += lens_score;
            }
        }
        score
    }
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?.replace("\n", "");
    let words = input.split(',').collect::<Vec<_>>();

    let sol1: usize = words.iter().map(|w| hash(w)).sum();
    println!("Task 1: {sol1}");

    // Task 2: Init
    let mut map = HashMap::empty();
    for instruction in words {
        map.apply(instruction);
    }
    println!("Task 2: {}", map.score());


    Ok(())
}

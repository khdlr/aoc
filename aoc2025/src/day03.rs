use std::fs::read_to_string;

fn joltage(nums: &[u64], n_digits: usize) -> u64 {
    let mut digits = vec![0_u64; n_digits];
    let len = nums.len();
    for (i, &num) in nums.into_iter().enumerate() {
        for k in 0..digits.len() {
            if i >= len + 1 - (digits.len() - k) {
                continue;
            }
            if num > digits[k] {
                digits[k] = num;
                for j in k + 1..digits.len() {
                    digits[j] = 0;
                }
                break;
            }
        }
    }

    digits
        .into_iter()
        .reduce(|l, r| 10 * l + r)
        .expect("No digits to reduce!?")
}

pub fn solve(path: &str) {
    let lines: Vec<String> = read_to_string(path)
        .unwrap()
        .lines()
        .map(String::from)
        .collect();

    let mut j2 = 0;
    let mut j12 = 0;
    for line in lines {
        let nums: Vec<_> = line
            .chars()
            .map(|c| ((c as u32) - ('0' as u32)) as u64)
            .collect();

        j2 += joltage(&nums, 2);
        j12 += joltage(&nums, 12);
        // println!("done.");
    }

    println!("Part 1: {j2}");
    println!("Part 2: {j12}");
}

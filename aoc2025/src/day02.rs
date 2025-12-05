use std::fs::read_to_string;

pub fn check_num(x: u64, exactly_two: bool) -> bool {
    let mut digits = 0;
    let mut check = x;
    while check >= 1 {
        check /= 10;
        digits += 1;
    }

    let check_nums = if exactly_two {
        vec![2]
    } else {
        (2..=digits).collect()
    };

    for reps in check_nums {
        // number of repetitions
        if digits % reps != 0 {
            continue;
        }

        let chunk_len = digits / reps;
        let mut divisor = 0;
        for i in 0..reps {
            divisor += 10_u64.pow(i * chunk_len);
        }
        if x % divisor == 0 {
            return true;
        }
    }
    false
}

pub fn validate_id(id: &str, exactly_two: bool) -> Vec<u64> {
    let nums = id
        .split("-")
        .map(|x| x.parse::<u64>())
        .collect::<Result<Vec<_>, _>>()
        .expect("Failed to parse ints");
    let a = nums[0];
    let b = nums[1];

    let filtered: Vec<u64> = (a..=b).filter(|n| check_num(*n, exactly_two)).collect();
    filtered
}

pub fn solve(path: &str) {
    let ids: Vec<String> = read_to_string(path)
        .unwrap()
        .trim()
        .split(',')
        .map(String::from)
        .collect();

    let mut sum = 0;
    for id in &ids {
        sum += validate_id(id, true).iter().sum::<u64>();
    }
    println!("Part 1: {sum}");

    let mut sum = 0;
    for id in &ids {
        sum += validate_id(id, false).iter().sum::<u64>();
    }
    println!("Part 2: {sum}");
}

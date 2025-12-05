use std::{fs::read_to_string, ops::Add};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Interval {
    pub a: i64,
    pub b: i64,
}

impl Interval {
    pub fn new(a: i64, b: i64) -> Self {
        Self { a, b }
    }

    pub fn as_tuple(&self) -> (i64, i64) {
        (self.a, self.b)
    }

    pub fn is_inner_point(&self, pt: i64) -> bool {
        pt > self.a && pt < self.b
    }

    pub fn contains(&self, other: &Interval) -> bool {
        self.a <= other.a && self.b >= other.b
    }

    pub fn contains_pt(&self, pt: i64) -> bool {
        self.a <= pt && self.b >= pt
    }
}

impl Add<i64> for Interval {
    type Output = Interval;

    fn add(self, rhs: i64) -> Self::Output {
        Interval::new(self.a + rhs, self.b + rhs)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BorelSet {
    pub ranges: Vec<Interval>,
}

impl BorelSet {
    pub fn from_ranges(ranges: Vec<Interval>) -> Self {
        let mut inp_ranges = ranges;
        inp_ranges.sort_by(|a, b| a.a.cmp(&b.a));
        let mut ranges = vec![];

        let (mut left, mut right) = inp_ranges[0].as_tuple();
        for int in inp_ranges[1..].iter() {
            if int.a > right {
                ranges.push(Interval::new(left, right));
                left = int.a;
                right = int.b;
            } else if int.b > right {
                right = int.b;
            }
        }
        ranges.push(Interval::new(left, right));

        Self { ranges }
    }

    pub fn contains_pt(&self, pt: i64) -> bool {
        self.ranges.iter().any(|int| int.contains_pt(pt))
    }
}

pub fn union(sets: impl IntoIterator<Item = BorelSet>) -> BorelSet {
    let ranges: Vec<Interval> = sets.into_iter().flat_map(|set| set.ranges).collect();
    BorelSet::from_ranges(ranges)
}

#[allow(unused)]
pub fn solve(path: &str) {
    let input = read_to_string(path).unwrap();
    let parts: Vec<_> = input.split("\n\n").collect();

    let ranges: Vec<_> = parts[0]
        .lines()
        .map(|l| {
            let ab: Vec<_> = l.split("-").map(|c| c.parse::<i64>().unwrap()).collect();
            Interval::new(ab[0], ab[1])
        })
        .collect();
    let borel_set = BorelSet::from_ranges(ranges);

    let ids: Vec<_> = parts[1]
        .lines()
        .map(|l| l.parse::<i64>().unwrap())
        .collect();

    let mut count = 0;
    for &id in &ids {
        if borel_set.contains_pt(id) {
            count += 1;
        }
    }

    println!("Part 1: {count}");

    let mut count_possible = 0;
    for range in &borel_set.ranges {
        if range.a > range.b {
            println!("{range:?}");
        }
        count_possible += range.b - range.a + 1;
    }

    println!("Part 2: {}", count_possible);
}

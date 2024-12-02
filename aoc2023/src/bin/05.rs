use std::collections::BTreeSet;
use std::ops::Add;
use std::{env, fs::read_to_string};
use std::error::Error;
use itertools::Itertools;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Interval {
    pub a: i64,
    pub b: i64
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
        Interval::new(self.a+rhs, self.b+rhs)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BorelSet {
    pub ranges: Vec<Interval>
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
            } else {
                right = int.b;
            }
        }
        ranges.push(Interval::new(left, right));

        Self { ranges }
    }

    pub fn empty() -> Self {
        Self::from_ranges(vec![])
    }

    pub fn from_num(inp: i64) -> Self {
        Self::from_ranges(vec![Interval::new(inp, inp)])
    }

    pub fn from_range(start: i64, end: i64) -> Self {
        Self::from_ranges(vec![Interval::new(start, end)])
    }

    pub fn union(&self, other: &BorelSet) -> Self {
        let ranges: Vec<Interval> = self.ranges.iter().chain(other.ranges.iter()).map(Clone::clone).collect();
        Self::from_ranges(ranges)
    }

    pub fn is_inner_point(&self, pt: i64) -> bool {
        self.ranges.iter().any(|int| int.is_inner_point(pt))
    }

    pub fn contains_pt(&self, pt: i64) -> bool {
        self.ranges.iter().any(|int| int.contains_pt(pt))
    }

    pub fn contains(&self, other: &BorelSet) -> bool {
        other.ranges.iter().all(|small| self.ranges.iter().any(|big| big.contains(small)))
    }
}


pub fn union(sets: impl IntoIterator<Item=BorelSet>) -> BorelSet {
    let ranges: Vec<Interval> = sets.into_iter().flat_map(|set| set.ranges).collect();
    BorelSet::from_ranges(ranges)
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MapEntry {
    interval: Interval,
    offset: i64,
}

impl From<&str> for MapEntry {
    fn from(s: &str) -> Self {
        let nums: Vec<i64> = s.split(" ").map(|c| c.parse::<i64>().unwrap()).collect();
        let dest_start = nums[0];
        let src_start = nums[1];
        let range_len = nums[2];
        MapEntry { interval: Interval::new(src_start, src_start+range_len), offset: dest_start - src_start }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Map {
    pub entries: Vec<MapEntry>,
}

impl Map {
    pub fn build(inp: &&str) -> Map {
        let mut entries: Vec<MapEntry> = inp
            .lines()
            .skip(1)
            .map(|line| MapEntry::from(line))
            .collect();
        entries.sort_by(|a, b| a.interval.a.cmp(&b.interval.a));
        Map { entries }
    }

    // pub fn map(&self, inp: i64) -> i64 {
    //     let mut lower = 0;
    //     let mut upper = self.entries.len();
    //     while lower+1 < upper {
    //         let check = (lower + upper) / 2;
    //         match self.entries[check].src_start.cmp(&inp) {
    //             Ordering::Less => lower = check,
    //             Ordering::Equal => lower = check,
    //             Ordering::Greater => upper = check,
    //         };
    //     }
    //     let entry = &self.entries[lower];
    //     if entry.src_start <= inp && entry.src_start + entry.range_len >= inp {
    //         inp + entry.offset
    //     } else {
    //         inp
    //     }
    // }
    //
    pub fn map(&self, set: &BorelSet) -> BorelSet {
        let ranges: Vec<Interval> = set.ranges.iter().flat_map(|i| self.map_interval(i).ranges).collect();
        BorelSet::from_ranges(ranges)
    }

    pub fn map_interval(&self, interval: &Interval) -> BorelSet {
        // Screw binary search
        // Find cutting points
        if interval.a == interval.b {
            return BorelSet::from_num(
                match self.entries.iter().find(|e| e.interval.contains_pt(interval.a)) {
                    Some(ent) => interval.a + ent.offset,
                    None => interval.a,
            })
        }
        let mut points = BTreeSet::new();
        points.insert(interval.a);
        points.insert(interval.b);
        for ent in &self.entries {
            if interval.contains_pt(ent.interval.a) {
                points.insert(ent.interval.a);
            }
            if interval.contains_pt(ent.interval.b) {
                points.insert(ent.interval.b);
            }
        }

        let mut out_ranges = vec![];
        for (a, b) in points.iter().tuple_windows() {
            let inp = Interval::new(*a, *b);
            match self.entries.iter().find(|e| e.interval.contains(&inp)) {
                Some(ent) => out_ranges.push(inp + ent.offset),
                None => out_ranges.push(inp),
            }
        }

        BorelSet::from_ranges(out_ranges)
    }
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let maps: Vec<&str> = input.split("\n\n").collect();
    let seeds: Vec<i64> = maps[0].split(' ').skip(1).map(|c| c.parse::<i64>().unwrap()).collect();

    let maps: Vec<Map> = maps.iter()
        .skip(1)
        .map(Map::build)
        .collect();

    let mut tmp: Vec<BorelSet> = seeds.iter().map(|i| BorelSet::from_num(*i)).collect();
    for map in &maps {
        tmp = tmp.iter().map(|i| map.map(&i)).collect();
    }
    println!("Task 1: {}", tmp.iter().map(|i| i.ranges[0].a).min().unwrap());

    let intervals = (0..seeds.len()).step_by(2).map(|i| Interval::new(seeds[i], seeds[i]+seeds[i+1]-1)).collect();
    let mut set = BorelSet::from_ranges(intervals);
    for map in &maps {
        set = map.map(&set);
    }
    println!("Task 2: {}", set.ranges[0].a);

    Ok(())
}

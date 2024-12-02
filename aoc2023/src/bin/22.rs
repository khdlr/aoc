use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashSet};
use std::error::Error;
use std::{env, fs::read_to_string};
use aoc2023::geometry::Z;

#[derive(Debug, Eq, PartialEq, Clone)]
struct Block {
    min: Z<3>,
    max: Z<3>,
}

fn parse_cube(str: &str) -> Block {
    let ab: Vec<&str> = str.split('~').collect();
    let a: [i32; 3] = ab[0].split(',').map(|num| num.parse::<i32>().unwrap()).collect::<Vec<_>>().try_into().unwrap();
    let b: [i32; 3] = ab[1].split(',').map(|num| num.parse::<i32>().unwrap()).collect::<Vec<_>>().try_into().unwrap();
    Block { min: a.into(), max: b.into() }
}

fn fall_height(blocks: &Vec<Block>, i: usize, mask_idx: Option<usize>) -> i32 {
    let mut lowest = 1;
    let [xi0, yi0, zi0] = blocks[i].min.vals;
    let [xi1, yi1, _] = blocks[i].max.vals;
    for j in 0..blocks.len() {
        if j == i { continue; }
        if let Some(idx) = mask_idx { if j == idx { continue; } }
        let [xj0, yj0, _] = blocks[j].min.vals;
        let [xj1, yj1, zj1] = blocks[j].max.vals;
        if zj1 < zi0 && !(xj1 < xi0 || xi1 < xj0 || yj1 < yi0 || yi1 < yj0) {
            // Block j is in i's falling path
            if zj1 + 1 > lowest {
                lowest = zj1 + 1;
            }
        }
    }
    zi0 - lowest
}

fn settle(blocks: &mut Vec<Block>) {
    loop {
        let mut all_settled = true;
        for i in 0..blocks.len() {
            let dz = fall_height(&blocks, i, None);
            if dz > 0 {
                all_settled = false;
                blocks[i].min[2] -= dz;
                blocks[i].max[2] -= dz;
            }
        }
        if all_settled {
            break;
        }
    }
}


#[derive(Eq, PartialEq)]
struct IndexedBlock(usize, Block);

impl Ord for IndexedBlock {
    fn cmp(&self, other: &Self) -> Ordering {
        self.1.max[2].cmp(&other.1.max[2]).then_with(|| self.0.cmp(&other.0))
    }
}

impl PartialOrd for IndexedBlock {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

struct DependencyGraph {
    blocks: Vec<Block>,
    depends_on: Vec<Vec<usize>>,
    dependant_of: Vec<Vec<usize>>,
}

impl From<Vec<Block>> for DependencyGraph {
    fn from(mut blocks: Vec<Block>) -> Self {
        settle(&mut blocks);
        let depends_on: Vec<Vec<usize>> = blocks.iter()
            .enumerate()
            .map(|(i, b)| {
                let [xi0, yi0, zi0] = b.min.vals;
                let [xi1, yi1, _] = b.max.vals;

                (0..blocks.len()).filter(|&j| {
                    if j == i { return false; }
                    let [xj0, yj0, _] = blocks[j].min.vals;
                    let [xj1, yj1, zj1] = blocks[j].max.vals;
                    zj1 + 1 == zi0 && !(xj1 < xi0 || xi1 < xj0 || yj1 < yi0 || yi1 < yj0)
                }).collect()
            }).collect();
        let dependant_of: Vec<Vec<usize>> = (0..blocks.len())
            .map(|i| (0..blocks.len()).filter(|j| depends_on[*j].contains(&i)).collect()).collect();
        Self { blocks, depends_on, dependant_of }
    }
}

impl DependencyGraph {
    fn count_fallen(&self, deleted: usize) -> usize {
        let mut heap = BinaryHeap::from([IndexedBlock(deleted, self.blocks[deleted].clone())]);
        let mut fallen = HashSet::from([deleted]);
        while let Some(IndexedBlock(i, _)) = heap.pop() {
            for &child in &self.dependant_of[i] {
                if self.depends_on[child].iter().all(|i| fallen.contains(i)) {
                    fallen.insert(child);
                    heap.push(IndexedBlock(child, self.blocks[child].clone()));
                }
            }
        }
        fallen.len() - 1
    }
}

/// F F F
/// B E
/// B D
/// C C C

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let mut blocks: Vec<Block>  = input.lines().map(parse_cube).collect();

    // Fall
    settle(&mut blocks);

    // Naive impl
    // let mut can_disintegrate = 0;
    // for d in (0..blocks.len()).tqdm() {
    //     if (0..blocks.len()).all(|i| fall_height(&blocks, i, Some(d)) == 0) {
    //         can_disintegrate += 1;
    //     }
    // }
    // println!("Task 1: {can_disintegrate}");

    let graph: DependencyGraph = blocks.into();
    let counts: Vec<usize> = (0..graph.blocks.len()).map(|i| graph.count_fallen(i)).collect();

    println!("Task 1: {}", counts.iter().filter(|&&i| i == 0).count());

    println!("Task 2: {}", counts.iter().sum::<usize>());

    Ok(())
}

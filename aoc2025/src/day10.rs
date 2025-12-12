use indicatif::ProgressIterator;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::{fs::read_to_string, i32, str::FromStr};

use regex::Regex;

struct Machine {
    target_state: u16,
    buttons: Vec<u16>,
    joltages: Vec<i32>,
}

fn check_recursive(current_state: u16, to_check: &[u16], pressed_already: u32) -> Option<u32> {
    if current_state == 0 {
        return Some(pressed_already);
    }
    if to_check.len() == 0 {
        return None;
    }
    let mut best_cost = None;
    if let Some(cost) = check_recursive(current_state, &to_check[1..], pressed_already) {
        best_cost = Some(cost);
    }
    if let Some(cost) = check_recursive(
        current_state ^ to_check[0],
        &to_check[1..],
        pressed_already + 1,
    ) {
        // Ugly conditional, could probably be made much more elegant with .or()/.and()/whatevs?
        if let Some(prev) = best_cost
            && cost < prev
        {
            best_cost = Some(cost);
        } else if best_cost == None {
            best_cost = Some(cost);
        }
    }

    return best_cost;
}

impl FromStr for Machine {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut target_state = 0;
        let mut buttons = vec![];
        let mut joltages = vec![];

        let re_lights = Regex::new(r"\[([\.#]+)\]").unwrap();
        for light in re_lights.captures_iter(s) {
            target_state = 0;
            for (i, char) in light[1].chars().enumerate() {
                if char == '#' {
                    target_state |= 1 << i;
                }
            }
        }

        let re_buttons = Regex::new(r"\(([\d,]+)\)").unwrap();
        for button in re_buttons.captures_iter(s) {
            let mut button_mask = 0;
            for num in button[1].split(',') {
                let idx: u32 = num.parse().expect("Invalid int");
                button_mask |= 1 << idx;
            }
            buttons.push(button_mask);
        }

        Ok(Self {
            target_state,
            buttons,
            joltages,
        })
    }
}

impl Machine {
    fn solve(&self) -> Option<u32> {
        check_recursive(self.target_state, &self.buttons, 0)
    }
}

#[derive(Debug)]
struct MachineV2 {
    joltages: Vec<usize>,
    btn2row: Vec<Vec<usize>>,
    row2btn: Vec<Vec<usize>>,
}

impl FromStr for MachineV2 {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut btn2row = vec![];
        let mut joltages = vec![];
        let re_joltages = Regex::new(r"\{([\d,]+)\}").unwrap();
        for joltage in re_joltages.captures_iter(s) {
            for num in joltage[1].split(',') {
                joltages.push(num.parse().expect("Couldn't parse joltage"));
            }
        }

        let re_buttons = Regex::new(r"\(([\d,]+)\)").unwrap();
        for capture in re_buttons.captures_iter(s) {
            let button = capture[1]
                .split(',')
                .map(|n| n.parse().unwrap())
                .collect::<Vec<_>>();
            btn2row.push(button);
        }

        Ok(Self::new(joltages, btn2row))
    }
}

impl MachineV2 {
    fn new(joltages: Vec<usize>, btn2row: Vec<Vec<usize>>) -> Self {
        let mut row2btn = vec![vec![]; joltages.len()];
        for (i, rows) in btn2row.iter().enumerate() {
            for &row in rows {
                row2btn[row].push(i);
            }
        }

        Self {
            joltages,
            btn2row,
            row2btn,
        }
    }

    /*
    fn solve_v2(&self) -> Option<usize> {
        let solution = self.solve_recurse(&self.joltages, &vec![None; self.btn2row.len()]);

        if let Some(counts) = solution {
            // TODO: Test where it goes wrong!
            let mut jolt = vec![0, self.joltages.len()];
            for (btn, count) in counts.iter().enumerate() {
                for row in self.btn2row[btn] {
                    jolt[row] += *count;
                }
            }
        };

        Some(solution.iter().sum())
    }

    fn solve_recurse(
        &self,
        target: &Vec<usize>,
        presses: &Vec<Option<usize>>,
    ) -> Option<Vec<usize>> {
        if target.iter().all(|&n| n == 0) {
            return Some(presses.iter().map(|opt| opt.unwrap_or(0)).collect());
        }

        let mut best_row = 1000;
        let mut best_heuristic = 100;

        for (row, btns) in self.row2btn.iter().enumerate() {
            let num_btns = btns.iter().filter(|&&btn| presses[btn] == None).count();
            if num_btns == 0 {
                continue;
            }
            let tgt = target[row];
            // let heuristic: usize = ((tgt - num_btns + 1)..=tgt).product();
            let heuristic = num_btns;
            if heuristic < best_heuristic {
                best_heuristic = heuristic;
                best_row = row;
            }
        }
        if best_row == 1000 {
            return None;
        }

        // Prefer buttons with many wires (they fill up joltage faster!)
        let btn = *self.row2btn[best_row]
            .iter()
            .filter(|b| presses[**b] == None)
            .max_by_key(|&&btn| self.btn2row[btn].len())
            .expect("No button in row for some reason!");

        let max_presses = target[best_row];
        if best_heuristic == 1 {
            // Single button determines row, yay!
            let mut next_target = target.clone();
            let mut next_presses = presses.clone();

            for &row in &self.btn2row[btn] {
                if next_target[row] < max_presses {
                    return None;
                }
                next_target[row] -= max_presses;
            }
            next_presses[btn] = Some(max_presses);

            return self.solve_recurse(&next_target, &next_presses);
        } else {
            let mut best_score: Option<usize> = None;
            // Save some allocations by re-using these...
            let mut next_target = target.clone();
            let mut next_presses = presses.clone();
            for n in 0..=max_presses {
                next_presses[btn] = Some(n);
                for &row in &self.btn2row[btn] {
                    if target[row] < n {
                        break;
                    }
                    next_target[row] = target[row] - n;
                }

                let recurse = self.solve_recurse(&next_target, &next_presses);
                best_score = match (best_score, recurse) {
                    (Some(a), Some(b)) => Some(a.min(b)),
                    (Some(a), None) => Some(a),
                    (None, Some(b)) => Some(b),
                    (None, None) => None,
                };
            }
            return best_score;
        }
    }
    */

    /*
    fn with_button_pressed(&self, idx: usize, count: i32) -> MachineV2 {
        let mut joltages = self.joltages.clone();
        let mut buttons = self.buttons.clone();

        for i in 0..joltages.len() {
            joltages[i] -= count * buttons[idx][i];
        }
        buttons.remove(idx);
        return Self { joltages, buttons };
    }
    */

    /*
    fn solve(&self) -> Option<i32> {
        if self.joltages.iter().all(|&c| c == 0) {
            return Some(0);
        }
        if self.buttons.len() == 0 {
            return None;
        }

        // Isolate the row with the fewest buttons
        let mut best_row = 0;
        let mut best_rowcount = 100;
        let mut best_active = Vec::new();
        for row in 0..self.buttons[0].len() {
            let mut rowcount = 0;
            let mut active = Vec::new();
            for (col, btn) in self.buttons.iter().enumerate() {
                if btn[row] > 0 {
                    rowcount += 1;
                    active.push(col);
                }
            }
            if rowcount > 0 && rowcount < best_rowcount {
                best_rowcount = rowcount;
                best_row = row;
                best_active = active;
            }
        }

        let button = best_active[0];
        if best_rowcount == 1 {
            // We have no choice but to press `button` joltages[best_row] times!
            return self
                .with_button_pressed(button, self.joltages[best_row])
                .solve()
                .map(|cost| cost + self.joltages[best_row]);
        } else {
            return (0..=self.joltages[best_row])
                .into_par_iter()
                .filter_map(|count| {
                    self.with_button_pressed(button, count)
                        .solve()
                        .map(|res| res + count)
                })
                .min();
        }
    }
    */
}

pub fn solve(path: &str) {
    let machines: Vec<Machine> = read_to_string(path)
        .unwrap()
        .lines()
        .map(Machine::from_str)
        .collect::<Result<Vec<Machine>, _>>()
        .expect("Couldn't parse machines!");

    let mut total_cost = 0;
    for machine in machines {
        total_cost += machine.solve().expect("Unsolvable machine!");
    }

    println!("Part 1: {total_cost}");

    let machines: Vec<MachineV2> = read_to_string(path)
        .unwrap()
        .lines()
        .map(MachineV2::from_str)
        .collect::<Result<Vec<MachineV2>, _>>()
        .expect("Couldn't parse machines!");

    let mut total_cost = 0;
    /*
        for machine in machines.iter().progress() {
            if let Some(cost) = machine.solve_v2() {
                total_cost += cost
            } else {
                println!("Unsolvable:");
                println!("{machine:?}");
            }
        }
        println!("Part 2: {total_cost}");
    */
}

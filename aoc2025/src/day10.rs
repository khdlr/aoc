use std::{fs::read_to_string, str::FromStr};

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

#[allow(unused)]
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
    println!("Part 2: {}", 0);
}

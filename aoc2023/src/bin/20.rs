use std::collections::HashMap;
use std::error::Error;
use std::{env, fs::read_to_string};

type Node = String;
type State = bool;
type NetworkState = HashMap<Node, State>;

const LOW: State = true;
const HIGH: State = false;

#[derive(Debug)]
enum Operation {
    Broadcast,
    FlipFlop,
    Conjunction,
    Output,
}

#[derive(Debug)]
struct Gate {
    op: Operation,
    label: String,
    inputs: Vec<Node>,
    outputs: Vec<Node>,
}

impl From<&str> for Gate {
    fn from(value: &str) -> Self {
        let [left, right] = value.split(" -> ").collect::<Vec<_>>().try_into().unwrap();
        let out: Vec<String> = right.split(", ").map(str::to_owned).collect();
        let (op, label) = if left == "broadcaster" {
            (Operation::Broadcast, "broadcaster".to_string())
        } else if let Some(label) = left.strip_prefix('%') {
            (Operation::FlipFlop, label.to_string())
        } else if let Some(label) = left.strip_prefix('&') {
            (Operation::Conjunction, label.to_string())
        } else {
            panic!("unhandled {left}");
        };
        Gate {
            op,
            label,
            inputs: vec![],
            outputs: out,
        }
    }
}

struct Network {
    nodes: HashMap<Node, Gate>,
}

impl Network {
    fn parse(input: &str) -> Self {
        let mut gates: HashMap<String, Gate> = input
            .lines()
            .map(Gate::from)
            .map(|g| (g.label.clone(), g))
            .collect();

        gates.insert(
            String::from("button"),
            Gate {
                op: Operation::Broadcast,
                label: String::from("button"),
                inputs: vec![],
                outputs: vec![String::from("broadcaster")],
            },
        );
        gates.insert(
            String::from("output"),
            Gate {
                op: Operation::Output,
                label: String::from("output"),
                inputs: vec![],
                outputs: vec![],
            },
        );

        let connections: Vec<(Node, Node)> = gates
            .values()
            .flat_map(|gate| {
                gate.outputs
                    .iter()
                    .map(|o| (gate.label.clone(), o.to_string()))
            })
            .collect();
        for (a, b) in connections {
            gates.get_mut(&b).unwrap().inputs.push(a);
        }

        Network { nodes: gates }
    }

    fn init_state(&self) -> NetworkState {
        self.nodes.keys().map(|k| (k.clone(), LOW)).collect()
    }
}

fn button_press(net: &Network, state: &mut NetworkState) {
    let mut stack = vec![(String::from("button"), LOW)];

    let mut count_low = 0;
    let mut count_high = 0;

    while let Some((label, signal)) = stack.pop() {
        let node = &net.nodes[&label];
        let signal = match node.op {
            Operation::Broadcast => Some(LOW),
            Operation::FlipFlop => {
                if signal == LOW {
                    Some(!state[&label])
                } else {
                    None
                }
            }
            Operation::Conjunction => {
                let all_high = node.inputs.iter().map(|c| state[c]).all(|s| s == HIGH);
                if all_high {
                    Some(LOW)
                } else {
                    Some(HIGH)
                }
            }
            Operation::Output => None,
        };
        if let Some(signal) = signal {
            state.insert(label.clone(), signal);
            for child in &node.outputs {
                if signal == LOW {
                    count_low += 1;
                } else {
                    count_high += 1;
                }
                println!(
                    "{label} -{}> {child}",
                    if signal == LOW { "low" } else { "high" }
                );
                stack.push((child.clone(), signal));
            }
        }
    }

    println!(
        "{count_low} low pulses, {count_high} high pulses -> {}",
        count_low * count_high
    );
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let network = Network::parse(&input);
    let mut state = network.init_state();

    button_press(&network, &mut state);

    Ok(())
}

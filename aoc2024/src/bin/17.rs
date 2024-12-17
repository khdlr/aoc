use std::error::Error;
use std::{env, fs::read_to_string};

type Data = i64;

#[derive(Clone)]
struct Y8Computer {
    program: Vec<u8>,
    pointer: usize,
    reg_a: Data,
    reg_b: Data,
    reg_c: Data,
    output: Vec<Data>,
}

impl Y8Computer {
    pub fn load(a: Data, b: Data, c: Data, program: Vec<u8>) -> Self {
        Self {
            program,
            pointer: 0,
            reg_a: a,
            reg_b: b,
            reg_c: c,
            output: vec![],
        }
    }

    fn get_operand(&self) -> Option<Data> {
        self.program.get(self.pointer + 1).map(|&l| l as Data)
    }

    fn combo(&self, literal: Data) -> Data {
        match literal {
            0 => 0,
            1 => 1,
            2 => 2,
            3 => 3,
            4 => self.reg_a,
            5 => self.reg_b,
            6 => self.reg_c,
            _ => panic!("Illegal combo operand!"),
        }
    }

    // Run a single instruction. Returns None if program halts.
    pub fn step(&mut self) -> Option<()> {
        let instruction = self.program.get(self.pointer)?;
        let data = self.get_operand()?;
        self.pointer += 2;
        match instruction {
            // adv
            0 => {
                self.reg_a =
                    self.reg_a / 2i64.pow(self.combo(data).try_into().expect("Exponent overflow!"))
            }
            // bxl
            1 => self.reg_b = self.reg_b ^ data,
            // bst
            2 => self.reg_b = self.combo(data) % 8,
            // jnz
            3 => {
                if self.reg_a != 0 {
                    self.pointer = data.try_into().expect("Illegal jump address!")
                }
            }
            // bxc
            4 => self.reg_b = self.reg_b ^ self.reg_c,
            // out
            5 => self.output.push(self.combo(data) % 8),
            // bdv
            6 => {
                self.reg_b =
                    self.reg_a / 2i64.pow(self.combo(data).try_into().expect("Exponent overflow!"))
            }
            // bdv
            7 => {
                self.reg_c =
                    self.reg_a / 2i64.pow(self.combo(data).try_into().expect("Exponent overflow!"))
            }
            _ => panic!("Illegal opcode!"),
        }

        Some(())
    }

    pub fn run_with_a(&self, a: i64) -> Vec<Data> {
        let mut computer = self.clone();
        computer.reg_a = a;
        while let Some(_) = computer.step() {}
        computer.output
    }
}

fn find_prefix_cycle(
    original_computer: &Y8Computer,
    prefix: Vec<Data>,
    search_start: i64,
    search_seq: Vec<i64>,
) -> (i64, Vec<i64>) {
    let mut sequence = vec![];
    let mut start = 0;
    let mut last_step = None;

    println!("Searching prefix cycle for prefix {prefix:?}");

    let mut a_val = search_start;
    for offset in search_seq.iter().cycle() {
        let mut computer = original_computer.clone();
        computer.reg_a = a_val;
        while let Some(_) = computer.step() {
            if computer.output.len() >= prefix.len() {
                break;
            }
        }
        if computer.output.len() >= prefix.len() && &computer.output[0..prefix.len()] == &prefix {
            if let Some(last) = last_step {
                sequence.push(a_val - last);
                start = a_val;
            }
            last_step = Some(a_val);

            if sequence.len() > 20000 {
                break;
            }
        }
        a_val += offset;
    }

    // Find cycle
    let mut cycle_length = 0;
    for cycle_len in 1..(sequence.len() / 2) {
        if sequence
            .iter()
            .enumerate()
            .all(|(i, c)| *c == sequence[i % cycle_len])
        {
            cycle_length = cycle_len;
            break;
        }
    }
    if cycle_length > 0 {
        println!("Found cycle of length {cycle_length}!");
    }
    let cycle: Vec<i64> = sequence[0..cycle_length].to_vec();
    println!("{cycle:?}");

    (start, cycle)
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let mut lines = input.lines();
    let reg_a: Data = lines
        .next()
        .expect("No reg A")
        .strip_prefix("Register A: ")
        .expect("No A prefix")
        .parse()
        .expect("Couldn't parse register A value");
    let reg_b: Data = lines
        .next()
        .expect("No reg B")
        .strip_prefix("Register B: ")
        .expect("No B prefix")
        .parse()
        .expect("Couldn't parse register B value");
    let reg_c: Data = lines
        .next()
        .expect("No reg C")
        .strip_prefix("Register C: ")
        .expect("No C prefix")
        .parse()
        .expect("Couldn't parse register C value");
    let _ = lines.next();
    let program: Vec<u8> = lines
        .next()
        .expect("No Program")
        .strip_prefix("Program: ")
        .expect("Wrong program line")
        .split(',')
        .map(|c| c.parse::<u8>().expect("Couldn't parse instruction"))
        .collect();

    let mut computer = Y8Computer::load(reg_a, reg_b, reg_c, program.clone());
    while let Some(_) = computer.step() {}
    let part1 = computer
        .output
        .iter()
        .map(|c| format!("{c}"))
        .collect::<Vec<_>>()
        .join(",");

    let computer = Y8Computer::load(reg_a, reg_b, reg_c, program.clone());
    println!("1: {:?}", computer.run_with_a(1));
    println!("8: {:?}", computer.run_with_a(8));
    println!("8*64: {:?}", computer.run_with_a(8 * 64));
    println!("64*64: {:?}", computer.run_with_a(64 * 64));

    let mut base_a = 0;
    for i in 1..=program.len() {
        println!("i={i}");
        let mut found = false;
        for add_a in 0.. {
            let outp = computer.run_with_a(base_a + add_a);
            println!("{outp:?}");
            if (outp.len() >= i) && outp[outp.len() - i] == program[program.len() - i] as i64 {
                println!("{} produces {outp:?}", base_a + add_a);
                base_a = 8 * (base_a + add_a);
                found = true;
                break;
            }
        }
    }
    println!(
        "105811504359834: {:?}",
        computer.run_with_a(105811504359834)
    );

    // let mut search_start = 0;
    // let mut search_cycle = vec![1];

    // for prefix_len in 3..10 {
    //     let prefix = program
    //         .iter()
    //         .map(|&c| c as Data)
    //         .take(prefix_len)
    //         .collect();
    //     (search_start, search_cycle) =
    //         find_prefix_cycle(&computer, prefix, search_start, search_cycle.clone());
    // }

    println!("Part 1: {}", part1);

    println!("Part 2: {}", "todo");

    Ok(())
}

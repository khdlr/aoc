use regex::Regex;
use std::error::Error;
use std::fmt::Display;
use std::{env, fs::read_to_string};
type F = fraction::Fraction;

struct Equation {
    ax: F,
    ay: F,
    bx: F,
    by: F,
    px: F,
    py: F,
}

fn parse_frac(inp: &str) -> F {
    F::from(inp.parse::<i64>().expect("Parsing error"))
}

impl Equation {
    fn parse(input: &str) -> Vec<Self> {
        let re = Regex::new(r"(?m)Button A: X\+([0-9]+), Y\+([0-9]+)\nButton B: X\+([0-9]+), Y\+([0-9]+)\nPrize: X=([0-9]+), Y=([0-9]+)").expect("Regex compile error");
        // let re = Regex::new(r"(?m)Button A: X\+([0-9]+), Y+([0-9]+)\nButton B: X\+([0-9]+), Y+([0-9]+)\nPrize: X=([0-9]+), Y=([0-9]+)").expect("Regex compile error");
        re.captures_iter(input)
            .filter_map(|cap| match cap.extract() {
                (_, [ax, ay, bx, by, px, py]) => Some(Equation {
                    ax: parse_frac(ax),
                    ay: parse_frac(ay),
                    bx: parse_frac(bx),
                    by: parse_frac(by),
                    px: parse_frac(px),
                    py: parse_frac(py),
                }),
            })
            .collect()
    }

    fn solve(&self) -> Option<(i64, i64)> {
        let det = self.ax * self.by - self.ay * self.bx;
        let sol = [self.by / det, -self.ay / det, -self.bx / det, self.ax / det];
        let a = sol[0] * self.px + sol[2] * self.py;
        let b = sol[1] * self.px + sol[3] * self.py;

        // print!("{self}");
        if let (Ok(a), Ok(b)) = (a.try_into(), b.try_into()) {
            // println!(" --> a={a}, b={b}");
            Some((a, b))
        } else {
            // println!(" --> No integer solution");
            None
        }
    }
}

impl Display for Equation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[ a⋅{}+b⋅{}={} ; a⋅{}+b⋅{}={} ]",
            self.ax, self.bx, self.px, self.ay, self.by, self.py
        )
    }
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let mut eqns = Equation::parse(&input);

    let cost: i64 = eqns
        .iter()
        .filter_map(Equation::solve)
        .map(|(a, b)| 3 * a + 1 * b)
        .sum();

    println!("Part 1: {}", cost);

    for eq in eqns.iter_mut() {
        eq.px += F::from(10000000000000i64);
        eq.py += F::from(10000000000000i64);
    }

    let cost2: i64 = eqns
        .iter()
        .filter_map(Equation::solve)
        .map(|(a, b)| 3 * a + 1 * b)
        .sum();

    println!("Part 2: {}", cost2);

    Ok(())
}

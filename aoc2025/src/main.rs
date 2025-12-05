use clap::Parser;

mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
mod day11;
mod day12;
mod utils;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Name of the person to greet
    #[arg(required = true)]
    day: u8,

    #[arg(default_value = "both")]
    stage: String,
}

fn main() {
    let args = Args::parse();

    for stage in ["test", "full"] {
        if &args.stage != "both" && args.stage != stage {
            continue;
        }
        println!("==== Stage {stage} ====");
        let input = format!("inputs/day{:02}.{stage}", args.day);
        match args.day {
            1 => day01::solve(&input),
            2 => day02::solve(&input),
            3 => day03::solve(&input),
            4 => day04::solve(&input),
            5 => day05::solve(&input),
            6 => day06::solve(&input),
            7 => day07::solve(&input),
            8 => day08::solve(&input),
            9 => day09::solve(&input),
            10 => day10::solve(&input),
            11 => day11::solve(&input),
            _ => {}
        }
    }
}

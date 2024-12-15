use std::{fmt::Display, str::FromStr};

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug, PartialOrd, Ord)]
pub enum Dir {
    N,
    W,
    E,
    S,
}

pub const DIRECTIONS: [Dir; 4] = [Dir::N, Dir::W, Dir::E, Dir::S];

impl Dir {
    pub fn apply(&self, inp: (i32, i32)) -> (i32, i32) {
        let (i, j) = inp;
        match self {
            Dir::N => (i - 1, j),
            Dir::W => (i, j - 1),
            Dir::E => (i, j + 1),
            Dir::S => (i + 1, j),
        }
    }

    pub fn from_arrow(chr: char) -> Self {
        match chr {
            '^' => Dir::N,
            '<' => Dir::W,
            '>' => Dir::E,
            'v' => Dir::S,
            _ => panic!("Cannot convert `{chr}` to direction!"),
        }
    }
}

impl Display for Dir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Dir::N => '↑',
                Dir::W => '←',
                Dir::E => '→',
                Dir::S => '→',
            }
        )
    }
}

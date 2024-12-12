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
}

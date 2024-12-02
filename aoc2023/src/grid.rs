use std::{
    fmt::Display,
    ops::{Index, IndexMut},
    path::Path,
};

use image::save_buffer;

pub struct GridIterator<'a, T>
where
    T: Clone,
{
    grid: &'a Grid<T>,
    pos: (usize, usize),
}

impl<'a, T> Iterator for GridIterator<'a, T>
where
    T: Clone,
{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let res = if self.pos.0 < self.grid.height() as usize {
            Some(&self.grid.grid[self.pos.0][self.pos.1])
        } else {
            None
        };

        let (mut y, mut x) = self.pos;
        if x < (self.grid.width() - 1) as usize {
            x += 1;
        } else {
            y += 1;
            x = 0;
        }
        self.pos = (y, x);
        res
    }
}

impl<'a, T> GridIterator<'a, T>
where
    T: Clone,
{
    pub fn with_pos(self) -> GridIteratorWithPos<'a, T> {
        GridIteratorWithPos::<'a, T> {
            grid: self.grid,
            pos: self.pos,
        }
    }
}

pub struct GridIteratorWithPos<'a, T>
where
    T: Clone,
{
    grid: &'a Grid<T>,
    pos: (usize, usize),
}

impl<'a, T> Iterator for GridIteratorWithPos<'a, T>
where
    T: Clone,
{
    type Item = ((i32, i32), &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        let res = if self.pos.1 < self.grid.height() as usize {
            Some((
                (self.pos.0 as i32, self.pos.1 as i32),
                &self.grid.grid[self.pos.0][self.pos.1],
            ))
        } else {
            None
        };

        let (mut y, mut x) = self.pos;
        if x < (self.grid.width() - 1) as usize {
            x += 1;
        } else {
            y += 1;
            x = 0;
        }
        self.pos = (y, x);
        res
    }
}

impl<'a, U> FromIterator<((i32, i32), &'a U)> for Grid<U>
where
    U: Clone,
{
    fn from_iter<T: IntoIterator<Item = ((i32, i32), &'a U)>>(iter: T) -> Self {
        let elements: Vec<((i32, i32), U)> =
            iter.into_iter().map(|(pos, u)| (pos, u.clone())).collect();
        let h = elements.iter().map(|el| el.0 .0).max().unwrap() as usize;
        let w = elements.iter().map(|el| el.0 .1).max().unwrap() as usize;
        let grid = vec![vec![elements[0].1.clone(); w]; h];
        let mut grid = Grid { grid };

        for (pos, val) in elements {
            grid.set(pos, val.clone());
        }
        grid
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Grid<T>
where
    T: Clone,
{
    pub grid: Vec<Vec<T>>,
}

impl<T> Grid<T>
where
    T: Clone,
{
    pub fn iter(&self) -> GridIterator<T> {
        GridIterator::<T> {
            grid: &self,
            pos: (0, 0),
        }
    }

    pub fn map<U>(&self, mut fun: impl FnMut(&T) -> U) -> Grid<U>
    where
        U: Clone,
    {
        let grid: Vec<Vec<U>> = self
            .grid
            .iter()
            .map(|row| row.iter().map(|el| fun(el)).collect())
            .collect();
        Grid::<U> { grid }
    }

    pub fn update(&mut self, mut fun: impl FnMut(&T) -> T) {
        self.grid = self
            .grid
            .iter()
            .map(|row| row.iter().map(|el| fun(el)).collect())
            .collect();
    }

    pub fn repeat(&self, vertical: i32, horizontal: i32) -> Grid<T> {
        let expanded_h: Vec<Vec<T>> = self
            .grid
            .iter()
            .map(|row| (1..=horizontal).flat_map(|_| row.clone()).collect())
            .collect();
        let expanded_v: Vec<Vec<T>> = (1..=vertical).flat_map(|_| expanded_h.clone()).collect();
        Grid::<T> { grid: expanded_v }
    }

    pub fn map_with_neighbors<U>(&self, mut fun: impl FnMut(&T, Vec<T>) -> U) -> Grid<U>
    where
        U: Clone,
    {
        let grid = self
            .grid
            .iter()
            .enumerate()
            .map(|(y, row)| {
                let y = y as i32;
                row.iter()
                    .enumerate()
                    .map(|(x, el)| -> U {
                        let x = x as i32;
                        let neighbors: Vec<T> = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]
                            .iter()
                            .filter_map(|(y, x)| self.get((*y as i32, *x as i32)))
                            .collect();
                        fun(el, neighbors)
                    })
                    .collect::<Vec<U>>()
            })
            .collect();
        Grid::<U> { grid }
    }

    pub fn update_with_neighbors(&mut self, mut fun: impl FnMut(&T, Vec<T>) -> T) {
        self.grid = self
            .grid
            .iter()
            .enumerate()
            .map(|(y, row)| {
                let y = y as i32;
                row.iter()
                    .enumerate()
                    .map(|(x, el)| {
                        let x = x as i32;
                        let neighbors: Vec<T> = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]
                            .iter()
                            .filter_map(|(y, x)| self.get((*y as i32, *x as i32)))
                            .collect();
                        fun(el, neighbors)
                    })
                    .collect::<Vec<T>>()
            })
            .collect();
    }

    pub fn idxwhere(&self, fun: impl Fn(&T) -> bool) -> Vec<(i32, i32)> {
        self.grid
            .iter()
            .enumerate()
            .flat_map(|(y, row)| {
                row.iter()
                    .enumerate()
                    .filter_map(|(x, el)| {
                        if fun(el) {
                            Some((y as i32, x as i32))
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<(i32, i32)>>()
            })
            .collect()
    }
}

impl<T> Grid<T>
where
    T: Clone + Default,
{
    pub fn empty(height: i32, width: i32) -> Self {
        let grid = vec![vec![Default::default(); width as usize]; height as usize];
        Self { grid }
    }
}

impl<T> Index<(i32, i32)> for Grid<T>
where
    T: Clone,
{
    type Output = T;

    fn index(&self, index: (i32, i32)) -> &Self::Output {
        &self.grid[index.0 as usize][index.1 as usize]
    }
}

impl<T> IndexMut<(i32, i32)> for Grid<T>
where
    T: Clone,
{
    fn index_mut(&mut self, index: (i32, i32)) -> &mut Self::Output {
        &mut self.grid[index.0 as usize][index.1 as usize]
    }
}

impl<T> Grid<T>
where
    T: ToString + Clone,
{
    pub fn print(&self) {
        for row in &self.grid {
            let line: String = row.iter().map(|c| c.to_string()).collect::<String>();
            println!("{}", line);
        }
    }
}

impl Grid<char> {
    pub fn from_str(str: &str) -> Self {
        let grid = str.lines().map(|line| line.chars().collect()).collect();
        Self { grid }
    }
}

impl Grid<usize> {
    pub fn parse(str: &str) -> Self {
        let grid = str
            .lines()
            .map(|line| {
                line.chars()
                    .map(|c| c.to_string().parse::<usize>().unwrap())
                    .collect()
            })
            .collect();
        Self { grid }
    }
}

impl<T> Grid<T>
where
    T: Clone + Display,
{
    pub fn print_overlay(&self, fun: impl Fn((i32, i32)) -> Option<char>) {
        for i in 0..self.height() {
            for j in 0..self.width() {
                match fun((i, j)) {
                    Some(chr) => print!("{}", chr),
                    None => print!("{}", self.get((i, j)).unwrap()),
                }
            }
            println!();
        }
    }
}

impl<T> Grid<T>
where
    T: Clone,
{
    pub fn get(&self, pos: (i32, i32)) -> Option<T> {
        let (y, x) = pos;
        if y < 0 || x < 0 {
            None
        } else {
            self.grid.get(y as usize).cloned()?.get(x as usize).cloned()
        }
    }

    pub fn get_wrap(&self, pos: (i32, i32)) -> T {
        let (mut y, mut x) = pos;
        let h = self.height();
        y = ((y % h) + h) % h;
        let w = self.height();
        x = ((x % w) + w) % w;
        self.grid[y as usize][x as usize].clone()
    }

    pub fn set(&mut self, pos: (i32, i32), val: T) {
        assert!(pos.0 >= 0, "y < 0");
        assert!(pos.1 >= 0, "x < 0");
        self.grid[pos.0 as usize][pos.1 as usize] = val;
    }

    pub fn height(&self) -> i32 {
        self.grid.len() as i32
    }

    pub fn width(&self) -> i32 {
        self.grid[0].len() as i32
    }

    pub fn print_map<F, S>(&self, fun: F)
    where
        F: Fn(&T) -> S,
        S: Into<String>,
    {
        for row in &self.grid {
            let line: String = row.iter().map(|c| fun(&c).into()).collect::<String>();
            println!("{}", line);
        }
    }

    pub fn save_img(&self, filename: &str, fun: impl Fn(&T) -> (u8, u8, u8)) {
        let mut bytes: Vec<u8> =
            Vec::with_capacity(self.height() as usize * self.width() as usize * 3);
        for row in &self.grid {
            for val in row {
                let (r, g, b) = fun(val);
                bytes.push(r);
                bytes.push(g);
                bytes.push(b);
            }
        }
        // let img Image
        let _ = save_buffer(
            &Path::new(filename),
            &bytes,
            self.width() as u32,
            self.height() as u32,
            image::ColorType::Rgb8,
        );
    }
}

impl<T> Display for Grid<T>
where
    T: Display + Clone,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.height() {
            for j in 0..self.width() {
                write!(f, "{}", self.get((i, j)).unwrap())?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

use std::{error::Error, ops::{Mul, Div, Deref, DerefMut, Add, Sub}, fmt::Display};
use std::{env, fs::read_to_string};
use itertools::Itertools;
use ibig::{ibig, IBig, ops::Abs};

#[derive(Debug, Clone, Eq, PartialEq)]
struct Z<const N: usize> {
    vals: [IBig; N]
}

impl<const N: usize> Z<N> {
    fn dot(&self, other: &Z<N>) -> IBig {
        self.vals.iter().zip(other.vals.iter()).fold(ibig!(0), |acc, (a, b)| acc + a*b)
    }

    fn canonical(&self) -> Z<N> {
        let mut gcd: IBig = self.vals.iter().cloned().reduce(|a, b| gcd(&a, &b)).unwrap();
        gcd = gcd.max(ibig!(1));
        let leader = self.vals.iter().cloned().find(|c| *c != ibig!(0)).unwrap_or(ibig!(0));
        if leader < ibig!(0) {
            gcd = -gcd;
        }
        Z::<N> { vals: self.vals.clone().map(|v| v / &gcd) }
    }

    fn dehomogenize(&self) -> Vec<f64> {
        let h = self[0].to_f64();
        self.vals.iter().skip(1).map(|v| v.to_f64() / h).collect()
    }
}

impl TryInto<Z<3>> for Z<4> {
    type Error = String;

    fn try_into(self) -> Result<Z<3>, Self::Error> {
        let h = &self[0];
        if self.iter().skip(1).all(|i| i % h == ibig!(0)) {
            Ok(Z::<3> { vals: [&self[1] / h, &self[2] / h, &self[3] / h]})
        } else {
            Err("Couldn't dehomogenize to int".to_owned())
        }
    }
}

impl<const N: usize> Display for Z<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;
        for entry in &self.vals {
            write!(f, "{entry}, ")?;
        }
        write!(f, "}}")
    }
}

impl<const N: usize> Add<&Z<N>> for &Z<N> {
    type Output = Z<N>;

    fn add(self, rhs: &Z<N>) -> Self::Output {
        Z::<N> { vals: self.vals.iter().zip(rhs.vals.iter()).map(|(a, b)| a + b).collect::<Vec<_>>().try_into().unwrap() }
    }
}

impl<const N: usize> Sub<&Z<N>> for &Z<N> {
    type Output = Z<N>;

    fn sub(self, rhs: &Z<N>) -> Self::Output {
        Z::<N> { vals: self.vals.iter().zip(rhs.vals.iter()).map(|(a, b)| a - b).collect::<Vec<_>>().try_into().unwrap() }
    }
}

impl<const N: usize> Mul<&Z<N>> for &Z<N> {
    type Output = Z<N>;

    fn mul(self, rhs: &Z<N>) -> Self::Output {
        Z::<N> { vals: self.vals.iter().zip(rhs.vals.iter()).map(|(a, b)| a * b).collect::<Vec<_>>().try_into().unwrap() }
    }
}

fn cross(a: &Z<3>, b: &Z<3>) -> Z<3> {
    let a = &a.vals;
    let b = &b.vals;
    Z::<3> { vals: [
        &a[1] * &b[2] - &a[2] * &b[1],
        &a[2] * &b[0] - &a[0] * &b[2],
        &a[0] * &b[1] - &a[1] * &b[0],
    ]}
}

impl<const N: usize> From<[i128; N]> for Z<N> {
    fn from(vals: [i128; N]) -> Self {
        Z::<N> { vals: vals.map(|i| i.into()) }
    }
}

impl<const N: usize> Deref for Z<N> {
    type Target = [IBig; N];

    fn deref(&self) -> &Self::Target {
        &self.vals
    }
}

impl<const N: usize> DerefMut for Z<N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.vals
    }
}

impl<const N: usize> Mul<&Z<N>> for &IBig {
    type Output = Z<N>;

    fn mul(self, rhs: &Z<N>) -> Self::Output {
        Z::<N> { vals: rhs.vals.clone().map(|c| c * self) }
    }
}

impl<const N: usize> Mul<&IBig> for &Z<N> {
    type Output = Z<N>;

    fn mul(self, rhs: &IBig) -> Self::Output {
        Z::<N> { vals: self.vals.clone().map(|c| c * rhs) }
    }
}

impl<const N: usize> Div<&Z<N>> for &Z<N> {
    type Output = Z<N>;

    fn div(self, rhs: &Z<N>) -> Self::Output {
        Z::<N> { vals: self.vals.iter().zip(rhs.vals.iter()).map(|(a, b)| a / b).collect::<Vec<_>>().try_into().unwrap() }
    }
}

impl<const N: usize> Div<&IBig> for &Z<N> {
    type Output = Z<N>;

    fn div(self, rhs: &IBig) -> Self::Output {
        Z::<N> { vals: self.vals.clone().map(|v| v / rhs) }
    }
}

fn gcd(inp1: &IBig, inp2: &IBig) -> IBig {
    let inp1 = inp1.clone().abs();
    let inp2 = inp2.clone().abs();
    let mut a = inp1.clone().max(inp2.clone());
    let mut b = inp1.clone().min(inp2.clone());
    while b > ibig!(0) {
        (b, a) = (&a - (&a / &b) * &b, b);
    }
    a
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Stone {
    pos: Z<3>,
    mov: Z<3>,
    line: Line,
}

impl Display for Stone {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}, {}, {} @ {}, {}, {}",
               self.pos[0], self.pos[1], self.pos[2],
               self.mov[0], self.mov[1], self.mov[2])
    }
}

impl Stone {
    fn ignore_z(&self) -> Stone {
        let mut stone = self.clone();
        stone.pos[2] = ibig!(0);
        stone.mov[2] = ibig!(0);
        stone.line = Line::new(stone.pos.clone(), stone.mov.clone());
        stone
    }

    fn from(str: &str) -> Stone {
        let ab: Vec<&str> = str.split(" @ ").collect();
        let a: Vec<IBig> = ab[0].split(", ")
            .map(|num| num.trim().parse().unwrap()).collect();
        let b: Vec<IBig> = ab[1].split(", ")
            .map(|num| num.trim().parse().unwrap()).collect();
        let pos = Z::<3> { vals: a.try_into().unwrap() };
        let mov = Z::<3> { vals: b.try_into().unwrap() };
        let line = Line::new(pos.clone(), mov.clone());
        Stone { pos, mov, line }
    }

    fn intersect(&self, other: &Stone) -> Relation {
        let Some(pt) = self.line.meet(&other.line) else {
            return Relation::Windschief;
        };
        if pt[0] == ibig!(0) {
            return Relation::Parallel;
        }
        let pt_d = pt.dehomogenize();

        // Check if pt is in the future for self
        let [x, y, z] = &self.pos.vals;
        let [dx, dy, dz] = &self.mov.vals;
        let dot = (pt_d[0] - x.to_f64()) * (dx.to_f64()) +
                  (pt_d[1] - y.to_f64()) * (dy.to_f64()) +
                  (pt_d[2] - z.to_f64()) * (dz.to_f64());
        if dot < 0.0 {
            return Relation::InThePast(pt);
        }

        // Check if pt is in the future for self
        let [x, y, z] = &other.pos.vals;
        let [dx, dy, dz] = &other.mov.vals;
        let dot = (pt_d[0] - x.to_f64()) * (dx.to_f64()) +
                  (pt_d[1] - y.to_f64()) * (dy.to_f64()) +
                  (pt_d[2] - z.to_f64()) * (dz.to_f64());
        if dot < 0.0 {
            return Relation::InThePast(pt);
        }

        return Relation::Intersect(pt);
    }

}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Relation {
    Windschief,
    Parallel,
    Intersect(Z<4>),
    InThePast(Z<4>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Line { d: Z<3>, m: Z<3> }

impl Line {
    fn new(pos: Z<3>, mov: Z<3>) -> Self {
        // Following "Geometric Intuition"
        let m = cross(&pos, &mov);
        let d = mov;
        Self { d, m }
    }

    fn coplanar(&self, h: &Line) -> bool {
        self.d.dot(&h.m) + self.m.dot(&h.d) == ibig!(0)
    }

    fn meet(&self, h: &Line) -> Option<Z<4>> {
        if self.coplanar(h) {
            let crs = cross(&self.d, &h.d);
            let f1 = &self.m.dot(&h.d) * &crs;
            let f2 = &self.d * &h.m.dot(&crs);
            let f3 = &h.d * &self.m.dot(&crs);
            let f = &(&f1 + &f2) - &f3;
            let denom = crs.dot(&crs);
            Some(Z::<4> { vals: [denom, f[0].clone(), f[1].clone(), f[2].clone()] })
        } else {
            None
        }
    }

    fn join(&self, other: &Line) -> Option<Z<4>> {
        if !self.coplanar(other) {
            return None;
        }
        let p0 = self.m.dot(&other.d);
        let p = cross(&self.d, &other.d);

        return Some(Z::<4> { vals: [p0, p[0].clone(), p[1].clone(), p[2].clone()] });
    }
}

fn print_mat<const N: usize>(mat: &Vec<Z<N>>) {
    print!("┌");
    print!("{}", String::from(" ").repeat(9*N));
    println!(" ┐");

    for row in mat.iter() {
        print!("│");
        for el in &row.vals {
            let mut str = format!("{:8}", el);
            if str.len() > 8 {
                str = format!("{}{}", &str[..7], '…');
            }
            print!(" {}", str);
        }
        println!(" │");
    }
    print!("└");
    print!("{}", String::from(" ").repeat(9*N));
    println!(" ┘");
}

fn gauss<const N: usize>(mat: &mut Vec<Z<N>>) {
    let mut h = 0;
    let mut k = 0;
    let m = mat.len();
    while h < m && k < N {
        let i_max = (h..m).max_by_key(|&i| mat[i][k].clone().abs()).unwrap();
        if mat[i_max][k] == ibig!(0) {
            k += 1;
        } else {
            mat.swap(i_max, h);
            for i in h+1..m {
                let f = mat[i][k].clone();
                let g = mat[h][k].clone();
                mat[i][k] = ibig!(0);
                for j in k+1..N {
                    mat[i][j] = &g * &mat[i][j] - &f * &mat[h][j]
                }
            }
            h += 1;
            k += 1;
        }
        for row in mat.iter_mut() {
            *row = row.canonical()
        }
        print_mat(&mat);
    }
}


pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let hail: Vec<Stone> = input.lines().map(Stone::from).collect();

    let mut count = 0;
    for h in hail.iter().map(|h| h.ignore_z()).combinations(2) {
        let int = h[0].intersect(&h[1]);

        let (tmin, tmax) = (200000000000000.0, 400000000000000.0);
        if let Relation::Intersect(pt) = int {
            let xyz = pt.dehomogenize();
            if xyz[0] >= tmin && xyz[0] <= tmax &&
               xyz[1] >= tmin && xyz[1] <= tmax {
                   count += 1;
               }
        }
    }
    println!("Task 1: {count}");

    // let hail = [
    //     Stone::from("1, 0, 0 @ 1, 0, 0"),
    //     Stone::from("1, 0, 1 @ 1, 1, 0"),
    //     Stone::from("1, 0, 2 @ 1, 1, 1"),
    //     Stone::from("1, 0, 3 @ 2, 1, 1"),
    //     Stone::from("1, 0, 4 @ 2, 5, 1"),
    //     Stone::from("1, 0, 5 @ 2, -1, 1"),
    // ];

    let mut mat: Vec<Z<12>> = (0..6).map(|k| {
        let mut vals: [IBig; 12] = Default::default();
        for i in 0..6 {
            let i = i.min(hail.len()-1);
            if k >= 3 {
                vals[i] = hail[i].line.d[k-3].clone()
            } else {
                vals[i] = hail[i].line.m[k].clone()
            }
        }
        for i in 0..6 {
            vals[i+6] = if k == i { ibig!(1) } else { ibig!(0) };
        }
        Z::<12> { vals }
    }).collect();

    print_mat(&mat);
    gauss(&mut mat);
    print_mat(&mat);
    let kernel = &mat[5][6..];
    let line = Line {
        m: Z::<3> { vals: [ kernel[3].clone(), kernel[4].clone(), kernel[5].clone() ] },
        d: Z::<3> { vals: [ kernel[0].clone(), kernel[1].clone(), kernel[2].clone() ] },
    };
    println!("{line:?}");

    let meet0: Z<3> = line.meet(&hail[0].line).unwrap().try_into().unwrap();
    let meet1: Z<3> = line.meet(&hail[1].line).unwrap().try_into().unwrap();
    println!("meet0: {meet0:?}");
    println!("meet1: {meet1:?}");

    let d0 = (&(&meet0 - &hail[0].pos) / &hail[0].mov).vals[0].clone();
    let d1 = (&(&meet1 - &hail[1].pos) / &hail[1].mov).vals[0].clone();

    println!("d0: {d0:?}");
    println!("d1: {d1:?}");
    let mov = &(&meet1 - &meet0) / &(&d1 - &d0);
    println!("mov: {mov:?}");
    let start = &meet0 - &(&mov * &d0);
    println!("start: {start:?}");

    let sum = &(&start[0] + &start[1]) + &start[2];
    println!("Task 2: {sum:?}");

    Ok(())
}

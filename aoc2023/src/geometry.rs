use std::{ops::{Mul, Div, Deref, DerefMut, Add, Sub}, fmt::Display};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Z<const N: usize> {
    pub vals: [i32; N]
}

impl<const N: usize> Z<N> {
    fn dot(&self, other: &Z<N>) -> i32 {
        self.vals.iter().zip(other.vals.iter()).fold(0, |acc, (a, b)| acc + a*b)
    }

    fn canonical(&self) -> Z<N> {
        let mut gcd: i32 = self.vals.iter().cloned().reduce(|a, b| gcd(&a, &b)).unwrap();
        gcd = gcd.max(1);
        let leader = self.vals.iter().cloned().find(|c| *c != 0).unwrap_or(0);
        if leader < 0 {
            gcd = -gcd;
        }
        Z::<N> { vals: self.vals.clone().map(|v| v / &gcd) }
    }

    fn dehomogenize(&self) -> Vec<f64> {
        let h = self[0] as f64;
        self.vals.iter().skip(1).map(|v| *v as f64 / h).collect()
    }
}

impl TryInto<Z<3>> for Z<4> {
    type Error = String;

    fn try_into(self) -> Result<Z<3>, Self::Error> {
        let h = &self[0];
        if self.iter().skip(1).all(|i| i % h == 0) {
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

impl<const N: usize> From<[i32; N]> for Z<N> {
    fn from(vals: [i32; N]) -> Self {
        Z::<N> { vals: vals }
    }
}

impl<const N: usize> Deref for Z<N> {
    type Target = [i32; N];

    fn deref(&self) -> &Self::Target {
        &self.vals
    }
}

impl<const N: usize> DerefMut for Z<N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.vals
    }
}

impl<const N: usize> Mul<&Z<N>> for &i32 {
    type Output = Z<N>;

    fn mul(self, rhs: &Z<N>) -> Self::Output {
        Z::<N> { vals: rhs.vals.clone().map(|c| c * self) }
    }
}

impl<const N: usize> Mul<&i32> for &Z<N> {
    type Output = Z<N>;

    fn mul(self, rhs: &i32) -> Self::Output {
        Z::<N> { vals: self.vals.clone().map(|c| c * rhs) }
    }
}

impl<const N: usize> Div<&Z<N>> for &Z<N> {
    type Output = Z<N>;

    fn div(self, rhs: &Z<N>) -> Self::Output {
        Z::<N> { vals: self.vals.iter().zip(rhs.vals.iter()).map(|(a, b)| a / b).collect::<Vec<_>>().try_into().unwrap() }
    }
}

impl<const N: usize> Div<&i32> for &Z<N> {
    type Output = Z<N>;

    fn div(self, rhs: &i32) -> Self::Output {
        Z::<N> { vals: self.vals.clone().map(|v| v / rhs) }
    }
}

fn gcd(inp1: &i32, inp2: &i32) -> i32 {
    let inp1 = inp1.clone().abs();
    let inp2 = inp2.clone().abs();
    let mut a = inp1.clone().max(inp2.clone());
    let mut b = inp1.clone().min(inp2.clone());
    while b > 0 {
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
        self.d.dot(&h.m) + self.m.dot(&h.d) == 0
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

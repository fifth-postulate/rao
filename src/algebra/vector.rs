use std::{
    iter::zip,
    ops::{Add, Mul, Sub},
};

#[derive(Debug, PartialEq)]
pub struct Vector {
    dimension: usize,
    coordinates: Vec<f64>,
}

impl Vector {
    fn new(coordinates: Vec<f64>) -> Self {
        Self {
            dimension: coordinates.len(),
            coordinates,
        }
    }
}

impl Add for Vector {
    type Output = Option<Self>;

    fn add(self, other: Self) -> Self::Output {
        if self.dimension == other.dimension {
            let coordinates = zip(self.coordinates, other.coordinates)
                .map(|(l, r)| l + r)
                .collect();
            Some(Self::new(coordinates))
        } else {
            None
        }
    }
}

impl Sub for Vector {
    type Output = Option<Self>;

    fn sub(self, other: Self) -> Self::Output {
        if self.dimension == other.dimension {
            let coordinates = zip(self.coordinates, other.coordinates)
                .map(|(l, r)| l - r)
                .collect();
            Some(Self::new(coordinates))
        } else {
            None
        }
    }
}

impl Mul<f64> for Vector {
    type Output = Self;

    fn mul(self, scalar: f64) -> Self::Output {
        let coordinates = self.coordinates.iter().map(|c| c * scalar).collect();

        Self::new(coordinates)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vectors_can_be_added() {
        let u = Vector::new(vec![1.0, 0.0]);
        let v = Vector::new(vec![0.0, 1.0]);

        let sum = u + v;

        assert_eq!(sum, Some(Vector::new(vec![1.0, 1.0])));
    }

    #[test]
    fn vectors_can_be_subtracted() {
        let u = Vector::new(vec![1.0, 1.0]);
        let v = Vector::new(vec![0.0, 1.0]);

        let difference = u - v;

        assert_eq!(difference, Some(Vector::new(vec![1.0, 0.0])));
    }

    #[test]
    fn vectors_can_be_multiplied_by_a_scalar() {
        let u = Vector::new(vec![1.0, 1.0]);

        let multiple = u * 2.0;

        assert_eq!(multiple, Vector::new(vec![2.0, 2.0]));
    }
}

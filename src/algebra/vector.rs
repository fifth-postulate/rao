use crate::algebra::inner::InnerProduct;
use std::{
    iter::{zip, FromIterator, Sum},
    ops::{Add, Mul, Sub},
};

#[derive(Debug, PartialEq)]
pub struct Vector<T> {
    dimension: usize,
    coordinates: Vec<T>,
}

impl<T> Vector<T> {
    fn new(coordinates: Vec<T>) -> Self {
        Self {
            dimension: coordinates.len(),
            coordinates,
        }
    }
}

impl<T> Add for Vector<T>
where
    T: Add<T>,
    Vec<T>: FromIterator<<T as Add>::Output>,
{
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

impl<T> Sub for Vector<T>
where
    T: Sub<T>,
    Vec<T>: FromIterator<<T as Sub>::Output>,
{
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

impl<T> Mul<T> for Vector<T>
where
    T: Mul<T> + Copy,
    Vec<T>: FromIterator<<T as Mul>::Output>,
{
    type Output = Self;

    fn mul(self, scalar: T) -> Self::Output {
        let coordinates = self.coordinates.iter().map(|c| *c * scalar).collect();

        Self::new(coordinates)
    }
}

impl<T> InnerProduct<&Vector<T>> for Vector<T>
where
    T: Add<T> + Mul<T> + Sum<<T as Mul>::Output> + Copy,
{
    type Output = Option<T>;

    fn dot(&self, other: &Self) -> Self::Output {
        if self.dimension == other.dimension {
            let inner_product = zip(self.coordinates.iter(), other.coordinates.iter())
                .map(|(l, r)| *l * *r)
                .sum();
            Some(inner_product)
        } else {
            None
        }
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

    #[test]
    fn vectors_have_a_dot_product() {
        let u = Vector::new(vec![3.0, 1.0]);
        let v = Vector::new(vec![4.0, 2.0]);

        let inner_product = u.dot(&v);

        assert_eq!(inner_product, Some(14.0));
    }
}

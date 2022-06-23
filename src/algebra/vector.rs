use crate::algebra::inner::InnerProduct;
use std::{
    iter::{zip, FromIterator, Sum},
    ops::{Add, Mul, Sub},
};

#[derive(Debug, PartialEq)]
pub struct Vector<T> {
    coordinates: Vec<T>,
}

impl<T> Vector<T> {
    fn new(coordinates: Vec<T>) -> Self {
        Self { coordinates }
    }

    pub fn dimension(&self) -> usize {
        self.coordinates.len()
    }
}

impl<T> From<(T, T)> for Vector<T> {
    fn from(source: (T, T)) -> Self {
        Self::new(vec![source.0, source.1])
    }
}

impl<T> From<(T, T, T)> for Vector<T> {
    fn from(source: (T, T, T)) -> Self {
        Self::new(vec![source.0, source.1, source.2])
    }
}

impl<T> From<(T, T, T, T)> for Vector<T> {
    fn from(source: (T, T, T, T)) -> Self {
        Self::new(vec![source.0, source.1, source.2, source.3])
    }
}

impl<T> From<(T, T, T, T, T)> for Vector<T> {
    fn from(source: (T, T, T, T, T)) -> Self {
        Self::new(vec![source.0, source.1, source.2, source.3, source.4])
    }
}

impl<T> From<(T, T, T, T, T, T)> for Vector<T> {
    fn from(source: (T, T, T, T, T, T)) -> Self {
        Self::new(vec![
            source.0, source.1, source.2, source.3, source.4, source.5,
        ])
    }
}

impl<T> Clone for Vector<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        let coordinates = self.coordinates.to_vec();

        Self::new(coordinates)
    }
}

impl<T> Add for Vector<T>
where
    T: Add<T>,
    Vec<T>: FromIterator<<T as Add>::Output>,
{
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        if self.dimension() == other.dimension() {
            let coordinates = zip(self.coordinates, other.coordinates)
                .map(|(l, r)| l + r)
                .collect();
            Self::new(coordinates)
        } else {
            panic!("dimensions of vectors do not correspond")
        }
    }
}

impl<T> Sub for Vector<T>
where
    T: Sub<T>,
    Vec<T>: FromIterator<<T as Sub>::Output>,
{
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        if self.dimension() == other.dimension() {
            let coordinates = zip(self.coordinates, other.coordinates)
                .map(|(l, r)| l - r)
                .collect();
            Self::new(coordinates)
        } else {
            panic!("dimensions of vectors do not correspond")
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
    type Output = T;

    fn dot(&self, other: &Self) -> Self::Output {
        if self.dimension() == other.dimension() {
            let inner_product = zip(self.coordinates.iter(), other.coordinates.iter())
                .map(|(l, r)| *l * *r)
                .sum();
            inner_product
        } else {
            panic!("dimensions of vectors do not correspond")
        }
    }
}

impl<T> Sum for Vector<T>
where
    T: Add<T>,
    Vec<T>: FromIterator<<T as Add>::Output>,
{
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = Vector<T>>,
    {
        iter.reduce(|l, r| (l + r)).unwrap(/* consider no elements */)
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

        assert_eq!(sum, Vector::new(vec![1.0, 1.0]));
    }

    #[test]
    fn vectors_can_be_subtracted() {
        let u = Vector::new(vec![1.0, 1.0]);
        let v = Vector::new(vec![0.0, 1.0]);

        let difference = u - v;

        assert_eq!(difference, Vector::new(vec![1.0, 0.0]));
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

        assert_eq!(inner_product, 14.0);
    }
}

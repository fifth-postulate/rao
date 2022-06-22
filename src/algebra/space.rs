use crate::algebra::{inner::InnerProduct, vector::Vector};
use std::{
    iter::FromIterator,
    iter::{zip, Sum},
    ops::{Add, AddAssign, Div, Mul, Sub},
};

struct VectorSpace<T> {
    dimension: usize,
    basis: Vec<Vector<T>>,
}

impl<T> VectorSpace<T> {
    pub fn new(dimension: usize) -> Self {
        Self {
            dimension,
            basis: vec![],
        }
    }
}

impl<T> VectorSpace<T>
where
    T: Add<T> + Sub<T> + Mul<T> + Div<T> + Sum<<T as Mul>::Output> + PartialEq + Copy,
    Vec<T>: FromIterator<<T as Div>::Output>,
    Vec<T>: FromIterator<<T as Mul>::Output>,
    Vec<T>: FromIterator<<T as Add>::Output>,
    Vec<T>: FromIterator<<T as Sub>::Output>,
{
    pub fn span(vectors: Vec<Vector<T>>) -> Self {
        if !vectors.is_empty() {
            let minimum_dimension = vectors.iter().map(|v| v.dimension()).min().unwrap();
            let maximum_dimension = vectors.iter().map(|v| v.dimension()).max().unwrap();
            if minimum_dimension == maximum_dimension {
                let dimension = maximum_dimension;
                let mut space = Self::new(dimension);

                for v in vectors {
                    space += v;
                }
                space
            } else {
                panic!("dimensions of vectors do not correspond");
            }
        } else {
            panic!("should have at least one vector to create a span");
        }
    }
}

impl<T> VectorSpace<T>
where
    T: Add<T> + Mul<T> + Div<T> + Sum<<T as Mul>::Output> + PartialEq + Copy,
    Vec<T>: FromIterator<<T as Div>::Output>,
    Vec<T>: FromIterator<<T as Mul>::Output>,
    Vec<T>: FromIterator<<T as Add>::Output>,
{
    pub fn contains(self, v: &Vector<T>) -> bool {
        self.projection(v) == *v
    }

    pub fn projection(&self, v: &Vector<T>) -> Vector<T> {
        if self.dimension == v.dimension() {
            let coefficients: Vec<T> = self.basis.iter().map(|b| b.dot(v) / b.dot(b)).collect();

            let projection = zip(self.basis.iter().cloned(), coefficients)
                .map(|(b, c)| b * c)
                .sum();
            projection
        } else {
            panic!("dimensions of vectors do not correspond")
        }
    }
}

impl<T> AddAssign<Vector<T>> for VectorSpace<T>
where
    T: Add<T> + Mul<T> + Div<T> + Sub<T> + Sum<<T as Mul>::Output> + PartialEq + Copy,
    Vec<T>: FromIterator<<T as Div>::Output>,
    Vec<T>: FromIterator<<T as Mul>::Output>,
    Vec<T>: FromIterator<<T as Add>::Output>,
    Vec<T>: FromIterator<<T as Sub>::Output>,
{
    fn add_assign(&mut self, v: Vector<T>) {
        if self.dimension == v.dimension() {
            if !self.basis.is_empty() {
                let projection = self.projection(&v);
                let b = v - projection;
                // TODO check if b is zero
                self.basis.push(b);
            } else {
                self.basis.push(v);
            }
        } else {
            panic!("dimensions of vectors do not correspond")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vector_space_can_determine_if_a_vector_is_contained() {
        let e1: Vector<f64> = (1.0, 0.0, 0.0).into();
        let e2: Vector<f64> = (0.0, 1.0, 0.0).into();
        let space = VectorSpace::span(vec![e1, e2]);

        let v: Vector<f64> = (1.0, 1.0, 0.0).into();

        assert!(space.contains(&v))
    }

    #[test]
    fn vector_space_can_determine_if_a_vector_is_not_contained() {
        let e1: Vector<f64> = (1.0, 0.0, 0.0).into();
        let e2: Vector<f64> = (0.0, 1.0, 0.0).into();
        let space = VectorSpace::span(vec![e1, e2]);

        let v: Vector<f64> = (0.0, 0.0, 1.0).into();

        assert!(!space.contains(&v))
    }
}

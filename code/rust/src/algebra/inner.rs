pub trait InnerProduct<Rhs = Self> {
    type Output;

    fn dot(&self, other: Rhs) -> Self::Output;
}

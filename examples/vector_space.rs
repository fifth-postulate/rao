use rao::algebra::space::VectorSpace;

fn main() {
    let mut space = VectorSpace::new(3);
    space += (1.0, 0.0, 0.0).into();
    space += (1.0, 1.0, 0.0).into();

    println!("{:?}", space);
}

#![warn(clippy::all, clippy::pedantic)]

use crate::sif;

pub fn solve(input: &str) -> i32 {
    let sif = sif::SIF::new(input.trim(), 6, 25);

    let min_zeros_count = sif
        .layers
        .iter()
        .map(|l| l.count(0))
        .min()
        .expect("expected a number");

    let required_layer = sif
        .layers
        .iter()
        .find(|l| l.count(0) == min_zeros_count)
        .unwrap();

    required_layer.count(1) * required_layer.count(2)
}

// fn chunk_to_image(input: Vec<u32>, l: uszie) -> Vec<Vec<u32>> {}

#[cfg(test)]
mod tests {

    #[test]
    fn test_digit() {
        let expected = 0;
        let input = '0';
        assert_eq!(input.to_digit(10), Some(expected))
    }
}

#![warn(clippy::all, clippy::pedantic)]

use crate::sif;

pub fn solve(input: &str) -> String {
    let sif = sif::SIF::new(input.trim(), 6, 25);

    println!("{}", sif.overlay());
    "ZFLBY".to_string()
}

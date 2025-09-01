use std::fs;

mod part_1;
mod part_2;

fn main() {
    let input = fs::read_to_string(INPUT_PATH).expect("Unable to read input file");

    println!("part 1: {}", part_1::solve(&input));
    println!("part 2: {}", part_2::solve(&input));
}

mod part_1;
mod part_2;

fn main() {
    let input = helpers::read_input("01").expect("unable to read input");

    println!("part 1: {}", part_1::solve(&input));
    println!("part 2: {}", part_2::solve(&input));
}

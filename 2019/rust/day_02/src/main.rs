mod part_1;
mod part_2;

fn main() {
    let input = helpers::read_input("02").expect("unable to read input");

    println!("part 1: {}", part_1::solve(&input)); // 2842648
    println!("part 2: {}", part_2::solve(&input)); // 9074
}

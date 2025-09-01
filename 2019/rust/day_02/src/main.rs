mod part_1;
mod part_2;

fn main() {
    let input = helpers::read_input("02").expect("unable to read input");

    println!("part 1: {}", part_1::solve(&input)); // 2842648
    println!("part 2: {}", part_2::solve(&input)); // 9074
}

// Including tests to ensure that as we change the intcode lib, we don't introduce breaking
// changes.
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_1_works() {
        let input = helpers::read_input("02").expect("unable to read input");
        let result = part_1::solve(&input);
        assert_eq!(result, 2842648);
    }

    #[test]
    fn part_2_works() {
        let input = helpers::read_input("02").expect("unable to read input");
        let result = part_2::solve(&input);
        assert_eq!(result, 9074);
    }
}

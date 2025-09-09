mod parsing;
mod part_1;
mod part_2;

fn main() {
    let input = helpers::read_input("04").expect("unable to read input");

    println!("part 1: {}", part_1::solve(&input)); // 
    println!("part 2: {}", part_2::solve(&input)); // 
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_1_works() {
        let input = helpers::read_input("04").expect("unable to read input");
        let result = part_1::solve(&input);
        assert_eq!(result, 931);
    }

    #[test]
    fn part_2_works() {
        let input = helpers::read_input("04").expect("unable to read input");
        let result = part_2::solve(&input);
        assert_eq!(result, 609);
    }
}

use std::str;

pub fn solve(input: &str) -> u64 {
    let parsed_input = parse_input(input);
    let test: Vec<u64> = parsed_input.into_iter().map(calc_fuel).collect();

    test.into_iter().sum()
}

fn parse_input(input: &str) -> Vec<u64> {
    input
        .lines()
        .map(|line| line.parse::<u64>().unwrap())
        .collect()
}

fn calc_fuel(n: u64) -> u64 {
    (n / 3) - 2
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(12, 2)]
    #[case(14, 2)]
    #[case(1969, 654)]
    #[case(100756, 33583)]
    fn calc_fuel_test(#[case] input: u64, #[case] expected: u64) {
        assert_eq!(expected, calc_fuel(input))
    }
}

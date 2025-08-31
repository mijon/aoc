use std::str;

pub fn solve(input: &str) -> i64 {
    let parsed_input = parse_input(input);
    let test: Vec<i64> = parsed_input.into_iter().map(calc_fuel).collect();

    test.into_iter().sum()
}

fn parse_input(input: &str) -> Vec<i64> {
    input
        .lines()
        .map(|line| line.parse::<i64>().unwrap())
        .collect()
}

fn calc_fuel_single(n: i64) -> i64 {
    (n / 3) - 2
}

fn calc_fuel(n: i64) -> i64 {
    let fuel_this_stage = calc_fuel_single(n);
    if fuel_this_stage <= 0 {
        return 0;
    }

    fuel_this_stage + calc_fuel(fuel_this_stage)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(14, 2)]
    #[case(1969, 966)]
    #[case(100756, 50346)]
    fn process_test(#[case] input: i64, #[case] expected: i64) {
        assert_eq!(expected, calc_fuel(input))
    }
}

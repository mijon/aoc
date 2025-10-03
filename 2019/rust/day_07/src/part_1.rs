use intcode::{IntcodeState, run_intcode};
use itertools::Itertools;

pub fn solve(input: &str) -> i64 {
    let mut output = Vec::new();
    for v in (0..=4).permutations(5) {
        output.push(run_through_once(input, v[0], v[1], v[2], v[3], v[4]))
    }
    output.iter().max().unwrap().unwrap()
}

fn run_through_once(input: &str, p1: i64, p2: i64, p3: i64, p4: i64, p5: i64) -> Option<i64> {
    let amp1 = IntcodeState::new(input, vec![p1, 0]);
    let mut intermediate1 = run_intcode(amp1).output;
    let result1 = intermediate1.pop()?;

    let amp2 = IntcodeState::new(input, vec![p2, result1]);
    let mut intermediate2 = run_intcode(amp2).output;
    let result2 = intermediate2.pop()?;

    let amp3 = IntcodeState::new(input, vec![p3, result2]);
    let mut intermediate3 = run_intcode(amp3).output;
    let result3 = intermediate3.pop()?;

    let amp4 = IntcodeState::new(input, vec![p4, result3]);
    let mut intermediate4 = run_intcode(amp4).output;
    let result4 = intermediate4.pop()?;

    let amp5 = IntcodeState::new(input, vec![p5, result4]);
    let mut intermediate5 = run_intcode(amp5).output;
    intermediate5.pop()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run_through_once() {
        let input = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0";
        let expected = 43210;
        assert_eq!(Some(expected), run_through_once(input, 4, 3, 2, 1, 0))
    }

    #[test]
    fn test_solve_1() {
        let input = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0";
        let expected = 43210;
        assert_eq!(expected, solve(input))
    }

    #[test]
    fn test_solve_2() {
        let input = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0";
        let expected = 54321;
        assert_eq!(expected, solve(input))
    }

    #[test]
    fn test_solve_3() {
        let input = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0";
        let expected = 65210;
        assert_eq!(expected, solve(input))
    }
}

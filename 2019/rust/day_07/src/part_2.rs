use intcode::{IntcodeState, run_intcode};
use itertools::Itertools;

pub fn solve(input: &str) -> i32 {
    let mut output = Vec::new();
    for v in (5..=9).permutations(5) {
        output.push(run_through_once(input, v[0], v[1], v[2], v[3], v[4]))
    }
    output.iter().max().unwrap().unwrap()
}

fn run_through_once(input: &str, p1: i32, p2: i32, p3: i32, p4: i32, p5: i32) -> Option<i32> {
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
    let result5 = intermediate5.pop();

    result5
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run_through_once() {
        let input =
            "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5";
        let expected = Some(2);
        assert_eq!(expected, run_through_once(input, 5, 6, 7, 8, 9))
    }

    #[test]
    fn test_solve_1() {
        let input =
            "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5";
        let expected = 139629729;
        assert_eq!(expected, solve(input))
    }

    #[test]
    fn test_solve_2() {
        let input = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10";
        let expected = 18216;
        assert_eq!(expected, solve(input))
    }
}

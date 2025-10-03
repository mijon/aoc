use intcode::{IntcodeState, RepeatingInput, reset_input, run_intcode_until};
use itertools::Itertools;

pub fn solve(input: &str) -> i64 {
    let mut output = Vec::new();
    for v in (5..=9).permutations(5) {
        output.push(run_through_once(input, v[0], v[1], v[2], v[3], v[4]))
    }
    let x = output.iter().max().unwrap();
    *x
}

// Not super happy with these solution, there's a lot of repetition, but I'm not yet comfortable
// with Rust enough to remove the repetitive code.
pub fn run_through_once(input_prog: &str, p1: i64, p2: i64, p3: i64, p4: i64, p5: i64) -> i64 {
    // initialise the amps and run through once
    let vp1 = vec![p1, 0];
    let mut amp1 = run_intcode_until(IntcodeState::new(input_prog, vp1), has_outputed);
    let mut result_from_1 = amp1.output.pop().expect("expected output");

    let vp2 = vec![p2, result_from_1];
    let mut amp2 = run_intcode_until(IntcodeState::new(input_prog, vp2), has_outputed);
    let mut result_from_2 = amp2.output.pop().expect("expected output");

    let vp3 = vec![p3, result_from_2];
    let mut amp3 = run_intcode_until(IntcodeState::new(input_prog, vp3), has_outputed);
    let mut result_from_3 = amp3.output.pop().expect("expected output");

    let vp4 = vec![p4, result_from_3];
    let mut amp4 = run_intcode_until(IntcodeState::new(input_prog, vp4), has_outputed);
    let mut result_from_4 = amp4.output.pop().expect("expected output");

    let vp5 = vec![p5, result_from_4];
    let mut amp5 = run_intcode_until(IntcodeState::new(input_prog, vp5), has_outputed);
    let mut result_from_5 = amp5.output.pop().expect("expected output");

    while !amp5.terminated {
        // continue looping through the amps while they output numbers
        amp1 = reset_input(
            amp1,
            RepeatingInput {
                values: vec![result_from_5],
            },
        );
        amp1 = run_intcode_until(amp1, has_outputed);

        // This is very janky. This is first way I found to get the result to propagate through to
        // come out the end
        result_from_1 = match amp1.output.pop() {
            Some(n) => n,
            None => amp1.input.next().unwrap(),
        };

        amp2 = reset_input(
            amp2,
            RepeatingInput {
                values: vec![result_from_1],
            },
        );
        amp2 = run_intcode_until(amp2, has_outputed);
        result_from_2 = match amp2.output.pop() {
            Some(n) => n,
            None => amp2.input.next().unwrap(),
        };

        amp3 = reset_input(
            amp3,
            RepeatingInput {
                values: vec![result_from_2],
            },
        );
        amp3 = run_intcode_until(amp3, has_outputed);
        result_from_3 = match amp3.output.pop() {
            Some(n) => n,
            None => amp3.input.next().unwrap(),
        };

        amp4 = reset_input(
            amp4,
            RepeatingInput {
                values: vec![result_from_3],
            },
        );
        amp4 = run_intcode_until(amp4, has_outputed);
        result_from_4 = match amp4.output.pop() {
            Some(n) => n,
            None => amp4.input.next().unwrap(),
        };

        amp5 = reset_input(
            amp5,
            RepeatingInput {
                values: vec![result_from_4],
            },
        );
        amp5 = run_intcode_until(amp5, has_outputed);
        result_from_5 = match amp5.output.pop() {
            Some(n) => n,
            None => amp5.input.next().unwrap(),
        };
    }

    amp5.input.next().unwrap()
}

fn has_outputed(int_state: &IntcodeState) -> bool {
    !int_state.output.is_empty() || int_state.terminated
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn problem_example1() {
        let input =
            "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5";
        let expected = 139629729;
        assert_eq!(expected, run_through_once(input, 9, 8, 7, 6, 5))
    }

    #[test]
    fn test_solve_2() {
        let input = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10";
        let expected = 18216;
        assert_eq!(expected, run_through_once(input, 9, 7, 8, 5, 6))
    }
}

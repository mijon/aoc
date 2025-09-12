use intcode::{IntcodeState, run_intcode};

pub fn solve(input: &str) -> i32 {
    let intcode = IntcodeState::new(&input, vec![5]);

    let mut result = run_intcode(intcode);
    result.output.pop().expect("Expected output value")
}

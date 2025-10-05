use intcode::{IntcodeState, run_intcode};

pub fn solve(input: &str) -> i64 {
    let intcodestate = IntcodeState::new(input, vec![2]);
    let mut run_program = run_intcode(intcodestate);
    println!("{:?}", run_program.output);
    run_program.output.pop().unwrap()
}

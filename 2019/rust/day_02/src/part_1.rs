use intcode::{IntcodeState, run_intcode};

pub fn solve(input: &str) -> i32 {
    let intcode = IntcodeState::new(&input, Vec::new())
        // We need to update the values in addresses 1 and 2.
        .edit_program(1, 12)
        .edit_program(2, 2);

    let result = run_intcode(intcode);
    result.program[0]
}

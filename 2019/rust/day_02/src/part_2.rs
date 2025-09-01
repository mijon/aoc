use intcode::{IntcodeState, run_intcode};

pub fn solve(input: &str) -> i64 {
    for noun in 1..99 {
        for verb in 1..99 {
            let intcode = IntcodeState::new(&input);
            let intcode = intcode.edit_program(1, noun as i64);
            let intcode = intcode.edit_program(2, verb as i64);

            let result = run_intcode(intcode);
            if result.program[0] == 19690720 {
                return 100 * noun + verb;
            }
        }
    }
    return 0;
}

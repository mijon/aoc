#![allow(dead_code)]
use crate::robot::{self, Paint};
use intcode::{IntcodeState, run_intcode_until};

pub fn solve(input: &str) -> i32 {
    let mut state = robot::State::new(0, 0, Paint::Black);
    let mut intcode_state = IntcodeState::new(input, vec![state.gen_intcode_input()]);

    let mut output = 0;
    while !intcode_state.terminated {
        intcode_state = run_intcode_until(intcode_state, |s| s.output.len() == 2 || s.terminated);

        match intcode_state.terminated {
            true => {
                output = state.hull.len();
            }
            false => {
                // We're pushing to the output vector onto the back and then also popping off the
                // back of the output vector. So while the intcode program emits paint colour and
                // then direction, we need `pop` first to get the direction, and then the paint.
                let output_direction = intcode_state.output.pop().expect("expected a value");
                let output_paint = intcode_state.output.pop().expect("expected a value");
                state = state.update((output_paint, output_direction));
                intcode_state = intcode_state.reset_input_from_vec(vec![state.gen_intcode_input()]);
            }
        }
    }
    output.try_into().unwrap()
}

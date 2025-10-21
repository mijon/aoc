#![allow(dead_code)]
// use crate::arcarde;
use intcode::{IntcodeState, run_intcode_until};

use crate::arcade::{Arcade, Tile};

pub fn solve(input: &str) -> i32 {
    let mut intcode_state = IntcodeState::new(input, vec![2]);
    let mut arcade = Arcade::new();

    while !intcode_state.terminated {
        intcode_state = run_intcode_until(intcode_state, |s| s.output.len() == 3 || s.terminated);

        match intcode_state.terminated {
            true => continue,
            false => {
                let tile_num = intcode_state.output.pop().expect("expect a value") as i32;
                let y = intcode_state.output.pop().expect("expect a value") as i32;
                let x = intcode_state.output.pop().expect("expect a value") as i32;

                arcade.add_tile(x, y, tile_num);
            }
        }
    }

    let output = arcade
        .get_tiles()
        .into_values()
        .filter(|t| *t == Tile::Block)
        .collect::<Vec<Tile>>()
        .len();

    output as i32
}

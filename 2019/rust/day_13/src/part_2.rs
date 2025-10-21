use std::io;

// use crate::arcarde;
use intcode::{IntcodeState, run_intcode_until};

use crate::arcade::{Arcade, Tile};

pub fn solve(input: &str, interactive: bool) -> i32 {
    let mut intcode_state = IntcodeState::new(input, vec![0]);
    intcode_state = intcode_state.edit_program(0, 2);
    let mut user_input = String::new();
    let mut num_unbroken = 100;
    let mut score: i32 = 0;

    while num_unbroken > 0 {
        let mut arcade = Arcade::new();
        // Run the intcode until it needs input
        println!("running");
        intcode_state = run_intcode_until(intcode_state, |s| {
            s.get_opcode_num(s.head) == 3 || s.terminated
        });

        // Extract the output into an arcade struct
        let arcade_input = intcode_state.output.chunks(3);
        for chunk in arcade_input {
            let (x, y, value) = (chunk[0], chunk[1], chunk[2]);
            if x == -1 && y == 0 {
                arcade.update_score(value as i32);
            } else {
                arcade.add_tile(x as i32, y as i32, value as i32);
            }
        }
        // draw to screen
        println!("{}", arcade.draw());

        //get input from user
        let user_input_num = if interactive {
            io::stdin()
                .read_line(&mut user_input)
                .expect("Failed to read line");
            user_input = user_input.trim().to_string();

            let tmp = match user_input.as_str() {
                "h" => -1,
                "l" => 1,
                _ => 0,
            };
            user_input = "".to_string();
            tmp
        } else {
            let ball_col = arcade
                .clone()
                .get_tiles()
                .iter()
                .find(|(_, tile)| tile == &&Tile::Ball)
                .unwrap()
                .0
                .x;

            let paddle_col = arcade
                .clone()
                .get_tiles()
                .iter()
                .find(|(_, tile)| tile == &&Tile::Paddle)
                .unwrap()
                .0
                .x;

            println!("{}", ball_col);
            println!("{}", paddle_col);

            (ball_col - paddle_col).signum()
        };

        println!("{}", user_input_num);

        num_unbroken = arcade
            .get_tiles()
            .into_values()
            .filter(|t| *t == Tile::Block)
            .collect::<Vec<Tile>>()
            .len();

        println!("num unbroken: {}", num_unbroken);

        intcode_state = intcode_state.reset_input_from_vec(vec![user_input_num as i64]);
        // intcode_state.terminated = false;
        // parse the input and insert into the intcode_state

        score = arcade.score;
    }

    score
}

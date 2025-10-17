#![allow(dead_code)]
use intcode::{IntcodeState, run_intcode_until};
use std::collections::HashMap;

pub fn solve(input: &str) -> i32 {
    let mut state = State::new(0, 0);
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
    println!("{}", output);
    // println!("{:?}", state.hull);
    4
}

// Temporary(?) way to display the hull for debugging
impl State {
    fn show(&self) -> String {
        let keys = self.hull.keys();

        let mut xs = Vec::new();
        let mut ys = Vec::new();
        for key in keys {
            xs.push(key.0);
            ys.push(key.1);
        }

        let top = ys.iter().max().unwrap();
        let bottom = ys.iter().min().unwrap();
        let left = xs.iter().min().unwrap();
        let right = xs.iter().max().unwrap();

        // then make an array that's that size
        // make everything a "." (black)
        // then go through every key and get the value from the hull and paint it
        // then grab the robot and draw its loction and direction on the array
        // then squash the array into a string and return it
        println!("{} {} {} {}", top, bottom, left, right);
        "hello".to_string()
    }
}

// Types and things to support the problem

#[derive(Debug, Clone, PartialEq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Debug, Clone, PartialEq)]
enum Paint {
    Black,
    White,
}

impl Direction {
    fn turn_left(self) -> Self {
        match self {
            Direction::Up => Direction::Left,
            Direction::Down => Direction::Right,
            Direction::Left => Direction::Down,
            Direction::Right => Direction::Up,
        }
    }

    fn turn_right(self) -> Self {
        match self {
            Direction::Up => Direction::Right,
            Direction::Down => Direction::Left,
            Direction::Left => Direction::Up,
            Direction::Right => Direction::Down,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Robot {
    position: (i32, i32),
    direction: Direction,
}

#[derive(Debug, Clone, PartialEq)]
struct State {
    hull: HashMap<(i32, i32), Paint>,
    robot: Robot,
}

// State actions
impl State {
    fn new(x: i32, y: i32) -> Self {
        State {
            hull: HashMap::from([((x, y), Paint::Black)]),
            robot: Robot {
                position: (x, y),
                direction: Direction::Up,
            },
        }
    }

    fn gen_intcode_input(&self) -> i64 {
        let pos = self.robot.position;
        let paint = self.hull.get(&pos).unwrap_or(&Paint::Black);
        match paint {
            &Paint::Black => 0,
            &Paint::White => 1,
        }
    }

    fn paint(mut self, input: i64) -> Self {
        let pos = self.robot.position;
        match input {
            0 => self.hull.insert(pos, Paint::Black),
            1 => self.hull.insert(pos, Paint::White),
            _ => panic!("Was expecting 0 or 1"),
        };
        self
    }

    // Simple pass through down to the Robot
    fn turn(mut self, input: i64) -> Self {
        // self.robot.turn(input);
        // self

        // let rbt = self.robot.turn(input);
        let new = self.robot.clone().turn(input);
        self.robot = new;
        self
    }

    fn walk(mut self) -> Self {
        match self.robot.direction {
            Direction::Up => self.robot.position.1 += 1,
            Direction::Down => self.robot.position.1 -= 1,
            Direction::Left => self.robot.position.0 -= 1,
            Direction::Right => self.robot.position.0 += 1,
        };
        self
    }

    fn update(self, (input_paint, input_rotation): (i64, i64)) -> Self {
        self.paint(input_paint).turn(input_rotation).walk()
    }
}

// Robot Actions
impl Robot {
    fn turn(mut self, input: i64) -> Self {
        let new_direction = match input {
            0 => self.direction.turn_left(),
            1 => self.direction.turn_right(),
            _ => panic!("Was not expecting that value"),
        };
        self.direction = new_direction;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_state_construction() {
        let mut example_hashmap = HashMap::new();
        example_hashmap.insert((0, 0), Paint::Black);
        let expected = State {
            hull: example_hashmap,
            robot: Robot {
                position: (0, 0),
                direction: Direction::Up,
            },
        };
        let constructed = State::new(0, 0);
        assert_eq!(expected, constructed)
    }

    #[test]
    fn test_step() {
        let initial_state = State::new(0, 0);
        let expected_state = State {
            hull: HashMap::from([((0, 0), Paint::White)]),
            robot: Robot {
                position: (-1, 0),
                direction: Direction::Left,
            },
        };
        assert_eq!(initial_state.update((1, 0)), expected_state)
    }

    #[test]
    fn test_two_steps() {
        let initial_state = State::new(0, 0);
        let expected_state = State {
            hull: HashMap::from([((0, 0), Paint::White), ((-1, 0), Paint::Black)]),
            robot: Robot {
                position: (-1, -1),
                direction: Direction::Down,
            },
        };
        assert_eq!(initial_state.update((1, 0)).update((0, 0)), expected_state)
    }

    #[test]
    fn test_four_steps() {
        let initial_state = State::new(0, 0);
        let expected_state = State {
            hull: HashMap::from([
                ((0, 0), Paint::White),
                ((-1, 0), Paint::Black),
                ((-1, -1), Paint::White),
                ((0, -1), Paint::White),
            ]),
            robot: Robot {
                position: (0, 0),
                direction: Direction::Up,
            },
        };
        assert_eq!(
            initial_state
                .update((1, 0))
                .update((0, 0))
                .update((1, 0))
                .update((1, 0)),
            expected_state
        )
    }

    #[test]
    fn test_getting_paint_number_out() {
        let initial_state = State::new(0, 0);
        let expected_paint = 0;
        assert_eq!(initial_state.gen_intcode_input(), expected_paint)
    }
}

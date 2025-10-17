use std::collections::HashMap;
// Temporary(?) way to display the hull for debugging
impl State {
    pub fn show(&self) -> String {
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

        let ncol = right.abs() + 1 + left.abs();
        let nrow = bottom.abs() + 1 + top.abs();

        // let x_offset = left.abs();
        // let y_offset = top.abs();

        let mut hull_vec = Vec::new();
        for _j in 0..nrow {
            let mut cur_row = Vec::new();
            for _i in 0..ncol {
                cur_row.push(" ");
            }
            hull_vec.push(cur_row);
        }

        for k in self.hull.keys() {
            let (x, y) = k;
            let paint = self.hull.get(k).unwrap();
            let ch = match paint {
                Paint::Black => " ",
                Paint::White => "#",
            };
            hull_vec[-(*y) as usize][*x as usize] = ch;
        }

        // then make an array that's that size
        // make everything a "." (black)
        // then go through every key and get the value from the hull and paint it
        // then grab the robot and draw its loction and direction on the array
        // then squash the array into a string and return it

        // hull_vec = hull_vec
        //     .into_iter()
        //     .map(|r| r.join(""))
        //     .collect::<Vec<&str>>();

        let mut rows = Vec::new();
        for i in 0..hull_vec.len() {
            rows.push(format!("{}", hull_vec[i].join("")));
        }

        rows.join("\n")
    }
}

// Types and things to support the problem

#[derive(Debug, Clone, PartialEq)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Paint {
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
pub struct Robot {
    pub position: (i32, i32),
    pub direction: Direction,
}

#[derive(Debug, Clone, PartialEq)]
pub struct State {
    pub hull: HashMap<(i32, i32), Paint>,
    pub robot: Robot,
}

// State actions
impl State {
    pub fn new(x: i32, y: i32, paint: Paint) -> Self {
        State {
            hull: HashMap::from([((x, y), paint)]),
            robot: Robot {
                position: (x, y),
                direction: Direction::Up,
            },
        }
    }

    pub fn gen_intcode_input(&self) -> i64 {
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

    pub fn update(self, (input_paint, input_rotation): (i64, i64)) -> Self {
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
        let constructed = State::new(0, 0, Paint::Black);
        assert_eq!(expected, constructed)
    }

    #[test]
    fn test_step() {
        let initial_state = State::new(0, 0, Paint::Black);
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
        let initial_state = State::new(0, 0, Paint::Black);
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
        let initial_state = State::new(0, 0, Paint::Black);
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
        let initial_state = State::new(0, 0, Paint::Black);
        let expected_paint = 0;
        assert_eq!(initial_state.gen_intcode_input(), expected_paint)
    }
}

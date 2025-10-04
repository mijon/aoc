//! Intcode Implementation
//!
//! The plan is to have a library that manages all the intcode programs. Over the course of the 2019
//! AoC, I will add to this library. The intention is that the full intcode VM will be specified
//! here and over time more features will be included.
#![allow(dead_code)]

use std::collections::HashMap;
use std::str;
/// Represents the state of the Intcode program
#[derive(PartialEq, Debug, Clone)]
pub struct IntcodeState {
    /// The vector of intcodes
    pub program: HashMap<i64, i64>,
    /// The 'current' position
    pub head: i64,
    /// Whether the program has terminated (used for stopping evaluation)
    pub terminated: bool,
    // input: VecDeque<i64>,
    pub input: RepeatingInput,
    pub output: Vec<i64>,
    pub relative_base: i64,
}

impl IntcodeState {
    pub fn new(program: &str, input: Vec<i64>) -> Self {
        Self {
            program: prep_program(program),
            head: 0,
            terminated: false,
            // input: VecDeque::from(input),
            input: RepeatingInput::new(input),
            output: Vec::new(),
            relative_base: 0,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct RepeatingInput {
    pub values: Vec<i64>,
}

impl RepeatingInput {
    pub fn new(values: Vec<i64>) -> Self {
        Self { values }
    }
}

impl Iterator for RepeatingInput {
    type Item = i64;
    fn next(&mut self) -> Option<Self::Item> {
        if self.values.len() > 1 {
            let output = self.values[0];
            self.values = self.values[1..self.values.len()].to_vec();
            Some(output)
        } else {
            let output = self.values[0];
            Some(output)
        }
    }
}

fn prep_program(program: &str) -> HashMap<i64, i64> {
    let values: Vec<i64> = program
        .split(',')
        .map(|n| n.trim().parse::<i64>().expect("unable to parse number"))
        .collect();
    let mut output = HashMap::new();
    for (pos, value) in values.iter().enumerate() {
        output.insert(pos as i64, *value);
    }
    output
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ParameterMode {
    Immediate,
    Position,
    Relative,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Opcode {
    Add(ParameterMode, ParameterMode, ParameterMode), // 1
    Multiply(ParameterMode, ParameterMode, ParameterMode), // 2
    Input(ParameterMode),                             // 3
    Output(ParameterMode),                            // 4
    JumpIfTrue(ParameterMode, ParameterMode),         // 5
    JumpIfFalse(ParameterMode, ParameterMode),        // 6
    LessThan(ParameterMode, ParameterMode, ParameterMode), // 7
    Equals(ParameterMode, ParameterMode, ParameterMode), // 8
    UpdateRelativeBase(ParameterMode),                //9
    Stop,                                             // 99
    Value(i64),
}

/// Break an integer into a vec of its digits, returning it reversed (123 -> [3,2,1])
fn int_to_vec(n: i64) -> Vec<i64> {
    if n == 0 {
        return vec![0];
    }

    let mut num = n;
    let mut result = std::iter::from_fn(move || {
        if num == 0 {
            None
        } else {
            let output = num % 10;
            num /= 10;
            Some(output)
        }
    })
    .collect::<Vec<i64>>();

    result.reverse();
    result
}

pub fn num_params(o: Opcode) -> i32 {
    match o {
        Opcode::Add(..) => 3,
        Opcode::Multiply(..) => 3,
        Opcode::Input(..) => 1,
        Opcode::Output(..) => 1,
        Opcode::JumpIfTrue(..) => 2,
        Opcode::JumpIfFalse(..) => 2,
        Opcode::LessThan(..) => 3,
        Opcode::Equals(..) => 3,
        Opcode::Stop => 0,
        Opcode::Value(_) => 1,
        Opcode::UpdateRelativeBase(_) => 1,
    }
}

/// Convert an int to an intcode Opcode
pub fn parse_intcode(n: i64) -> Opcode {
    let mut rev_vec = int_to_vec(n);
    let op_int = rev_vec.pop().unwrap() + 10 * rev_vec.pop().unwrap_or(0);
    let mut modes = rev_vec
        .iter()
        .map(|n| match n {
            0 => ParameterMode::Position,
            1 => ParameterMode::Immediate,
            2 => ParameterMode::Relative,
            _ => panic!("INTCODE ERROR: Unexpected mode int: {n} in {op_int}"), // probably dont want to panic here, but rather do
                                                                                // None?
        })
        .collect::<Vec<ParameterMode>>();

    match op_int {
        1 => Opcode::Add(
            modes.pop().unwrap_or(ParameterMode::Position),
            modes.pop().unwrap_or(ParameterMode::Position),
            modes.pop().unwrap_or(ParameterMode::Position),
        ),
        2 => Opcode::Multiply(
            modes.pop().unwrap_or(ParameterMode::Position),
            modes.pop().unwrap_or(ParameterMode::Position),
            modes.pop().unwrap_or(ParameterMode::Position),
        ),

        3 => Opcode::Input(modes.pop().unwrap_or(ParameterMode::Position)),
        4 => Opcode::Output(modes.pop().unwrap_or(ParameterMode::Position)),
        5 => Opcode::JumpIfTrue(
            modes.pop().unwrap_or(ParameterMode::Position),
            modes.pop().unwrap_or(ParameterMode::Position),
        ),
        6 => Opcode::JumpIfFalse(
            modes.pop().unwrap_or(ParameterMode::Position),
            modes.pop().unwrap_or(ParameterMode::Position),
        ),
        7 => Opcode::LessThan(
            modes.pop().unwrap_or(ParameterMode::Position),
            modes.pop().unwrap_or(ParameterMode::Position),
            modes.pop().unwrap_or(ParameterMode::Position),
        ),
        8 => Opcode::Equals(
            modes.pop().unwrap_or(ParameterMode::Position),
            modes.pop().unwrap_or(ParameterMode::Position),
            modes.pop().unwrap_or(ParameterMode::Position),
        ),
        9 => Opcode::UpdateRelativeBase(modes.pop().unwrap_or(ParameterMode::Position)),
        99 => Opcode::Stop,
        _ => Opcode::Value(n),
    }
}

// Can probably be improved?
pub fn run_intcode(mut s: IntcodeState) -> IntcodeState {
    loop {
        if s.terminated {
            break;
        }
        s = s.step_intcode();
    }
    s
}

pub fn run_intcode_until<P>(mut s: IntcodeState, mut p: P) -> IntcodeState
where
    P: FnMut(&IntcodeState) -> bool,
{
    loop {
        s = s.step_intcode();
        if p(&s) {
            break;
        }
    }
    s
}

impl IntcodeState {
    pub fn step_intcode(mut self) -> Self {
        let current_opcode = self.current_opcode();
        // println!(
        //     "{current_opcode:?}, input: {:?}, output: {:?}, prog: {:?}",
        //     self.input, self.output, self.program
        // );
        match current_opcode {
            Opcode::Add(m1, m2, m3) => {
                let a = match m1 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 1),
                    ParameterMode::Immediate => self.head + 1,
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 1)
                    }
                };
                let b = match m2 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 2),
                    ParameterMode::Immediate => self.head + 2,
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 2)
                    }
                };
                let target_pos = match m3 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 3),
                    ParameterMode::Immediate => self.head + 3,
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 3)
                    }
                };
                self.set_opcode_num(target_pos, self.get_opcode_num(a) + self.get_opcode_num(b));
                self.head += 4;
            }
            Opcode::Multiply(m1, m2, m3) => {
                let a = match m1 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 1),
                    ParameterMode::Immediate => self.head + 1,
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 1)
                    }
                };
                let b = match m2 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 2),
                    ParameterMode::Immediate => self.head + 2,
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 2)
                    }
                };
                let target_pos = match m3 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 3),
                    ParameterMode::Immediate => self.head + 3,
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 3)
                    }
                };
                // println!(
                //     "{} * {}",
                //     self.program[a as usize], self.program[b as usize]
                // );
                self.set_opcode_num(target_pos, self.get_opcode_num(a) * self.get_opcode_num(b));
                self.head += 4;
            }
            Opcode::Stop => {
                self.terminated = true;
            }
            Opcode::Value(_) => {
                self.head += 1;
            }
            Opcode::Input(m1) => {
                // let input_val = self.input.pop_front().expect("Expected input!");
                let input_val = self.input.next().expect("Expected input!");
                let target_pos = match m1 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 1),
                    ParameterMode::Immediate => self.head + 1,
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 1)
                    }
                };
                self.set_opcode_num(target_pos, input_val);
                self.head += 2;
            }
            Opcode::Output(m1) => {
                let target_pos = match m1 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 1),
                    ParameterMode::Immediate => self.head + 1,
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 1)
                    }
                };
                self.output.push(self.get_opcode_num(target_pos));
                self.head += 2;
            }
            Opcode::JumpIfTrue(m1, m2) => {
                let a = match m1 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 1),
                    ParameterMode::Immediate => self.head + 1,
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 1)
                    }
                };
                let b = match m2 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 2),
                    ParameterMode::Immediate => self.head + 2,
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 2)
                    }
                };
                if self.get_opcode_num(a) != 0 {
                    self.head = self.get_opcode_num(b)
                } else {
                    self.head += 3
                };
            }
            Opcode::JumpIfFalse(m1, m2) => {
                let a = match m1 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 1),
                    ParameterMode::Immediate => self.head + 1,
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 1)
                    }
                };
                let b = match m2 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 2),
                    ParameterMode::Immediate => self.head + 2,
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 2)
                    }
                };
                if self.get_opcode_num(a) == 0 {
                    self.head = self.get_opcode_num(b)
                } else {
                    self.head += 3
                };
            }
            Opcode::LessThan(m1, m2, m3) => {
                let a = match m1 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 1),
                    ParameterMode::Immediate => self.head + 1,
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 1)
                    }
                };
                let b = match m2 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 2),
                    ParameterMode::Immediate => self.head + 2,
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 2)
                    }
                };
                let c = match m3 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 3),
                    ParameterMode::Immediate => self.head + 4, // NOTE: I did change this from + 4
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 3)
                    }
                };
                if self.get_opcode_num(a) < self.get_opcode_num(b) {
                    self.set_opcode_num(c, 1);
                } else {
                    self.set_opcode_num(c, 0);
                }
                self.head += 4
            }
            Opcode::Equals(m1, m2, m3) => {
                let a = match m1 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 1),
                    ParameterMode::Immediate => (self.head + 1) as i64,
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 1)
                    }
                };
                let b = match m2 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 2),
                    ParameterMode::Immediate => self.head + 2,
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 2)
                    }
                };
                let c = match m3 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 3),
                    ParameterMode::Immediate => self.head + 4, // NOTE: also changed this one
                    // from 4
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 3)
                    }
                };
                // println!("{:?}", self.head + 1);
                // println!("{:?}", a);
                // println!("{:?}", b);
                // println!("{:?}", c);
                if self.get_opcode_num(a) == self.get_opcode_num(b) {
                    self.set_opcode_num(c, 1);
                } else {
                    self.set_opcode_num(c, 0);
                }
                self.head += 4
            }
            Opcode::UpdateRelativeBase(m1) => {
                let a = match m1 {
                    ParameterMode::Position => self.get_opcode_num(self.head + 1),
                    ParameterMode::Immediate => self.head + 1,
                    ParameterMode::Relative => {
                        self.relative_base + self.get_opcode_num(self.head + 1)
                    }
                };
                self.relative_base += a;
                self.head += 2
            }
        }
        self
    }
}

impl IntcodeState {
    pub fn edit_program(mut self, idx: i64, val: i64) -> Self {
        self.set_opcode_num(idx, val);
        self
    }

    pub fn reset_input(mut self, input: RepeatingInput) -> Self {
        self.input = input;
        self
    }

    pub fn get_opcode_num(&self, pos: i64) -> i64 {
        *self.program.get(&pos).unwrap_or(&0)
    }

    pub fn get_opcode(&self, pos: i64) -> Opcode {
        parse_intcode(self.get_opcode_num(pos))
    }

    pub fn set_opcode_num(&mut self, pos: i64, value: i64) {
        self.program.insert(pos, value);
    }

    pub fn current_opcode(&self) -> Opcode {
        parse_intcode(self.get_opcode_num(self.head))
    }
}

pub fn reset_input(mut is: IntcodeState, input: RepeatingInput) -> IntcodeState {
    is.input = input;
    is
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[test]
    fn test_repeating_iterator() {
        let mut my_iterator = RepeatingInput { values: vec![1, 2] };
        assert_eq!(Some(1), my_iterator.next());
        assert_eq!(Some(2), my_iterator.next());
        assert_eq!(Some(2), my_iterator.next());
        assert_eq!(Some(2), my_iterator.next());
        assert_eq!(Some(2), my_iterator.next());
    }

    #[test]
    fn test_prepping_program_works() {
        let result = prep_program("0,1,0");
        let mut expected = HashMap::new();
        expected.insert(0, 0);
        expected.insert(1, 1);
        expected.insert(2, 0);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_new_program_works() {
        let result = IntcodeState {
            program: HashMap::from([(0, 1), (1, 1), (2, 1)]),
            head: 0,
            terminated: false,
            output: Vec::new(),
            input: RepeatingInput { values: vec![] },
            relative_base: 0,
        };
        assert_eq!(IntcodeState::new("1,1,1", Vec::new()), result);
    }

    #[test]
    fn test_parse_single_intcode() {
        let result = parse_intcode(99);
        assert_eq!(Opcode::Stop, result);
    }

    #[test]
    fn test_update_program() {
        let result = HashMap::from([(0, 2), (1, 0), (2, 0), (3, 0), (4, 99)]);
        let input = "1,0,0,0,99";
        let intcode_state = IntcodeState::new(input, Vec::new());
        let edited_program = intcode_state.edit_program(0, 2);
        assert_eq!(edited_program.program, result);
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            Opcode::Add(
                ParameterMode::Position,
                ParameterMode::Position,
                ParameterMode::Immediate
            ),
            parse_intcode(10001)
        )
    }

    #[rstest]
    #[case("1,0,0,0,99", HashMap::from([(0,2), (1,0), (2,0), (3,0), (4,99)]))]
    #[case("2,3,0,3,99", HashMap::from([(0,2),(1,3),(2,0),(3,6),(4,99)]))]
    #[case("2,4,4,5,99,0", HashMap::from([(0,2),(1,4),(2,4),(3,5),(4,99),(5,9801)]))]
    #[case("1,1,1,4,99,5,6,0,99", HashMap::from([(0,30),(1,1),(2,1),(3,4),(4,2),(5,5),(6,6),(7,0),(8,99)]))]
    fn test_prog(#[case] input: &str, #[case] expected: HashMap<i64, i64>) {
        let intcode_state = IntcodeState::new(input, Vec::new());
        let run_program = run_intcode(intcode_state);
        assert_eq!(expected, run_program.program)
    }

    // New tests for day 5
    #[test]
    fn test_parse_with_modes() {
        let input_code = 1002;
        let expected_opcode = Opcode::Multiply(
            ParameterMode::Position,
            ParameterMode::Immediate,
            ParameterMode::Position,
        );
        assert_eq!(expected_opcode, parse_intcode(input_code))
    }

    #[test]
    fn test_int_to_vec() {
        assert_eq!(vec![1, 2, 3], int_to_vec(123))
    }

    #[test]
    fn day_5_example_works() {
        let programstring = "3,0,4,0,99";
        let input = vec![10];
        let intcode_state = IntcodeState::new(programstring, input);
        let run_program = run_intcode(intcode_state);
        assert_eq!(10, run_program.output[0])
    }

    #[test]
    fn day_5_part_2_example_works() {
        let programstring = "3,9,8,9,10,9,4,9,99,-1,8";
        let input = vec![9];
        let intcode_state = IntcodeState::new(programstring, input);
        let run_program = run_intcode(intcode_state);
        assert_eq!(0, run_program.output[0])
    }

    #[test]
    fn test_early_stop() {
        let programstring = "3,0,4,0,99";
        let input = vec![10];
        let intcode_state = IntcodeState::new(programstring, input);
        let run_program = run_intcode_until(intcode_state, |int_state| {
            match int_state.current_opcode() {
                Opcode::Output(_) => true,
                _ => false,
            }
        });
        assert_eq!(2, run_program.head);
    }

    #[test]
    fn test_early_stop2() {
        let programstring = "3,0,4,0,99";
        let input = vec![10];
        let intcode_state = IntcodeState::new(programstring, input);
        let run_program = run_intcode_until(intcode_state, |int_state| int_state.output.len() > 0);
        assert_eq!(10, run_program.output[0]);
    }

    #[test]
    fn test_16_dig_output() {
        let programstring = "1102,34915192,34915192,7,4,7,99,0";
        let expected = vec![1219070632396864];
        let input = vec![];
        let intcode_state = IntcodeState::new(programstring, input);
        let run_program = run_intcode(intcode_state);
        assert_eq!(expected, run_program.output)
    }

    #[test]
    fn test_big_num() {
        let programstring = "104,1125899906842624,99";
        let expected = vec![1125899906842624];
        let input = vec![];
        let intcode_state = IntcodeState::new(programstring, input);
        let run_program = run_intcode(intcode_state);
        assert_eq!(expected, run_program.output)
    }

    #[test]
    fn test_quine() {
        let programstring = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99";
        let expected = vec![
            109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99,
        ];
        let input = vec![];
        let intcode_state = IntcodeState::new(programstring, input);
        let run_program = run_intcode(intcode_state);
        assert_eq!(expected, run_program.output)
    }
}

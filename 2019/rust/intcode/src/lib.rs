#![allow(dead_code)]

#[derive(PartialEq, Debug, Clone)]
pub struct IntcodeState {
    pub program: Vec<i64>,
    head: usize,
    terminated: bool,
}

impl IntcodeState {
    pub fn new(program: &str) -> Self {
        Self {
            program: prep_program(program),
            head: 0,
            terminated: false,
        }
    }
}

fn prep_program(program: &str) -> Vec<i64> {
    program
        .split(',')
        .map(|n| n.trim().parse::<i64>().expect("unable to parse number"))
        .collect()
}

#[derive(Debug, PartialEq)]
pub enum Opcode {
    Add(i64),
    Multiply(i64),
    Stop(i64),
    Value(i64),
}

pub fn parse_intcode(v: i64) -> Opcode {
    match v {
        1 => Opcode::Add(v),
        2 => Opcode::Multiply(v),
        99 => Opcode::Stop(v),
        _ => Opcode::Value(v),
    }
}

pub fn run_intcode(mut s: IntcodeState) -> IntcodeState {
    loop {
        if s.terminated {
            break;
        }
        s = s.step_intcode();
    }
    // while !&s.terminated {
    //     s.step_intcode();
    // }
    s
}

impl IntcodeState {
    fn step_intcode(mut self) -> Self {
        let current_opcode = parse_intcode(self.program[self.head]);
        match current_opcode {
            Opcode::Add(_) => {
                let a = self.program[self.head + 1];
                let b = self.program[self.head + 2];
                let target_pos = self.program[self.head + 3] as usize;
                self.program[target_pos] = self.program[a as usize] + self.program[b as usize];
                self.head = self.head + 4;
            }
            Opcode::Multiply(_) => {
                // println!("mul");
                // println!("{:?}", self);
                let a = self.program[self.head + 1];
                let b = self.program[self.head + 2];
                let target_pos = self.program[self.head + 3] as usize;
                self.program[target_pos] = self.program[a as usize] * self.program[b as usize];
                self.head = self.head + 4;
            }
            Opcode::Stop(_) => {
                // println!("stop");
                // println!("{:?}", self);
                self.terminated = true;
            }
            Opcode::Value(_) => {
                // println!("val: {v}");
                // println!("{:?}", self);
                self.head = self.head + 1;
            }
        }
        self
    }
}

impl IntcodeState {
    pub fn edit_program(mut self, idx: usize, val: i64) -> Self {
        self.program[idx] = val;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[test]
    fn prepping_program_works() {
        let result = prep_program("0,1,0");
        assert_eq!(result, [0, 1, 0]);
    }

    #[test]
    fn new_program_works() {
        let result = IntcodeState {
            program: vec![1, 1, 1],
            head: 0,
            terminated: false,
        };
        assert_eq!(IntcodeState::new("1,1,1"), result);
    }

    #[test]
    fn parse_single_intcode() {
        let result = parse_intcode(99);
        assert_eq!(Opcode::Stop(99), result);
    }

    #[test]
    fn test_update_program() {
        let result = vec![2, 0, 0, 0, 99];
        let input = "1,0,0,0,99";
        let intcode_state = IntcodeState::new(input);
        let edited_program = intcode_state.edit_program(0, 2);
        assert_eq!(edited_program.program, result);
    }

    #[rstest]
    #[case("1,0,0,0,99", vec![2, 0, 0, 0, 99])]
    #[case("2,3,0,3,99", vec![2,3,0,6,99])]
    #[case("2,4,4,5,99,0", vec![2,4,4,5,99,9801])]
    #[case("1,1,1,4,99,5,6,0,99", vec![30,1,1,4,2,5,6,0,99])]
    fn test_prog(#[case] input: &str, #[case] expected: Vec<i64>) {
        let intcode_state = IntcodeState::new(input);
        let run_program = run_intcode(intcode_state);
        assert_eq!(expected, run_program.program)
    }
}

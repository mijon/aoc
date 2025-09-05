#![allow(dead_code)]

use nom::{
    IResult, Parser, branch::alt, bytes::complete::tag, character::complete::i32,
    multi::separated_list1, sequence::pair,
};

// Types
#[derive(Debug, PartialEq)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Debug, PartialEq)]
pub struct Instruction {
    pub direction: Direction,
    pub amount: i32,
}

pub type InstructionLine = Vec<Instruction>;
pub type Instructions = Vec<InstructionLine>;

// #[derive(Debug, Copy, Clone)]
// pub struct Point {
//     pub x: i32,
//     pub y: i32,
// }

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Orientation {
    Vertical,
    Horizontal,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct LineSegment {
    pub x1: i32,
    pub y1: i32,
    pub x2: i32,
    pub y2: i32,
    pub orientation: Orientation,
}

impl LineSegment {
    pub fn new(x1: i32, y1: i32, x2: i32, y2: i32) -> Self {
        let orientation = if x1 == x2 {
            Orientation::Horizontal
        } else {
            Orientation::Vertical
        };

        Self {
            x1,
            y1,
            x2,
            y2,
            orientation,
        }
    }
}

pub type PathLine = Vec<LineSegment>;

// Parsing using Nom
fn point(_input: &str) -> IResult<&str, &str> {
    todo!()
}

// e.g. R75 -> Direction::Right
fn p_direction(input: &str) -> IResult<&str, Direction> {
    alt((
        tag("U").map(|_| Direction::Up),
        tag("D").map(|_| Direction::Down),
        tag("L").map(|_| Direction::Left),
        tag("R").map(|_| Direction::Right),
    ))
    .parse(input)
}

// e.g. R75
fn p_instruction(input: &str) -> IResult<&str, Instruction> {
    let (rest, (d, a)) = pair(p_direction, i32).parse(input)?;

    Ok((
        rest,
        Instruction {
            direction: d,
            amount: a,
        },
    ))
}

// e.g. R75,D12
pub fn p_instructionline(input: &str) -> IResult<&str, InstructionLine> {
    separated_list1(tag(","), p_instruction).parse(input)
}

pub fn p_instructions(input: &str) -> IResult<&str, Instructions> {
    separated_list1(tag("\n"), p_instructionline).parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instruction() {
        assert_eq!(
            p_instruction("R75"),
            Ok((
                "",
                Instruction {
                    direction: Direction::Right,
                    amount: 75
                }
            ))
        );
    }

    #[test]
    fn test_direction() {
        assert_eq!(p_direction("R"), Ok(("", Direction::Right)))
    }

    #[test]
    fn test_instructionline() {
        assert_eq!(
            p_instructionline("R74,D12"),
            Ok((
                "",
                vec![
                    Instruction {
                        direction: Direction::Right,
                        amount: 74
                    },
                    Instruction {
                        direction: Direction::Down,
                        amount: 12
                    }
                ]
            ))
        )
    }
}

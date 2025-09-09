#![allow(dead_code)]

use nom::{IResult, Parser, bytes::complete::tag, character::complete::i32};

// Types

#[derive(Debug, PartialEq)]
pub struct InputRange {
    pub from: i32,
    pub to: i32,
}

// Parsing using Nom
pub fn p_input_range(input: &str) -> IResult<&str, InputRange> {
    let (input, from) = i32.parse(input)?;
    let (input, _) = tag("-")(input)?;
    let (input, to) = i32.parse(input)?;

    Ok((input, InputRange { from, to }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_input_range() {
        assert_eq!(
            p_input_range("123-456"),
            Ok(("", InputRange { from: 123, to: 456 }))
        );
    }
}

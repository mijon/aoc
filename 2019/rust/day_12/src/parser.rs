use glam::IVec3;
use nom::Parser;
use nom::character::complete::{i32, newline, one_of};
use nom::multi::separated_list1;
use nom::{IResult, bytes::complete::tag};

fn p_value(input: &str) -> IResult<&str, i32> {
    let (input, _) = one_of(b"xyz".as_slice())(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, num) = i32(input)?;
    Ok((input, num))
}

fn p_vector(input: &str) -> IResult<&str, IVec3> {
    let (input, x) = p_value.parse(input)?;
    let (input, _) = tag(", ").parse(input)?;
    let (input, y) = p_value.parse(input)?;
    let (input, _) = tag(", ").parse(input)?;
    let (input, z) = p_value.parse(input)?;

    Ok((input, IVec3::new(x, y, z)))
}

fn p_line(input: &str) -> IResult<&str, IVec3> {
    let (input, _) = tag("<").parse(input)?;
    let (input, v) = p_vector(input)?;
    let (input, _) = tag(">").parse(input)?;

    Ok((input, v))
}

pub fn parse_input(input: &str) -> IResult<&str, Vec<IVec3>> {
    let (input, result) = separated_list1(newline, p_line).parse(input)?;
    Ok((input, result))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_value() {
        let input = "x=2";
        let expected_value = 2;
        let expected = Ok(("", expected_value));
        assert_eq!(expected, p_value(input))
    }

    #[test]
    fn test_parse_vector() {
        let input = "x=1, y=2, z=3";
        let expected_vec = IVec3::new(1, 2, 3);
        let expected = Ok(("", expected_vec));
        assert_eq!(expected, p_vector(input))
    }

    #[test]
    fn test_parse_line() {
        let input = "<x=1, y=2, z=3>";
        let expected_vec = IVec3::new(1, 2, 3);
        let expected = Ok(("", expected_vec));
        assert_eq!(expected, p_line(input))
    }

    #[test]
    fn test_parse_input() {
        let input = "<x=1, y=2, z=3>
<x=4, y=5, z=6>";
        let expected = Ok(("", vec![IVec3::new(1, 2, 3), IVec3::new(4, 5, 6)]));
        assert_eq!(expected, parse_input(input))
    }
}

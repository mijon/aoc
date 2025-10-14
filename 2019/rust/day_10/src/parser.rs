use glam::IVec2;
use nom::{
    IResult, Parser, bytes::complete::take_till, character::complete::satisfy, multi::many1,
    sequence::preceded,
};
use nom_locate::{LocatedSpan, position};

pub type Span<'a> = LocatedSpan<&'a str>;

fn hash_pos(input: Span) -> IResult<Span, IVec2> {
    let (input, pos) = position(input)?;
    let x = pos.get_column() as i32 - 1;
    let y = pos.location_line() as i32 - 1;
    // let (input, _c) = tag("#")(input)?;
    let (input, _c) = satisfy(|c| c == '#')(input)?;
    Ok((input, IVec2::new(x, y)))
}

pub fn parse(input: Span) -> IResult<Span, Vec<IVec2>> {
    many1(preceded(take_till(|c| c == '#'), hash_pos)).parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_single_hash() {
        let input = ".....
.....
.....
.....
...#.";
        let expected = vec![IVec2::new(3, 4)];
        assert_eq!(expected, parse(input.into()).expect("parse result").1)
    }

    #[test]
    fn parse_two_hashes() {
        let input = "....#.
..#...";
        let expected = vec![IVec2::new(4, 0), IVec2::new(2, 1)];
        assert_eq!(expected, parse(input.into()).expect("parse result").1)
    }
}

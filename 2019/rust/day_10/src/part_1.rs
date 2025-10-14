#![warn(clippy::all, clippy::pedantic)]

use std::ops::Mul;

use crate::parser;
use glam::{IVec2, Vec2};
use itertools::Itertools;

pub fn solve(input: &str) -> i32 {
    let result = process(input);
    println!("{result:?}");
    result[0].1
}

pub fn process(input: &str) -> Vec<(IVec2, i32)> {
    let locations = parser::parse(input.into()).expect("parses").1;
    let results = locations
        .iter()
        .map(|v| count_unique_directions(*v, locations.clone()));

    let max_seen = results.clone().map(|v| v.1).max().unwrap();

    let answer_vec = results.filter(|(_v, i)| *i == max_seen);

    answer_vec.collect()
}

fn unit_vec(origin: IVec2, destination: IVec2) -> Vec2 {
    (destination - origin).as_vec2().normalize()
}

fn count_unique_directions(current: IVec2, field: Vec<IVec2>) -> (IVec2, i32) {
    // This ended up being a little janky, essentially the plan is that for each `current`
    // asteroid, we find unit vectors pointing at all the rest, then keep the unique unit vectors,
    // this is the vector of directions where the `current` asteroid can see at least one other.
    // Unfortunately I couldn't think of a better way to do the conversions to have something
    // that's able to be compared as comparing f32s isn't correct.
    let n_seen = field
        .iter()
        .map(|v| unit_vec(current, *v))
        .filter(|v| !v.is_nan())
        .map(|v| v.to_angle().mul(10000.0) as i32)
        .collect::<Vec<i32>>()
        .into_iter()
        .unique()
        .collect::<Vec<i32>>()
        .len();
    (current, n_seen as i32)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_example1() {
        let input = ".#..#
.....
#####
....#
...##";
        let expected = vec![(IVec2::new(3, 4), 8)];
        assert_eq!(process(input), expected)
    }

    #[test]
    fn test_example2() {
        let input = "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####";
        let expected = 33;
        assert_eq!(solve(input), expected)
    }

    #[test]
    fn test_example3() {
        let input = "#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.";
        let expected = 35;
        assert_eq!(solve(input), expected)
    }

    #[test]
    fn test_example4() {
        let input = "
.#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..";
        let expected = 41;
        assert_eq!(solve(input), expected)
    }

    #[test]
    fn test_example5() {
        let input = ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##";
        let expected = 210;
        assert_eq!(solve(input), expected)
    }

    #[test]
    fn test_unit_vec() {
        let a = IVec2::new(0, 0);
        let b = IVec2::new(2, 0);
        assert_eq!(unit_vec(a, b), Vec2::new(1.0, 0.0))
    }
}

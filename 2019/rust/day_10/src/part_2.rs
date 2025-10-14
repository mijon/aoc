#![warn(clippy::all, clippy::pedantic)]

use std::cmp::Ordering;
use std::f32::consts::PI;
use std::ops::Mul;

use serde::ser::SerializeTuple;
use serde::{Serialize, Serializer};

use crate::parser;
use crate::part_1;
use glam::{IVec2, Vec2};

pub fn solve(input: &str, target_index: i32) -> i32 {
    let target_asteroid = get_vec(input, target_index);
    println!(
        "---> asteroid {} destroyed: {:?}",
        target_index, target_asteroid
    );
    target_asteroid.x * 100 + target_asteroid.y
}

fn get_vec(input: &str, target_index: i32) -> IVec2 {
    // use the machinery we made in part 1 to get the starting point. Obviously we could hard code
    // it, but where's the fun in that (and would make testing on the examples harder).
    let start_pos = part_1::process(input)[0].0;
    println!("{:?}", start_pos);

    let locations = parser::parse(input.into()).expect("parses").1;
    let processed_locations = locations_to_asteroid_infos(start_pos, locations);
    let output = sort_asteroids_by_rotation(processed_locations);
    println!("{:?}", serde_json::to_string(&output));
    // println!("{:?}", output);
    // Couldn't figre out how to do this with an iterator and Map
    output[(target_index - 1) as usize].vec
}

fn sort_asteroids_by_rotation(processed_locations: Vec<AsteroidInfo>) -> Vec<AsteroidInfo> {
    // First, we chunk all the asteroids into groups based on their angle. This way we end up with
    // a vec of vecs, where each element is a vec of all the asteroids along one ray. Within the
    // element, the Asteroids are ordered by distance from the starting point.
    let chunked_asteroids = processed_locations
        .chunk_by(|a, b| a.angle == b.angle)
        .collect::<Vec<_>>();

    // Then we run through each chunk and add a full turn to their angle for each position they are
    // in the vec, i.e. the second element gets 1 * pi, as we have to take a full turn after
    // blasting the first element before we see the second.
    let mut output = Vec::new();
    for chunk in &chunked_asteroids {
        for i in 0..chunk.len() {
            // chunk[i].angle = chunk[i].angle * (i as i32 + 1) * 31415;
            output.push(AsteroidInfo {
                vec: chunk[i].vec,
                angle: chunk[i].angle,
                scaled_angle: chunk[i].angle + (i as i32) * 2 * quantise_angle(PI),
                length: chunk[i].length,
            });
        }
    }

    output.sort_by(sort_asteroids_second_pass);
    output
}

fn locations_to_asteroid_infos(start_pos: IVec2, locations: Vec<IVec2>) -> Vec<AsteroidInfo> {
    let mut processed_locations = locations
        .iter()
        .filter(|l| **l != start_pos)
        // .inspect(|e| println!("{:?}", (*e - start_pos).as_vec2()))
        .map(|v| {
            let vec_to = (v - start_pos).as_vec2();
            AsteroidInfo::new(
                *v,
                quantise_angle(my_angle_to(Vec2::new(0.0, -1.0), vec_to)), // seems
                // to produce the right answer in testing
                vec_to.length_squared().abs() as i32,
            )
        })
        .collect::<Vec<AsteroidInfo>>();

    processed_locations.sort_by(sort_asteroids_first_pass);
    processed_locations
}

#[derive(Debug, Clone, Copy)]
struct AsteroidInfo {
    vec: IVec2,
    angle: i32,
    scaled_angle: i32,
    length: i32,
}

impl Serialize for AsteroidInfo {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let the_vec = (self.vec.x, self.vec.y);
        let mut tup = serializer.serialize_tuple(2)?;
        tup.serialize_element(&the_vec)?;
        tup.end()
    }
}

fn my_angle_to(lhs: Vec2, rhs: Vec2) -> f32 {
    // Angle_to returns an angle in the range [-pi, pi], picking whichever direction gives the
    // smallest magnitude. Instead, we need the angle in a particular direction ranging between [0,
    // 2*pi)
    let angle_to = lhs.angle_to(rhs);
    if angle_to == 0.0 {
        0.0
    } else if angle_to < 0.0 {
        // -1.0 * angle_to
        2.0 * PI + angle_to
    } else {
        // 2.0 * PI - angle_to
        1.0 * angle_to
    }
}

impl AsteroidInfo {
    fn new(vec: IVec2, angle: i32, length: i32) -> Self {
        Self {
            vec,
            angle,
            scaled_angle: angle,
            length,
        }
    }
}

fn sort_asteroids_first_pass(a: &AsteroidInfo, b: &AsteroidInfo) -> Ordering {
    match a.angle.cmp(&b.angle) {
        Ordering::Equal => a.length.cmp(&b.length),
        _ => a.angle.cmp(&b.angle),
    }
}
fn sort_asteroids_second_pass(a: &AsteroidInfo, b: &AsteroidInfo) -> Ordering {
    a.scaled_angle.cmp(&b.scaled_angle)
}

fn quantise_angle(angle: f32) -> i32 {
    angle.mul(10000.0) as i32
}

#[cfg(test)]
mod tests {
    use rstest::rstest;
    use std::f64::consts::PI;

    use super::*;

    #[test]
    fn test_example6_part2() {
        let input = ".#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....#...###..
..#.#.....#....##";
        let expected = vec![(IVec2::new(8, 3), 30)];
        assert_eq!(part_1::process(input), expected)
    }

    #[test]
    fn test_angles() {
        let test_point = Vec2::new(10.0, 19.0); // the point of the asteroid
        let ref_point = Vec2::new(11.0, 13.0); // the point of the laser
        let vec_to = test_point - ref_point;
        let ref_vec = Vec2::new(0.0, -1.0); // the reference vector "straight up" in
        // the matrix
        dbg!(test_point);
        dbg!(ref_point);
        dbg!(vec_to);
        println!("base angle from glam: {}", ref_vec.angle_to(vec_to));
        let expected_angle = 2.0 / 4.0 * PI as f32;
        let calculated_angle = my_angle_to(ref_vec, vec_to);
        dbg!(expected_angle);
        dbg!(calculated_angle);
        assert!((expected_angle - calculated_angle).abs() < 0.1)
    }

    // #[test]
    // fn test_mine_simple() {
    //     let input = "..#..
    // ..#..
    // ..#..
    // #.#.#";
    //     let expected = 100;
    //     assert_eq!(solve(input), expected)
    // }

    #[rstest]
    #[case(1, IVec2::new(11, 12))] // ok
    #[case(2, IVec2::new(12, 1))] // ok
    #[case(3, IVec2::new(12, 2))] // ok
    #[case(10, IVec2::new(12, 8))] // ok
    #[case(20, IVec2::new(16, 0))] // ok
    #[case(50, IVec2::new(16, 9))] // ok
    #[case(100, IVec2::new(10, 16))]
    #[case(199, IVec2::new(9, 6))]
    #[case(200, IVec2::new(8, 2))]
    #[case(201, IVec2::new(10, 9))]
    #[case(299, IVec2::new(11, 1))] // ok
    fn test_example8_part2(#[case] index: i32, #[case] expected_vec: IVec2) {
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
        assert_eq!(get_vec(input, index), expected_vec)
    }

    #[test]
    fn test_angle_for_same_vec() {
        let vec1 = Vec2::new(0.0, -1.0);
        let vec2 = Vec2::new(0.0, -1.0);
        let expected = 0.0;
        let actual = my_angle_to(vec1, vec2);
        assert_eq!(actual, expected)
    }
}

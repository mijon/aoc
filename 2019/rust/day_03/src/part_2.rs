use itertools::Itertools;

use crate::parsing::{Direction, Instruction, LineSegment, Orientation, p_instructions};

#[derive(Debug, PartialEq, Clone, Copy)]
struct LinePath {
    segment: LineSegment,
    path_length: i32, // This is the path length to the *end* of the segement
}

impl LinePath {
    fn new(x1: i32, y1: i32, x2: i32, y2: i32) -> Self {
        Self {
            segment: LineSegment::new(x1, y1, x2, y2),
            path_length: 0,
        }
    }
}

pub fn solve(input: &str) -> i32 {
    let ins_to_line_path = |cur_lp: &mut LinePath, i: Instruction| -> Option<LinePath> {
        let cur_path_length = &mut cur_lp.path_length;
        *cur_path_length += i.amount;

        let cur_ls = &mut cur_lp.segment;
        let cur_x = (cur_ls).x2;
        let cur_y = (cur_ls).y2;
        *cur_ls = match i {
            Instruction {
                direction: Direction::Up,
                amount: a,
            } => LineSegment::new(cur_x, cur_y, cur_x, cur_y + a),
            Instruction {
                direction: Direction::Down,
                amount: a,
            } => LineSegment::new(cur_x, cur_y, cur_x, cur_y - a),
            Instruction {
                direction: Direction::Left,
                amount: a,
            } => LineSegment::new(cur_x, cur_y, cur_x - a, cur_y),
            Instruction {
                direction: Direction::Right,
                amount: a,
            } => LineSegment::new(cur_x, cur_y, cur_x + a, cur_y),
        };
        Some(LinePath {
            segment: *cur_ls,
            path_length: *cur_path_length,
        })
    };

    let instructions = p_instructions(input).expect("Error parsing").1;
    let paths: Vec<Vec<LinePath>> = instructions
        .into_iter()
        .map(|l| -> Vec<LinePath> {
            l.into_iter()
                .scan(LinePath::new(0, 0, 0, 0), ins_to_line_path)
                .collect()
        })
        .collect();

    let collected_intersections = paths[0]
        .iter()
        .cartesian_product(paths[1].iter())
        .map(|a| get_intersection(a.0, a.1));

    collected_intersections
        .into_iter()
        .flatten()
        .collect::<Vec<(i32, i32, i32)>>()
        .into_iter()
        .filter(|x| x.1 != 0 && x.2 != 0)
        .map(path_dist)
        .collect::<Vec<i32>>()
        .into_iter()
        .min()
        .expect("Expected there to be some minimum point")
}

fn path_dist(p: (i32, i32, i32)) -> i32 {
    p.2
}

fn between(a: i32, b: i32, c: i32) -> bool {
    ((a <= b) && (b <= c)) || ((a >= b) && (b >= c))
}

fn get_intersection(a: &LinePath, b: &LinePath) -> Option<(i32, i32, i32)> {
    let has_match = match a.segment.orientation {
        crate::parsing::Orientation::Vertical => {
            between(b.segment.x1, a.segment.x1, b.segment.x2)
                && between(a.segment.y1, b.segment.y1, a.segment.y2)
                && b.segment.orientation == Orientation::Horizontal
        }
        crate::parsing::Orientation::Horizontal => {
            between(a.segment.x1, b.segment.x1, a.segment.x2)
                && between(b.segment.y1, a.segment.y1, b.segment.y2)
                && b.segment.orientation == Orientation::Vertical
        }
    };

    if !has_match {
        return None;
    }

    match a.segment.orientation {
        Orientation::Vertical => {
            let intersection_x = a.segment.x1;
            let intersection_y = b.segment.y1;

            let path_a = (a.segment.y2 - intersection_y).abs();
            let path_b = (b.segment.x2 - intersection_x).abs();

            Some((
                intersection_x,
                intersection_y,
                a.path_length + b.path_length - path_a - path_b,
            ))
        }
        Orientation::Horizontal => {
            let intersection_x = b.segment.x1;
            let intersection_y = a.segment.y1;

            let path_a = (a.segment.x2 - intersection_x).abs();
            let path_b = (b.segment.y2 - intersection_y).abs();

            Some((
                intersection_x,
                intersection_y,
                a.path_length + b.path_length - path_a - path_b,
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("R8,U5,L5,D3\nU7,R6,D4,L4", 30)]
    #[case(
        "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83",
        610
    )]
    #[case(
        "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7",
        410
    )]
    fn test_provided_input(#[case] input: &str, #[case] expected: i32) {
        assert_eq!(solve(&input), expected)
    }
}

use itertools::Itertools;

use crate::parsing::{Direction, Instruction, LineSegment, Orientation, p_instructions};

pub fn solve(input: &str) -> i32 {
    let ins_to_line_seg = |cur_ls: &mut LineSegment, i: Instruction| -> Option<LineSegment> {
        let cur_x = (*cur_ls).x2;
        let cur_y = (*cur_ls).y2;
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
        Some(*cur_ls)
    };
    let instructions = p_instructions(input).expect("Error parsing").1;
    let paths: Vec<Vec<LineSegment>> = instructions
        .into_iter()
        .map(|l| -> Vec<LineSegment> {
            l.into_iter()
                .scan(LineSegment::new(0, 0, 0, 0), ins_to_line_seg)
                .collect()
        })
        .collect();

    let collected_intersections = paths[0]
        .iter()
        .cartesian_product(paths[1].iter())
        .map(|a| get_intersection(&a.0, &a.1));

    let nearest_point = collected_intersections
        .into_iter()
        .flatten()
        .collect::<Vec<(i32, i32)>>()
        .into_iter()
        .map(manhatten_dist)
        .collect::<Vec<i32>>()
        .into_iter()
        .filter(|x| *x != 0)
        .min()
        .expect("Expected there to be some minimum point");

    nearest_point
}

fn manhatten_dist(p: (i32, i32)) -> i32 {
    p.0.abs() + p.1.abs()
}

fn between(a: i32, b: i32, c: i32) -> bool {
    ((a <= b) && (b <= c)) || ((a >= b) && (b >= c))
}

// Assuming that line segments will only ever cross and not be colinear
fn get_intersection(a: &LineSegment, b: &LineSegment) -> Option<(i32, i32)> {
    let has_match = match a.orientation {
        crate::parsing::Orientation::Vertical => {
            between(b.x1, a.x1, b.x2)
                && between(a.y1, b.y1, a.y2)
                && b.orientation == Orientation::Horizontal
        }
        crate::parsing::Orientation::Horizontal => {
            between(a.x1, b.x1, a.x2)
                && between(b.y1, a.y1, b.y2)
                && b.orientation == Orientation::Vertical
        }
    };

    if !has_match {
        return None;
    }

    match a.orientation {
        Orientation::Vertical => Some((a.x1, b.y1)),
        Orientation::Horizontal => Some((b.x1, a.y1)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(1, 2, 3, true)]
    #[case(3, 2, 1, true)]
    #[case(1, 1, 1, true)]
    #[case(1, 2, 1, false)]
    #[case(1, 5, 2, false)]
    fn test_between(#[case] a: i32, #[case] b: i32, #[case] c: i32, #[case] result: bool) {
        assert_eq!(between(a, b, c), result)
    }

    #[test]
    fn test_get_intersection() {
        let a = LineSegment::new(1, 2, 5, 2);
        let b = LineSegment::new(3, 1, 3, 4);
        println!("{:?}", a);
        println!("{:?}", b);
        assert_eq!(get_intersection(&a, &b), Some((3, 2)))
    }

    #[test]
    fn test_get_intersection2() {
        let a = LineSegment::new(8, 5, 3, 5);
        let b = LineSegment::new(6, 7, 6, 3);
        println!("{:?}", a);
        println!("{:?}", b);
        assert_eq!(get_intersection(&a, &b), Some((6, 5)))
    }

    #[test]
    fn test_provided_input() {
        let example_input = "R8,U5,L5,D3\nU7,R6,D4,L4".to_string();
        let expected = 6;
        println!("{:?}", p_instructions(&example_input));
        assert_eq!(expected, solve(&example_input))
    }
}

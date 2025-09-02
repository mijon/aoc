use crate::parsing::{Direction, Instruction, Point, p_instructions};

pub fn solve(input: &str) -> i64 {
    let instructions = p_instructions(input).expect("Error parsing").1;
    println!("{}", instructions.len());
    let paths: Vec<Vec<Point>> = instructions
        .into_iter()
        .map(|l| -> Vec<Point> {
            l.into_iter()
                .scan(Point { x: 0, y: 0 }, ins_to_pts)
                .collect()
        })
        .collect();
    println!("{}", paths.len());
    4
}

fn ins_to_pts(cur_point: &mut Point, i: Instruction) -> Option<Point> {
    match i {
        Instruction {
            direction: Direction::Up,
            amount: a,
        } => Some(Point {
            x: cur_point.x,
            y: cur_point.y + a,
        }),
        Instruction {
            direction: Direction::Down,
            amount: a,
        } => Some(Point {
            x: cur_point.x,
            y: cur_point.y - a,
        }),
        Instruction {
            direction: Direction::Left,
            amount: a,
        } => Some(Point {
            x: cur_point.x - a,
            y: cur_point.y,
        }),
        Instruction {
            direction: Direction::Right,
            amount: a,
        } => Some(Point {
            x: cur_point.x + a,
            y: cur_point.y,
        }),
    }
}
#[cfg(test)]
mod tests {

    #[test]
    fn test_scan() {
        let a = [1, 2, 3, 4];
        let iter = a.into_iter().scan(100, |state, x| {
            *state = *state + x;
            Some(*state)
        });
        assert_eq!(vec![101, 103, 106, 110], iter.collect::<Vec<i32>>());
    }
}

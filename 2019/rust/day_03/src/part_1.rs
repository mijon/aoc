use crate::parsing::{Direction, Instruction, LineSegment, p_instructions};

pub fn solve(input: &str) -> i64 {
    let instructions = p_instructions(input).expect("Error parsing").1;
    println!("{}", instructions.len());
    let paths: Vec<Vec<LineSegment>> = instructions
        .into_iter()
        .map(|l| -> Vec<LineSegment> {
            l.into_iter()
                .scan(
                    LineSegment {
                        x1: 0,
                        y1: 0,
                        x2: 0,
                        y2: 0,
                    },
                    ins_to_line_seg,
                )
                .collect()
        })
        .collect();
    println!("{:?}", paths);
    println!("{}", paths.len());
    4
}

fn ins_to_line_seg(cur_ls: &mut LineSegment, i: Instruction) -> Option<LineSegment> {
    match i {
        Instruction {
            direction: Direction::Up,
            amount: a,
        } => Some(LineSegment {
            x1: cur_ls.x2,
            y1: cur_ls.y2,
            x2: cur_ls.x2,
            y2: cur_ls.y2 + a,
        }),
        Instruction {
            direction: Direction::Down,
            amount: a,
        } => Some(LineSegment {
            x1: cur_ls.x2,
            y1: cur_ls.y2,
            x2: cur_ls.x1,
            y2: cur_ls.y2 - a,
        }),
        Instruction {
            direction: Direction::Left,
            amount: a,
        } => Some(LineSegment {
            x1: cur_ls.x2,
            y1: cur_ls.y2,
            x2: cur_ls.x1 - a,
            y2: cur_ls.y2,
        }),
        Instruction {
            direction: Direction::Right,
            amount: a,
        } => Some(LineSegment {
            x1: cur_ls.x2,
            y1: cur_ls.y2,
            x2: cur_ls.x1 + a,
            y2: cur_ls.y2,
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

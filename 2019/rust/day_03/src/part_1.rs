use crate::parsing::{Direction, Instruction, LineSegment, p_instructions};

pub fn solve(input: &str) -> i64 {
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
    println!("{:?}", paths);
    4
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

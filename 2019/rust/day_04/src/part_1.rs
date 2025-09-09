use crate::parsing::p_input_range;
// use std::cmp::max;
use std::vec;

pub fn solve(input: &str) -> usize {
    let (_, input_range) = p_input_range(input).expect("Input is malformed");

    let rng = input_range.from..=input_range.to;
    rng.filter(check_value).collect::<Vec<_>>().len()
}

fn check_value(i: &i32) -> bool {
    let i_vec = int_to_vec(*i);

    check_pair(&i_vec) && check_never_decrease(&i_vec)
}

fn check_never_decrease(i_vec: &Vec<i32>) -> bool {
    i_vec
        .windows(2)
        .map(|n| n[1] - n[0] >= 0)
        .fold(true, |acc, n| acc && n)
}

fn check_pair(i_vec: &Vec<i32>) -> bool {
    // Take a vector, sort it, then partition it then y
    // i_vec_clone.sort();
    let max_count = i_vec
        .chunk_by(|a, b| a == b)
        .map(|v| v.len())
        .into_iter()
        .max()
        .expect("number");
    max_count > 1
}

//     let mut v2 = v.clone();
//     v2.sort_unstable();
//     v2.dedup();
//     v2.to_vec()
// }
//
// fn count_occurences(vals: Vec<i32>) -> Vec<(i32, i32)> {
//     let unique_vals = sort_and_dedup(&vals);
//     jj
// }

// Break an integer into a vec of its digits
fn int_to_vec(n: i32) -> Vec<i32> {
    if n == 0 {
        return vec![0];
    }

    let mut num = n;
    let mut result = std::iter::from_fn(move || {
        if num == 0 {
            None
        } else {
            let output = num % 10;
            num /= 10;
            Some(output)
        }
    })
    .collect::<Vec<i32>>();

    result.reverse();
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[test]
    fn break_numbers_to_vec() {
        assert_eq!(int_to_vec(123), vec![1, 2, 3])
    }

    #[rstest]
    #[case (vec![1,1,1], true)]
    #[case (vec![1,2,3], false)]
    #[case (vec![1,2,1], false)]
    fn test_pair_detection(#[case] v: Vec<i32>, #[case] result: bool) {
        assert_eq!(check_pair(&v), result)
    }

    #[rstest]
    #[case (vec![1,2,3,4], true)]
    #[case (vec![1,1,1,1], true)]
    #[case (vec![1,2,1], false)]
    fn test_not_decreasing(#[case] v: Vec<i32>, #[case] p: bool) {
        assert_eq!(check_never_decrease(&v), p)
    }
}

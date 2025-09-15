use std::collections::{HashMap, VecDeque};

pub fn solve(input: &str) -> i32 {
    let orbits = parse_input(input);
    let depths = label_depths("COM", orbits);

    depths.values().sum()
}

fn parse_input(input: &str) -> HashMap<&str, Vec<&str>> {
    let parsed_input = input.lines().map(|s| {
        let splitted = s.split(")").collect::<Vec<&str>>();
        (splitted[0], splitted[1])
    });

    let mut orbits: HashMap<&str, Vec<&str>> = HashMap::new();

    for (major, minor) in parsed_input {
        if orbits.contains_key(major) {
            let mut tmp: Vec<&str> = orbits.get(major).unwrap().to_vec();
            tmp.push(minor);
            orbits.insert(major, tmp);
        } else {
            orbits.insert(major, vec![minor]);
        }
    }
    orbits
}

fn label_depths<'a>(
    start: &'a str,
    input: HashMap<&'a str, Vec<&'a str>>,
) -> HashMap<&'a str, i32> {
    let mut outputhm = HashMap::new();

    let mut deq = VecDeque::from([(start, 0)]);

    while !deq.is_empty() {
        let (cur_label, cur_depth) = deq.pop_front().unwrap();
        outputhm.insert(cur_label, cur_depth);
        if let Some(children) = input.get(cur_label) {
            for c in children {
                deq.push_back((c, cur_depth + 1))
            }
        }
    }

    outputhm
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let input = "A)B\nB)C";
        let mut expected: HashMap<&str, Vec<&str>> = HashMap::new();
        expected.insert("A", vec!["B"]);
        expected.insert("B", vec!["C"]);

        assert_eq!(expected, parse_input(input))
    }

    #[test]
    fn test_depths_for_g2() {
        let mut expected: HashMap<&str, i32> = HashMap::new();
        expected.insert("A", 0);
        expected.insert("B", 1);

        let input = parse_input("A)B");

        assert_eq!(expected, label_depths("A", input))
    }

    #[test]
    fn test_depths_for_g3() {
        let mut expected: HashMap<&str, i32> = HashMap::new();
        expected.insert("A", 0);
        expected.insert("B", 1);
        expected.insert("C", 2);

        let input = parse_input("A)B\nB)C");

        assert_eq!(expected, label_depths("A", input))
    }

    #[test]
    fn test_depths_for_g_branch() {
        let mut expected: HashMap<&str, i32> = HashMap::new();
        expected.insert("A", 0);
        expected.insert("B", 1);
        expected.insert("C", 1);

        let input = parse_input("A)B\nA)C");

        assert_eq!(expected, label_depths("A", input))
    }

    #[test]
    fn test_part_1() {
        let input = "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L";
        assert_eq!(42, solve(input))
    }
}

use std::collections::HashMap;

pub fn solve(input: &str) -> usize {
    let orbits = parse_input_rev(input);
    let mut you_path = follow_to_source("YOU", &orbits);
    let mut san_path = follow_to_source("SAN", &orbits);

    println!("{:?}", you_path);
    println!("{:?}", san_path);

    let mut still_going = true;

    while still_going {
        let first_you = you_path.pop().unwrap();
        let first_san = san_path.pop().unwrap();

        if first_you != first_san {
            still_going = false;
        }
    }

    you_path.len() + san_path.len()
}

fn parse_input_rev(input: &str) -> HashMap<&str, &str> {
    let parsed_input = input.lines().map(|s| {
        let splitted = s.split(")").collect::<Vec<&str>>();
        (splitted[0], splitted[1])
    });

    let mut orbits: HashMap<&str, &str> = HashMap::new();

    for (major, minor) in parsed_input {
        orbits.insert(minor, major);
    }
    orbits
}

fn follow_to_source<'a>(start: &'a str, orbits: &HashMap<&'a str, &'a str>) -> Vec<&'a str> {
    let mut output = Vec::<&str>::new();
    let mut current = start;
    output.push(current);

    while let Some(orbited) = orbits.get(current) {
        output.push(orbited);
        current = orbited;
    }

    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let input = "A)B\nB)C";
        let mut expected: HashMap<&str, &str> = HashMap::new();
        expected.insert("B", "A");
        expected.insert("C", "B");

        assert_eq!(expected, parse_input_rev(input))
    }

    #[test]
    fn test_follow_path() {
        let input = "COM)B\nB)C";
        let expected = vec!["C", "B", "COM"];
        let parsed_input = parse_input_rev(input);

        assert_eq!(expected, follow_to_source("C", &parsed_input))
    }

    #[test]
    fn test_part_2() {
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
K)L
K)YOU
I)SAN";
        assert_eq!(4, solve(input))
    }
}

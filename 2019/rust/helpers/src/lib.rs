use std::fs;
use std::io;
use std::path::Path;

pub fn read_input(day: &str) -> io::Result<String> {
    const INPUT_DIR: &str = "/home/michael/projects/aoc/2019/input/";
    let day_input = format!("{}_input.txt", day);

    let input_path: &Path = &Path::new(&INPUT_DIR).join(&day_input);
    fs::read_to_string(input_path)
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//
//     #[test]
//     fn it_works() {
//         let result = add(2, 2);
//         assert_eq!(result, 4);
//     }
// }

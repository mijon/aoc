library(tidyverse)

test_input <- c(
  "O....#....",
  "O.OO#....#",
  ".....##...",
  "OO.#O....O",
  ".O.....O#.",
  "O.#..O.#.#",
  "..O..#O..O",
  ".......O..",
  "#....###..",
  "#OO..#....")

input <- read_lines("../input/14_input.txt")

test <- c(".", ".", ".", "O", ".", "#")
test_str <- "...O.#"

roll_left <- function(string) {
  input_string <- string
  rolled <- str_replace_all(string, "\\.O", "O\\.")
  if (identical(input_string, rolled)) {
    return(rolled)
  } else {
    roll_left(rolled)
  }
}



part_1 <- function(input) {
  input |>
    str_split("") |>
    transpose() |>
    map(paste, collapse = "") |>
    map(roll_left) |>
    str_split("") |>
    transpose() |>
    map_chr(paste, collapse = "") |>
    tibble(pattern = _) |>
    mutate(n = rev(1:n()),
           count = str_count(pattern, "O"),
           score = n * count) |>
    pull(score) |>
    sum()
}




# ---- evaluations ----
part_1(input) # 107430
library(tidyverse)

input <- read_lines("../input/09_input.txt")

parse_input <- function(input) {
  str_split(input, " ") |>
    map(as.numeric)
}

gen_all_diffs <- function(v) {
  accumulate(1:length(v),
             \(x, y) diff(x), 
             .init = v) |>
    discard(\(x) {all(x == 0)}) 
}


calculate_next <- function(sequence, selector, reducer) {
  sequence |>
    gen_all_diffs() |>
    map(selector) |>
    as.numeric() |>
    rev() |>
    reduce(reducer, .init = 0)
}

part_1 <- function(input) {
  input|>
    parse_input() |>
    map(calculate_next,
        selector = last,
        reducer = sum) |>
    as.numeric() |>
    sum()
}

# ---- part 2 ---- 
algebra_bit <- function(x, y) {
  y - x
}

part_2 <- function(input) {
  input |>
    parse_input() |>
    map(calculate_next,
        selector = first,
        reducer = algebra_bit) |>
    as.numeric() |>
    sum()
}

# ---- evaluations ----
part_1(input) # 1868368343
part_2(input) # 1022


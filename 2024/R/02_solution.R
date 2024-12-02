library(tidyverse)
INPUT_PATH <- "../input/02_input.txt"

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

parse_input <- function(input) {
  tibble(
    runs = input |>
    str_split(" ") |>
    map(as.numeric))
}


# ---- Working ----
append_checks <- function(input) {
  input |>
    mutate(diffs = map(runs, diff),
           max_diff = map_dbl(diffs, compose(max, abs)),
           min_diff = map_dbl(diffs, compose(min, abs)),
           signs_diff = map_dbl(diffs, compose(length, unique, sign)),
           check = max_diff <= 3 & min_diff >= 1 & signs_diff == 1)
}

count_passes <- function(input) {
  input |>
    filter(check) |>
    nrow()
}

part_1 <- function(input) {
  parse_input(input) |>
  append_checks() |>
  count_passes()
}


expand_run <- function(v) {
  append(map(seq_along(v), \(x) v[-x]),
         list(v))
}

# part 2 is essentially the same as part 1, but we first need to generate all
# the subsequences you get if you drop a single element, then we make sure that
# at least one of those subsequences (or the original) passes all the checks.
part_2 <- function(input) {
  parse_input(input) |>
    transmute(runs = map(runs, expand_run),
              init_run = 1:n()) |>
    unnest(runs) |>
    append_checks() |>
    summarise(check = reduce(check, `|`), .by = init_run) |>
    count_passes()
}


# ---- Results ----
part_1(input) # 483
part_2(input) # 528

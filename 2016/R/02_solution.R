# ---- About ----
#
# This time, we just need to repeatedly move around a space, doing nothing when
# we hit a wall. As the spaces for both parts are fairly small, and we only have
# four possible moves (i.e. UDLR and only single space moves) we can just
# enumerate all the possibilities in a look up table and use that.

library(tidyverse)
INPUT_PATH <- "../input/02_input.txt"

# ---- Reference data ----
lookup_table1 <- list(
  "1" = list("U" = "1", "D" = "4", "L" = "1", "R" = "2"),
  "2" = list("U" = "2", "D" = "5", "L" = "1", "R" = "3"),
  "3" = list("U" = "3", "D" = "6", "L" = "2", "R" = "3"),
  "4" = list("U" = "1", "D" = "7", "L" = "4", "R" = "5"),
  "5" = list("U" = "2", "D" = "8", "L" = "4", "R" = "6"),
  "6" = list("U" = "3", "D" = "9", "L" = "5", "R" = "6"),
  "7" = list("U" = "4", "D" = "7", "L" = "7", "R" = "8"),
  "8" = list("U" = "5", "D" = "8", "L" = "7", "R" = "9"),
  "9" = list("U" = "6", "D" = "9", "L" = "8", "R" = "9")
)

lookup_table2 <- list(
  "1" = list("U" = "1", "D" = "3", "L" = "1", "R" = "1"),
  "2" = list("U" = "2", "D" = "6", "L" = "2", "R" = "3"),
  "3" = list("U" = "1", "D" = "7", "L" = "2", "R" = "4"),
  "4" = list("U" = "4", "D" = "8", "L" = "3", "R" = "4"),
  "5" = list("U" = "5", "D" = "5", "L" = "5", "R" = "6"),
  "6" = list("U" = "2", "D" = "A", "L" = "5", "R" = "7"),
  "7" = list("U" = "3", "D" = "B", "L" = "6", "R" = "8"),
  "8" = list("U" = "4", "D" = "C", "L" = "7", "R" = "9"),
  "9" = list("U" = "9", "D" = "9", "L" = "8", "R" = "9"),
  "A" = list("U" = "6", "D" = "A", "L" = "A", "R" = "B"),
  "B" = list("U" = "7", "D" = "D", "L" = "A", "R" = "C"),
  "C" = list("U" = "8", "D" = "C", "L" = "B", "R" = "C"),
  "D" = list("U" = "B", "D" = "D", "L" = "D", "R" = "D")
)

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

parse_input <- function(input) {
  input |> str_split("")
}

# ---- Working ----
move <- function(pos, direction, lookup_table) {
  lookup_table[[pos]][[direction]]
}

single_code <- function(pos, directions, lookup_table) {
  purrr::reduce(directions, .f = move, .init = pos, lookup_table)
}

process <- function(input, lookup_table) {
  input <- parse_input(input)
  points <- purrr::accumulate(input, .f = single_code, .init = "5", lookup_table)
  points[2:length(points)] |>
    paste(collapse = "") 
}

# ---- Part 1 ----
part_1 <- function(input) {
  process(input, lookup_table1)
}

# ---- Part 2 ----
part_2 <- function(input) {
  process(input, lookup_table2) 
}

# ---- Results ----
part_1(input) # 56983
part_2(input) # 8B8B1

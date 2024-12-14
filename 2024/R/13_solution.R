library(tidyverse)
library(gmp) # for big numbers
options(digits = 22)
INPUT_PATH <- "../input/13_input.txt"

# ---- input reading and parsing ----
input <- readr::read_file(INPUT_PATH)

parse_claw_machine <- function(input) {
  nums <- str_split(input, "\n") |>
    map(str_extract_all, "[0-9]+") |>
    first() |>
    map(as.numeric)
  
  list(
    move_mat = matrix(as.numeric(flatten(nums[1:2])), ncol = 2),
    target_mat = as.matrix(nums[[3]], ncol = 1)
  )
}

parse_input <- function(input) {
  input |>
    str_split("\n\n") |>
    first() |>
    map(parse_claw_machine)
}

# ---- Working ----
solver <- function(l, costs = c(3,1)) {
  solve(l$move_mat, l$target_mat) * costs
}

check_result <- function(r) {
  r[1,1]/3 <= 100 &&
    r[2,1] <= 100 &&
    abs(round(r[1,1], 0) - r[1,1]) < 1e-10 &&
    abs(round(r[2,1], 0) - r[2,1]) < 1e-10
}

check_result2 <- function(r) {
  abs(round(r[1,1], 0) - r[1,1]) < 1e-10 &&
    abs(round(r[2,1], 0) - r[2,1]) < 1e-10
}

part_1 <- function(input) {
  input |>
    parse_input() |>
    map(solver) |>
    keep(check_result) |>
    map(sum) |>
    as.numeric() |>
    sum()
}


apply_part_2_factor <- function(input) {
  part_2_factor <- 10000000000000
  input$target_mat <- input$target_mat + part_2_factor
  input
}

prep_big_nums <- function(input) {
  input$move_mat <- as.bigz(input$move_mat)
  input$target_mat <- as.bigz(input$target_mat)
  input
}

part_2 <- function(input) {
  input |>
    parse_input() |>
    map(compose(solver, prep_big_nums, apply_part_2_factor)) |>
    keep(check_result2) |>
    map(sum) |>
    reduce(`+`)
}


# ---- Results ----
part_1(input) # 29877
part_2(input) # 99423413811305

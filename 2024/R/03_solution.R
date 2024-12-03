library(tidyverse)
INPUT_PATH <- "../input/03_input.txt"

# ---- input reading and parsing ----
input <- readr::read_file(INPUT_PATH)

# ---- Working ----
# Extract anything from the text that looks like "mul(x,y)"
extract_muls <- function(s) { 
  str_extract_all(s, "mul\\([0-9]+,[0-9]+\\)")[[1]]
}

# Given a "mul(x,y)", evaluate it
eval_mul <- function(s) {
  str_extract_all(s, "[0-9]+") |>
    map(compose(prod, as.numeric)) |>
    as.numeric()
}

part_1 <- function(input) {
  input |>
    extract_muls() |>
    eval_mul() |>
    sum()
}

# Now we also want to extract "do()"s and "don't()"s as well as the "mul(x,y)"s
extract_muls_and_dos <- function(s) {
  str_extract_all(s, "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)")[[1]]
}

# We construct a vector of {0, 1} that represents our current state: 1 means we
# *are* doing the muls, 0 means we are not.
convert_to_multiplier <- function(s) {
  step_state <- function(state, input) {
    if (input %in% c(state, NA)) {
      state
    } else {
      1 - state
    }
  }
  
  case_when(
    s == "don't()" ~ 0,
    s == "do()" ~ 1,
    TRUE ~ NA
  ) |>
    accumulate(step_state, .init = 1) |>
    tail(-1)
}

# eval_mul unfortunately results in "do()" and "don't()" evaluating to 1, as the
# product of an empty vector is the multiplicative identity in R, so we need to
# go through and reset them to zero.
reset_dos_to_zero <- function(evaled, original) {
  if_else(original %in% c("do()", "don't()"), 0, evaled)
}

# Part 2 is the same structure as part 1 with additional fiddling having to
# generate the state vector.
part_2 <- function(input) {
  mults <- extract_muls_and_dos(input) 
  state <- convert_to_multiplier(mults)
  
  mults_parsed <- mults |>
    eval_mul() |>
    reset_dos_to_zero(mults)

  sum(mults_parsed * state)
}

# ---- Results ----
part_1(input) # 187194524
part_2(input) # 127092535

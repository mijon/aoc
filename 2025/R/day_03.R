library(tidyverse)

input <- read_lines("../input/input_03.txt")

parse_input <- function(input) {
  input |> str_split("") |>
    map(as.numeric)
}

# ---- part 1 ----
optimise_joltage_single_bank <- function(jolts) {
  joltage_1 <- max(head(jolts, -1))
  joltage_1_pos <- which(jolts == joltage_1)[[1]]
  joltage_2 <- max(tail(jolts, -joltage_1_pos))

  joltage_1 * 10 + joltage_2
}

part_1 <- function(input) {
  input |>
    parse_input() |>
    map_dbl(optimise_joltage_single_bank) |>
    sum()
}

# ---- part 2 ----
optimise_joltage_single_bank_with_length <- function(jolts, n) {
  if (n == 1) {
    return(max(jolts))
  }

  current_joltage <- max(head(jolts, -(n-1)))
  current_joltage_pos <- which(jolts == current_joltage)[[1]]
  other_joltages <- optimise_joltage_single_bank_with_length(tail(jolts, -current_joltage_pos), n - 1)

  current_joltage * 10^(n-1) + other_joltages
}

part_2 <- function(input) {
  input |>
    parse_input() |>
    map_dbl(optimise_joltage_single_bank_with_length, 12) |>
    sum() |>
    scales::comma(big.mark = "")
}

# ---- results ----
part_1(input) # 17311
part_2(input) # 171419245422055


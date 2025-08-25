# ---- About ----
#
# There's not too much to this one. It's pretty much just following the
# instructions given. I'm sure one can deviate from the exact instructions to
# make part 2 faster, but this approach still works even if it is a little slow.
library(tidyverse)

INPUT_PATH <- "../input/16_input.txt"
input <- read_lines(INPUT_PATH)

# Figure out how many reps to do based on roughly doubling each time.
n_reps <- function(input_len, target_len) {
  (target_len/input_len) |> log(base = 2)
}

expand <- function(s, target_len) {
  n_reps <- ceiling(n_reps(str_length(s), target_len))
  
  for (i in 1:n_reps) {
    flipped <- stringi::stri_reverse(s) |>
      str_replace_all("1", "t") |>
      str_replace_all("0", "1") |>
      str_replace_all("t", "0")
    s <- paste0(s, "0", flipped)
  }
  
  str_sub(s, 1, target_len)
}

pairs <- function(s) {
  str_extract_all(s, "[01]{2}")[[1]]
}

pairs_to_single <- function(s) {
  if_else(s %in% c("00", "11"), "1", "0")
}

check_sum <- function(s) {
  if (str_length(s) %% 2 == 1) {
    return(s)
  }
  pairs(s) |>
    pairs_to_single() |>
    paste0(collapse = "") |>
    check_sum()
}

do <- function(target_len) {
  function(s) {
    s |> expand(target_len) |>
      check_sum()
  }
}

part_1 <- do(272)
part_2 <- do(35651584)


part_1(input)      # 10111110010110110

# part 2 is part 1 but slower...
part_1(input) # 01101100001100100




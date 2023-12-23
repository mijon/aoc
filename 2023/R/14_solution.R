library(tidyverse)

input <- read_lines("../input/14_input.txt")

# ---- part 1 ----
roll <- function(string, from, to) {
  input_string <- string
  rolled <- str_replace_all(string, from, to)

  if (identical(input_string, rolled)) {
    return(rolled)
  } else {
    roll(rolled, from, to)
  }
}

roll_left <- partial(roll, from = "\\.O", to = "O\\.")
roll_right <- partial(roll, from = "O\\.", to = "\\.O")

roll_up <- function(input) {
  input |> 
    str_split("") |>
    transpose() |>
    map(paste, collapse = "") |>
    map(roll_left) |>
    str_split("") |>
    transpose() |>
    map_chr(paste, collapse = "")
}

roll_down <- function(input) {
  input |> 
    str_split("") |>
    transpose() |>
    map(paste, collapse = "") |>
    map(roll_right) |>
    str_split("") |>
    transpose() |>
    map_chr(paste, collapse = "")
}

calc_load <- function(df) {
  df |>
    mutate(n = rev(1:n()),
           count = str_count(pattern, "O"),
           score = n * count) |>
    pull(score) |>
    sum()
}

part_1 <- function(input) {
  input |>
    roll_up() |>
    tibble(pattern = _) |>
    calc_load()
}

# ---- part 2 ----
#
# We take advantage of the cyclical nature. There's a burn in period that's
# acyclical, then the stones get into a repeating pattern with some period. We
# only need to simulate up to the point that we get in a pattern we've seen
# before. Then we can predict what pattern the stones will be in after any
# number of iterations of a full cycle.
cycle <- function(input) {
  input |>
    roll_up() |>
    roll_left() |>
    roll_down() |>
    roll_right()
}

generate_enough_history <- function(input) {
  digest_history <- list()
  pattern_history <- list()
  i <- 1
  current_digest <- digest::digest(input)
  while(!current_digest %in% digest_history) {
    print(i)
    digest_history <- append(digest_history, current_digest)
    pattern_history <- append(pattern_history, list(input))
    i <- i + 1
    input <- cycle(input)
    current_digest <- digest::digest(input)
  }
  
    digest_history <- append(digest_history, current_digest)
    pattern_history <- append(pattern_history, list(input))
    
    tibble(digest = as.character(digest_history),
           pattern = pattern_history)
}

count_timings <- function(history_df) {
  timings <- history_df |>
    mutate(index = 1:n()) |>
    add_count(digest) |>
    filter(n > 1) |>
    pull(index)
  
  list(burn_in = timings[1], period = timings[2] - timings[1])
}

get_target_pattern <- function(history_df, timings, target) {
  index <- ((target - timings$burn_in) %% timings$period) + 1 + timings$burn_in
  history_df$pattern[[index]]
}


part_2 <- function(input) {
  history_df <- generate_enough_history(input) 
  timings <- count_timings(history_df)
  target_pattern <- get_target_pattern(history_df, timings, 1e9)
  calc_load(tibble(pattern = target_pattern))
}

# ---- evaluations ----
part_1(input) # 107430
part_2(input) # 96317
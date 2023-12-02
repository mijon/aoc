library(tidyverse)

input <- tibble(input = read_lines("../input/02_input.txt"))

as_numeric <- function(v) {
  tmp <- as.numeric(v)
  if_else(is.na(tmp), 0, tmp)
}

get_col <- function(round, colour) {
  str_extract(round, glue::glue("([0-9]+)(?= {colour})"))
}


# "3 blue, 4 red" -> c(r, g, b) where r = max number of reds seen this game
parse_rounds <- function(round) {
  reds   <- get_col(round, "red") |> as_numeric() |> max()
  greens <- get_col(round, "green") |> as_numeric() |> max()
  blues  <- get_col(round, "blue") |> as_numeric() |> max()
  
  c(r = reds, g = greens, b = blues)
}

parse_games <- function(games) {
  separate(games,
           input,
           into = c("game", "rounds"),
           sep = ": ") |>
    mutate(game = parse_number(game),
           rounds = str_split(rounds, "; "),
           rounds = map(rounds, parse_rounds))
}

check_round <- function(round, target) {
  
}

# target = c(r, g, b) conditions
filter_games <- function(games, target) {
  games |>
    filter(map_lgl(rounds, \(x) {all(x <= target)}))
}

part_1 <- function(input) {
  input |> 
    parse_games() |>
    filter_games(target = c(12,13,14)) |>
    pull(game) |>
    sum()
}

part_2 <- function(games) {
  games |>
    parse_games() |>
    mutate(power = map_dbl(rounds, prod)) |>
    pull(power) |>
    sum()
}

part_1(input) # 2006
part_2(input) # 84911

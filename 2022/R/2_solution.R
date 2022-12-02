library(tidyverse)

read_strategy <- function(path, col_names) {
  read_delim(path,
    delim = " ",
    col_names = col_names
  )
}

translate <- function(v, from, to) {
  names(to) <- from
  map_chr(v, ~ to[.x])
}

score_you <- function(v) {
  case_when(
    v == "R" ~ 1,
    v == "P" ~ 2,
    v == "S" ~ 3
  )
}

score_game <- function(opponent, you) {
  case_when(
    you == opponent ~ 3,
    opponent == "R" & you == "P" ~ 6,
    opponent == "P" & you == "S" ~ 6,
    opponent == "S" & you == "R" ~ 6,
    TRUE ~ 0
  )
}

score <- function(df) {
  sum(score_you(df$you) + score_game(df$opponent, df$you))
}

choose_move <- function(opponent, result) {
  case_when(
    result == "D" ~ opponent,
    result == "W" & opponent == "R" ~ "P",
    result == "W" & opponent == "S" ~ "R",
    result == "L" & opponent == "P" ~ "R",
    result == "L" & opponent == "S" ~ "P",
    TRUE ~ "S"
  )
}

part_1 <- function() {
  read_strategy("../input/2_input.txt", c("opponent", "you")) %>%
    mutate(
      opponent = translate(
        opponent,
        c("A", "B", "C"),
        c("R", "P", "S")
      ),
      you = translate(
        you,
        c("X", "Y", "Z"),
        c("R", "P", "S")
      )
    ) %>%
    score()
}

part_2 <- function() {
  read_strategy("../input/2_input.txt", c("opponent", "result")) %>%
    mutate(
      opponent = translate(
        opponent,
        c("A", "B", "C"),
        c("R", "P", "S")
      ),
      result = translate(
        result,
        c("X", "Y", "Z"),
        c("L", "D", "W")
      ),
      you = choose_move(opponent, result)
    ) %>%
    score()
}


part_1() # 9651
part_2() # 10560

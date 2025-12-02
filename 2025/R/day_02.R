library(tidyverse)

# ---- Reading in and parsing ----
input <- read_file("../input/input_02.txt")

parse_input <- function(input) {
  input_str <- input |>
    str_trim() |>
    str_replace_all("\\-", ":")

  eval(str2lang(paste0("c(", input_str, ")")))
}

# ---- part 1 ----
part_1 <- function(input) {
  processing <- tibble(input = parse_input(input)) |>
    mutate(input_str = as.character(input),
           lengths = str_length(input_str)) |>
    filter(lengths %% 2 == 0)

  max_length <- max(processing$lengths)

  processing |> mutate(
    padded = str_pad(input_str, max_length, side = "both"),
    left = str_sub(padded, start = 1, end = max_length / 2) |> str_trim(),
    right = str_sub(padded, start = max_length / 2 + 1, end = max_length) |> str_trim()
  ) |>
    filter(left == right) |>
    pull(input) |> sum()
}

# ---- part 2 ----
part_2 <- function(input) {
  tibble(input = parse_input(input)) |>
    mutate(input_str = as.character(input),
           test = str_detect(input_str, pattern = "^([0-9]+)\\1+$")) |>
    filter(test) |>
    pull(input) |>
    sum()
}


# ---- Results ----
part_1(input) # 30599400849, but it's slow
part_2(input) # 46270373595

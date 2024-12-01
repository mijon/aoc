library(tidyverse)
INPUT_PATH <- "../input/01_input.txt"

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

parse_input <- function(input) {
  tibble(input = input) |>
    separate(input, into = c("L", "R"), sep = "[ ]+") |>
    mutate(across(everything(), as.numeric))
}

# ---- Working ----
part_1 <- function(input) {
  parse_input(input) |> 
    mutate(diff = abs(sort(L) - sort(R))) |>
    pull(diff) |>
    sum()
}


part_2 <- function(input) {
  init <- parse_input(input)
  
  r_counts <- count(init, R, name = "nR")
  init |> left_join(r_counts, by = c("L" = "R")) |>
    mutate(calc = L * nR) |>
    pull(calc) |>
    sum(na.rm = TRUE)
}


# ---- Results ----
part_1(input) # 1388114
part_2(input) # 23529853

library(tidyverse)
INPUT_PATH <- "../input/{{day}}_input.txt"

example_data <- "{{example_data}}" |>
  str_split("\n")

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

parse_input <- function(input) {
  
}

# ---- Working ----


part_1 <- function(input) {
  parse_input(input)
}


part_2 <- function(input) {
  parse_input(input)
}


# ---- Results ----
part_1(input) # 
part_2(input) # 
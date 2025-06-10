# ---- About ----
#
# This day is all about munging the data into a suitable format to then
# calculate the Mode or antimode of a few vectors. R being a statistical
# language has these tools (fairly) readily available. Honestly, I'm surprised
# there's no mode function in the base or stats packages, but oh well.

library(tidyverse)
INPUT_PATH <- "../input/06_input.txt"

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

parse_input <- function(input) {
  tibble(n = input) |>
    separate(sep = "", col = n,
             into = LETTERS[1:(str_length(input[1])+1)]) |>
    select(-A)
}

# ---- Working ----
# Both parts need essentially the same processing, but with part 1 being the
# mode and part 2 being the anti mode. So let's just make a function factory
# that lets us slot in the appropriate function as needed.

general_solution <- function(f) {
  function(input) {
    parse_input(input) |>
      summarise(across(everything(), f)) |>
      unite(col = n, everything(), sep = "") |>
      pull(n)
  }
}

part_1 <- general_solution(DescTools::Mode)
part_2 <- general_solution(clickR:::antimoda)

# ---- Results ----
part_1(input) # bjosfbce
part_2(input) # veqfxzfx


# ---- About ----
#
# It's a cellular automaton day. Fairly straightforward day, just part 2 is
# rather slow.

library(tidyverse)
INPUT_PATH <- "../input/18_input.txt"

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

parse_input <- function(input) {
  str_split(input, "")[[1]]
}

# ---- Working ----

# Convenience function to add a safe border around the main puzzle space
pad_line <- function(l) c(".", l, ".")

# We need to look at three elements in the previous row to make the new element
# in this row.
expand_triplets <- function(l) {
  map(1:(length(l) - 2), 
      \(i) l[i:(i+2)])
}

# The easiest way to check for traps is just to enumerate all the ways that a
# trap *could* be the next cell, and then check for them
next_char <- function(cs) {
  traps <- list(c("^", "^", "."),
                c(".", "^", "^"),
                c("^", ".", "."),
                c(".", ".", "^"))
  result <- map_lgl(traps, \(x) identical(x, cs)) |>
    any()
  
  if (result) "^" else "."
}

next_line <- function(l) {
  l |>
    pad_line() |>
    expand_triplets() |>
    map_chr(next_char) 
}

process <- function(prev_line, ignore) {
  next_line(prev_line)
}

count_safe <- function(l) {
  sum(l == ".")
}

part_1 <- function(input) {
  input <- parse_input(input)
  accumulate(1:39, process, .init = input) |>
    map_dbl(count_safe) |>
    sum()
}

# Part 2 is just doing part 1 many more times. 
part_2 <- function(input) {
  print(Sys.time())
  input <- parse_input(input)
  result <- accumulate(1:(400000-1), process, .init = input) |>
    map_dbl(count_safe) |>
    sum()
  print(Sys.time())
  result
}


# ---- Results ----
part_1(input) # 2005
part_2(input) # 20008491 # Really slow, sorry!

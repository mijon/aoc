# ---- About ----
#
# We have to expand some compressed data. The data was compressed with a method
# roughly analogous to rle. For part 1 (at least), we can just decompress it. I
# do this using a few deques as that allows us to push or pop as we need.

library(tidyverse)
source("data_structures.R")
INPUT_PATH <- "../input/09_input.txt"

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

parse_input <- function(input) {
  str_split(input, "") |>
    first() |>
    deque()
}

# ---- Working ----
part_1 <- function(input) {
  i <- parse_input(input)
  output <- deque(c()) # We build this over time
  
  while (i$len() > 0) {
    l <- i$pop_front()
    
    # At the top level, we only need to do something different if we're at the
    # start of the repetition signal. This could be optimised by taking off more
    # than one character at a time, but at the cost of code complexity (i.e.
    # take all of the characters until the next "(" and push them onto `output`,
    # rather than doing a character at a time).
    if (l != "(") {
      output$push_back(l)
    } else {
      # As we don't know how many chatacters make up the ([a]x[b]) format, we
      # can just set up a couple more deques to store them.
      lookahead <- deque(c())
      times <- deque(c())
      
      # Inside the `while` condition, we have both the assignment and the
      # condition. We need this as we want to keep consuming characters until
      # the condition breaks and this way is neater than the alternatives.
      while ((l <- i$pop_front()) != "x") {
        lookahead$push_back(l)
      }
      # As the assignment to `l` happens before the loop condition is tested, we
      # end this loop having assigned the "x" to `l`. This means that we can
      # just do nothing with it and the next time we pop, we'll get the next
      # meaningful character.
      
      while ((l <- i$pop_front()) != ")") {
        times$push_back(l)
      }
      # at the end of this loop, `l` will be ")". As before, we don't need this
      # character, so just continuing to pop gives us what we need.
      
      lookahead <- paste(lookahead$get(), collapse = "") |> as.numeric()
      times <- paste(times$get(), collapse = "") |> as.numeric()
      
      to_add <- i$pop_front(n = lookahead)
      
      for (count in 1:times) {
        output$push_back(to_add)
      }
    }
  }
  output$get() |>
    discard(\(x) x == " ") |>
    length()
}


# ---- part 2 ----
#
# Previously, we didn't need to expand deeper than the first level (i.e.
# expansion instructions that were duplicated were not 'true' expansion
# instructions). But in true AoC fashion, part 2 removes this simplification. We
# need to change strategy to avoid making things too much data. Luckily, we
# don't need to know what the expanded data is, just how long it is. And any (1
# depth) expansion of the form "([a]x[b])..." will expand to a * b characters.
# So we just need a tree of these values, and then roll up the tree.

# Assume the group starts with an expansion parameter or has none
parse_group <- function(chars) {
  if (!str_detect(chars, "\\(")) {
    return(str_length(chars))
  }
  
  beginning <- str_extract(chars, "^[A-Z]+\\(") |> str_sub(1, -2) 
  if (is.na(beginning)) beginning <- ""
  n_chars <- str_extract(chars, "(?<=\\()[0-9]+") 
  n_reps <- str_extract(chars, "(?<=x)[0-9]+")
  msg_start <- str_length(beginning) + 3 + str_length(n_chars) + str_length(n_reps) + 1
  msg_end <- msg_start + as.numeric(n_chars) - 1
  
  message <- str_sub(chars, msg_start, msg_end)
  rest <- str_sub(chars, msg_end + 1)
  
  if (!str_detect(message, "\\(")) {
    str_length(beginning) + as.numeric(n_chars) * as.numeric(n_reps) + parse_group(rest)
  } else {
    str_length(beginning) + as.numeric(n_reps) * parse_group(message) + parse_group(rest)
  }
}


part_2 <- function(input) {
  input[[1]] |>
    parse_group()
}


# ---- Results ----
part_1(input) # 110346
part_2(input) # 10774309173

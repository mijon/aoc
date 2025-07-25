# ---- About ----
#
# This is the first of this year that we'll be solving using the Chinese
# Remainder Theorem. This theorem provides a solution to x for the following
# system of congruences:
#
# x = a_1 (mod m_1) x = a_2 (mod m_2) ...etc...
#
# We need to do a little work up front to reconfigure the problem data from
# "ball perspective", where the ball takes time between passing through each
# ball to allow the discs to rotate, to "beam" perspectives, where we offset the
# starting positions such that the discs' holes align all at the same time as if
# we were shining a light through the holes. We're finding a point where they
# all open up at once. That time will be the `x` that solves all the
# congruences and lets us use the CRT.


library(tidyverse)
library(numbers)   # a useful package for all things number theory
INPUT_PATH <- "../input/15_input.txt"

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

# Use a few regexes to pull out the relevant parts
parse_input <- function(input) {
  tibble(x = input) |>
    transmute(
      disc = str_extract(x, "Disc #([0-9])", group = 1) |> as.numeric(),
      n_positions = str_extract(x, "([0-9]+) positions", group = 1) |> as.numeric(),
      start_pos = str_extract(x, "position ([0-9]+)", group = 1) |> as.numeric(),
    )
}

# ---- Working ----

# Here is where we have to manipulate the values we are given in order to use
# the CRT. We have the size of the disc and its starting point, from that we
# need to get a `a` value. This is a value less than the size of the disc that
# we want the solution (`x`) to be equal to modulo the size of the disc. In
# other words, this would be the lowest t that this disc is orientated such that
# that our light beam (not the ball) can pass through it (ignoring all other
# discs).
#
# We can calculate this by asking how long it would take for the disc to get
# back to position 0. If it has n positions and is on position s, then it has (n
# - s) to go. But then we need to offset it by the time it takes the ball to get
# there, i.e. the disc level, so our final `a` value would be the number of
# positions less the starting position less the disc level in the stack. We need
# to do it mod n_position in case the calculation goes negative.
crt_solve <- function(df) {
  df <- df |>
    mutate(
    a = (n_positions - start_pos - disc) %% n_positions, # here
    m = n_positions)
  chinese(df$a, df$m)
}

part_1 <- function(input) {
  parse_input(input) |>
    crt_solve()
}

# Part 2 is just to add another disc. Easy enough, and has no real impact on our
# solution time as {numbers} is well coded.
part_2 <- function(input) {
  parse_input(input) |>
    add_row(disc = 7, n_positions = 11, start_pos = 0) |>
    crt_solve()
}

# ---- Results ----
part_1(input) # 16824
part_2(input) # 3543984

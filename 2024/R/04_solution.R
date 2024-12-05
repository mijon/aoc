library(tidyverse)
INPUT_PATH <- "../input/04_input.txt"

# Okay, so this file doesn't show the pain this day caused. My first thought was
# that we can just generate a character matrix and then crawl through it finding
# 'X's and then search from there in all directions for "XMAS". I thought that
# would be too much effort, and that it would be simpler to smash the input
# together and solve the search using regular expressions. This *would* have
# been simple except for the (literal) edge case of having to avoid counting
# when patterns wrap over the edge of the grid.
#
#
# If you have a regex like "X.{n_cols}M.{n_cols}A.{n_cols}S", you'll feel smug
# that you've solved it so simply, but then sad because that won't work.
# 
#  `n_cols`
# <------>
# .X...... }
# ..M..... } This gets picked up
# ...A.... } which is fine
# ....S... }
# ........
# ........
# ......X. }
# .......M } This also gets picked up
# ........ } which is not fine
# A....... }
# .S...... }
#
# So you have to add an extra "barrier" character to allow for the new lines and
# then make sure to disallow instances where there's a barrier character in a
# place that it should not be. I chose "@" as it was easier than writing regexes
# that handled "\n" characters correctly.
#
# But once you figure out the regexes, there's not that much more to this
# solution and part 2 is just looking for slightly different things, so it drops
# out fairly easily as well.


# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

input_to_mat <- function(input) {
  str_split(input, "", simplify = TRUE)
}

input_to_flat <- function(input) {
  paste(input, collapse = "@")
}

# ---- Workings ----
# Given some collection of regexes, this function makes a new function that
# counts matches.
regex_counter <- function(regexes) {
  function(input) {
    flat <- input_to_flat(input)
    num_cols <- ncol(input_to_mat(input))
    
    # This allows us to generate regexes that respond to different grid sizes
    # (not stricly necessary, but I didn't want to have two *slightly* different
    # regex collections for the example data and the real data).
    g <- function(s) {
      glue::glue(s, .open = "|", .close = "|") 
    }
    
    regexes <- map_chr(regexes, g)
    
    str_count(flat, regexes) |>
      sum()
  }
}

# taking the third as an example, here we're checking for XMAS written
# diagonally from top left to bottom right, so we check for an X, skip forward
# `num_col` number of any characters, then ensure the next character isn't our
# barrier character, then check for an A, then continue up to the S.
part_1 <- regex_counter(c(
  "XMAS", # horizontal forward
  "SAMX", # horizontal backward
  "(?=X.{|num_cols|}[^@]M.{|num_cols|}[^@]A.{|num_cols|}[^@]S)", # diag down right forward
  "(?=S.{|num_cols|}[^@]A.{|num_cols|}[^@]M.{|num_cols|}[^@]X)",#  diag down right backward
  "(?=X.{|num_cols - 1|}M[^@].{|num_cols - 2|}A[^@].{|num_cols - 2|}S)",# diag down left forward
  "(?=S.{|num_cols - 1|}A[^@].{|num_cols - 2|}M[^@].{|num_cols - 2|}X)",# diag down left backward
  "(?=X.{|num_cols|}M.{|num_cols|}A.{|num_cols|}S)", # direct down forward
  "(?=S.{|num_cols|}A.{|num_cols|}M.{|num_cols|}X)"  # direct down backward
))

part_2 <- regex_counter(c(
  "(?=M[^@]S.{|num_cols - 1|}A.{|num_cols - 1|}M[^@]S)",
  "(?=S[^@]S.{|num_cols - 1|}A.{|num_cols - 1|}M[^@]M)",
  "(?=M[^@]M.{|num_cols - 1|}A.{|num_cols - 1|}S[^@]S)",
  "(?=S[^@]M.{|num_cols - 1|}A.{|num_cols - 1|}S[^@]M)"
))

# ---- Results ----
part_1(input) # 2593
part_2(input) # 1950

library(tidyverse)
options(digits = 22) # big numbers, so suppress R's default use of scientific notation
INPUT_PATH <- "../input/07_input.txt"

# This is a slow solution but I quite like it. Essentially all we're doing is
# recursively constructing our calculation as we go along by trying all
# two/three operators on the first two elements of the test vector, replacing
# the first two with the running value of our calculation until we use them all
# up. If the final result is the target, we output TRUE, if not, FALSE and crawl
# back up the call stack. 


# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

parse_input <- function(input) {
  tibble(input = input) |>
    separate(input, into = c("target", "vec"), sep = ": ") |>
    mutate(target = as.numeric(target),
           vec = str_split(vec, " ") |> map(as.numeric))
}

# ---- Working ----
# The form of both parts is the same, the only difference is how we check
# whether the text vector can construct the target, so we can pull out all the
# scaffolding for handling the final total check into a function factory.
make_part <- function(checker) {
  function(input) {
    parse_input(input) |>
      mutate(test = map2_lgl(target, vec, checker, .progress = TRUE)) |>
      filter(test) |>
      pull(target) |>
      sum()
  }
}

# BFS through the combinations of operators for part 1
check_vec <- function(target, vec, result = FALSE) {
  if (length(vec) <= 1) {
    return(vec == target)
  }
  
  first_two <- head(vec, 2)
  rst <- tail(vec, -2)
  
  add <- sum(first_two)
  mul <- prod(first_two)
  
  result || check_vec(target, c(add, rst)) || check_vec(target, c(mul, rst))
}


# part 2 just introduces another operator, so a small tweak to the checker is
# required.
check_vec_2 <- function(target, vec, result = FALSE) {
  if (length(vec) <= 1) {
    return(vec == target)
  }
  
  first_two <- head(vec, 2)
  rst <- tail(vec, -2)
  
  add <- sum(first_two)
  mul <- prod(first_two)
  cat <- as.numeric(paste(first_two, collapse = ""))
  
  result ||
    check_vec_2(target, c(add, rst)) ||
    check_vec_2(target, c(mul, rst)) ||
    check_vec_2(target, c(cat, rst))
}

part_1 <- make_part(check_vec)
part_2 <- make_part(check_vec_2)

# ---- Results ----
part_1(input) # 1298300076754
part_2(input) # 248427118972289


# --- Extension ---
#
# Suppose instead of adding one single extra operator in part 2, what if we had
# to add ten more. The current method would require much copying and pasting. A
# nicer method would be to accept a list of operators of any length and then to
# or reduce the results:
#
# However, in testing, this resulted in an even slower run time, and is only
# presented here because I think it's pretty.
make_part_general <- function(ops) {
  checker <- make_checker(ops)
  function(input) {
    parse_input(input) |>
      mutate(test = map2_lgl(target, vec, checker, .progress = TRUE)) |>
      filter(test) |>
      pull(target) |>
      sum()
  }
}

make_checker <- function(ops) {
  check_vec <- function(target, vec, result = FALSE) {
    if (length(vec) <= 1) {
      return(vec == target)
    }
    
    first_two <- head(vec, 2)
    rst <- tail(vec, -2)
    
    calc_results <- map_dbl(ops, \(op) reduce(first_two, op))
    reduce(map_lgl(calc_results, \(r) check_vec(target, c(r, rst))), `||`, .init = result)
  }
}

`%|%` <- function(l, r) {
  as.numeric(paste0(l, r, collapse = ""))
}

part_1_general <- make_part_general(list(`+`, `*`))
part_2_general <- make_part_general(list(`+`, `*`, `%|%`))

part_1_general(input)
part_2_general(input)

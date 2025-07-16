# ---- About ----
#
# For this puzzle, we are given regions of excluded numbers (that might
# overlap), and we need to identify the places where there are no exclusions.
#
# This is very similar to counting the nesting of brackets in an expression, so
# we just need to know when the nesting depth hits zero, then the allowed region
# is from there up to the next "open bracket".
library(tidyverse)
INPUT_PATH <- "../input/20_input.txt"


# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

# We go from x-y to a table with a column of x and y, and another column showing
# +1 for the left hand side of the range, and -1 for the right. From that, we
# can add extra information like `diff` being the difference between subsequent
# boundary points (if we close on n, but open on n+1, since we're dealing with
# integers, there's no free space between these), and `coverage` being the width
# of the area before we change nesting depth.
parse_input <- function(input) {
  tibble(a = input) |>
    separate(a, into = c("+1", "-1"),
             sep = "-") |>
    pivot_longer(everything(), names_to = "type", values_to = "value") |>
    mutate(across(everything(), as.numeric)) |>
    arrange(value) |>
    mutate(cumsum = cumsum(type),
           diff = lead(value) - value,
           coverage = diff - 1)
}

# ---- Working ----

# Part 1 then becomes just finding the first point where the nesting depth hits
# zero, *and* we have open space after it, i.e. the distance to the next
# boundary point is more than one.
part_1 <- function(input) {
  value <- parse_input(input) |>
    filter(cumsum == 0, diff > 1) |>
    slice(1) |>
    pull(value)
  
  # add one because we want the first open point, and `value` is the value of
  # the closing boundary
  value + 1 
}

# Part 2 is simply to count the widths of the regions where the nesting depth is
# zero.
part_2 <- function(input) {
  parse_input(input) |>
    filter(cumsum == 0) |>
    pull(coverage) |>
    sum(na.rm = TRUE)
}


# ---- Results ----
part_1(input) # 32259706
part_2(input) # 113

library(tidyverse)
INPUT_PATH <- "../input/05_input.txt"

# Today's puzzle is really about sorting.
#
# I had wanted to extract the ordering from the first part of the input to get a
# global ordering, from there it would be simple to convert the updates into R's
# `factor` data type which can represent notions of ordering on non-numeric data
# (which this is, as the page numbers are just labels rather than integers).
# This works for the example data.
#
# It does not work for the main data.
#
# This is because the example data satisfies the transitivity requirement, i.e.
# if a < b and b < c, then a < c. However, the actual data has cycles. This
# tripped me up a bit at first. But we can just subset the actual data (split
# the cycle, in effect) for each individual update and handle it as we planned
# all along.
#
# The presence of a cycle also is why topological sort wouldn't have worked,
# which would have been a nice way to solve the problem.

# ---- input reading and parsing ----
input <- readr::read_file(INPUT_PATH)

# ---- quick sort ----
# There are many sorts prepackages, but why not implement our own quicksort.
# Indeed, applying this quicksort is ultimately the slowest part of the whole
# process, so optimisations should start here.
#
# We accept custom less than (or equal) and greater than functions here, so we
# can be general. We'll need them later anyway, as the `lt` and `gt` functions
# *have* to be custom for each update as each update requires a different snip
# to be made in the cycle.
quicksort <- function(v, lt, gt) {
  if (length(v) <= 1) {
    return(v)
  }
  
  p <- v[1]
  rst <- tail(v, -1)
  l <- keep(rst, \(x) lt(x, p))
  r <- keep(rst, \(x) gt(x, p))
  c(quicksort(l, lt, gt), p, quicksort(r, lt, gt))
}

# ---- working ----
# There's a native `is.unsorted` in R, but no `is.sorted`... I wonder if this
# speaks to the authors' expectations of the sorts of data they were expecting
# to receive...
is_sorted <- negate(is.unsorted)

setup_updates <- function(ordering, updates) {
  updates |> 
    mutate(updates = map(updates, convert_to_factors, ordering),
           check = map_lgl(updates, is_sorted))
}

# Converting to factors is where we snip the cycle, calculate the correct
# ordering and then change our numerical vector of updates into an ordered
# factor of updates.
convert_to_factors <- function(updates, ordering) {
  ordering <- ordering |>
    filter(L %in% updates | R %in% updates)
  
  # Given an ordering subset, generate custom comparison functions
  less_than <- function(x, y) {
    paste(x, y, sep = "|") %in% ordering$pair | (x == y)
  }
  
  greater_than <- function(x, y) {
    paste(y, x, sep = "|") %in% ordering$pair
  }
  
  factor_orders <- quicksort(updates, less_than, greater_than)
  factor(updates, levels = factor_orders, ordered = TRUE)
}


parse_input <- function(input) { # probably can tidy
  input <- input |>
    str_split("\n\n") |>
    first()
  
  ordering <- tibble(pair = input[[1]] |> str_split("\n") |> first()) |>
    separate(pair, into = c("L", "R"), sep = "\\|", remove = FALSE)
  
  updates <- tibble(updates = input[[2]] |>
                      str_split("\n") |> first() |>
                      head(n=-1) |>
                      map(str_split, ",") |>
                      map(first))
  
  setup_updates(ordering, updates)
}

# This is R, so why not find the middle as the median of 1:n?
middle_element <- function(v) {
  v[median(seq_along(v))] |>
    as.character() |>
    as.numeric()
}

# Both parts need to sum the mids, so let's pull it out into a function.
sum_mids <- function(df) {
  df |>
    mutate(mid = map_dbl(updates, middle_element)) |>
    pull(mid) |>
    sum()
}

part_1 <- function(input) {
  input |>
    filter(check) |>
    sum_mids()
}

part_2 <- function(input) {
  input |>
    filter(!check) |>
    mutate(updates = map(updates, sort)) |>
    sum_mids()
}

# ---- Results ----
parsed <- parse_input(input)
part_1(parsed) # 5275
part_2(parsed) # 6191

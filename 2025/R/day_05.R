library(tidyverse)

input <- read_file("../input/input_05.txt")

# A bit more parsing code than usual today. We split the input into the ranges
# and the ingredient ID lists. It's advantageous to pre-sort the input.
parse_input <- function(input) {
  initial_parse <- input |>
    str_trim() |>
    str_split("\n\n") |>
    first() |>
    map(str_split, "\n") |>
    map(first)

  ranges <- initial_parse[1] |>
    first() |>
    str_split("-") |>
    # first() |>
    map(as.numeric) |>
    map(set_names, c("start", "stop"))

  starts <- map_dbl(ranges, "start")
  ranges <- ranges |> sort_by(starts)

  ingredients <- initial_parse[2] |>
    first() |>
    as.numeric() |>
    sort()

  list(ranges = ranges,
       ingredients = ingredients)
}

# ---- part 1 ----
# Because we've sorted the inputs, we can run sequentially through each list
# once with guarantees that we won't need to back track and look at a range
# we've already looked at. As such, I think this algorithm is O(n) (if we ignore
# the computational cost of sorting the input).
part_1 <- function(input) {
  input <- parse_input(input)
  ranges <- input$ranges
  ingredients <- input$ingredients

  cur_range_num <- 1
  cur_ingredient_num <- 1
  total_fresh <- 0

  keep_going <- TRUE

  # We handle the following situations
  # - ingredient ID below the current range's start point => ID can be discarded
  # - ID in the range => note ID is fresh and move on to the next ID
  # - ID is beyond the end of the current range => drop this range and try the next
  while(keep_going) {
    current_ingredient <- ingredients[cur_ingredient_num]
    if (current_ingredient < ranges[[cur_range_num]][["start"]]) {
      cur_ingredient_num <- cur_ingredient_num + 1
    } else if (current_ingredient <= ranges[[cur_range_num]][["stop"]]) {
      total_fresh <- total_fresh + 1
      cur_ingredient_num <- cur_ingredient_num + 1
    } else if (current_ingredient > ranges[[cur_range_num]][["stop"]]) {
      cur_range_num <- cur_range_num + 1
    }

    if (cur_ingredient_num == length(ingredients)) {
      keep_going <- FALSE
    }
  }
  total_fresh
}

# ---- Part 2 ----
# For part 2, we take each range in turn (again, because we have pre-sorted
# these, the rest of the algorithm is O(n)). We look at the ranges in sequence,
# so if the current range doesn't overlap with the next, then we can just add
# its width to the total, if it does overlap we can make a new current range
# that is the union of the two ranges.
part_2 <- function(input) {
  ranges <- parse_input(input)$ranges

  total_range_width <- 0
  cur_range <- c(start = 1, stop = 0)

  for (i in seq_along(ranges)) {
    next_range <- ranges[[i]]

    if (next_range[["start"]] > cur_range[["stop"]]) {
      total_range_width <- total_range_width + (cur_range[["stop"]] - cur_range[["start"]] + 1)
      cur_range <- next_range
    } else {
      cur_range <- c(start = min(cur_range[["start"]], next_range[["start"]]),
                     stop = max(cur_range[["stop"]], next_range[["stop"]]))
    }
  }
  (total_range_width + (cur_range[["stop"]] - cur_range[["start"]] + 1)) |>
    scales::comma(big.mark = "")
}

# ---- Results ----
part_1(input) # 707
part_2(input) # 361615643045059

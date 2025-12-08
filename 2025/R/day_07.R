library(tidyverse)

input <- read_lines("../input/input_07.txt")

parse_input <- function(input) {
  input <- input |>
    map(str_split, "") |>
    map(first) |>
    map(\(x) matrix(x, ncol = length(x))) |>
    matrix()
  do.call(rbind, input)
}

# As for day 4, we want to avoid handling edge cases by simply padding the matrix.
pad_mat <- function(mat, width, with) {
  old_dims <- dim(mat)
  new_mat <- matrix(with,
                    nrow = old_dims[1] + 2 * width,
                    ncol = old_dims[2] + 2 * width)
  new_mat[2:(old_dims[1] + 1), 2:(old_dims[2] + 1)] <- mat
  new_mat
}


part_1 <- function(input) {
  input <- parse_input(input) |>
    pad_mat(1, ".")

  n_splits <- 0

  for (row in 3:(nrow(input) - 1)) { # start at 3 to skip first (actual) layer
    for (col in 2:(ncol(input) - 1)) {
      cur_cell <- input[row,col]
      parent <- input[row-1, col]

      # propagation
      if (cur_cell == "." && parent %in% c("S", "|")) {
        input[row, col] <- "|"
      }

      # splitting
      if (cur_cell == "^" && parent == "|") {
        n_splits <- n_splits + 1
        input[row, col - 1] <- "|"
        input[row, col + 1] <- "|"
      }
    }
  }
  n_splits
}

# ---- part 2 ----
# If we generate all the paths, things will get out of hand quickly. Another way
# is to start at the bottom and build up a running totals of "if you get to this
# point, what is the total number of paths from this point?" Starting at the
# bottom means that the question is easy, it's 2 for any split and 1 for any
# part where there's no splitter.
#
# Build up from there, any point where there's a splitter, we replace the
# splitter with the sum of the values from either side. Where there's a missing
# splitter, we simply scan down the matrix to find the next available number, if
# there isn't one, then the value is 1.

part_2 <- function(input) {
  chars <- parse_input(input)
  vals <- matrix(0, nrow = nrow(chars), ncol = ncol(chars))

  get_tree_edges <- function(row) {
    list(left = (nrow(chars) + 3 - row)/2,
      right = (nrow(chars) -3 + row)/2)
  }

  # bottom line is a special case as the "^" here can just be replaced with '2'
  row <- nrow(chars) - 1
  extents <- get_tree_edges(row)
  for (col in seq(extents$left, extents$right, 2)) {
    if (chars[row, col] == "^") {
      vals[row,col] <- 2
    } else {
      vals[row, col] <- 1
    }
  }

  # Then we work our way up the tower, one row at a time
  for (row in seq((nrow(chars) - 3), 3, -2)) {
    extents <- get_tree_edges(row)
    for (col in seq(extents$left, extents$right, 2)) {

      if (chars[row, col] == "^") {
        # when we get to a splitter, we add the values for each leg
        vals[row, col] <- vals[row + 2, col - 1] + vals[row + 2, col + 1]
      } else {
        # when we don't find a splitter we look down the tower and bring up the
        # first number we see, or if we see no numbers then we just have 1 route
        # down to the bottom from here.
        scan_down <- vals[row:nrow(vals), col]
        val_to_input <- coalesce(scan_down[which(scan_down != 0)[1]], 1)
        vals[row, col] <- val_to_input
      }
    }
  }
  vals[3, (1 + ncol(vals)) / 2] |> scales::comma(big.mark = "")
}

# ---- results ----
part_1(input) # 1553
part_2(input) # 15811946526915

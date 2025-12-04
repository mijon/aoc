library(tidyverse)

# ---- Inputs and parsing ----
input <- read_lines("../input/input_04.txt")

parse_input <- function(input) {
  input <- input |>
    map(str_split, "") |>
    map(first) |>
    map(\(x) matrix(x, ncol = length(x))) |>
    matrix()
  do.call(rbind, input)
}

# One way to handle the edge cases is simply to wrap the matrix in a padding of
# "empty" values, then run over the shape of the original, that way our testing
# element doesn't need to handle the edges and corners as special cases.
pad_mat <- function(mat, width, with) {
  old_dims <- dim(mat)
  new_mat <- matrix(with,
                    nrow = old_dims[1] + 2 * width,
                    ncol = old_dims[2] + 2 * width)
  new_mat[2:(old_dims[1] + 1), 2:(old_dims[2] + 1)] <- mat
  new_mat
}

process <- function(mat) {
  dims <- dim(mat)

  padded <- pad_mat(mat, width = 1, with = ".")
  output_mat <- matrix(".", nrow = nrow(padded), ncol = ncol(padded))

  count_accessibile <- 0
  for (row in 1+(1:dims[1])) {
    for (col in 1+(1:dims[2])) {
      focus_mat <- padded[(row-1):(row+1),(col-1):(col+1)]
      nearby_occupied <- (which(focus_mat == "@") |> length()) - 1
      if (padded[row, col] == "@" && nearby_occupied < 4) {
        output_mat[row, col] <- "x"
        count_accessibile <- count_accessibile + 1
      } else {
        output_mat[row, col] <- padded[row, col]
      }
    }
  }
  list(new_mat = output_mat,
       count_accessible = count_accessibile)
}

part_1 <- function(input) {
  mat <- parse_input(input)
  process(mat)$count_accessible
}


# ---- part 2 ----
part_2 <- function(input) {
  mat <- parse_input(input)

  keep_going <- TRUE
  while (keep_going) {
    processed <- process(mat)

    if (processed$count_accessible == 0) {
      keep_going <- FALSE
    }
    mat <- processed$new_mat
  }
  sum(mat == "x")
}


# ---- results ----

part_1(input) # 1389
part_2(input) # 9000

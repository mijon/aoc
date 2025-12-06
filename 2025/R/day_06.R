library(tidyverse)

input <- read_lines("../input/input_06.txt")

# ---- part 1 ----
part_1 <- function(input) {
  nums <- head(input, -1) |> str_split(" +") |> map(as.numeric)
  ops <- tail(input, 1) |> str_split(" +") |> first()

  # rotate the lists
  nums <- map(1:length(nums[[1]]), \(n) map_dbl(nums, n))

  sums <- map_dbl(nums, sum)
  prods <- map_dbl(nums, prod)

  sum(sums * (ops == "+")  + prods * (ops == "*")) |>
    scales::comma(big.mark = "")
}

# ---- part  2 ----
# The basic process is the same as part 1 but for the manipulation of the
# numbers.
part_2 <- function(input) {
  chars <- head(str_split(input, ""), -1) |> map(\(x) c(x, " "))
  ops <- tail(input, 1) |> str_split(" +") |> first()

  space_column_indices <- map(1:length(chars[[1]]), \(n) map_chr(chars, n)) |>
    map_lgl(\(x) all(x == " ")) |> which()
  space_diffs <- c(space_column_indices[1], diff(space_column_indices))

  blocks <- map(chars, partition, space_diffs) |>
    map(\(x) map(x, head, -1)) # strip off the trailing space

  # rotate the blocks
  blocks <- map(1:length(blocks[[1]]), \(n) map(blocks, n))

  # collapse to nums
  blocks <- blocks |>
    map(\(b) map(1:length(b[[1]]),
                 \(n) map_chr(b, n)) |>
          map_chr(paste, collapse = "") |>
          as.numeric())

  sums <- map_dbl(blocks, sum)
  prods <- map_dbl(blocks, prod)

  sum(sums * (ops == "+")  + prods * (ops == "*")) |>
    scales::comma(big.mark = "")
}

partition <- function(v, runs) {
  split(v, rep.int(1:length(runs), runs)) |>
    unname()
}



# ---- results ----
part_1(input) #  5733696195703
part_2(input) # 10951882745757

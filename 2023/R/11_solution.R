library(tidyverse)

input <- read_lines("../input/11_input.txt")

# ---- part 1 ----
lines_to_mat <- function(ls) {
  ls |>
    str_split("") |>
    stringi::stri_list2matrix(byrow = TRUE)
}

solve <- function(input, gap_size) {
  mat <- lines_to_mat(input)
  row_insert <- map_lgl(1:nrow(mat), \(r) all(mat[r,] == ".")) |> which()
  col_insert <- map_lgl(1:ncol(mat), \(c) all(mat[,c] == ".")) |> which()
  
  pairs <- which(mat == "#",
                 arr.ind = TRUE) |>
    as_tibble() |>
    arrange(row, col) |>
    mutate(id = 1:n())
  tmp <- crossing(left = pairs, right = pairs) |>
    filter(left$id < right$id) |>
    mutate(
      row_gaps_crossed = map2_dbl(
        left$row,
        right$row,
        \(l, r) {row_insert[row_insert > min(l,r) & row_insert < max(l, r)] |> length()}),
      col_gaps_crossed = map2_dbl(
        left$col,
        right$col,
        \(l, r) {col_insert[col_insert > min(l, r) & col_insert < max(l, r)] |> length()}),
      dist = abs(left$row - right$row) + abs(left$col - right$col) +
        gap_size * row_gaps_crossed + gap_size * col_gaps_crossed) 
  
  tmp |>
    pull(dist) |>
    sum()
}

part_1 <- function(input) {
  solve(input, 1)
}


# ---- part 2 ----

part_2 <- function(input) {
  solve(input, 1e6-1)
}

# ---- evaluations ---- 
part_1(input) # 10313550
part_2(input) # 611998089572

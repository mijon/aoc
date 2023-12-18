library(tidyverse)

test_input1 <- c(
  "#.##..##.",
  "..#.##.#.",
  "##......#",
  "##......#",
  "..#.##.#.",
  "..##..##.",
  "#.#.##.#.")

test_input2 <- c(
  "#...##..#",
  "#....#..#",
  "..##..###",
  "#####.##.",
  "#####.##.",
  "..##..###",
  "#....#..#")

input <- read_lines("../input/13_input.txt")

parse_input <- function(input) {
  patterns <- input |>
    split(cumsum(input == "")) |>
    map(\(x) discard(x, \(y) y == ""))
  
  tibble(pattern = patterns)
}


# ---- part 1 ----
tmp_test <- c("ABC",
              "XYZ")

flip_input <- function(input) {
  map_chr(1:str_length(input[[1]]),
          \(n) {map_chr(input,
                        \(x) {substr(x, n, n)}) |> paste(collapse = "")})
}

rows_above_join <- function(input) {
  which(input == lead(input))
}

confirm_symmetry <- function(input, n) {
  left <- input[1:n] |> rev()
  right <- input[(n + 1):length(input)]
  
  min_len <- min(length(left), length(right))
  
  all(left[1:min_len] == right[1:min_len])
}

count_vertical_symmetry <- function(pattern) {
  candidates <- rows_above_join(pattern) 
  checks <- map_lgl(candidates, \(n) confirm_symmetry(pattern, n))
  
  confirmed_candidates <- candidates[which(checks)]
  if (length(confirmed_candidates) == 1) {
    confirmed_candidates
  } else if (length(confirmed_candidates) > 1) {
    stop("too many candidates!")
  } else {
    0
  }
}

count_horizontal_symmetry <- function(pattern) {
  pattern <- flip_input(pattern) 
  count_vertical_symmetry(pattern)
}

part_1 <- function(input) {
  parse_input(input) |>
    mutate(verts = map_dbl(pattern, count_vertical_symmetry),
           horiz = map_dbl(pattern, count_horizontal_symmetry),
           score = 100 * verts + horiz) |>
    pull(score) |>
    sum()
}


# ---- part 2 ----
find_potential_smudge_pairs <- function(pattern) {
  dist <- adist(pattern)
  which(dist * upper.tri(dist) == 1, arr.ind = TRUE) |>
    as_tibble()
}

flip_smudge <- function(r, c, pattern) {
  tmp <- str_split(pattern[r], "")[[1]]
  curr_char <- tmp[[c]]
  if(curr_char == ".") {
    new_char <- "#"
  } else {
    new_char <- "."
  }
  tmp[[c]] <- new_char
  pattern[r] <- paste(tmp, collapse = "")
  pattern
}

smudge_pair_to_pos <- function(l, r, input) {
  input[c(l,r)] |>
    str_split("") |>
    purrr::transpose() |>
    map_lgl(\(x) {x[[1]] != x[[2]]}) |>
    which()
}

count_vertical_symmetry2 <- function(pattern) {
  potential_pairs <- find_potential_smudge_pairs(pattern)
  
  pot_cols <- map2_dbl(potential_pairs$row,
                       potential_pairs$col,
                       smudge_pair_to_pos,
                       pattern)
  potential_smudges <- tibble(row = potential_pairs$row,
                              col = pot_cols) |>
    distinct()
  
  map2(potential_smudges$row, potential_smudges$col,
       flip_smudge,
       pattern) |>
    map_dbl(count_vertical_symmetry) |>
    discard(\(x) {x == 0})
}

count_horizontal_symmetry2 <- function(pattern) {
  pattern <- flip_input(pattern) 
  count_vertical_symmetry2(pattern)
}

part_2 <- function(input) {
  parse_input(input) |>
    mutate(verts = map_dbl(pattern, count_vertical_symmetry2),
           horiz = map_dbl(pattern, count_horizontal_symmetry2),
           score = 100 * verts + horiz) |>
    pull(score) |>
    sum()
}

# ---- evaluations ----
part_1(input) # 27202
part_2(input)

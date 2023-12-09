library(tidyverse)

input <- read_lines("../input/03_input.txt")

# Convert a list of character vectors into a matrix of single characters
lines_to_mat <- function(ls) {
  ls |>
    str_split("") |>
    stringi::stri_list2matrix(byrow = TRUE)
}

# given a regex pattern, find the start and stop position, then tidy the result
locate_pattern <- function(input_strings, pattern) {
  input_strings |>
    str_locate_all(pattern) |>
    map(as_tibble) |>
    bind_rows(.id = "row") |>
    mutate(row = as.numeric(row))
}

# Extract the part number from the input given the location table
addorn_values <- function(loc_table, input) {
  loc_table |>
    mutate(value = pmap_chr(list(r = row,
                                 s = start,
                                 e = end),
                            \(r,s,e) {input[r] |> substr(s,e)}),
           value = as.numeric(value))
}

# add to the df the boundary around the part number (i.e. a one-unit border)
addorn_check_extent <- function(df, row_max, col_max) {
  df |>
    mutate(check_row_min = pmax(1, row - 1),
           check_row_max = pmin(row_max, row + 1),
           check_col_min = pmax(1, start - 1),
           check_col_max = pmin(col_max, end + 1))
}

# Given a matrix of characters, check whether any are symbols
has_symbol <- function(mat) {
  any(str_detect(as.character(mat), "[^0-9.]"))
}

# given a table of locations to search and a matrix matching the input,
# calculate the sub matrix where we need to search and then apply the
# `check_fun`.
addorn_has_symbol <- function(df, mat, check_fun) {
  df |> 
    rowwise() |>
    mutate(sub_mat = list(mat[check_row_min:check_row_max, check_col_min:check_col_max]),
           has_symbol = check_fun(sub_mat)) |>
    ungroup()
}

# use `locate_numbers` to find the extents of the strings,
# then for each, generate a mask of chars in the matrix to test
# then extract those chars and see if any contain symbols.
part_1 <- function(input) {
  input_mat <- lines_to_mat(input)
  row_max = nrow(input_mat)
  col_max = ncol(input_mat)
  
  reference_table <- locate_pattern(input, "[0-9]+") |>
    addorn_values(input) |>
    addorn_check_extent(row_max, col_max) |>
    addorn_has_symbol(input_mat, has_symbol)
  
  reference_table |>
    filter(has_symbol) |>
    pull(value) |>
    sum()
}


# --- part 2 ----
# We need to check for gears now, so we can make this function so we can reuse
# `addorn_has_symbol()` later on.
has_gear <- function(mat) {
  any(str_detect(as.character(mat), "[*]"))
}

# We need to be able to filter candidate part numbers down to only consider ones
# where their checking extent (unit boundary) contains a gear
filter_contains <- function(test_row, test_col, df) {
  df |>
    filter(check_row_max >= test_row &
             check_row_min <= test_row &
             check_col_max >= test_col &
             check_col_min <= test_col)
}

# We find the locations of all the gears, then the boundaries of all the part
# numbers. We then filter the part numbers to only include pairs that share the
# same gear.
part_2 <- function(input) {
  input_mat <- lines_to_mat(input)
  row_max = nrow(input_mat)
  col_max = ncol(input_mat)
  
  gears <- locate_pattern(input, "[*]")
  numbers_near_gears <- locate_pattern(input, "[0-9]+") |>
    addorn_values(input) |>
    addorn_check_extent(row_max, col_max) |>
    addorn_has_symbol(input_mat, has_gear) |>
    filter(has_symbol)
  
  gears |>
    mutate(candidates = map2(row, start, filter_contains, numbers_near_gears),
           nrow_candidates = map_dbl(candidates, nrow)) |>
    filter(nrow_candidates == 2) |>
    mutate(gear_ratio = map_dbl(candidates, \(df) df$value |> prod())) |>
    pull(gear_ratio) |>
    sum()

}

# ---- running ----
part_1(input) # 527369
part_2(input) # 73074886

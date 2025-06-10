# ---- About ----
#
# We can do a lot of this in data frames. Most of today's puzzle is all about
# reshaping a block of data; something that R is reasonably good at doing. I'm
# sure there's a very neat array based approach, probably a very neat data.table
# approach, but I will use a more verbose (but hopefully clearer) tidyverse
# method.

library(tidyverse)
INPUT_PATH <- "../input/03_input.txt"

# ---- input reading and parsing ----
input <- readr::read_table(INPUT_PATH,
                           col_names = c("a", "b", "c"))

# ---- Part 1 ----
part_1 <- function(input) {
  input |>
    mutate(test = a + b > c &
             a + c > b &
             b + c > a) |>
    filter(test) |>
    nrow()
}

# ---- Part 2 ----
# Now instead of taking each row of the table, we need to take blocks of three
# values in a column (for all rows, and all three columns of the original
# dataset). If we just reshape the data into the new format, then we can feed it
# into the part_1 function to get the answer.
re_label_input <- function(df) {
  df <- df |>
    dplyr::mutate(group = rep(1:(nrow(df)/3), each = 3)) |>
    tidyr::pivot_longer(cols = -group, names_to = "col") |>
    dplyr::arrange(group, col) 
  df |>
    dplyr::mutate(header = rep(c("a", "b", "c"), nrow(df)/3)) |>
    tidyr::unite(rowlabel, group, col) |>
    tidyr::pivot_wider(names_from = "header")
}

part_2 <- function(input) {
  input |>
    re_label_input() |>
    part_1()
}

# ---- Results ----
part_1(input) # 869
part_2(input) # 1544

# This day is *screaming* for an APL solution.

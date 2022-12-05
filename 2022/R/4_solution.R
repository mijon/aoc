library(tidyverse)

example_data <- c("2-4,6-8",
                  "2-3,4-5",
                  "5-7,7-9",
                  "2-8,3-7",
                  "6-6,4-6",
                  "2-6,4-8")

# part 1

get_edges <- function(range) {
  str_split(range, "-") %>%
    map(as.numeric)
}

firsts <- function(x) map_dbl(x, 1)
seconds <- function(x) map_dbl(x, 2)
  
is_within <- function(r1, r2) {
  r1 <- get_edges(r1)
  r2 <- get_edges(r2)
  
  firsts(r1) >= firsts(r2) & seconds(r1) <= seconds(r2)
}

check_contains <- function(df) {
  df %>%
    mutate(is_contained = is_within(left, right) | is_within(right, left))
}

part_1 <- function() {
  read_csv("../input/4_input.txt",
           col_names = c("left", "right")) %>%
    check_contains() %>%
    pull(is_contained) %>%
    sum()
}

# part 2

is_overlap <- function(r1, r2) {
  r1 <- get_edges(r1)
  r2 <- get_edges(r2)
  
  (seconds(r1) >= firsts(r2) & firsts(r1) <= seconds(r2)) | (seconds(r2) >= firsts(r1) & firsts(r2) <= seconds(r1))
}

part_2 <- function() {
  read_csv("../input/4_input.txt",
           col_names = c("left", "right")) %>%
    mutate(overlaps = is_overlap(left, right)) %>%
    pull(overlaps) %>%
    sum()
}

part_1() # 588
part_2() # 911
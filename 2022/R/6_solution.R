library(tidyverse)
library(slider)

input <- read_lines("../input/6_input.txt")
# part 1
char_to_list <- function(v) {
  str_split(v, "")[[1]]
}

length_different <- function(v) {
  length(reduce(v, union))
}

finder <- function(unique_length) {
  function(input) {
    uniques <- slide_dbl(char_to_list(input),
                         length_different,
                         .after = unique_length - 1,
                         .complete = TRUE)
    which(uniques == (unique_length))[[1]] + unique_length - 1
  }
}

part_1 <- finder(4)
part_2 <- finder(14)

part_1(input) # 1093
part_2(input) # 3534

library(tidyverse)

# part 1

split_compartment <- function(string) {
  str_len <- str_length(string)/2
  
  list(
    left = str_sub(string, 1, str_len) %>%
      str_extract_all("[a-zA-Z]") %>%
      `[[`(1),
    right = str_sub(string, str_len + 1, -1) %>%
      str_extract_all("[a-zA-Z]") %>%
      `[[`(1)
  )
}

find_intersection <- function(bag) {
  intersect(bag$left, bag$right)
}

calc_priority <- function(c) {
  if (str_detect(c, "[a-z]")) {
    offset <- 96
  } else {
    offset <- 38
  }
  utf8ToInt(c) - offset
}


part_1 <- function() {
  read_lines("../input/3_input.txt") %>%
    map(split_compartment) %>%
    map(find_intersection) %>%
    map(calc_priority) %>%
    reduce(sum)
}

# part 2

divide_into_groups <- function(bags) {
  tibble(bag = bags) %>%
    mutate(test = rep(1:(nrow(.)/3), each = 3)) %>%
    group_by(test) %>%
    nest() %>%
    pull(data) %>%
    map(pull, bag)
}

find_shared_item <- function(group) {
  group %>%
    str_extract_all("[a-zA-Z]") %>%
    reduce(intersect)
}

part_2 <- function() {
  read_lines("../input/3_input.txt") %>%
    divide_into_groups() %>%
    map(find_shared_item) %>%
    map(calc_priority) %>%
    reduce(sum)
}


part_1() # 8401
part_2() # 2641

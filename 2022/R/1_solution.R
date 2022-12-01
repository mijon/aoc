library(tidyverse)

read_calories <- function(path) {
  read_file(path) %>%
    str_split("\n\n") %>%
    map(str_split, "\n") %>%
    map(~map(.x, as.numeric)) %>%
    map(~map(.x, sum, na.rm = TRUE)) %>% 
    flatten() 
}
  
part_1 <- function(path) {
  read_calories(path) %>%
    reduce(max)
}

part_2 <- function(path) {
  read_calories("../input/1_input.txt") %>%
    as.numeric() %>%
    sort() %>%
    tail(3) %>%
    sum()
}

part_1("../input/1_input.txt") # 70720
part_2("../input/1_input.txt") # 207148

library(tidyverse)
library(stringi)

input <- read_lines("../input/01_input.txt")

numbers <- c(
  "one" = 1,
  "two" = 2,
  "three" = 3,
  "four" = 4,
  "five" = 5,
  "six" = 6,
  "seven" = 7,
  "eight" = 8,
  "nine" = 9,
  "1" = 1,
  "2" = 2,
  "3" = 3,
  "4" = 4,
  "5" = 5, 
  "6" = 6,
  "7" = 7,
  "8" = 8,
  "9" = 9
)

last <- function(v) {
  tail(v, n = 1)
}

part1 <- function(ls) {
  nums <- str_extract_all(ls, "[0-9]")
  firsts <- map_chr(nums, first)
  lasts <- map_chr(nums, last) 
  
  paste0(firsts, lasts) |>
    as.numeric() |>
    sum()
}

get_first_occurence <- function(ls, lookup) {
  str_extract(ls, paste0(lookup, collapse = "|"))
}

part2 <- function(ls) {
  numbers_rev <- numbers |> set_names(nm = stri_reverse(names(numbers)))
  
  firsts <- get_first_occurence(ls, names(numbers)) |>
    map_dbl(\(x) numbers[x])
  lasts <- get_first_occurence(stri_reverse(ls), stri_reverse(names(numbers))) |>
    map_dbl(\(x) numbers_rev[x])
  
  paste0(firsts, lasts) |>
    as.numeric() |>
    sum()
}

part1(input) # 54708
part2(input) # 54087
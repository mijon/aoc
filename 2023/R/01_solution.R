library(tidyverse)

test <- c("1abc2",
"pqr3stu8vwx",
"a1b2c3d4e5f",
"treb7uchet")

input <- read_lines("../input/01_input.txt")

last <- function(v) {
  v[[length(v)]]
}

part1 <- function(ls) {
  nums <- str_extract_all(ls, "[0-9]")
  firsts <- map_chr(nums, first)
  lasts <- map_chr(nums, last) 
  
  paste0(firsts, lasts) |>
    as.numeric() |>
    sum()
}


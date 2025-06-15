library(tidyverse)
source("data_structures.R")
INPUT_PATH <- "../input/09_input.txt"

example_data1 <- "ADVENT"
example_data2 <- "A(1x5)BC"
example_data3 <- "(3x3)XYZ"
example_data4 <- "A(2x2)BCD(2x2)EFG"
example_data5 <- "(6x1)(1x3)A"
example_data6 <- "X(8x2)(3x3)ABCY"

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

parse_input <- function(input) {
  str_split(input, "") |>
    first() |>
    deque()
}

# ---- Working ----


part_1 <- function(input) {
  i <- parse_input(input)
  output <- deque(c())
  
  while (i$len() > 0) {
    l <- i$pop_front()
    if (output$len() == "48792") {
      browser()
    }
    if (l != "(") {
      output$push_back(l)
    } else {
      lookahead <- deque(c())
      times <- deque(c())
      while ((l <- i$pop_front()) != "x") {
        lookahead$push_back(l)
      }
      # at the end of this loop, `l` will be "x", so we can continue popping
      
      while ((l <- i$pop_front()) != ")") {
        times$push_back(l)
      }
      # at the end of this loop, `l` will be ")", so we can pick up popping later 
      
      lookahead <- paste(lookahead$get(), collapse = "") |> as.numeric()
      times <- paste(times$get(), collapse = "") |> as.numeric()
      
      to_add <- i$pop_front(n = lookahead)
      
      for (count in 1:times) {
        output$push_back(to_add)
      }
    }
  }
  output$get() |>
    discard(\(x) x == " ") |>
    length()
}



part_2 <- function(input) {
  parse_input(input)
}


# ---- Results ----
part_1(input) # 110346
part_2(input) # 
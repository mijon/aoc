library(tidyverse)
options(digits = 22)
INPUT_PATH <- "../input/07_input.txt"

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

parse_input <- function(input) {
  tibble(input = input) |>
    separate(input, into = c("target", "vec"), sep = ": ") |>
    mutate(target = as.numeric(target),
           vec = str_split(vec, " ") |> map(as.numeric))
}

# ---- Working ----
# BFS through the combinations of operators 
check_vec <- function(target, vec, result = FALSE) {
  if (length(vec) == 1) {
    return(FALSE)
  }
  
  first_two <- head(vec, 2)
  rst <- tail(vec, -2)
  
  add <- sum(first_two)
  mul <- prod(first_two)
  
  if (add == target || mul == target) {return(TRUE)}
  
  result || check_vec(target, c(add, rst)) || check_vec(target, c(mul, rst))
}


part_1 <- function(input) {
  parse_input(input) |>
    mutate(test = map2_lgl(target, vec, check_vec)) |>
    filter(test) |>
    pull(target) |>
    sum()
}

# part 2 just introduces another operator
check_vec_2 <- function(target, vec, result = FALSE) {
  if (length(vec) == 1) {
    return(FALSE)
  }
  
  first_two <- head(vec, 2)
  rst <- tail(vec, -2)
  
  add <- sum(first_two)
  mul <- prod(first_two)
  cat <- first_two[1] %|% first_two[2]
  
  # Additional check here required to ensure we're using the full vector
  if (add == target & length(rst) == 0) {
    return(TRUE)
  }
  if (mul == target & length(rst) == 0) {
    return(TRUE)
  }
  if (cat == target & length(rst) == 0) {
    return(TRUE)
  }
  
  result ||
    check_vec_2(target, c(add, rst)) ||
    check_vec_2(target, c(mul, rst)) ||
    check_vec_2(target, c(cat, rst))
}

`%|%` <- function(l, r) {
  as.numeric(paste0(l, r, collapse = ""))
}


part_2 <- function(input) {
  parse_input(input) |>
    mutate(test = map2_lgl(target, vec, check_vec_2, .progress = TRUE)) |>
    filter(test) |>
    pull(target) |>
    sum()
}


# ---- Results ----
part_1(input) # 1298300076754
part_2(input) # 248427118972289
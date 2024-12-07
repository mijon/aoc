library(tidyverse)
INPUT_PATH <- "../input/07_input.txt"

example_data <- "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20" |>
  str_split("\n") |> first()

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

parse_input <- function(input) {
  tibble(input = input) |>
    separate(input, into = c("target", "vec"), sep = ": ") |>
    mutate(target = as.numeric(target),
           vec = str_split(vec, " ") |> map(as.numeric))
}

# ---- Working ----
check_vec <- function(target, vec, result = FALSE) {
  if (length(vec) == 1) {
    # return(target == vec)
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

check_vec_2 <- function(target, vec, result = FALSE) {
  if (length(vec) == 1) {
    # return(target == vec)
    return(FALSE)
  }
  
  first_two <- head(vec, 2)
  rst <- tail(vec, -2)
  
  add <- sum(first_two)
  mul <- prod(first_two)
  con <- as.numeric(paste(first_two, collapse = ""))
  
  if (add == target || mul == target || con == target) {return(TRUE)}
  
  if (add > target) {
    add_value <- FALSE
  } else {
    add_value <- check_vec_2(target, c(add, rst))
  }
  
  if (mul > target) {
    mul_value <- FALSE
  } else {
    mul_value <- check_vec_2(target, c(mul, rst))
  }
  
  if (con > target) {
    con_value <- FALSE
  } else {
    con_value <- check_vec_2(target, c(con, rst))
  }
  
  result || add_value || mul_value || con_value
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
part_2(input) # 

# 248427127368476 (wrong - too high)
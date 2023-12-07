library(tidyverse)

input <- read_lines("../input/06_input.txt")

# ---- part 1 ----
parse_input <- function(input) {
  vecs <- input |>
    map(\(x) str_remove(x, "[a-zA-Z]+:[ ]+")) |>
    map(parse_num_group)
  tibble(time = vecs[[1]],
         distance = vecs[[2]])
}

parse_num_group <- function(g) {
  g |> 
    str_split("[ ]+") |>
    first() |>
    as.numeric()
}

count_winning_strategies <- function(t, d) {
  sum((0:t) * (t:0) > d)
}

part_1 <- function(input) {
  input |>
    parse_input() |>
    mutate(
      winning_strats = map2_dbl(time, distance, count_winning_strategies)) |>
    pull(winning_strats) |>
    prod()
}

# ---- part 2 ----
parse_input2 <- function(input) {
  input |>
    map(\(x) str_remove(x, "[a-zA-Z]+:[ ]+") |>
          str_remove_all(" ")) |>
    map(as.numeric) |>
    set_names(c("time", "distance"))
  
}

get_roots <- function(t, d) {
  c(
    (t - sqrt(t^2 - 4 * d))/2,
    (t + sqrt(t^2 - 4 * d))/2
  )
}

part_2 <- function(input) {
  parsed_input <- input |> parse_input2()
  roots <- get_roots(parsed_input$time,
                     parsed_input$distance)
  floor(roots[2]) - ceiling(roots[1]) + 1
}


# ---- evaluations -----
part_1(input) # 449550
part_2(input) # 28360140

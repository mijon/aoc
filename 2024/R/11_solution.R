library(tidyverse)
options(digits = 22)
INPUT_PATH <- "../input/11_input.txt"

example_data <- "125 17"

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

parse_input <- function(input) {
  input |> str_split(" ") |> first() |> as.numeric()
}

# ---- Working ----
split_strings_in_half <- function(s) {
  str_extract_all(s, paste0(".{", ceiling(str_length(s)/2), "}"))
}

process <- function(input) {
  rule_1 <- if_else(input == 0, list(1), list(NULL))
  rule_2 <- if_else(str_length(input) %% 2 == 0, split_strings_in_half(input), list(NULL))
  rule_3 <- as.list(input * 2024)
  
  # browser()
  coalesce(rule_1, rule_2, rule_3) |>
    flatten() |>
    as.numeric()
}

run_f_n_times <- function(f, n, ...) {
  if (n == 1) {
    f(...)
  } else {
    run_f_n_times(f, n - 1, f(...))
  }
}

part_1 <- function(input) {
  input <- parse_input(input)
  run_f_n_times(process, 25, input) |>
    length()
}




# ---- Part 2 ----
# In classic AoC style, we now need to do something a few more times which means
# that it takes much longer to solve. So we need a change of tack. Through some
# noodling, I've noticed that certain inputs result in cycles of varying
# lengths, but closed. The model is a directed graph, which means it's
# representable as a matrix, which means we can model repeated processing of the
# input as multiplications by the matrix. Our input is simply a vector equal in
# length to the number of nodes (therefore the number of rows in the matrix),
# with a 1 matching each input value.
#
# In classic me solving AoC style, this is not really that fast... but it does
# work, so I'm going to count that as a win and move on.
#
# First things first: we need a way to generate the graph. Let's make another
# data structure! Last time we made a deque, which is a double ended queue. This
# time we need to make a single ended queue, or a "queue".
queue <- function(v) {
  env <- new.env()
  assign("values", value = v)
  
  give <- function() {
    get("values", env)
  }
  
  pop <- function(n = 1) {
    values <- give()
    output <- head(values, n)
    assign("values", value = tail(values, -n), envir = env)
    output
  }
  
  push <- function(x) {
    new <- append(give(), x)
    assign("values", value = new, envir = env)
  }
  
  len <- function() {
    length(give())
  }
  
  list(
    get = give,
    pop = pop,
    push = push,
    len = len
  )
}

connections_to_matrix <- function(connections) {
  missing_tos <- connections |> filter(!from %in% to) |>
    mutate(to = from, from = -1, n = 0) |>
    distinct()
  
  connections <- bind_rows(connections, missing_tos) |>
    arrange(to) |>
    pivot_wider(names_from = "to", values_from = "n", values_fill = 0) |>
    filter(from != -1) |>
    arrange(from) 
    
  rows <- connections$from
  connections <- connections |>
    select(-from) |>
    as.matrix()
  rownames(connections) <- rows
  connections
}

gen_graph <- function(start) {
  todo <- queue(start)
  connections <- tibble(from = numeric(), to = numeric(), n = numeric())
  while(todo$len() != 0) {
    current_from <- todo$pop()
    tos <- process(current_from)
    appendable <- tibble(from = current_from, to = tos) |>
      count(from, to) |>
      filter(!from %in% connections$from)
    connections <- add_row(connections, appendable)
    future_todos <- keep(tos, \(x) !x %in% connections$from)
    todo$push(future_todos)
  }
  connections
}


# I wanted to be fancy and use the following, but it takes a while to compute
# the eigen vectors and `solve()` doesn't work anyway.
# pow_mat <- function(m, n) {
#   eigs <- eigen(m)
#   eigs$vectors %*% diag(eigs$values)^n %*% solve(eigs$vectors)
# }

pow_mat <- function(m, n) {
  if (n == 1) {
    return(m)
  }
  m %*% pow_mat(m, n - 1)
}

part_2 <- function(input, n) {
  input <- parse_input(input)
  g <- input |>
    gen_graph() |>
    connections_to_matrix() |>
    pow_mat(n = n)
  
  # make a vector to represent the input
  input_vec <- rep(0, nrow(g))
  input_vec[which(colnames(g) %in% input)] <- 1
  sum(input_vec %*% g)
}

# ---- Results ----
part_1(input) # 197157
part_2(input, n = 75) # 234430066982597

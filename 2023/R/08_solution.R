library(tidyverse)
library(igraph)
library(DiagrammeR)

input <- read_lines("../input/08_input.txt")
# ---- part 1 ----

# We can get around the issue of having to reuse the directions provided
# infinitely, by making a function that gives us the next value each time we
# call it.
create_pullable <- function(v) {
  item <- 1
  function() {
    item_to_give <- v[item]
    if (item == length(v)) {
      item <<- 1
    } else {
      item <<- item + 1
    }
    item_to_give
  }
}

# The input is a list of Ls and Rs, then a list of nodes and pairs of nodes.
parse_input <- function(input) {
  list(directions = parse_directions(input[[1]]),
       nodes = parse_nodes(input[3:length(input)]))
}

parse_directions <- function(dirs) {
  str_split(dirs, "") |>
    first() |>
    create_pullable()
}

parse_nodes <- function(dirs) {
  names <- str_extract(dirs, "^[0-9A-Z]{3}")
  dirs <- dirs |>
    str_extract("[0-9A-Z]{3}, [0-9A-Z]{3}") |>
    map(\(s) str_split(s, ", ") |> first()) |>
    map(set_names, c("L", "R"))
  set_names(dirs, names)
}

# We can also parse the nodes into a data frame, which we use in part 2.
parse_nodes_df <- function(nodes) {
  tibble(nodes = nodes) |>
    separate(nodes,
             into = c("name", "left", "right"),
             sep = " = \\(|, ") |>
    mutate(right = str_remove(right, "\\)"))
}

# This steps through the input from start to end and returns how many steps we
# took.
stepper <- function(input, start, end) {
  n_steps <- 0
  current_pos <- start
  
  # unpack
  get_direction <- input$directions
  nodes <- input$nodes
  while(current_pos != end) {
    direction <- get_direction()
    current_pos <- nodes[[current_pos]][[direction]]
    n_steps <- n_steps + 1
  }
  
  n_steps
}

part_1 <- function(input) {
  parse_input(input) |>
    stepper(start = "AAA", end = "ZZZ")
}

# ---- part 2 ----

# A lot of part 2 is informed from some exploratory work, see file
# "08_exploration.R".

lcm <- function(v) {
  reduce(v, pracma::Lcm)
}

get_clusters <- function(nodes_df) {
  ndf <- nodes_df |> transmute(id = 1:n(), label = name) 
  edf <- bind_rows(
    nodes_df |> select(from_label = name,
                      to_label = left) |>
      left_join(ndf |> rename(from_label = label, from = id), by = "from_label") |>
      left_join(ndf |> rename(to_label = label, to = id), by = "to_label"),
    nodes_df |> select(from_label = name,
                      to_label = right) |>
      left_join(ndf |> rename(from_label = label, from = id), by = "from_label") |>
      left_join(ndf |> rename(to_label = label, to = id), by = "to_label")
  ) |> select(from, to)
  
  g <- create_graph() |>
    add_nodes_from_table(ndf, label_col = label) |>
    add_edges_from_table(edf, from_col = from, to_col = to, from_to_map = id_external) |>
    to_igraph()
  
  clusters_df <- g |>
    clusters() |>
    pluck(membership) |>
    as.list() |>
    enframe(name = "id", value = "cluster") |>
    mutate(id = as.integer(id)) |>
    unnest(cluster)
  
  ndf |> left_join(clusters_df, by = "id") |>
    select(-id)
}

# The exploratory work shows that we are really dealing with siz independent
# cycles. And that the starting point gets you right into the cycle. So we just
# need to find the period of the cycle for each independent one, and then find
# the LCM
part_2 <- function(input) {
  directions <- parse_directions(input[[1]])
  nodes_df <- parse_nodes_df(input[3:length(input)])
  parsed_input <- parse_input(input)
  
  clusters <- get_clusters(nodes_df)
  starters  <- nodes_df |> filter(str_detect(name, "A$")) |> pull(name)
  finishers <- nodes_df |> filter(str_detect(name, "Z$")) |> pull(name)
  

  matched <- left_join(
    filter(clusters, label %in% starters),
    filter(clusters, label %in% finishers),
    by = "cluster",
    suffix = c("_start", "_end")) 
  
  matched |>
    mutate(length = map2_dbl(
      label_start,
      label_end,
      \(s, e) {stepper(parsed_input, s, e)})) |>
    pull(length) |>
    lcm()
}



# ---- Evaluations ----
part_1(input) # 21389
part_2(input) # 21083806112641


# ---- About ----
#
# We need to move a robot around a maze, visiting certain points, and we need to
# cover the shortest distance possible. This calls for a graph approach, and is
# a travelling salesman problem (TSP).
#
# Part 1 is a slightly adapted version, an "open TSP", i.e. we don't need to get
# back to our starting point, but we can generate the normal distance matrix,
# and then replace the "cost" from any one point back to the start with 0.

library(tidyverse)
library(igraph)
library(TSP)
source("a_star.R")

# `input` becomes a list of character vectors, `input_mat` is a matrix
# representation of the maze.
input <- read_lines("../input/24_input.txt") |>
  map(str_split, "") |>
  map(first)

input_mat <- matrix(input |> flatten(),
                    nrow = length(input),
                    byrow = TRUE)

# ---- Utils ----

# get the character in the matrix given by the point
lookup <- function(point, mat) mat[point[1], point[2]][[1]]

# "Open" cells are any matrix elements that are "." or any of the numbers.
is_open <- function(point, mat) !lookup(point, mat) == "#"

gen_neighbours <- function(point, mat) {
  candidate_points <- list(
    c(point[1] - 1, point[2]),
    c(point[1] + 1, point[2]),
    c(point[1], point[2] - 1),
    c(point[1], point[2] + 1)
  )
  
  keep(candidate_points, is_open, mat)
}

# ---- Set up for the a* algorithm ----
# In other problems, I've found *not* taking the `sqrt` to be faster, but I
# suspect that since this maze requires far more "wiggly" routes, only the
# `sqrt` version works (but it is a little slower).
cost_fn <- function(p1, p2) sqrt((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2)

# The implementation of a* takes a function node -> [nodes], so we just need to
# fill in the `input_mat` here. We could have done this with a `purrr::partial`,
# but this is essentially the same.
gen_neighbours2 <- function(node) gen_neighbours(node, input_mat)

# This is our core route function, so given two points in the matrix (i.e. two
# numbers), give the a* shortest distance between them.
get_path <- function(from, to, mat) {
  start_point <- which(mat == from, arr.ind = TRUE) |> as.numeric()
  end_point   <- which(mat == to, arr.ind = TRUE) |> as.numeric()
  
  a_star(start_point, end_point, 
         identical,
         cost_fn, 
         gen_neighbours2, 
         const_d_between_neighbours
  )$path
}

# We need to calculate the shortest distances between all the relevant points,
# since the distances are symmetric (a -> b is as far to go as b -> a), we can
# just compute one half of the matrix, and then flip it.
gen_dist_mat <- function() {
  start_char <- c("0", "1", "2", "3", "4", "5", "6", "7")
  results <- matrix(Inf, nrow = length(start_char), ncol = length(start_char))
  
  for (i in 1:length(start_char)) {
    for (j in i:length(start_char)) {
      path_length <- get_path(start_char[i], start_char[j], input_mat) |>
        length()
      
      results[i,j] <- path_length - 1
    }
  }
  results[lower.tri(results)] <- t(results)[lower.tri(results)]
  
  # We rename these to avoid the fact that R is 1-based, and AoC is 0-based
  colnames(results) <- start_char
  rownames(results) <- start_char
  
  results
}

# This little function sets the first column of the distance matrix to zero,
# implying the cost to get from anywhere to the start point is zero. This small
# edit changes the traditional TSP into an open TSP.
make_open_tsp <- function(mat) {
  mat[1:nrow(mat), 1] <- 0
  mat
}

part_1 <- function() {
  # Make a distance matrix of the shortest path between each pair of nodes.
  dist_mat <- gen_dist_mat() |>
    make_open_tsp()
  
  # The TSP solver is slightly probabilistic, but doesn't take long to run, so
  # let's run it 5 times and take the minimum.
  run_once <- function(dist_mat) {
    dist_mat |>
      ATSP() |> # <-- We need to use `ATSP` here because our distance matrix is not symmetric
      solve_TSP(method = "nearest_insertion", start = 1) 
  }
  
  results <- replicate(5, run_once(dist_mat), simplify = FALSE)
  results[[which.min(map_dbl(results, tour_length))]]
}

# The only difference for part 2 is that we're back on a traditional TSP.
part_2 <- function() {
  dist_mat <- gen_dist_mat()
  
  run_once <- function(dist_mat) {
    dist_mat |>
      TSP() |>
      solve_TSP(method = "nearest_insertion", start = 1)
  }
  
  results <- replicate(5, run_once(dist_mat), simplify = FALSE)
  results[[which.min(map_dbl(results, tour_length))]]
}

# ---- results ----

p1 <- part_1()
p2 <- part_2()

p1 |> tour_length() # 470
p2 |> tour_length() # 740

# ---- Appendix ----
#
# It's nice to be able to plot the tours we have taken:

# A function to plot the base maze.
plot_maze <- function(input) {
  tibble(data = input) |>
    mutate(row = 1:n(),
           data = map(data, \(x)
                      tibble(value = x) |>
                        mutate(column = 1:n()))) |>
    unnest(data) |>
    mutate(space = if_else(value == "#", "#", "."),
           label = if_else(!value %in% c("#", "."), value, "")) |>
    ggplot(aes(x = column, y = row, fill = space)) + 
    geom_tile() +
    geom_text(aes(label = label)) +
    scale_fill_brewer(direction = 1) +
    coord_equal() + 
    scale_y_reverse() +
    theme_void() + 
    theme(legend.position = "none")
}


path_to_df <- function(path_list) {
  tibble(data = path_list) |>
    unnest_wider(data, names_sep = "") |>
    rename(row = data1, column = data2)
}


plot_tour <- function(tour, input, loop = FALSE) {
  tour <- labels(tour)
  
  g <- plot_maze(input)
  
  for (i in 1:(length(tour) - 1)) {
    g <- g + geom_path(data = get_path(tour[i],
                                       tour[i + 1],
                                       input_mat) |>
                         path_to_df(),
                       mapping = aes(x = column, y = row),
                       colour = "red",
                       inherit.aes = FALSE)
  }
  
  if (loop) {
    g <- g + geom_path(data = get_path(tour[length(tour)],
                                       tour[1],
                                       input_mat) |>
                         path_to_df(),
                       mapping = aes(x = column, y = row),
                       colour = "red",
                       inherit.aes = FALSE)
  }
  
  g + labs(subtitle = paste(tour, collapse = " -> "))
}


library(patchwork)
plot_tour(p1, input) / plot_tour(p2, input, loop = TRUE)


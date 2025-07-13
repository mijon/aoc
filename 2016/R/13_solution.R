# ---- About ----
#
# This is our first real grid/maze problem of 2016. It is tempting (indeed, it
# was my first idea) to pre-generate the grid, but that's not necessary. We can
# actually solve this by only generating the cells in the grid we need as we
# come across them. We're going to use the a* algorithm to efficiently find the
# best path.
#
# The meat of part 1 is the a* algorithm that I've factored out into a separate
# file, then it's setting up all the functions that are used in the a* search.


library(tidyverse)
source("a_star.R") # helper functions for a*

INPUT_PATH <- "../input/13_input.txt"

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH) |>
  as.numeric()

# We need a way to determine whether a particular point is open or closed. Our
# input forms a part of this function.
sample_grid <- function(x, y, favourite_num = 10) {
  init_val <- x^2 + 3*x + 2*x*y + y + y^2 + favourite_num
  num_ones <- count_binary_ones(init_val)
  
  if (num_ones %% 2 == 0) {
    "open"
  } else {
    "closed"
  }
}

# R has intToBin, but this doesn't work for really large numbers. We need
# something that does work for really large numbers, so we make our own.
count_binary_ones <- function(x, total_so_far = 0) {
  if (x == 1) {
    return(total_so_far + 1)
  }
  
  if (x %% 2 == 0) {
    count_binary_ones(x / 2, total_so_far)
  } else {
    count_binary_ones(x %/% 2, total_so_far + 1)
  }
}

# For a* we need an heuristic function (h(x)) that will provide a way to rank
# the candidate nodes in the frontier and pick the one that's closest. We will
# use the squared euclidean distance. We pick squared distance rather than plain
# distance because:
# - That way we don't need to calculate a sqrt so we'll be faster
# - It acutally works better as it penalises the wrong directions more.
dist_squared_euclidean <- function(p1, p2) {
  (p2[1] - p1[1])^2 + (p2[2] - p1[2])^2
}

# We also need a way to generate the "open" neighbours of any point in the maze:
get_neighbours <- function(p) {
  x0 <- p[1]
  y0 <- p[2]
  
  points <- tribble(     ~x, ~y,
                         x0 - 1, y0,
                         x0 + 1, y0,
                         x0    , y0 - 1,
                         x0    , y0 + 1) |>
    filter(x >= 0, y >= 0) # We're told that the points cannot go negative
  
  # Note that we're only using our input here --------------------|
  points |> #                                                     v
    mutate(status = map2_chr(x, y, sample_grid, favourite_num = input)) |>
    filter(status == "open") |>
    rowwise() |>
    transmute(data = list(c(x, y))) |>
    pull(data)
}

# ---- Patt 1 ----
part_1 <- function() {
  result <- a_star(start = c(1, 1),
                   end   = c(31, 39),
                   heuristic_cost_fn = dist_squared_euclidean,
                   get_neighbours = get_neighbours,
                   d_between_neighbours = const_d_between_neighbours)
  length(result$path) - 1 # don't count the starting point
}

# ---- part 2 ----
# This is a flood fill where we stop after 50 steps

flood_fill <- function() {
  # We initialise containers for all the things we might want to track.
  visited <- list()
  step_count <- list()
  frontier  <- list(c(1,1))
  steps <- 0
  
  # Condition of 51 here as we want to stop *after* doing 50 steps
  while (steps < 51) { 
    for (p in frontier) {
      
      # generate candidates for the next expansion
      neighbours <- get_neighbours(p) |>
        discard(\(x) elem(x, visited)) |> # remove out any that we've already been to
        discard(\(x) elem(x, frontier)) # remove any that we are already planning to visit
      
      frontier <- append(frontier, neighbours)
      step_count <- append(step_count, steps)
      
      # remove from frontier and add to visited
      visited <- append(visited, values = list(p))
      frontier <- discard(frontier, \(x) identical(x, p))
    }
    steps <- steps + 1
  }
  list(visited = visited,
       step_count = step_count)
}

part_2 <- function() {
  result <- flood_fill()
  length(result$visited)
}


# ---- Results ----
part_1() # 92
part_2() # 124



# ---- Bonus: Plotting ----
# Here is a plot of the first part. We show the landscape of the cells and also
# all the points we visited in our search in black, with the actual path in
# green. If you plot this, you will see how efficient this search is as there
# are only 6 black dots, i.e. six places points we visited that were not in the
# shortest path.
search <- a_star(start = c(1, 1),
                 end   = c(31, 39),
                 heuristic_cost_fn = dist_squared_euclidean,
                 get_neighbours = get_neighbours,
                 d_between_neighbours = const_d_between_neighbours)

crossing(x = 0:40, y = 0:40) |>
  mutate(status = map2_chr(x, y, sample_grid, favourite_num = input)) |>
  ggplot(aes(x = x, y = y, fill = status)) +
  geom_tile(colour = "black") +
  coord_equal() +
  scale_y_reverse() +
  geom_point(data = tibble(points = search$visited) |>
               unnest_wider(points, names_sep = c("_")),
             mapping = aes(x = points_1, y = points_2), inherit.aes = FALSE) +
  geom_point(data = tibble(points = search$path) |>
               unnest_wider(points, names_sep = c("_")),
             mapping = aes(x = points_1, y = points_2), inherit.aes = FALSE,
             colour = "darkgreen") 


# Here is a plot of the second part:

flood <- flood_fill()

crossing(x = 0:40, y = 0:40) |>
  mutate(status = map2_chr(x, y, sample_grid, favourite_num = input)) |>
  ggplot(aes(x = x, y = y, fill = status)) +
  geom_tile(colour = "black") +
  coord_equal() +
  scale_y_reverse() +
  geom_text(data = tibble(points = flood$visited,
                          steps = as.numeric(flood$step_count)) |>
              unnest_wider(points, names_sep = c("_")),
            mapping = aes(x = points_1, y = points_2, label = steps), inherit.aes = FALSE)

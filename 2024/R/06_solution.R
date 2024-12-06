library(tidyverse)
INPUT_PATH <- "../input/06_input.txt"

example_data <-"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
"

# ---- input reading and parsing ----
UP    <- c(-1,  0)
DOWN  <- c( 1,  0)
LEFT  <- c( 0, -1)
RIGHT <- c( 0,  1)


turn_right <- function(v) {
  v %*% matrix(c(0, -1, 1, 0), byrow = TRUE, nrow = 2) |> as.numeric()
}

input <- readr::read_file(INPUT_PATH)

# Grid helpers

# Mark on the grid that you've visited a point
# visit :: mat -> point -> mat
visit <- function(mat, point) {
  mat[point[1], point[2]] <- 1 + mat[point[1], point[2]]
  mat
}

# Peak into a cell in the grid
# look_in :: mat -> point -> char
look_in <- function(mat, point) {
  mat[point[1], point[2]]
}

# Given a point location, what's the next location?
# next_point :: point -> direction -> step_size -> point
next_point <- function(point, direction, step_size = 1) {
  point + step_size * direction
}

# Is a point on the grid?
# in_bounds :: point -> mat -> bool
in_bounds <- function(point, mat) {
  point[1] >= 0 & point[1] <= nrow(mat) &
    point[2] >= 0 & point[1] <= ncol(mat)
}

# Go from input to a `state` that we can manipulate over time.
parse_input <- function(input) {
  grid <- input |>
    str_trim() |>
    str_split("\n") |>
    map(str_split, "", simplify = TRUE) |>
    first()
  
  init_pos <- which(grid == "^", arr.ind = TRUE) |>
    as.numeric()
  
  visited <- matrix(data = 0, nrow = nrow(grid), ncol = ncol(grid))
  visited <- visit(visited, init_pos)
  
  list(grid = grid,          # The grid we're working in
       pos = init_pos,       # The current point (c(r,c)) that we're at
       direction = UP,       # The direction we're facing
       within_bounds = TRUE, # Is out current point within the bounds of the grid?
       visited = visited)    # A matrix representing where we've been
}



# set_direction :: state -> state
set_direction <- function(state) {
  next_pos_value <- "#"
  while(next_pos_value == "#") {
    next_pos <- next_point(state$pos, state$direction)
    if (!in_bounds(next_pos, state$grid)) {
      return(state)
    }
    next_pos_value <- look_in(state$grid, next_pos)
    if (length(next_pos_value) == 0) {
      return(state)
    }
    if (next_pos_value == "#") {
      state$direction <- turn_right(state$direction)
    }
  }
  state
}

# ---- Working ----
# move :: state -> state
move <- function(state) {
  state <- set_direction(state)
  
  # move the current position
  state$pos <- next_point(state$pos, state$direction)
  if (!in_bounds(state$pos, state$grid)) {
    state$within_bounds <- FALSE
    return(state)
  }
  
  # update the visited matrix
  state$visited <- visit(state$visited, state$pos)
  state
}

part_1 <- function(input) {
  state <- parse_input(input)
  while (state$within_bounds) {
    state <- move(state)
    # print(state$visited)
  }
  
  sum(state$visited > 0)
}


part_2 <- function(input) {
  state <- parse_input(input)
  while (state$within_bounds) {
    state <- move(state)
    # print(state$visited)
  }
  
  state
}


# ---- Results ----
part_1(input) # 5101, but slower than I'd like
# part_2(input) # 
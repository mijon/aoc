library(tidyverse)
INPUT_PATH <- "../input/10_input.txt"

# ---- input reading and parsing ----
input <- readr::read_file(INPUT_PATH)

# Make a big matrix that's full of numbers
parse_input <- function(input) {
  output <- input |>
    str_trim() |>
    str_split("\n") |>
    map(str_split, "", simplify = TRUE) |>
    first()
  class(output) <- "numeric"
  output
}

# Since we're looking at neighbours, it's easier to pad the grid rather than
# handle the edges. Here's a general function that can handle any width, even
# though we'll just us a single width pad border.
pad_grid <- function(m, x, width = 1) {
  padded <- matrix(x, nrow = nrow(m) + 2 * width, ncol = ncol(m) + 2 * width)
  padded[(width + 1):(nrow(m) + width), (width + 1):(ncol(m) + width)] <- m
  padded
}

# Get the points and values for all up, down, left, right of some point, p.
plus_on_point <- function(p, grid) {
  tribble(
    ~p,      ~val,
    c(p[1] - 1, p[2] + 0), grid[p[1] - 1, p[2] + 0],
    c(p[1] + 1, p[2] + 0), grid[p[1] + 1, p[2] + 0],
    c(p[1] + 0, p[2] - 1), grid[p[1] + 0, p[2] - 1],
    c(p[1] + 0, p[2] + 1), grid[p[1] + 0, p[2] + 1]
  )
}

# ---- Working ----
count_trailhead <- function(p, grid) {
  paths <- count_paths(p, grid, part = 1)
  all_paths <- unlist(paths, recursive = TRUE)
  all_paths
  map(seq(1, length(all_paths), 2), \(i) list(all_paths[i], all_paths[i+1])) |>
    unique() |>
    length()
}

count_paths <- function(p, grid, path = list(), part) {
  path <- append(path, list(p))
  
  # The two parts mainly differ by what we return when we get to the `9` points.
  # In part 1, we just need to count the total `9` points reachable from any
  # trailhead, so it's sufficient to return the location of the `9` point
  # reached in this arm of the DFS. We can count unique points later. For part
  # 2, we need the unique trails, so we keep a description o four path and then
  # we can de-dupe and count these later.
  if (grid[p[1], p[2]] == 9) {
    if (part == 1) {
      return(p)
    } else if (part == 2) {
      return(map_chr(path, \(x) paste(x, collapse = ",")) |>
               paste(collapse = " -> "))
    }
  }
  
  # We get the current value, find it's neighbours that are higher and then work
  # on those.
  curr_val <- grid[p[1], p[2]]
  candidates <- plus_on_point(p, grid) |>
    filter(val == curr_val + 1) |>
    pull(p)
  
  map(candidates, count_paths, grid, path, part)
}

part_1 <- function(input) {
  grid <- parse_input(input) |> pad_grid(x = -1)
  trailheads <- which(grid == 0, arr.ind = TRUE)|>
    as_tibble() |>
    mutate(p = map2(row, col, c)) |>
    pull(p)
  
  map(trailheads, count_trailhead, grid) |>
    as.numeric() |>
    sum()
}

part_2 <- function(input) {
  grid <- parse_input(input) |> pad_grid(x = -1)
  trailheads <- which(grid == 0, arr.ind = TRUE)|>
    as_tibble() |>
    mutate(p = map2(row, col, c)) |>
    pull(p)
  
  map(trailheads, count_paths, grid, part = 2) |>
    map(\(x) unlist(x, recursive = TRUE)) |>
    lengths() |>
    sum()
}

# ---- Results ----
part_1(input) # 709
part_2(input) # 1326
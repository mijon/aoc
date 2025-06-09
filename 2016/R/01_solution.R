# ---- About ----
#
# We start pointing due North and get instructions of the form
# "<direction:R/L><amount:int>". We quarter turn the amount given by `direction`
# and then walk the amount given by `amount`.
#
# For this task, the state of the ssytem at any point in time is a position and
# a direction. For fun, I will encode thie state in a 2-element complex vector.
# The first element is the orientation we face, and the second is the position.
# Thus we can rotate by multiplying the first element by i, and move by adding a
# multiple of the first element to the second.

INPUT_PATH <- "../input/01_input.txt"
source("comp_geom.R") # various computational geometry tools.

# ---- input reading and parsing ----
parse_input <- function(input) {
  input |>
    stringr::str_split(", ") |>
    dplyr::first() |>
    purrr::map(\(x) {list(direction = stringr::str_sub(x, start = 1, end = 1),
                          amount = as.numeric(stringr::str_sub(x, start = 2)))})
}

input <- readr::read_file(INPUT_PATH) |>
  parse_input()
# ---- Working ----

# state is a 2-element complex vector, state[1] is the orientation, state[2] is
# the position

rotate <- function(state, direction = c("R", "L")) {
  state[1] <- state[1] * (-1)^(direction == "R") * 1i
  state
}

move <- function(state, amount) {
  state[2] <- state[2] + amount * state[1]
  state
}

# the `abs` here are important to make sure we get the correct value
taxicab_distance <- function(n) {
  abs(Re(n)) + abs(Im(n))
}

# We get given a list of `direction` and `amount`, so we first rotate, then we
# move as instructed.
dir_and_move <- function(state, dir_and_move) {
  state |>
    rotate(dir_and_move$direction) |>
    move(dir_and_move$amount)
}

# --- Part 1 ----
# Being only interested in our end point, once we've run through the states, we
# can just look at the result of a `reduce` over the instructions.
part_1 <- function(input) {
  initial_state <- c(1i, 0)
  purrr::reduce(.x = input, .f = dir_and_move,
                .init = initial_state) |>
    dplyr::nth(2) |>
    taxicab_distance()
}


# ---- Part 2 ----
# The difference now is that we want to keep a track of where we've been before.
# For more complex days, it might make sense to build in a check to exit early,
# but for now, we can walk the whole way and then check any intersections at the
# end.
#
# The main objective here is to find the first point we visit twice. Of course,
# when I first read this, I assumed it was the first in the list of points that
# we had already visited. But now, it's really the first point our track
# *between* the points crosses over itself.


intersection_point <- function(a, b, x, y) {
  # Take advantage of the fact we are dealing only with vertical and horizontal
  # lines to make this function far more specific than the full general case.
  if (Re(a) == Re(b)) {
    Re(a) + Im(x)
  } else {
    Re(x) + Im (a)
  }
}


part_2 <- function(input) {
  initial_state <- c(1i, 0)
  df <- purrr::accumulate(.x = input, .f = dir_and_move,
                          .init = initial_state) |>
    purrr::map(2) |>
    tibble::enframe() |>
    tidyr::unnest(value) |>
    dplyr::mutate(end = dplyr::lead(value))
  
  for (row in 3:nrow(df)) {
    a <- df$value[row]
    b <- df$end[row]
    for (i in 1:(row-2)) {
      x <- df$value[i]
      y <- df$end[i]
      
      if (do_intersect(a, b, x, y)) {
        return(taxicab_distance(intersection_point(a, b, x, y)))
      }
    }
  }
}

# ---- Results ----
part_1(input) # 181
part_2(input) # 140

# ---- Appendix ----

# We can do part 1 with matrices, but it seems to be slightly slower:
rotate2 <- function(state, direction = c("R", "L")) {
  state %*% matrix(c((-1)^(direction == "R") * 1i, 0, 0, 1), nrow = 2)
}

move2 <- function(state, amount) {
  state %*% matrix(c(1, 0, amount, 1), nrow = 2)
}

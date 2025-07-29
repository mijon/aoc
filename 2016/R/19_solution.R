library(tidyverse)
INPUT_PATH <- "../input/19_input.txt"

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)[[1]] |>
  as.numeric()

# ---- About ----
#
# This time we're playing a game where elves sit in a circle and steal presents
# from each other. My initial impulse was to simulate the whole process - an
# impulse which is almost invariably the wrong thing to do in Advent of Code.
# But it did teach me certain useful things. The actual solution I came up with
# doesn't simulate the process at all, rather jumps straight to the final result
# by maths, but I (specifically me, I'm sure other people can look at the
# problem spec and reason the maths out) wouldn't have got to that solution
# without first simulating the problem for a few lower values. So I've left in
# my simulation code at the bottom of this file along with a few graphs that
# really helped see the pattern.
#
# This solution really shows the value in having plotting tools ready to hand.
# The brain is a powerful visual computer and it's always good to get it
# involved in pattern recognition. Often finding a visual/geometrical solution
# to a problem is rewarding (both in getting the actual result, but also in
# having fun).
#
# The solution is pretty much about spotting the pattern and then building a
# linear interpolation to hit a specific point.


# ---- actual code ----
part_1 <- function(k) {
  n_minus <- 2^floor(log(k, 2))
  n_plus <- 2^ceiling(log(k, 2))
  
  # linear interpolation on the 
  1 + (k - n_minus) / (n_plus - 1 - n_minus) * (n_plus - 1)
}

part_2 <- function(k) {
  n_minus <- 3^floor(log(k, 3))
  n_plus <- 3^ceiling(log(k, 3))
  mid_point <- n_minus + 0.5 * (n_plus - n_minus)
  
  if (k <= mid_point) {
    1 + (k - (n_minus + 1)) / (mid_point - (n_minus + 1)) * (0.5 * mid_point - 1)
  } else {
    (0.5 * mid_point) + (k - mid_point)/(n_plus - mid_point) * (n_plus - 0.5 * mid_point)
  }
}

part_1(input) # 1830117
part_2(input) # 1417887

# ---- Initial Explorations ----
#
# Okay, as mentioned above, the way I got to this solution was by simulating the
# first couple of 1,000 and then noticing patterns. So here's the code I used.

# Model each elf as an S3 class, just so we can make nicer print methods. It
# turns out we really don't need to track the amount of presents each elf has, I
# was thinking that part 2 might involve different rules or a question on how
# many presents a particular elf has at some point.
entity <- function(name, amount) {
  output <- list(name = name, amount = amount)
  class(output) <- "elf"
  output
}

format.elf <- function(x) {
  paste0(x$name, " [", x$amount, "]", collapse = " ")
}

print.elf <- function(x) {
  print(format(x))
}

# For n players, start the table, i.e. n elves (entities) each starting with 1.
create_table <- function(n) {
  purrr::map(1:n, \(x) entity(x, 1))
}

give <- function(r, g) {
  r$amount <- r$amount + g$amount
  r
}

part_1_simulation <- function(n, output_type = c("table", "winner")) {
  
  froms <- c()
  tos <- c()
  
  ts <- create_table(n)
  
  while (length(ts) > 1) {
    receiver <- ts[[1]]
    giver <- ts[[2]]
    
    froms <- c(froms, giver$name)
    tos <- c(tos, receiver$name)
    
    rest <- tail(ts, n = -2)
    receiver <- give(receiver, giver)
    ts <- append(rest, list(receiver))
  }
  
  if (output_type == "table") {
    tibble(from = froms, to = tos)
  } else {
    ts
  }
}


# Here we simulate the first 1024 games (i.e. games with between 1 and 1024
# players). We are interested in which elf wins each game.
tmp <- tibble(input = 3:1024) |>
  mutate(result = map_dbl(input, \(x) part_1_simulation(x, output_type = "winner")[[1]]$name))

# The trivial game is a single player game where that player (1) wins
# immediately. As the number of players increases, the winner number increases
# at twice the rate, until we hit a power of two (when it resets, i.e. in a
# 4-player game, the winner is player 1 again).
#
# This table shows the run-length before the winner resets back to 1, which
# confirms the power of two nature.
tmp |>
  mutate(peak = result > lag(result, default = 0) & result > lead(result),
         group = cumsum(as.numeric(peak))) |>
  count(group)

# This graph shows the pattern emerging over the first 1024 games. Along with
# commentary annotations. This also shows that the task is a simple linear
# interpolation task, but the difficulty comes from needing to figure out the
# known points first.
tmp |>
  mutate(peak = result > lag(result, default = 0) & result > lead(result),
         group = cumsum(as.numeric(peak)),
         n = 1:n()) |>
  ggplot(aes(x = input, y = result)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  scale_x_continuous(breaks = 2^(4:10), minor_breaks = FALSE) +
  theme_bw() +
  annotate("text", x = 511, y = 511, label = str_wrap("inputs that are one less than powers of 2 have outputs equal to themselves", 20),
           vjust = -0.1) +
  annotate("point", x = 511, y = 511, colour = "orange") +
  annotate("text", x = 512, y = 1, label = str_wrap("inputs that are powers of 2 have outputs equal to 1", 20), vjust = 0.1, hjust = 1) +
  annotate("point", x = 512, y = 1, colour = "lightblue") +
  annotate("text", x = 1.75 * 512, y = 400, label = str_wrap("Between powers of two, the outputs are every other value, so they trace out a line y = 2x", 25))

# ---- part 2 ----

# For part 2, the rules change slightly, so we need to adapt our simulation
# function. And let's take the opportunity to do it recursively just for fun as
# I feel I've been writing more loops than I'd like recently.
part_2_simulation <- function(n) {
  inner <- function(v) {
    if (length(v) == 1) {
      return(v[[1]])
    }
    
    # We need this to model the stepping to the next player
    rot <- function(v) {
      c(tail(v, -1), head(v, 1))
    }
    
    to_remove <- ceiling((length(v)+1) / 2)
    new_v <- v[-to_remove]
    inner(rot(new_v))
  }
  inner(c(1:n))
}

# The max value here is chosen because I've already worked this out previously:
# where we were dealing with powers of 2 before, we are now looking at powers of
# 3 before the "reset".
tmp2 <- tibble(input = 3:(3^6)) |> mutate(result = map_dbl(input, \(x) part_2_simulation(x)))

# In each block (which are wider than before), we see two regimes. The first is
# a slow increasing in the winner number from 1 to a point on the line y = 0.5 *
# x (blue), then we change regime to rise up to the line y = x (red).
tmp2 |>
  mutate(peak = result > lag(result, default = 0) & result > lead(result),
         group = cumsum(as.numeric(peak)),
         n = 1:n()) |>
  ggplot(aes(x = input, y = result)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  geom_abline(slope = 0.5, intercept = 0, colour = "blue") +
  scale_x_continuous(breaks = 3^(4:6), minor_breaks = FALSE) +
  theme_bw()

library(tidyverse)

# ---- libraries and parsing ----
input <- read_file("../input/input_01.txt")

parse_input <- function(input) {
  input |>
    str_trim() |>
    str_split("\n") |>
    first() |>
    str_replace("L", "-") |>
    str_replace("R", "+") |>
    as.numeric()
}

# ---- part 1 ----
part_1 <- function(input) {
  input |>
    parse_input() |>
    accumulate(.f = \(x, y) (x + y) %% 100,
               .init = 50) |>
    keep(\(n) n == 0) |>
    length()
}

# ---- part 2 ----
count_zeros <- function(n) {
  sign(n) * floor(abs(n / 100))
}

count_zeros_between <- function(start, diff) {
  end <- start + diff
  cz_start <- count_zeros(start)
  cz_end <- count_zeros(end)
  s <- sign(diff)
  s * cz_end - s * cz_start + 1 * (sign(start + s/2) != sign(end)) # 0.5 here to avoid issues when close to zero.
}


part_2 <- function(input) {
  input <- parse_input(input)
  # state is a tuple of
  # - position on the dial
  # - the number of zeros seen so far
  init_state <- c(50, 0)

  step <- function(state, diff) {
    c((state[1] + diff) %% 100,
      state[2] + count_zeros_between(state[1], diff))
  }

  accumulate(
    input,
    .f = step,
    .init = init_state
  ) |>
  last() |>
  last()
}


# ---- Results ----
part_1(input) # 1100
part_2(input) # 6358

# ---- Alternatives and benchmarking ----
part_2_alt <- function(input) {
  input <- parse_input(input)

  cur_pos <- 50
  zeros <- 0

  for (i  in input) {
    n_zeros <- sum((seq(cur_pos + sign(i), i + cur_pos, sign(i)) %% 100) == 0)
    zeros <- zeros + n_zeros
    cur_pos <- (cur_pos + i) %% 100
  }

  zeros
}


bench::mark(main = part_2(input),
            alt = part_2_alt(input))
#   expression     min  median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory     time       gc
#   <bch:expr> <bch:t> <bch:t>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>     <list>     <list>
# 1 main        17.7ms  19.8ms     50.4    213.8KB     18.9    16     6      318ms <dbl>  <Rprofmem> <bench_tm> <tibble>
# 2 alt          123ms   123ms      8.13    20.1MB     24.4     1     3      123ms <dbl>  <Rprofmem> <bench_tm> <tibble>


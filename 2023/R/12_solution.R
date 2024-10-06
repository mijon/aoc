library(tidyverse)

test_input <- c(
  "???.### 1,1,3",
  ".??..??...?##. 1,1,3",
  "?#?#?#?#?#?#?#? 1,3,1,6",
  "????.#...#... 4,1,1",
  "????.######..#####. 1,6,5",
  "?###???????? 3,2,1")

parse_input <- function(input) {
  tibble(input = input) |>
    separate(input, into = c("template", "desc"),
             sep = " ") |>
    mutate(desc = str_split(desc, ",") |>
             map(as.numeric))
}

input <- read_lines("../input/12_input.txt")

# ---- recurrsive search ----
m <- ".??..??...?##."
d <- c(1,1,3)
msplit <- strsplit(m, "")[[1]]

# With 1,1,3, the smallest legal pattern would be #.#.### (7 characters, i.e.
# 1+1+3+1+1), so I *do* know that if we get to 6 characters left and haven't
# placed any, then we've got a dead end in our search tree.

# So let's try a partial solutions. We're going to need to come back and fix the
# issue of stopping early when we know we cannot complete, but for now, let's
# keep things simple.

gen_all_legal_placings <- function(template, block_len) {
  placings <- list()
  
  len_template <- length(template)
  for (i in 1:(len_template - block_len + 1)) {
    print(i)
    if ((i == 1 || template[i - 1] %in% c(".", "?")) &&
        template[i] == "?" &&
        all(template[(i):(i + block_len - 1)] %in% c("#", "?")) &&
        (i == (len_template - block_len + 1) || template[block_len + i ] %in% c(".", "?"))) {
      replaced <- template
      replaced[1: i - 1] <- ifelse(replaced[1: i - 1] == "?", ".", replaced[1: i - 1])
      replaced[i:(i + block_len - 1)] <- "#"
      replaced[block_len + i ] <- "."
      placings <- append(placings, list(replaced))
    }
  }
  
  placings
}

gen_all_legal_placings_faster <- function(info_list, block_len) {
  placings <- list()
  placed_pos <- list()
  template <- info_list$template
  start_pos <- info_list$next_start_pos
  
  len_template <- length(template)
  for (i in start_pos:(len_template - block_len + 1)) {
    print(i)
    if ((i == 1 || template[i - 1] %in% c(".", "?")) &&
        template[i] == "?" &&
        all(template[(i):(i + block_len - 1)] %in% c("#", "?")) &&
        (i == (len_template - block_len + 1) || template[block_len + i ] %in% c(".", "?"))) {
      replaced <- template
      replaced[1: i - 1] <- ifelse(replaced[1: i - 1] == "?", ".", replaced[1: i - 1])
      replaced[i:(i + block_len - 1)] <- "#"
      replaced[block_len + i ] <- "."
      placings <- append(placings, list(list(template = replaced,
                                        next_start_pos = i + block_len + 1)))
    }
  }
  
  placings
}

count_matches <- function(template, ds) {
  candidates <- str_split(template, "")
  
  for (d in ds) {
    candidates <- map(candidates, gen_all_legal_placings, d) |> flatten()
  }
  length(candidates)
}

count_matches_faster <- function(template, ds) {
  candidates <- list(list(template = str_split(template, "")[[1]],
                     next_start_pos = 1))
  for (d in ds) {
    candidates <- map(candidates, gen_all_legal_placings_faster, d) |> flatten()
  }
  length(candidates)
}

# ---- regex version (works but very slow) ----
# I really should have known better, since this is a "generate all
# possibilities, then find the ones that work" strategy. That's always a stupid
# strategy for AoC, but a part of me really wanted to write a solution with
# regex. Well, here it is, it's slow, but I still like it.


# Generate all possible options patterns from the garbled input, here's where a
# lot of the slowness happens. On my machine, running this on an input of
# ".?????.???" takes about 26ms.
enumerate_potentials_r <- function(template) {
  single <- function(template, results = c()) {
    if (!str_detect(template, "\\?")) {
      return(template)
    }
    
    f <- str_replace(template, "\\?", "#")
    e <- str_replace(template, "\\?", ".")
    c(results, single(f), single(e))
  }
  single(template)
}

# Generate a regex string that represents the correct run of values
gen_regex <- function(counts) {
  core <- map_chr(counts, \(c) strrep("#", c)) |> paste(collapse = "\\.+")
  paste0("^\\.*", core, "\\.*$")
}

count_matches_regex <- function(template, d) {
  possibilities <- enumerate_potentials_r(template)
  test_pattern <- gen_regex(d)
  str_detect(possibilities, test_pattern) |>
    sum()
}

part_1_regex <- function(input, counter = count_matches_regex) {
  input |>
    parse_input() |>
    mutate(counts = map2_dbl(template, desc, counter, .progress = TRUE)) |>
    pull(counts) |>
    sum()
}

# Only uncomment this if you have time to waste.
#part_1_regex(input) # 7541


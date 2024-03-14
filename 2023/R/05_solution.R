library(tidyverse)

input <- read_lines("../input/05_input.txt")

# ---- part 1 ----

rest <- function(v) {
  v[2:length(v)]
}

parse_input <- function(ls) {
  list(seeds = parse_seeds(ls[1]),
       maps = parse_maps(rest(ls), make_map_fn))
}

parse_num_group <- function(g) {
  g |> 
    str_split(" ") |>
    first() |>
    as.numeric()
}

parse_seeds <- function(s) {
  s |> 
    str_remove("seeds: ") |>
    parse_num_group()
}

parse_maps <- function(ls, process_fn) {
  ls |>
    split(cumsum(ls == "")) |>
    map(\(x) discard(x, \(y) y == "")) |>
    map(process_fn)
}

gen_map_dfs <- function(m) {
  tibble(tmp = rest(m)) |>
    separate(tmp,
             into = c("dst_start", "src_start", "width"),
             sep = " ",
             convert = TRUE) |>
    mutate(dst_end = dst_start + width,
           src_end = src_start + width) 
}

# the map can be represented as a function of n -> n, i.e. the map defines a
# function that will take an integer and return an integer. This function here,
# just creates that function from the given information.
make_map_fn <- function(m) {
  m <- gen_map_dfs(m)
  
    function(n) {
      trans_table <- m |>
        filter(n < src_end & n >= src_start)
      if(nrow(trans_table) == 0) {
        n
      } else if (nrow(trans_table) == 1) {
        (n - trans_table$src_start) + trans_table$dst_start
      } else {
        stop("broken")
      }
    }
}


part_1 <- function(input) {
  parsed_input <- parse_input(input)
  all_the_maps <- compose(!!!(parsed_input$maps), .dir = "forward")
  map_dbl(parsed_input$seeds, all_the_maps) |>
    min()
}

# ---- part 2 ----

# I'm coming to this a few months later. The plan is to rewrite a lot of the
# code to handle *pairs* of values that delineate a range. In fact, our
# functions will handle lists of pairs so we can propagate multiple ranges at
# once. The intention will be to take in a single input pair, break it into any
# number of pairs that represent a subregion of the main pair. These sub regions
# are chosen such that they are mapped continuously to the next stage.
#
# The algorithm will be to consider a pair denoting a start and stop of a run of
# seeds. We only then need to propagate through the almanac the details of the
# stop and start positions of the ranges. The wrinkle is that our range may span
# two blocks of the mapping, i.e. the start few in the range might get mapped to
# an area separate from the last few. To solve this we can simply break out
# input range pair by the separately mapped parts of the mapping table.
# Diagrammatically:
#
#    Input:                       o----------------o
#                                 |                |
#                                 v                v
#    Mapping src level:    <-----------><-------------->
#                                 |    ||          |
#                                 v    vv          v
#    Resultant new input:         0----00----------0
#
# Hope that's clear, it's crystal to me :)

# We start by modifying slightly the original parsing function.
parse_input2 <- function(ls) {
  list(pairs = parse_seeds2(ls[1]),
       maps = parse_maps(rest(ls), make_map_fn),
       map_range_pairs = parse_maps(rest(ls), make_map_range_fn)) # <--
  # this is the new item, we're tracking the left and right extents of each
  # mapped region, both the src and the dst parts.
}

# helper to drop n items from a list
drop_n <- function(v, n) {
  if (length(v) <= n) {
    vector(mode = mode(v))
  } else {
    v[(n+1):length(v)]
  }
}

# convert a vector of elements into a list of pairs. This function doesn't work
# for vectors that aren't of an even length, but we're not going to encounter
# those in this instance, and quite frankly, I've spent too long on this puzzle
# to code a more robust solution.
pair <- function(v = vector(mode = mode(v)), l = list()) {
  if (length(v) == 2) {
    append(l, list(v))
  } else {
    pair(drop_n(v, 2), append(l, list(c(v[1], v[2]))))
  }
}

parse_seeds2 <- function(s) {
  s |> 
    str_remove("seeds: ") |>
    str_split(" ") |> first() |>
    as.numeric() |>
    pair() |>
    map(\(x) {c(x[1], x[1] + x[2] - 1)}) # This is the part 2 naming convention
}

# Here we want to take the input ranges like "50 98 2" and return a list of
# pairs demarcating the range that is mapped, i.e. this would produce 
# (98,99) -> (50,51)
make_map_range_fn <- function(m) {
  rest(m) |>
    str_split(" ") |>
    map(as.numeric) |>
    map(\(v) list(src = c(start = v[2], stop = v[2] + v[3] - 1),
                  dst = c(start = v[1], stop = v[1] + v[3] - 1)))
}


# Here's a load of helper functions to get bits of vectors or lists of pairs.
lefts  <- function(splits) {map_dbl(splits, 1)}
rights <- function(splits) {map_dbl(splits, 2)}
srcs <- function(map_range_pairs) {map(map_range_pairs, "src")}
odds <- function(v) {v[seq(1,length(v), 2)]}
evens <- function(v) {v[seq(2,length(v),2)]}
contained <- function(v, start, stop) {
  v[v <= stop & v >= start]
}

# v is a pair denoting a range, splits is the sub ranges we want to align on.
# This function implements what I 'drew' out above. It's a mess and I hate it.
partition_with_splits <- function(v, splits) {
  splits <- splits |> discard(\(x) x[2] < v[1]) |> discard(\(x) x[1] > v[2])
  edges <- c(v, contained(c(lefts(splits), rights(splits)), v[1], v[2])) |> sort()
  if (v[1] %in% lefts(splits)) {edges <- c(v[1], edges)}
  if (v[2] %in% rights(splits)) {edges <- c(edges, v[2])}
  if (v[1] < min(lefts(splits))) {edges <- c(edges, min(lefts(splits)) - 1)}
  if (v[2] > max(rights(splits))) {edges <- c(edges, max(rights(splits)) + 1)}
  if (v[1] > max(rights(splits)) || v[2] < min(lefts(splits))) {edges <- v}
  edges <- sort(edges)
  if(length(edges) %% 2 != 0) {
    browser()
  }
  map2(odds(edges), evens(edges), \(x, y) {c(start = x, stop = y)})
}

# like `rest` from above, but with more care for corner cases. Essentially drop
# the first element of a list.
rest_list <- function(l) {
  if (length(l) == 1) {
    list()
  } else if (length(l) == 2) {
    l[2]
  } else {
    l[2:length(l)]
  }
}

# Given a single input pair, run it through the full tower of ranges. Only ever
# return the minimum value found at the end of the tower.
process_single_pair <- function(input_pair, maps, map_range_pairs) {
  if (length(map_range_pairs) == 0) {
    return(min(lefts(input_pair)))
  }
  
  broken_inputs <- partition_with_splits(input_pair, srcs(map_range_pairs[[1]]))
  mapped_inputs <- map(broken_inputs, \(x) map_dbl(x, maps[[1]]))
  
  min(map_dbl(mapped_inputs, process_single_pair, rest_list(maps), rest_list(map_range_pairs)))
}


part_2 <- function(input) {
  input <- parse_input2(input)
  min(map_dbl(input$pairs, process_single_pair, input$maps, input$map_range_pairs))
}


# ---- evaluations ----
part_1(input) # 484023871
part_2(input) # 46294175

library(tidyverse)
options(digits = 22)
INPUT_PATH <- "../input/09_input.txt"

#
# I have lost all desire to compete for speed, so now I compete for needless
# complexity.

example_data <- "2333133121414131402" 

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

# ---- Utils ----

evens <- function(v) {
  v[(1:floor(length(v)/2) * 2)]
}

odds <- function(v) {
  v[(1:ceiling(length(v)/2) * 2) - 1]
}

# R doesn't have a native `deque` data structure, so we can jank one together
# using the lexical scoping rules of functions. Given any R data structure that
# implements `head` and `tail`, we can make a new `deque` data structure that
# allows popping off the front or back using the following code. `a <-
# deque(1:10)` creates a value `a` that's actually a list of three functions,
# `get()` gives you the value of the list as it is currentlly, `pop_front()`
# gives you the first element, while also removing it from the front of the
# list, a subsequent `get()` will show you that's the case. `pop_back()` removes
# from the end.
deque <- function(values) {
  
  env <- new.env()
  assign("values", value = values)
  
  give <- function() {
    get("values", env)
  }
  
  pop_front <- function(n = 1) {
    values <- give()
    output <- head(values, n)
    assign("values", value = tail(values, -n), envir = env)
    output
  }
  
  pop_back <- function(n = 1) {
    values <- give()
    output <- tail(values, n)
    assign("values", value = head(values, -n), envir = env)
    rev(output)
  }
  
  push_front <- function(x) {
    new <- append(x, give())
    assign("values", value = new, envir = env)
  }
  
  push_back <- function(x) {
    new <- append(give(), x)
    assign("values", value = new, envir = env)
  }
  
  len <- function() {
    length(give())
  }

  
  output <- list(
    get = give,
    pop_front = pop_front,
    pop_back = pop_back,
    push_front = push_front,
    push_back = push_back,
    len = len
  )
  class(output) <- "deque"
  output
}

print.deque <- function(d) {
  print(d$get())
}


parse_input <- function(input) {
  nums <- str_split(input, "")[[1]]
  files <- odds(nums) |> as.numeric()
  frees <- evens(nums) |> as.numeric()
  ids <- seq_along(files) - 1
  
  list(files = deque(files),
       frees = deque(frees),
       ids = ids,
       expanded = deque(rep(ids, files)),
       part_1_length = sum(as.numeric(files)))
}

expand_block <- function(length, id) {
  rep(id, length)
}

# ---- Working ----
# What we are really doing is  nibbling the file length off the front, then
# nibbling the gap length off the back, then the file length off the front, then
# the gap length off the back etc. until the we've used up all the file blocks.
process <- function(parsed) {
  files <- parsed$files
  frees <- parsed$frees
  ids <- parsed$ids
  expanded <- parsed$expanded
  
  vec_len <- parsed$part_1_length
  output_vec <- numeric(length = vec_len)
  
  pointer <- 1
  while (expanded$len() > 0) {
    n_files <- files$pop_front()
    output_vec[pointer:min(vec_len, pointer + n_files - 1)] <- expanded$pop_front(min(n_files, expanded$len()))
    pointer <- pointer + n_files
    
    # early exit in case we run out of files before running out of gaps
    if(expanded$len() == 0) {break}
    
    n_blanks <- frees$pop_front()
    if (n_blanks != 0) {
      output_vec[pointer:min(vec_len, pointer + n_blanks - 1)] <- expanded$pop_back(min(n_blanks,expanded$len()))
      pointer <- pointer + n_blanks
    }
  }  
  output_vec
}

checksum <- function(v) {
  v |>
    imap(\(x, i) c(x, i - 1)) |>
    map_dbl(prod) |>
    sum()
}

part_1 <- function(input) {
  parse_input(input) |>
    process() |>
    checksum()
}


part_2 <- function(input) {
  parse_input(input)
}


# ---- Results ----
part_1(input) # 6283404590840
part_2(input) # 
# ---- About ----
#
# In the continues spirit of over-engineering, we're going to have a parser
# function for each instruction, then a mechanism to select the right parser.
# The parsing function produces a function out the end that does the
# instruction. We end up with a load of functions we can just feed our password
# through to scramble it.
#
# I'm not super pleased about this one, I think it could be improved.

library(tidyverse)
INPUT_PATH <- "../input/21_input.txt"

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)
password <- letters[1:8]

# For each type of instruction we have a regular expression that will extract
# out what we want (always either a digit or a letter).
templates <- list(
  swap_pos = "swap position (\\w) with position (\\w)",
  swap_letter = "swap letter (\\w) with letter (\\w)",
  rotate = "rotate (\\w+) (\\w) step[s]?",
  rotate_based = "rotate based on position of letter (\\w)",
  reverse = "reverse positions (\\w) through (\\w)",
  move = "move position (\\w) to position (\\w)"
)


# ---- parser functions ----
# Then we need to actually write all the parsing functions.


parse_swap_pos <- function(instruction) {
  positions <- str_extract(
    string = instruction,
    pattern = templates$swap_pos,
    group = c(1,2)) |>
    as.numeric() |>
    map_dbl(\(x) x + 1) # AoC is zero-based
  
  function(password) {
    tmp <- password[[positions[1]]]
    password[positions[1]] <- password[positions[2]]
    password[positions[2]] <- tmp
    password
  }
}

parse_swap_letter <- function(instruction) {
  letters <- str_extract(
    string = instruction,
    pattern = templates$swap_letter,
    group = c(1,2))
  
  function(password) {
    # positions <- which(password == letters)
    positions <- which(password %in% letters)
    tmp <- password[[positions[1]]]
    password[positions[1]] <- password[positions[2]]
    password[positions[2]] <- tmp
    password
  }
}

parse_rotate <- function(instruction) {
  directions <- str_extract(
    string = instruction,
    pattern = templates$rotate,
    group = c(1,2))
  
  function(password) {
    rotate(password, as.numeric(directions[2]) * (-1)^(directions[1] == "right"))
  }
}

parse_rotate_based <- function(instruction) {
  letter <- str_extract(string = instruction, pattern = templates$rotate_based,
                          group = 1)
  function(password) {
    position <- which(password == letter) - 1
    n <- 1 + position + 1 * (position >= 4)
    rotate(password, -n)
  }
}

parse_reverse <- function(instruction) {
  positions <- str_extract(
    string = instruction, pattern = templates$reverse,
    group = c(1,2)) |>
    as.numeric() |> 
    map_dbl(\(x) x + 1)
  function(password) {
    password[positions[1]:positions[2]] <- password[positions[2]:positions[1]]
    password
  }
}

parse_move <- function(instruction) {
  positions <- str_extract(
    string = instruction,
    pattern = templates$move,
    group = c(1,2)) |>
    as.numeric() |>
    map_dbl(\(x) x + 1) # AoC is zero-based
  
  function(password) {
    letter <- password[positions[1]]
    password <- password[-positions[1]]
    append(password, letter, after = positions[2] - 1)
  }
}

# n >0 => rotate left
rotate <- function(l, n) {
  if (n %% length(l) == 0) {
    return(l)
  }
  n <- n %% length(l)
  left <- l[1:n]
  right <- l[(1+n):length(l)]
  append(right, left)
}

parse_single <- function(instruction) {
  instruction_matches <- map_lgl(templates, \(t) str_detect(instruction, t))
  instruction_type <- instruction_matches[instruction_matches] |> names()
  parsers[[instruction_type]](instruction)
}

parse_input <- function(input) {
  input |>
    map(parse_single)
}


# We will want to select a particular function later one, so we're going to use
# a named list of functions so we can go from a character string to function
# with a simple `[` lookup.
parsers <- list(
  swap_pos = parse_swap_pos,
  swap_letter = parse_swap_letter,
  rotate = parse_rotate,
  rotate_based = parse_rotate_based,
  reverse = parse_reverse,
  move = parse_move
)

# ---- Working ----
scramble <- function(instructions, password) {
  reduce(instructions, \(pword, inst) inst(pword), .init = password) |> paste0(collapse = "")
}

part_1 <- function(input, password) {
  instructions <- parse_input(input)
  scramble(instructions, password)
}

# I was thinking that we could do something fancy and invert all the functions,
# but some aren't reversible. i.e. while some of the swap positions are the same
# forward as they are backwards, some others (like the rotate based on the
# position of some character) are one way: you can't tell from the output what
# the input was. So we just take the coward's way out and generate all possible
# input passwords and then scramble them in a forward direction until we get the
# target.
part_2 <- function(input, password, target) {
  tictoc::tic()
  instructions <- parse_input(input)
  
  all_passwords <- combinat::permn(password)
  
  for (p in all_passwords) {
    if (scramble(instructions, p) == target) {
      tictoc::toc()
      return(paste0(p, collapse = ""))
    }
  }
  return(FALSE)
}


# ---- Results ----
part_1(input ,password) # baecdfgh
part_2(input, password, target = "fbgdceah") # cegdahbf , executes in ~10s 

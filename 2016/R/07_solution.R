# ---- About ----
#
# This one's a all about character mapping, but it's not one for which regex is
# particularly suitable. We first need to parse the input that can have any
# number of supernet sequences (alphabetic codes outside square brackets) and
# hypernet sequences (alphabetic codes within square brackets).
#
# Because we need to enforce requirements, e.g. that the first and last at the
# same and differen from the second and third, we need to work through the
# strings character by character.

library(tidyverse)
INPUT_PATH <- "../input/07_input.txt"

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

parse_ip <- function(ip) {
  list(
    supernet_pieces = ip |> str_replace_all(
      pattern = "\\[[a-z]+\\]", "|") |>
      str_split("\\|") |>
      first(),
    hypernet_pieces = str_extract_all(
      ip, 
      pattern = "\\[[a-z]+\\]") |>
      first()
  )
}

parse_input <- function(input) {
  input |>
    map(parse_ip)
}

# ---- Working ----
has_abba <- function(s) {
  s <- str_split(s, "")[[1]]
  for (i in 1:(length(s)-3)) {
    if (all(c(s[i], s[i+1]) == rev(c(s[i+2], s[i+3]))) & s[i] != s[i+1]) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Are there any abbas on the outside, are there any abbas on the inside, then
# finally is there an abba on the outside and no abbas on the inside? (outside
# being the supernet, inside being the hypernet).
part_1_conditions <- function(parsed_ip) {
  has_abba_outside <- map(parsed_ip$supernet_pieces,
                          .f = has_abba) |>
    reduce(`||`)
  
  has_abba_inside <- map(parsed_ip$hypernet_pieces,
                         .f = has_abba) |>
    reduce(`||`)
  
  has_abba_outside & !has_abba_inside
}

# Part 2 is a little more involved in the condition state, but first, we need to
# find all the possible aba sequences, we're no longer content just to know
# there is at least one.
get_aba <- function(s) {
  s <- str_split(s, "")[[1]]
  abas <- c()
  for (i in 1:(length(s)-2)) {
    if (s[i] == s[i + 2] & s[i] != s[i + 1]) {
      abas <- c(abas, paste(s[i], s[i+1], s[i+2], sep = ""))
    }
  }
  abas
}

# Convenience function to flip the aba to a bab.
aba_to_bab <- function(s) {
  paste(
    str_sub(s, 2, 2),
    str_sub(s, 1, 1),
    str_sub(s, 2, 2),
    sep = ""
  )
}

# Don't really need to do this, but it's nice to be clear what we're doing I
# guess.
check_for_bab <- str_detect

# This needs some care as there may be more than one supernet, each of which
# might give more than one ABA. Then we need to flip them all. Then there might
# be more than one hypernet, so we need to check all the BABs against all the
# hypernets. Something like an outer product and then a 2D reduce would be a
# nice way to do this, but for now, we'll do a load of `map`s and `reduce`s.
part_2_conditions <- function(parsed_ip) {
  abas <- map(parsed_ip$supernet_pieces,
                  get_aba) |>
    discard(is.null)
  
  if (length(abas) == 0) {
    return(FALSE)
  }
  
  babs <- abas |>
    flatten() |> as.character() |>
    map_chr(aba_to_bab)
  
  map(parsed_ip$hypernet_pieces,
      \(x) map_lgl(babs, \(y) check_for_bab(x, y))) |>
    map(reduce, `||`) |>
    reduce(`||`)
}


# ---- Part 1 ----
process <- function(conditions) {
  function(input) {
    parse_input(input) |>
      map_lgl(conditions) |>
      sum()
  }
}

part_1 <- process(part_1_conditions)
part_2 <- process(part_2_conditions)

# ---- Results ----
part_1(input) # 115
part_2(input) # 231

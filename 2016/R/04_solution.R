# ---- About ----
#
# We're decrypting things today. Part 1 is relatively simple: We parse the input
# into the three pieces (code, sector_id, checksum), then generate our own
# checksum and count matches.
#
# For part 2, we just need to implement a shift cipher and then find a
# particular string in the plaintext.

library(tidyverse)
INPUT_PATH <- "../input/04_input.txt"

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

# Most of Part 1 actually happens in the parsing stage
parse_input <- function(input) {
  tibble(input = input) |>
    mutate(
      name = str_extract(input, pattern = "^[a-z\\-]+") |>
        str_replace_all("-", ""),
      sector_id = str_extract(input, pattern = "[0-9]+") |>
        as.numeric(),
      checksum = str_extract(input, pattern = "\\[[a-z]+\\]$") |>
        str_replace_all("\\[", "") |>
        str_replace_all("\\]", ""),
      generated_checksum = map_chr(name, gen_checksum))
}

# Generating the checksum is a case of counting the occurrences of each element.
# I use R's built in `letters` vector to count all possible letters, remove the
# zeros and sort.
gen_checksum <- function(s) {
  stringr::str_count(s, letters) |>
    purrr::set_names(letters) |>
    # purrr::keep(\(x) x > 0) |>
    sort(decreasing = TRUE) |>
    names() |>
    paste(collapse = "") |>
    stringr::str_sub(1,5)
}

# ---- Part 1 ----
part_1 <- function(input) {
  parse_input(input) |>
    filter(checksum == generated_checksum) |>
    pull(sector_id) |>
    sum()
}

# ---- Part 2 ----

# Shifting the letters is a simple modulo shift, taking some care over the case
# when we shift by 26.
shift_letter <- function(s, n) {
  target_ind <- (which(letters == s) + n) %% length(letters)
  if (target_ind == 0) {
    "z"
  } else {
    letters[target_ind]
  }
}

# Apply the shift as given by `n`.
decode_name <- function(name, n) {
  name |> 
    stringr::str_split("") |>
    dplyr::first() |>
    purrr::map_chr(shift_letter, n = n) |>
    paste(collapse = "")
}

# Okay, I generated them all, pasted them into an Excel and found the only one
# that mentioned North Pole then came back and edited the code...
part_2 <- function(input) {
  input |>
    parse_input() |>
    dplyr::filter(checksum == generated_checksum) |> # only look at actual rooms.
    dplyr::mutate(sector_id = as.numeric(sector_id),
                  room_name = purrr::map2_chr(name, sector_id, decode_name)) |>
    dplyr::filter(room_name == "northpoleobjectstorage") |>
    dplyr::pull(sector_id)
}

# ---- Results ----
part_1(input) # 185371
part_2(input) # 984

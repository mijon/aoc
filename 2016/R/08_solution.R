# ---- About ----
#
# In this puzzle, we need to parse some instructions, then make changes to some
# grid. The instructions take one of three possible choices:
# - draw a rectangle in the top left
# - rotate a row
# - rotate a col
#
# We will use a matrix to model the screen, drawing a rectangle is simply
# writing to the matrix (we're not toggling, so can just overwrite). Shifting
# can be done with the `DescTools::VecRot`

library(tidyverse)
INPUT_PATH <- "../input/08_input.txt"

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

# The input is the list of instructions. As all the instructions represent
# things done to a matrix, we can parse them into functions that take a matrix.
# Further below we define what these functions actually do (and the parameters
# they take), but right now we're just interested in the transformation from
# instruction string -> function.
#
# Ultimately we want to end up with a list of single value functions (matrix ->
# matrix) that we can `reduce` along, but the instructions have numerical
# parameters. To get round this, we will partially apply the instruction
# functions to 'bake in' the instruction parameters, thus each instruction line
# gets its own unique matrix -> matrix function that does exactly what the
# instruction requires.

parse_rect <- function(s) {
  nums <- str_extract(s, "[0-9]+x[0-9]+") |>
    str_split("x") |>
    first() |>
    as.numeric()
  partial(rect, a = nums[1], b = nums[2])
}

parse_rot_col <- function(s) {
  col <- str_extract(s, "(?<=x=)[0-9]+") |> as.numeric()
  shift <- str_extract(s, "(?<=by )[0-9]+") |> as.numeric()
  partial(rot_col, col = col, shift = shift)
}

parse_rot_row <- function(s) {
  row <- str_extract(s, "(?<=y=)[0-9]+") |> as.numeric()
  shift <- str_extract(s, "(?<=by )[0-9]+") |> as.numeric()
  partial(rot_row, row = row, shift = shift)
}

parse_input <- function(input) {
  selector <- function(s) {
    # Possible room to improve here in terms of style, but it's fine for now.
    if (str_detect(s, "rect")) return(parse_rect(s))
    if (str_detect(s, "row")) return(parse_rot_row(s)) 
    if (str_detect(s, "col")) return(parse_rot_col(s)) 
  }
  map(input, selector)
}

# ---- Working ----
# This doesn't *need* to be a function, but I think it makes the code more
# readable later on.
gen_screen <- function(w = 50, h = 6) {
  matrix(FALSE, nrow = h, ncol = w)
}

rect <- function(mat, a, b) {
  mat[1:b, 1:a] <- TRUE
  mat
}

rot_row <- function(mat, row, shift) {
  mat[row + 1, ] <- DescTools::VecRot(mat[row + 1, ], shift)
  mat
}

rot_col <- function(mat, col, shift) {
  mat[, col + 1] <- DescTools::VecRot(mat[, col+ 1], shift)
  mat
}

# ---- part 1 ----
# We have the parsed input as a list of functions, so if we reduce these all
# together using the `compose` function from purrr, then we go from a list of
# matrix -> matrix functions into a single matrix -> matrix function that
# carried out *all* of the instructions.
apply_all_instructions <- function(instructions) {
  do_all_instructions <- reduce(rev(instructions), compose)
  gen_screen() |>
    do_all_instructions()
}

part_1 <- function(input) {
  parse_input(input) |>
    apply_all_instructions() |>
    sum()
}

# ---- part 2 ----
# We have already got the machinery to generate the matrix that we want, the
# rest is all munging the matrix and plotting code in ggplot2.
part_2 <- function(input) {
  result <- input |>
    parse_input() |>
    apply_all_instructions()
  colnames(result) <- 1:ncol(result)
  
  as_tibble(result, .name_repair = "minimal") |>
    mutate(row = 1:n()) |>
    pivot_longer(cols = -row, names_to = "col") |>
    mutate(col = as.numeric(col)) |>
    ggplot(aes(x = col, y = row, fill = value)) +
    geom_tile() +
    coord_fixed() +
    scale_y_reverse() +
    scale_fill_manual(values = c("TRUE"="black", "FALSE" = "white")) +
    theme_void() + theme(legend.position = "none")
}


# ---- Results ----
part_1(input) # 123
part_2(input) # AFBUPZBJPS

library(tidyverse)

test_input1 <- c(
  "-L|F7",
  "7S-7|",
  "L|7||",
  "-L-J|",
  "L|-JF")

test_input2 <- c(
  "7-F7-",
  ".FJ|7",
  "SJLL7",
  "|F--J",
  "LJ.LJ")

input <- read_lines("../input/10_input.txt")

# ---- part 1 ----
lines_to_mat <- function(ls) {
  ls |>
    str_split("") |>
    stringi::stri_list2matrix(byrow = TRUE)
}
# position is a (row, col) pair, heading is a (-1,0,1) valued pair, being the
# vector you add to position, essentially a velocity
new_mouse <- function(pos, heading, history = list()) {
  list(p = pos,
       h = heading,
       history = history)
}

change_heading <- function(current_heading, char) {
  flip <- function(v) c(v[[2]], v[[1]])
  flip_neg <- function(v) -flip(v)
  
  if (char %in% c("-", "|", "S")) {
    current_heading
  } else if (char %in% c("7", "L")) {
    flip(current_heading)
  } else if (char %in% c("F", "J")) {
    flip_neg(current_heading)
  }
}

update_mouse <- function(mouse, input_mat) {
  cur_pos <- mouse$p
  updated_history <- append(mouse$history, list(cur_pos))
  char <- input_mat[cur_pos[1], cur_pos[2]]
  new_heading <- change_heading(mouse$h, char)
  new_mouse(pos = cur_pos + new_heading, new_heading, updated_history)
}

find_point <- function(mat, label) {
  which(mat == label, arr.ind = TRUE) |>
    as.numeric() |>
    set_names(c("row", "col"))
}

choose_starting_heading <- function(point, mat) {
  pr <- point["row"]
  pc <- point["col"]
  
  tibble(
    direction = c("north", "east", "south", "west"),
    heading = list(c(-1,0), c(0,1), c(1,0), c(0,-1)),
    char = c(mat[pr - 1, pc], mat[pr, pc + 1], mat[pr + 1, pc], mat[pr, pc - 1])
  ) |>
    filter(case_when(
      direction == "north" ~ char %in% c("7", "|", "F"),
      direction == "east"  ~ char %in% c("J", "-", "7"),
      direction == "south" ~ char %in% c("J", "|", "L"),
      direction == "west"  ~ char %in% c("L", "-", "F"),
    )) |>
    pull(heading) |>
    first()
}

set_up_mouse <- function(input_mat) {
  starting_point <- find_point(input_mat, "S")
  starting_heading <- choose_starting_heading(starting_point, input_mat)
  new_mouse(starting_point, starting_heading)
}

run_mouse <- function(input_mat) {
  # input_mat <- lines_to_mat(input)
  mouse <- set_up_mouse(input_mat)
  starting_point <- mouse$p
  
  while (!(identical(mouse$p, starting_point) && length(mouse$history) > 1)) {
    # print(mouse$p)
    mouse <- update_mouse(mouse, input_mat)
  }
  mouse
}

part_1 <- function(input) {
  input_mat <- lines_to_mat(input)
  mouse_run <- run_mouse(input_mat)
  length(mouse_run$history) / 2
}




# ---- evaluations ----
part_1(input) # 6815

library(tidyverse)

read_input <- function(file) {
  tab <- read_csv(file, col_names = "lines") 
  
  output <- tab %>%
    mutate(lines = str_split(lines, ""),
           lines = map(lines, ~set_names(.x, paste0("x", 1:nrow(tab))))) %>% 
    unnest_wider(lines, transform = as.numeric) %>%
    as.matrix()
  
  unname(output)
}

# part 1
check_a_line <- function(line) {
  len <- length(line)
  line > accumulate(line, max, .init = 0)[1:len]
}

check_rows <- function(m, f) {
  blank_mat <- matrix(logical(), nrow = nrow(m), ncol = ncol(m))
  
  for (i in 1:nrow(m)) {
    blank_mat[i,] = f(check_a_line(f(m[i,])))
  }
  blank_mat
}

check_cols <- function(m, f) {
  blank_mat <- matrix(logical(), nrow = nrow(m), ncol = ncol(m))
  
  for (j in 1:ncol(m)) {
    blank_mat[,j] = f(check_a_line(f(m[,j])))
  }
  blank_mat
}


part_1 <- function(m) {
  m <- m + 1
  reduce(
    list(
      check_rows(m, identity),
      check_rows(m, rev),
      check_cols(m, identity),
      check_cols(m, rev)
    ),
    `|`
  ) %>%
    sum()
}

# part 2
find_viewing_distance <- function(curr_tree_height, viewing) {
  scan <- match(TRUE, viewing >= curr_tree_height)
  if_else(is.na(scan), length(viewing), scan)
}

handle_range <- function(at, end, offset) {
  if(at == end) {0} else {(at + offset):end}
}

part_2 <- function(m) {
  viewing_mat <- matrix(0, nrow = nrow(m), ncol = ncol(m))
  m <- m + 1
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      curr_tree_height <- m[i,j]
      
      left  <- find_viewing_distance(curr_tree_height, m[i,handle_range(j, 1, -1)])
      right <- find_viewing_distance(curr_tree_height, m[i,handle_range(j, ncol(m), 1)])
      up    <- find_viewing_distance(curr_tree_height, m[handle_range(i, 1, -1),j])
      down  <- find_viewing_distance(curr_tree_height, m[handle_range(i, nrow(m), 1),j])
      viewing_mat[i,j] <- left * right * up * down
    }
  }
  viewing_mat %>% max()
}


input <- read_input("../input/8_input.txt")
part_1(input) # 1845
part_2(input) #230112

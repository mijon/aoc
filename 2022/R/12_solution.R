library(tidyverse)


read_heightmap <- function(path) {
  lines <- read_lines(path)
  
  letter_mat <- lines %>%
    str_split(pattern = "") %>%
    flatten() 
  
  height_map <- str_replace(letter_mat, "S", "a") %>%
    str_replace("E", "z") %>%
    map(~which(letters == .x)) %>%
    matrix(nrow = length(lines), byrow = TRUE)
  
  letter_mat <- letter_mat %>%
    matrix(nrow = length(lines), byrow = TRUE)
  
  list(height_map = height_map,
       start_pos = which(letter_mat == "S", arr.ind = TRUE),
       end_pos = which(letter_mat == "E", arr.ind = TRUE))
}


init_num_steps <- function(height_map, end_pos) {
  tmp <- matrix(NA,
         nrow = nrow(height_map),
         ncol = ncol(height_map))
  tmp[end_pos] <- 0
  tmp
}

update_num_steps <- function(num_steps, stepcount, frontier) {
  for (pos in frontier) {
    num_steps[pos[[1]], pos[[2]]] <- stepcount
  }
  num_steps
}

rows_to_list <- function(m) {
  as.list(as.data.frame(t(m))) %>%
    map(as.numeric)
}

gen_candidate_frontier <- function(num_steps, heightmap) {
  not_na <- which(!is.na(num_steps), arr.ind = TRUE) %>% rows_to_list()
  max_steps <- which(num_steps == max(num_steps, na.rm = TRUE), arr.ind = TRUE) %>% rows_to_list()
  
  gen_neighbours <- function(pair) {
    list(c(pair[[1]] - 1, pair[[2]]),
         c(pair[[1]] + 1, pair[[2]]),
         c(pair[[1]],     pair[[2]] + 1),
         c(pair[[1]],     pair[[2]] - 1))
  }
  
  get_height <- function(pair) {
    heightmap[pair[1], pair[2]][[1]]
  }
  
  out_of_bounds <- function(pair) {
    pair[[1]] < 1 ||
      pair[[1]] > nrow(heightmap) ||
      pair[[2]] < 1 ||
      pair[[2]] > ncol(heightmap)
  }
  
  discard_oob <- function(candidate) {
    candidate$neighbours <- discard(candidate$neighbours, out_of_bounds)
    candidate
  }
  
  discard_visited <- function(candidate) {
    candidate$neighbours <- discard(candidate$neighbours, \(x) list(x) %in% not_na)
    candidate
  }
  
  test <- map(max_steps,
      ~list(curr_space = .x,
            curr_height = get_height(.x),
            neighbours = gen_neighbours(.x))) %>%
    map(discard_oob) %>%
    map(discard_visited) %>%
    map(\(x) {x$neighbour_heights = map(x$neighbours, get_height); x})
  
  test
}

prune_frontier <- function(candidate_frontier, heightmap, curr_level) {
  drop_too_big <- function(candidate) {
    candidate$neighbours[candidate$neighbour_heights >= candidate$curr_height - 1]
  }
  
  map(candidate_frontier, drop_too_big) %>%
    flatten() %>%
    unique()
}

check_start_found <- function(frontier, start) {
  rows_to_list(start) %in% frontier
}

gen_step_count <- function(input) {
  num_steps <- init_num_steps(input$height_map, input$end_pos)
  curr_step <- 0
  curr_level <- 26
  
  while (TRUE) {
    frontier <- gen_candidate_frontier(num_steps, input$height_map) %>%  
      prune_frontier(input$height_map, curr_level)  
    
    curr_step <- curr_step + 1
    
    num_steps <- update_num_steps(num_steps, curr_step, frontier)
    curr_level <- input$height_map[[frontier[[1]][[1]], frontier[[1]][[2]]]]
    
    if (check_start_found(frontier, input$start_pos)) {   
      return(num_steps)
    }
  }
}

part_1 <- function(start_pos, step_count) {
  step_count[start_pos]
}

part_2 <- function(heightmap, step_count) {
  min(step_count[which(heightmap == 1, arr.ind = TRUE)], na.rm = TRUE)
}


# ---- Run ----
# Today, as the sample data was so instrumental to solving this, I'll keep it in
# the solution
sample_input <- read_heightmap("../input/12_sample.txt")
sample_step_count <- gen_step_count(input = sample_input)
sample_result <- part_1(start_pos = sample_input$start_pos,
                        step_count = sample_step_count)

# But for real:
input <- read_heightmap("../input/12_input.txt")
step_count <- gen_step_count(input)

part_1(start_pos = input$start_pos,
       step_count = step_count) # 520
part_2(input$height_map, step_count) # 508


# ---- Bonus ---- 
# This section has a function to generate a candidate path from the step count
# (because many paths are available while traversing over flat ground). It then
# plots this candidate path against the height map.
#
# There's a better way to do this, but I wrote it while debugging and I don't
# have the energy to make it better.
gen_path <- function(step_count, input) {
  curr_row <- input$start_pos[1,"row"]
  curr_col <- input$start_pos[1,"col"] 
  
  rows <- numeric(length = 1000)
  cols <- numeric(length = 1000)
  steps <- numeric(length = 1000)
  
  ind <- 1
  
  while(TRUE) {
    
    rows[ind] <- curr_row
    cols[ind] <- curr_col
    
    curr_step <- step_count[curr_row, curr_col]
    steps[ind] <- curr_step
    
    if (curr_step == 0) {
      break
    }
    
    # This bit in particular is horrible
    if (curr_row - 1 > 0 && !is.na(step_count[curr_row - 1, curr_col]) && step_count[curr_row - 1, curr_col] == curr_step - 1) {
      curr_row <- curr_row - 1
    } else if (curr_row + 1 <= nrow(step_count) && !is.na(step_count[curr_row + 1, curr_col]) && step_count[curr_row + 1, curr_col] == curr_step - 1) {
      curr_row <- curr_row + 1
    } else if (curr_col - 1 > 0 && !is.na(step_count[curr_row, curr_col - 1]) && step_count[curr_row, curr_col - 1] == curr_step - 1) {
      curr_col <- curr_col - 1
    } else if (curr_col + 1 <= ncol(step_count) && !is.na(step_count[curr_row, curr_col + 1]) && step_count[curr_row, curr_col + 1] == curr_step - 1) {
      curr_col <- curr_col + 1
    } else {
      browser()
    }
    
    ind <- ind + 1
  }
  tibble(col = cols, row = rows, value = 1, step = steps) %>%
    slice(1:ind)
}


plot_path <- function(input, step_count) {
  
  plot_mat(input$height_map) +
    geom_point(data = tibble(row = input$start_pos[[1]], col = input$start_pos[[2]], value = 1),
               colour = "red") +
    geom_path(data = gen_path(step_count, input),
              colour = "red")
}


plot_path(sample_input, sample_step_count)
plot_path(input, step_count)

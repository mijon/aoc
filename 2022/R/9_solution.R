library(tidyverse)

read_input <- function(path) {
  input <- read_delim(path,
             delim = " ", col_names = c("dir", "value"),
             col_types = cols(dir = col_character(),
                              value = col_double()))
  rep(input$dir, input$value)
}

state <- function(hpos, tpos, places_t_has_been) {
  list(
    h = hpos,
    t = tpos,
    places_t_has_been = places_t_has_been
  )
}

step <- function(state, instruction) {
  new_h <- update_h(state$h, instruction)
  new_t <- update_t(state$t, new_h)
  state(
    new_h,
    new_t,
    append(state$places_t_has_been, list(new_t))
  )
}

update_h <- function(pos, instruction) {
  list(
    x = pos$x + (instruction == "R") - (instruction == "L"),
    y = pos$y + (instruction == "U") - (instruction == "D")
  )
}

update_t <- function(tpos, hpos) {
  if ((tpos$x - hpos$x)^2 + (tpos$y - hpos$y)^2 >2 ) {
    list(
      x = tpos$x + sign(hpos$x - tpos$x),
      y = tpos$y + sign(hpos$y - tpos$y)
    )
  } else {
    tpos
  }
}

part_1 <- function(input) {
  init_state <- state(list(x = 0, y = 0),
                      list(x = 0, y = 0),
                      list())
  reduce(input, step, .init = init_state) %>%
    pluck("places_t_has_been") %>%
    unique() %>%
    length()
}



part_1 <- function(input) {
  init_state <- state(list(x = 0, y = 0),
                      list(x = 0, y = 0),
                      list())
  reduce(input, step, .init = init_state) %>%
    pluck("places_t_has_been") %>% 
    unique() %>%
    length()
}


# Part 2
state2 <- function(positions, places_t_has_been) {
  list(
    positions = positions,
    places_t_has_been = places_t_has_been
  )
}

part_2 <- function(input) {
  init_state <- state2(
    positions = rep(list(list(x = 0, y = 0)), 10),
    places_t_has_been = list()
  )
  
  reduce(input, step2, .init = init_state) %>% 
    pluck("places_t_has_been") %>%
    unique() %>%
    length()
}

step2 <- function(state, instruction) {
  positions <- state$positions
  positions[[1]] <- update_h(positions[[1]], instruction)
  for (i in 2:length(positions)) {
    positions[[i]] <- update_t(positions[[i]], positions[[i-1]])
  }
  
  state2(
    positions,
    append(state$places_t_has_been, list(positions[[length(positions)]]))
  )
}

# answers
input <- read_input("../input/9_input.txt")
part_1(input) # 6190
part_2(input) # 2516

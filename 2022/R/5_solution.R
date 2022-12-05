library(tidyverse)

read_input <- function() {
  tmp <- read_lines("../input/5_input.txt")
  break_point <- which(tmp == "")
  
  list(
    stacks = parse_stacks(tmp[1:(break_point - 1)]),
    instructions = parse_instructions(tmp[(break_point + 1):length(tmp)])
  )
}

parse_instructions <- function(instructions) {
  instructions %>%
    str_extract_all("[0-9]+") %>%
    map(as.numeric)
}

parse_stacks <- function(stacks) {
  # drop the last row that's just stack numbers
  stacks <- head(stacks, n = length(stacks) - 1)
    
  map(seq(2, 35, 4), \(y) {map_chr(stacks, ~substr(.x, y, y))}) %>%
    map(~keep(.x, \(x) {x != " "}))
}

pick_up <- function(v, n) {
  v[1:n]
}

drop_from_stack <- function(v, n) {
  tail(v, length(v) - n)
}

put_on_stack <- function(v, stack, preprocess = rev) {
  append(stack, preprocess(v), 0)
}

move_on_stack <- function(stacks, instructions, preprocess = rev) {
  n <- instructions[[1]]
  from <- instructions[[2]]
  to <- instructions[[3]]
  
  stacks[[to]] <- put_on_stack(pick_up(stacks[[from]], n),
                               stacks[[to]],
                               preprocess)
  stacks[[from]] <- drop_from_stack(stacks[[from]], n)
  stacks
}


part_1 <- function(input) {
  reduce(input$instructions,
         move_on_stack,
         .init = input$stacks) %>%
    map(1) %>%
    paste(collapse = "")
}

# part 2
part_2 <- function(input) {
  reduce(input$instructions,
         move_on_stack,
         preprocess = identity,
         .init = input$stacks) %>%
    map(1) %>%
    paste(collapse = "")
}


input <- read_input()

part_1(input) # CWMTGHBDW
part_2(input) # SSCGWJCRB

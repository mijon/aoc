library(tidyverse)





parse_instructions <- function(df) {
  df %>% mutate(add = parse_number(instructions,
                                   na = "noop") %>%
                  if_else(is.na(.), 0, .),
                cycles = if_else(
                  str_detect(instructions, "noop"), 1, 2),
                value = 1 + cumsum(add),
                cycle_count = cumsum(cycles))
}

gen_cycle_seq <- function(start, prog_df, step) {
  seq(start, max(prog_df$cycle_count), step)
}

get_register_value <- function(cycle, prog_df) {
  prog_df %>%
    filter(cycle_count < cycle) %>%
    tail(n = 1) %>%
    pull(value)
}

part_1 <- function(input) {
  prog_df <- parse_instructions(input)
  cycles <- gen_cycle_seq(20, prog_df, 40)
  registers <- map_dbl(cycles, get_register_value, prog_df)
  
  sum(cycles * registers)
}

# part 2
get_sprite_positions <- function(prog_df) {
  tibble(t = seq(1, max(prog_df$cycle_count))) %>%
    left_join(prog_df %>% select(x = value, cycle_count),
              by = c("t" = "cycle_count")) %>%
    fill(x, .direction = "down") %>%
    mutate(x = lag(x),
           x = if_else(is.na(x), 1, x))
}

add_pixels <- function(df) {
  df %>%
    mutate(
      cursor = rep(0:39, length.out = nrow(df)),
      pixel = if_else(cursor <= x + 1 & cursor >= x - 1,
                           "#",
                           "."),
           row = ceiling(t/40)) 
}

part_2 <- function(input) {
  input %>%
    parse_instructions() %>%
    get_sprite_positions() %>%
    add_pixels() %>%
    ggplot(aes(x = cursor,
               y = row,
               fill = pixel)) +
    geom_tile() +
    scale_y_reverse() +
    coord_equal()
}

input <- read_csv("../input/10_input.txt", col_names = "instructions")
# input <- read_csv("../input/10_sample.txt", col_names = "instructions")

part_1(input) # 14060
part_2(input) # PAPKFKEJ

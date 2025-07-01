# ---- About ----
# Well I definitely over-engineered this one. But I'm not here to play golf.

library(tidyverse)
INPUT_PATH <- "../input/10_input.txt"

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

# The input is a collection of lines falling into one of two forms:
# - bot x gives low to (bot/output) y and high to (bot/output) z
# - value x goes to bot y
# The "value..." lines are the initialisation lines, and the others define the
# movements. Note that the movements can be to other bots, or to outputs, so we
# need to handle both. Note also that the movements essentially define a graph,
# which we might as well plot to see what we're working with.


# There's more to this parsing function than usual as I want to process all the
# input data into a final list representing the initial state.
parse_input <- function(input) {
  bot_defs  <- keep(input, \(x) str_detect(x, "^bot")) 
  init_defs <- keep(input, \(x) str_detect(x, "^value"))
  
  # We make a named list of the bots, each bot it itself a list made with
  # `bot()`, and the names of the elements of the list are the bot names
  # themselves, e.g. "bot 1".
  #
  # We use `liftdl` which transforms f(x, y, z) into f(list()), which is rather
  # useful (similar to `do.call`, I guess, but this seems more functional as you
  # can change the domain separately from actually calling the function. It's a
  # shame that it's deprecated on the grounds of being a bad fit in {purrr} with
  # seemingly not new package for it).
  bot_list <- bot_defs |>
    map(\(y) str_extract_all(y, "[bot|output]+ [0-9]+") |>
          first()) |>
    map(lift_dl(bot)) |> 
    reduce(add_bot, .init = list())
  
  inits <- init_defs |>
    str_extract("value ([0-9]+) goes to (bot [0-9]+)",
                group = c(2,1)) |> 
    t() |> # this is a little dance to change a nx2 matrix into a list of elements of length 2.
    as.data.frame() |>
    as.list()
  
  init_bot <- function(bot_list, init_val) {
    bot_list[[init_val[[1]]]] <- receive_value(bot_list[[init_val[[1]]]], init_val[[2]])
    bot_list
  }
  
  # give all the bots their initial values
  reduce(inits,
         init_bot,
         .init = bot_list)
}

# --- bot making functionality ----
bot <- function(name, low_to, high_to) {
  list(name = name,
       low_to = low_to,
       high_to = high_to,
       values = vector(mode = "character")
  )
}

# Given a list of bots, we find the first that has exactly 2 values currently
# and so is ready to pass them on.
get_candidate <- function(bl) {
  # we need this `if` for part two, when we want to break once all the outputs
  # are filled.
  if (bl |> map("values") |> lengths() |> max() == 1) {
    return (bl |> keep(\(x) length(x) == 1))
  }
  
  bl |> keep(\(b) {length(b$values) == 2}) |>
    first() |>
    pluck("name")
}

# helper function that models a bot receiving a value
receive_value <- function(bot, value) {
  bot$values <- append(bot$values, value)
  bot$values <- str_sort(bot$values, numeric = TRUE)
  bot
}

# condition is a function we can run on each `bot_of_interest`, if it returns
# TRUE, then do `effect`
process_exchange <- function(bl, bot_of_interest, condition, effect) {
  if (condition(bot_of_interest)) {
    return(effect(bot_of_interest))
  }
  
  vals <- bl[[bot_of_interest]]$values
  
  # transfer the value to the appropriate bots or outputs
  high_to <- bl[[bot_of_interest]]$high_to
  low_to <- bl[[bot_of_interest]]$low_to
  bl[[high_to]] <- bl[[high_to]] |> receive_value(vals[2])
  bl[[low_to]] <- bl[[low_to]] |> receive_value(vals[1])
  
  # remove the values from the `bot_of_interest`
  bl[[bot_of_interest]]$values <- vector(mode = "character")
  
  bl
}

# Helper function to add a bot to a bot_list
add_bot <- function(bot_list, bot) {
  bot_list[[bot$name]] <- bot
  bot_list
}

# ---- Working ----
part_1 <- function(input) {
  bot_list <- parse_input(input)
  condition <- function(bot) {all(bot_list[[bot]]$values == c(17, 61))}
  effect <- function(bot) {
    return(bot_list[[bot]]$name)
    }
  
  while (inherits(bot_list, "list")) {
    candidate <- get_candidate(bot_list)
    bot_list <- process_exchange(bot_list, candidate, condition, effect)
  }
  
  bot_list
}


part_2 <- function(input) {
  bot_list <- parse_input(input)
  condition <- function(bot) {FALSE}
  effect <- function(bot) {
    bot
  }
  
  
  while (TRUE) {
    candidate <- get_candidate(bot_list)
    if (length(candidate) > 1) break # stop if all the bots are empty.
    bot_list <- process_exchange(bot_list, candidate, condition, effect)
  }
  
  c(bot_list$`output 0`,
    bot_list$`output 1`,
    bot_list$`output 2`) |>
    as.numeric() |>
    prod()
}
# ---- Results ----
part_1(input) # bot 93
part_2(input) # 47101


# ---- graph workings ----
library(DiagrammeR)

parsed_graph_inputs <- input |>
  keep(\(x) str_detect(x, "^bot")) |>
  as_tibble(input_line = .x) |>
  separate(col = value, into = c("from_label", "temp"),
           sep = " gives low to ") |>
  separate(col = temp, into = c("low", "high"),
           sep = " and high to ")



nodes <- tibble(
  node_label = unique(c(parsed_graph_inputs$from_label,
                        parsed_graph_inputs$low,
                        parsed_graph_inputs$high))) |>
  mutate(node_id = 1:n()) |>
  # mutate(type = if_else(str_detect(node_label, "bot"), "bot", "output"))
  mutate(type = case_when(
    node_label %in% c("bot 198", "bot 61") ~ "init_bots",
    str_detect(node_label, "bot") ~ "bot",
    TRUE ~ "output"
  ))

graph_info <- parsed_graph_inputs |>
  pivot_longer(cols = -from_label, names_to = "rel", values_to = "to_label") |>
  left_join(nodes |> select(from = node_id, node_label), by = c("from_label" = "node_label")) |>
  left_join(nodes |> select(to = node_id, node_label), by = c("to_label" = "node_label"))


create_graph() |>
  add_node_df(node_df = select(nodes,
                               id = node_id,
                               type,
                               label = node_label) |>
                as.data.frame() # Apparently, add_node_df won't take tibbles?
  ) |> 
  add_edge_df(
    graph_info |>
      mutate(id = 1:n()) |>
      as.data.frame()
  ) |>
  visnetwork()

# Running the above results in a lattice stretching out from a single bot out to
# each of the outputs


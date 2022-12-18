library(tidyverse)


# Monkey Infrastructure ----
new_monkey <- function(id, items, operation, test, divisor) {
  output <- list(
    id = id, 
    items = items,
    operation = operation,
    test = test,
    divisor = divisor,
    inspections = 0
  )
  class(output) <- "monkey"
  output
}

# These two functions aren't really necessary, but it's a metaprogramming dance
# to make it easy to debug the monkey objects, as their functions will
# accurately show the parameters.
expr2fn <- function(body) {
  body <- str2lang(body)
  eval(str2expression(deparse(bquote(
    function(old) {
      .(body)
    }
  ))))
}

parse_test <- function(a, b, c) {
  a <- parse_number(a)
  b <- parse_number(b)
  c <- parse_number(c)
  eval(str2expression(deparse(bquote(
  function(x) {
    if (x %% .(a) == 0) {.(b)} else {.(c)}
  }
  ))))
}

parse_monkey <- function(input_df) {
  id <- parse_number(input_df$info[[1]])
  items <- str_extract_all(input_df$info[[2]], "[0-9]+")[[1]] %>% as.numeric()
  operation <- input_df$info[[3]] %>% str_remove("  Operation: new = ") %>% expr2fn()
  test <- parse_test(input_df$info[[4]], input_df$info[[5]], input_df$info[[6]])
  divisor <- parse_number(input_df$info[[4]])
  new_monkey(id, items, operation, test, divisor)
}

read_monkeys <- function(path) {
  read_delim(path,
             col_names = "info",
             col_types = cols("info" = col_character()),
             delim = "|") %>%
    mutate(monkey_group = if_else(str_detect(info, "^Monkey"), 1, 0) %>%
             cumsum()) %>%
    split(~.$monkey_group) %>%
    map(parse_monkey)
}

monkey_push <- function(item, monkey) {
  monkey$items <- append(monkey$items, item)
  monkey
}

first <- function(v) {
  if (length(v) > 0) {
    v[[1]]
  } else {
    NA
  }
}

rest <- function(v) {
  if (length(v) > 1) {
    v[2:length(v)]
  } else {
    NA
  }
}

monkey_pop <- function(monkey) {
  item <- first(monkey$items)
  monkey$items <- rest(monkey$items)
  list(
    item = item,
    monkey = monkey
  )
}

process_monkey_items <- function(monkey, personal_worry_update = \(x) {floor(x / 3)}, special) {
  items_in_play <- map_dbl(monkey$items, monkey$operation) %>%
    map_dbl(personal_worry_update) %>%
    
    # This is used in part 2 to keep the numbers from getting too big. `special`
    # is the product of all the numbers used in the tests for all monkeys.
    # Taking the modulus of this product each time we have a monkey deal with an
    # item means that we can keep the numbers from getting to big without
    # breaking any of the other monkey's calculations. 
    map_dbl(\(x) {x %% special}) 
  monkey$items <- vector(mode = "numeric", length = 0)
  monkey$inspections <- monkey$inspections + length(items_in_play)
  
  throw_df <- tibble(item = items_in_play,
                     catcher = map_dbl(items_in_play, monkey$test))
  
  list(
    monkey = monkey,
    throw_df = throw_df
  )
}

process_throws <- function(monkeys, throws) {
  throws <- throws %>%
    mutate(catcher = catcher + 1) %>%
    transpose()
  
  for (throw in throws) {
    monkeys[[throw$catcher]] <- monkey_push(throw$item, monkeys[[throw$catcher]])
  }
  monkeys
}

run_round <- function(monkeys, personal_worry_update, special) {
  for (i in seq_along(monkeys)) {
    curr_monkey <- monkeys[[i]]
    turn <- process_monkey_items(curr_monkey, personal_worry_update, special)
    monkeys[[i]] <- turn$monkey
    monkeys <- process_throws(monkeys, turn$throw_df)
  }
  monkeys
}

run_n_rounds <- function(monkeys, n, personal_worry_update, special = Inf) {
  for (i in 1:n) {
    monkeys <- run_round(monkeys, personal_worry_update, special)
  }
  monkeys
}

part_1 <- function(input) {
  run_n_rounds(input, 20, personal_worry_update = \(x) {floor(x / 3)}) %>%
    map_dbl("inspections") %>%
    sort(decreasing = TRUE) %>%
    head(n = 2) %>%
    reduce(`*`)
}

part_2 <- function(input) {
  special <- map_dbl(input, "divisor") %>% reduce(`*`)
  run_n_rounds(input, 10000, personal_worry_update = identity, special = special) %>%
    map_dbl("inspections") %>%
    sort(decreasing = TRUE) %>%
    head(n = 2) %>%
    reduce(`*`)
}


input <- read_monkeys("../input/11_input.txt")
part_1(input) # 182293
part_2(input) # 54832778815

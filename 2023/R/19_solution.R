library(tidyverse)

# ---- sample data and loading ----

input <- read_lines("../input/19_input.txt")

# This function takes the input, e.g. "px{a<2006:qkq,m>2090:A,rfg}" and parses
# it into a character vector containing R code that implents the logic encoded
# in the instruction.
parse_fn_line <- function(l) {
  tmp <- str_split(l, "\\{")[[1]]
  fn_name <- tmp[1]
  if (fn_name == "in") {fn_name <- "start_here"} # `in` is a reserved keyword
  
  conditions <- tmp[2] |>
    str_remove("\\}$") |>
    str_split(",") |>
    map(\(x) str_split(x, ":")) |>
    first()
  
  process_outcome <- function(outcome) {
    case_when(outcome == "A" ~ "TRUE",
              outcome == "R" ~ "FALSE",
              TRUE ~ paste0(outcome, "(input)"))
  }

  process_condition <- function(cond) {
    if (length(cond) == 2) {
      glue::glue("if (input${{cond[1]}}) {
                 {{process_outcome(cond[2])}}
                 } else ", .open = "{{", .close = "}}")
    } else {
      glue::glue("{
                 {{process_outcome(cond[1])}}
      }", .open = "{{", .close = "}}")
    }
  }
  
  processed_conditions <- paste(map_chr(conditions, process_condition), collapse = "\n")
  fn_string <- glue::glue("function(input) {
             {{processed_conditions}}
             }", .open = "{{", .close = "}}")
  list(fn_name = fn_name,
       fn_string = fn_string)
}

# A helper function for later when parsing the input vectors
num_extractor <- function(letter) {
  function(string) {
    str_extract(string, paste0("(?<=", letter, "\\=)[0-9]+")) |> as.numeric()
  }
}

# convert "{x=123,m=123...}" into an R list
parse_input_vector <- function(string) {
  list(x = num_extractor("x")(string),
       m = num_extractor("m")(string),
       a = num_extractor("a")(string),
       s = num_extractor("s")(string))
}

# We create character vectors containing R functions that implement the logic of
# the first half of the input and then we load that into an environment with
# them all in. Doing this means that we don't personally have to handle the
# redirection of one workflow to another workflow, that's handled entirely by
# R's dispatch mechanism.
parse_input <- function(input) {
  functions <- input |> keep(\(x) str_detect(x, "^[a-z]")) |> map(parse_fn_line)
  vectors   <- input |> keep(\(x) str_detect(x, "^\\{")) |> map(parse_input_vector)
  
  sample_environment <- new.env()
  walk(functions, \(fn) assign(x = fn$fn_name,
                               value = eval(str2expression(fn$fn_string)),
                               envir = sample_environment))
  
  list(
    env = sample_environment,
    vectors = vectors
  )
}

part_1 <- function(input) {
  input <- parse_input(input)
  parts <- input[[2]]
  env <- input[[1]]
  
  # The functions we implemented ultimately return a boolean:
  # (TRUE -> A(ccept), FALSE -> R(eject))
  # so that fits nicely with `purrr::keep`
  check_part <- function(part) {
    withr::with_environment(env, start_here(part))
  }
  
  parts |> keep(check_part) |>
    map(as.numeric) |>
    map(sum) |>
    reduce(sum)
}


# ---- running ----
part_1(input) # 350678
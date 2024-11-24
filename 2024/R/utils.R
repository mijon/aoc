# Set up
get_input <- function(day, year = 2024) {
  dotenv::load_dot_env(file = "../.env")
  session = Sys.getenv("session")
  
  url <- glue::glue("https://adventofcode.com/{year}/day/{day}/input")
  input <- httr::GET(url,
                     httr::set_cookies(session = session))
  
  output_file_name <- glue::glue("{stringr::str_pad(day, width = 2, pad = '0')}_input.txt")
  readr::write_file(httr::content(input),
                    file = glue::glue("../input/{output_file_name}"))
}


new_day <- function(day, year = 2024) {
  padded_day <- stringr::str_pad(day, width = 2, pad = "0")
  solution_path <- glue::glue("{padded_day}_solution.R")
  
  get_input(day, year)
  rendered_template <- whisker::whisker.render(readr::read_file("template.R"),
                                               data = list(day = padded_day))
  readr::write_file(rendered_template,
                    file = solution_path)
  rstudioapi::documentOpen(path = solution_path)
}
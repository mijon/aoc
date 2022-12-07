library(tidyverse)
library(data.tree)

# Reading in the data
read_input <- function() {
  tibble(line = read_lines("../input/7_input.txt")) %>%
    mutate(line_type = classify_command(line),
           payload = extract_payload(line))
}

classify_command <- function(line) {
  case_when(
    str_detect(line, "^\\$ cd") ~ "cd_command",
    str_detect(line, "^\\$ ls") ~ "ls_command",
    str_detect(line, "^[0-9]") ~ "file_listing",
    str_detect(line, "^dir") ~ "directory")
}

extract_payload <- function(line) {
  str_remove(line, "^\\$ cd ") %>%
    str_remove("^dir ") %>%
    str_remove("^\\$ ls")
}

# Conversion to {data.tree} object
input_to_tree <- function(df) {
  dfl <- split(df, seq(nrow(df)))
  
  output <- Node$new("files")
  output$AddChild("/")
  output$children[["/"]]$filetype <- "directory"
  current <- output
  
  for (command in dfl) {
    line_type <- command$line_type
    payload <- command$payload
    
    if (line_type == "cd_command") {
      if (payload == "..") {
        current <- current$parent
      } else {
        current <- current$children[[payload]]
      }
    } else if (line_type == "directory") {
      current$AddChild(payload)
      current$children[[payload]]$filetype <- "directory"
    } else if (line_type == "file_listing") {
      size <- as.numeric(str_extract(payload, "[0-9]+"))
      name <- str_remove(payload, "[0-9]+ ")
      current$AddChild(name = name)
      current$children[[name]]$size <- size
      current$children[[name]]$filetype <- "file"
    }
  }
  
  output
}

# Use {data.tree} aggregation methods to fill in directory sizes
annotate_with_sizes <- function(tree) {
  calc_size <- function(self) {
    map_dbl(self$children,
            GetAttribute,
            "size") %>%
      sum()
  }

  tree$Set(
    size = c(calc_size),
    filterFun = isNotLeaf
  )
}

part_1 <- function(tree) {
  ToDataFrameTree(tree, "filetype", "size") %>%
    filter(filetype == "directory",
           size <= 100000) %>%
    pull(size) %>%
    sum()
}

input <- read_input() %>%
  input_to_tree() %>%
  annotate_with_sizes()


# part 2
part_2 <- function(input) {
  total_disc <- 70000000
  need_free  <- 30000000
  tree_df    <- ToDataFrameTree(input, "filetype", "size")
  
  current_total <- tree_df  %>%
    pull(size) %>%
    max()
  
  tofind <- need_free - (total_disc - current_total)
  
  tree_df %>%
    filter(filetype == "directory",
           size >= tofind) %>% 
    pull(size) %>% 
    min()
}

part_1(input) # 1792222
part_2(input) # 1112963

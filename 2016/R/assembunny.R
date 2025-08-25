
# ---- Assembunny instructions ----
#
# Instructions implemented
# - NOP - no operation
# - CPY
# - INC
# - DEC
# - MOV
# - JNZ
# All parsing functions take the instruction in as a string, and return a
# function that implements the instruction.

parse_nop <- function(instruction) {
  function(registers, HEAD, instructions) {
    list(registers = registers, HEAD = HEAD + 1, instructions = instructions, out = NA)
  }
}

parse_cpy <- function(instruction) {
  parts <- str_split(instruction, " ")[[1]]
  from <- parts[2]
  to <- parts[3]
  
  if (from %in% c("a", "b", "c", "d")) {
    from <- str2lang(paste0('registers["', from, '"]'))
  } else {
    from <- as.numeric(from)
  }
  
  bquote(
    function(registers, HEAD, instructions) {
      registers[.(to)] <- .(from)
      list(registers = registers, HEAD = HEAD + 1, instructions = instructions, out = NA)
    }
  ) |> eval()
}

parse_inc <- function(instruction) {
  parts <- str_split(instruction, " ")[[1]]
  reg <- parts[2]
  
  function(registers, HEAD, instructions) {
    registers[reg] <- registers[reg] + 1
    list(registers = registers, HEAD = HEAD + 1, instructions = instructions, out = NA)
  }
}

parse_mov <- function(instruction) {
  parts <- str_split(instruction, " ")[[1]]
  from <- parts[2]
  to <- parts[3]
  
  function(registers, HEAD, instructions) {
    registers[to] <- registers[to] + registers[from]
    registers[from] <- 0
    list(registers = registers, HEAD = HEAD + 1, instructions = instructions, out = NA)
  }
}

parse_dec <- function(instruction) {
  parts <- str_split(instruction, " ")[[1]]
  reg <- parts[2]
  
  function(registers, HEAD, instructions) {
    registers[reg] <- registers[reg] - 1
    list(registers = registers, HEAD = HEAD + 1, instructions = instructions, out = NA)
  }
}

parse_jnz <- function(instruction) {
  parts <- str_split(instruction, " ")[[1]]
  x <- parts[2]
  y <- parts[3]
  
  if (x %in% c("a", "b", "c", "d")) {
    x <- str2lang(paste0('registers["', x, '"]'))
  } else {
    x <- as.numeric(x)
  }
  
  if (y %in% c("a", "b", "c", "d")) {
    y <- str2lang(paste0('registers["', y, '"]'))
  } else {
    y <- as.numeric(y)
  }
  
  bquote(
    function(registers, HEAD, instructions) {
      if (.(x) != 0) {
        HEAD <- HEAD + .(y) - 1
      }
      list(registers = registers, HEAD = HEAD + 1, instructions = instructions, out = NA)
    }
  ) |> eval()
}

flip_mov <- function(instruction) {
  str_replace(instruction, "mov ([a-z0-9]+) ([a-z0-9]+)", "mov \\2 \\1")
}

parse_tgl <- function(instruction) {
  parts <- str_split(instruction, " ")[[1]]
  offset <- parts[2]
  
  if (offset %in% c("a", "b", "c", "d")) {
    offset <- str2lang(paste0('registers["', offset, '"]'))
  } else {
    offset <- as.numeric(offset)
  }  
  
  bquote(
    function(registers, HEAD, instructions) {
      if (HEAD + .(offset) < 0 | HEAD + .(offset) > length(instructions)) {
        return(list(registers = registers, HEAD = HEAD + 1, instructions = instructions, out = NA))
      }
      candidate_instruction <- instructions[[HEAD + .(offset)]] 
      candidate_instruction_type <- str_sub(candidate_instruction, 1, 3)
      
      # browser()
      # if (candidate_instruction_type == "tgl") browser()
      
      instructions[[HEAD + .(offset)]] <- case_when(
        candidate_instruction_type == "inc" ~ str_replace(candidate_instruction, "inc", "dec"),
        candidate_instruction_type == "dec" ~ str_replace(candidate_instruction, "dec", "inc"),
        candidate_instruction_type == "tgl" ~ str_replace(candidate_instruction, "tgl", "inc"),
        
        candidate_instruction_type == "cpy" ~ str_replace(candidate_instruction, "cpy", "jnz"),
        candidate_instruction_type == "jnz" ~ str_replace(candidate_instruction, "jnz", "cpy"),
        
        candidate_instruction_type == "mov" ~ flip_mov(candidate_instruction),
        candidate_instruction_type == "nop" ~ candidate_instruction
        
        # TRUE ~ cli::cli_abort("instruction '{candidate_instruction_type}' not implemented")
      )
      
      list(registers = registers, HEAD = HEAD + 1, instructions = instructions, out = NA)
    }) |> eval()
}

parse_mul_old <- function(instruction) {
  parts <- str_split(instruction, " ")[[1]]
  from  <- parts[2] 
  to    <- parts[3] 
  times <- parts[4] 
  ref   <- parts[5] 
  
  bquote(
    function(registers, HEAD, instructions) {
      registers[to] <- (registers[times] * registers[ref]) + registers[to]
      registers[from] <- 0
      list(registers = registers, HEAD = HEAD + 1, instructions = instructions, out = NA)
  }) |> eval()
}


parse_mul <- function(instruction) {
  parts <- str_split(instruction, " ")[[1]]
  from  <- parts[2] 
  to    <- parts[3] 
  times <- parts[4] 
  ref   <- parts[5] |> register_or_value()
  
  bquote(
    function(registers, HEAD, instructions) {
      registers[to] <- (registers[times] * .(ref)) + registers[to]
      registers[from] <- 0
      list(registers = registers, HEAD = HEAD + 1, instructions = instructions, out = NA)
    }
  ) |> eval()
}

register_or_value <- function(x) {
  if (x %in% c("a", "b", "c", "d")) {
    str2lang(paste0('registers["', x, '"]'))
  } else {
    as.numeric(x)
  }
}

parse_out <- function(instruction) {
  parts <- str_split(instruction, " ")[[1]]
  part <- parts[2] |> register_or_value()
  
  bquote(
    function(registers, HEAD, instructions) {
      print(.(part))
      list(registers = registers, HEAD = HEAD + 1, instructions = instructions, out = .(part))
    }
  ) |> eval()
}

parse_d25 <- function(instruction) {
  function(registers, HEAD, instructions) {
    new_registers <- c(
      "a" = registers[["a"]] + floor(registers[["b"]] / 2),
      "b" = 0,
      "c" = 1 - (registers[["a"]] %% 2 != 0),
      "d" = registers[["d"]]
    )
    
    list(registers = new_registers, HEAD = HEAD + 1, instructions = instructions, out = NA)
  }
}

parse_instruction <- function(instruction) {
  ins_type <- str_sub(instruction, 1, 3)
  if (ins_type == "cpy") {
    parse_cpy(instruction) 
  } else if (ins_type == "inc") {
    parse_inc(instruction)
  } else if (ins_type == "dec") {
    parse_dec(instruction)
  } else if (ins_type == "jnz") {
    parse_jnz(instruction) 
  } else if (ins_type == "mov") {
    parse_mov(instruction)
  } else if (ins_type == "nop") {
    parse_nop(instruction)
  } else if (ins_type == "tgl") {
    parse_tgl(instruction)
  } else if (ins_type == "mul") {
    parse_mul(instruction)
  } else if (ins_type == "out") {
    parse_out(instruction)
  } else if (ins_type == "d25") {
    parse_d25(instruction)
  }
  else {
    cli::cli_abort("Unknown instruction: {instruction}")
  }
}

# ---- Running ----

# `reprocess` allows you to reprocess the instruction set between loops
run_assembunny <- function(input, registers, verbose = FALSE, reprocess = identity) {
  instructions <- input
  HEAD <- 1
  i <- 1
  
  while (HEAD <= length(instructions)) {
    
    cur_instruction <- instructions[HEAD]
    if (verbose) {
      cat(str_pad(i, 4, side = "right"), "[H:", str_pad(HEAD, 4), "]", str_pad(paste0(registers, collapse = ","), 20, side = "right"), "|", str_pad(cur_instruction, 8), "\n")
    }
    results <- parse_instruction(cur_instruction)(registers, HEAD, instructions)
    instructions <- reprocess(results$instructions)
    registers <- results$registers
    HEAD <- results$HEAD
    i <- i + 1
  }
  registers
}

run_assembunny_check_outs <- function(input, registers, verbose = FALSE, reprocess = identity) {
  outs <- c()
  
  instructions <- input
  HEAD <- 1
  i <- 1
  
  while (HEAD <= length(instructions)) {
    if (length(outs) == 4) {
      if (all(outs = c(0,1,0,1))) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
    
    cur_instruction <- instructions[HEAD]
    if (verbose) {
      cat(str_pad(i, 4, side = "right"), "[H:", str_pad(HEAD, 4), "]", str_pad(paste0(registers, collapse = ","), 20, side = "right"), "|", str_pad(cur_instruction, 8), "\n")
    }
    results <- parse_instruction(cur_instruction)(registers, HEAD, instructions)
    instructions <- reprocess(results$instructions)
    if (!is.na(results$out)) {
      outs <- c(outs, results$out)
    }
    registers <- results$registers
    HEAD <- results$HEAD
    i <- i + 1
  }
  registers
}


# ---- MOV optimisation ----
#
# A sequence of instructions with the following pattern:
# - inc A
# - dec b
# - jnz b -2
# can be replaced with a MOV instruction (followed by two NOPs to avoid having
# to edit all other JNZ references). This can dramatically decrease running time
# as some costly loops can be avoided.

do_mov_opt <- function(a) {
  pttrn <- "inc ([abcd])\r\ndec ([abcd])\r\njnz ([abcd]) -2\r\n"
  
  if (length(a) > 1) {
    a <- paste(a, collapse = "\r\n")
  }
  
  str_replace_all(a, pttrn, replacement = "mov \\2 \\1\r\nnop\r\nnop\r\n")[[1]] |>
    str_trim() |>
    str_split("\r\n") |>
    first()
}

do_mov_mul_opt <- function(a) {
  a <- a |> do_mov_opt()
  
  # pttrn <- "inc ([abcd])\r\ndec ([abcd])\r\njnz ([abcd]) -2\r\n"
  pttrn <- "cpy ([abcd0-9]+) ([abcd])\r\nmov ([abcd]) ([abcd])\r\nnop\r\nnop\r\ndec ([abcd])\r\njnz ([abcd]) -5\r\n"
  
  if (length(a) > 1) {
    a <- paste(a, collapse = "\r\n")
  }
  
  str_replace_all(a, pttrn, replacement = "mul \\2 \\4 \\5 \\1\r\nnop\r\nnop\r\nnop\r\nnop\r\nnop\r\n")[[1]] |>
    str_trim() |>
    str_split("\r\n") |>
    first()
}

day_25_opt <- function(instructions) {
  
  if (length(instructions) > 1) {
    instructions <- paste(instructions, collapse = "\r\n")
  }
  
  
  pttrn <- "cpy 2 c\r\njnz b 2\r\njnz 1 6\r\ndec b\r\ndec c\r\njnz c -4\r\ninc a\r\njnz 1 -7\r\n"
  
  replacement <- "d25\r\nnop\r\nnop\r\nnop\r\nnop\r\nnop\r\nnop\r\nnop\r\n"
  
  str_replace_all(instructions, pttrn, replacement) |>
    str_split(patter = "\r\n") |>
    first()
}



# ---- plotting ----



plot_assembunny <- function(instructions) {
  
  colour_vertices <- function(v) {
  c("cpy" = "orange",
    "jnz" = "lightblue",
    "inc" = "palegreen",
    "dec" = "violet",
    "mov" = "red",
    "END" = "black")[v]
  }
  
  tmp <- tibble(instruction = instructions) |>
    mutate(instruction_num = 1:n(),
           to_number = map2(instruction, instruction_num, determine_tos),
           ins_type = str_sub(instruction, 1, 3))
  
  g <- igraph::make_empty_graph() |>
    igraph::add_vertices(nv = nrow(tmp),
                         attr = list(label = tmp$instruction,
                                     ins_type = tmp$ins_type,
                                     color = colour_vertices(tmp$ins_type))) |>
    igraph::add_vertices(nv = 1,
                         attr = list(label = "END",
                                     ins_type = "END",
                                     color = "grey")) |>
    igraph::add_edges(edges = handle_edges(tmp),
                      attr = list(label = handle_edge_labels(tmp),
                                  color = "grey")) 
  
  visNetwork::visIgraph(g, idToLabel = FALSE, smooth = TRUE)
}


determine_tos <- function(instruction, cur_num) {
  if(str_detect(instruction, "jnz")) {
    parts <- str_split(instruction, " ")[[1]]
    label <- c(paste(parts[[2]], "== 0"), paste(parts[[2]], "!= 0"))
    values <- c(cur_num + 1, cur_num + as.numeric(parts[[3]]))
  } else {
    label <- ""
    values <- cur_num + 1
  }
  
  tibble(label = label, value = values)
}


interleave_cols <- function(df, a, b) {
  l <- pull(df, {{a}})
  r <- pull(df, {{b}})
  
  interleave_vecs <- function(l, r, output) {
    if (length(r) == 0) {
      return(output)
    }
    interleave_vecs(
      tail(l, n = -1),
      tail(r, n = -1),
      c(output, head(l, n = 1), head(r, n = 1)))
  }
  interleave_vecs(l, r, c())
}




handle_edges <- function(df) {
  df |>
    select(instruction_num, to_number) |>
    unnest(to_number) |> 
    interleave_cols(instruction_num, value)
}

handle_edge_labels <- function(df) {
  df |> unnest(to_number) |> pull(label)
}


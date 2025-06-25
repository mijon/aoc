# ---- About----
# This is a hash processing task, similar to the way block chains work, I guess.
# As we are checking a lot of hashes, the run time of the solution is very
# dependent on the hashing implementation we use. It turns out that the md5
# implementation in the cli package is about 20 times faster than the
# implementation in the openssl package (which surprised me, actually) so we
# will use that. It will also be nice to have the password slowly be filled in
# on the console, so a chunk of our solution will be taken up with console
# escape codes to delete lines and replace them so it looks like the password is
# being filled in rather than all the incremental lines stacking up in the
# console history (if that doesn't make sense, just run the code).

library(tidyverse)
INPUT_PATH <- "../input/05_input.txt"


# ---- input reading and parsing ----
# possibly the simplest input in AoC history?
input <- readr::read_lines(INPUT_PATH)

# ---- part 1 ----
# The processing and calculation of the passwords takes a while, so for each
# found character of the password, I want to display an information block
# consisting of the hash, the loop iteration, and the time the character was
# found. I want this to flow up the console window as new blocks come in, but I
# want the in-progress password solve to be displayed at the bottom of the
# console. So we write the in-progress password to console, then as each block
# comes in, we delete the current line showing the password, write the block,
# then write the password.

# A general way to print the info blocks to the screen that we can use in both
# parts.
cat_progress <- function(hash, iteration, time, pass_displayer) {
  cat("\33[2K\r") # clear the current printed line
  cat("hash:      ", hash, "\n")
  cat("iteration: ", iteration, "\n")
  cat("found time:", strftime(time, format = "%H:%M:%S"), "\n\n")
  pass_displayer() # each part has a slightly different way to display the password
}

part_1 <- function(input) {
  password <- c()
  i <- 0
  
  # part-specific way to display the password
  disp_pass <- function() {
    cat(str_pad(paste(password, collapse = ""), width = 8, pad = "_", side = "right"))
  }
  
  start <- Sys.time()
  cat("start:", strftime(start, format = "%H:%M:%S"), "\n")
  disp_pass()
  while (length(password) < 8) {
    hash <- cli::hash_md5(paste(input, i, sep = ""))
    if (startsWith(hash, "00000")) {
      password <- c(password, substr(hash, 6, 6))
      cat_progress(hash, i, Sys.time(), disp_pass)
    }
    i <- i + 1
  }
  end <- Sys.time()
  
  cat("\33[2K\r\n") # clear out the filled in password to display the elapsed time:
  cat("\nMinutes elapsed:", difftime(end, start, units = "mins"), "\n")
  disp_pass()
  
  password |> toupper() |>
    paste(collapse = "") |>
    invisible()
}


# ---- Part 2 ----
# Part 2 follows a similar format, but rather than building up the password from
# the left to right, we dot around in the password filling in positions out of
# sequence. That's fine, we just need a slightly different data structure to
# hold the in-progress password, and a little more logic to extract that
# information, but the overall process is similar to part 1.
part_2 <- function (input) {
  password <- rep(NA, 8)
  i <- 0
  valid_positions <- as.character(0:7)
  
  disp_pass <- function() {
    if_else(is.na(password), "_", password) |>
      paste(collapse = "") |>
      cat()
  }
  
  start <- Sys.time()
  cat("start:", strftime(start, format = "%H:%M:%S"), "\n")
  disp_pass()
  while (sum(!is.na(password)) < 8) {
    hash <- cli::hash_md5(paste(input, i, sep = ""))
    if (startsWith(hash, "00000")) {
      position_str <- substr(hash, 6, 6)
      position_value <- as.numeric(position_str) + 1
      if (position_str %in% valid_positions & is.na(password[position_value])) {
        password_chr <- substr(hash, 7, 7)
        password[position_value] <- password_chr
        cat_progress(hash, i, Sys.time(), disp_pass)
      }
    }
    i <- i + 1
  }
  end <- Sys.time()
  cat("\33[2K\r\n") # clear out the filled in password to display the elapsed time:
  cat("\nMinutes elapsed:", difftime(end, start, units = "mins"), "\n")
  disp_pass()
  
  password |> toupper() |>
    paste(collapse = "") |>
    invisible()
}


# ---- Results ----
part_1(input) # 2414bc77 (in about 1.3 mins)
part_2(input) # 437e60fc (in about 4.2 mins)


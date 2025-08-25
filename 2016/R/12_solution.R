# ---- About ----
#
# In this problem, we're given a set of instructions written in assembunny. We
# need to run the program given a set of initial values in the four registers
# and then return what value is in register A.
#
# By graphing out the chain of instructions we can see a recurring pattern where
# we increment a register, decrement another, and loop until the second is zero.
# We end up evaluating a total number of instructions equal to three times the
# value of the second register. However, in effect, all we are doing is moving
# the value from the second register onto the first (adding it). So if we can
# recognise that pattern we can replace it with a single operation (in reality,
# we replace it with three operations, the move and to "NOP" instructions, i.e.
# no operation, meaning to nothing and skip to the next instruction. We do this
# so we don't have to adjust any references in the goto-like jnz instructions
# elsewhere in the program).
#
# In effect we are transpiling from base assembunny to an expanded
# instruction-set version of assembunny.

library(tidyverse)

# Since assembunny appears a few times this year, I have extracted all the
# parsing and optimisation code to a separate file.
source("assembunny.R") 

# ---- Input -----

# The optimisation works on string pattern matching, so it's easier to have the
# input as one big string.
INPUT_PATH <- "../input/12_input.txt"

# But the graphing is easier with one instruction per line.
input <- read_lines(INPUT_PATH)

# ---- Processing ----

# Parts 1 and 2 are the same structure with differing initial registers. So we
# have a single function that handles running the optimisation and then the
# code, and then extracts the value of the "a" register.
solver <- function(input, registers, verbose) {
  input <- input |> do_mov_opt() 
  result <- run_assembunny(input, registers, verbose)
  result[["a"]]
}

part_1 <- function(input, verbose) {
  registers <- c("a" = 0, "b" = 0, "c" = 0, "d" = 0)
  solver(input, registers, verbose)
}

part_2 <- function(input, verbose) {
  registers <- c("a" = 0, "b" = 0, "c" = 1, "d" = 0)
  solver(input, registers, verbose)
}

part_1(input, verbose = FALSE)  # 318009
part_2(input, verbose = FALSE) # 922763


# ---- Appendix: Drawing the code ----
plot_assembunny(input)

# ---- About ----
# Another Assembunny problem, by now, we're experts in this. All we needed was
# to add the new instructions to the assembunny emulator, and we're off. 

library(tidyverse)
source("assembunny.R")

input <- read_lines("../input/23_input.txt")

# part 1
run_assembunny(input, registers = c("a" = 7, "b" = 0, "c" = 0, "d" = 0), TRUE, do_mov_mul_opt) # 13140

# part 2
run_assembunny(input, registers = c("a" = 12, "b" = 0, "c" = 0, "d" = 0), TRUE, do_mov_mul_opt) # 479009700

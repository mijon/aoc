library(tidyverse)

input <- tibble(input = read_lines("../input/07_input.txt"))

cards_1_vec <- c("2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A")
cards_2_vec <- c("J", "2", "3", "4", "5", "6", "7", "8", "9", "T", "Q", "K", "A")
cards_ref_vec <- c("2", "3", "4", "5", "6", "7", "8", "9", "T", "Q", "K", "A")
cards_1 <- factor(cards_1_vec, levels = cards_1_vec, ordered = TRUE)
cards_2 <- factor(cards_2_vec, levels = cards_2_vec, ordered = TRUE)
ref_cards <- factor(cards_ref_vec, levels = cards_ref_vec, ordered = TRUE)

# ---- part 1 ----
parse_input <- function(input, classifier, cards_vec) {
  input |>
    separate(input,
             into = c("hand", "bid"),
             sep = " ",
             convert = TRUE) |>
    mutate(hand = str_split(hand, ""),
           classification = map_dbl(hand, classifier),
           hand = map(hand, conv_to_factors, cards_vec),
           secondary_rank = map_dbl(hand, hand_to_num))
}

conv_to_factors <- function(v, card_map) {
  v |>
    factor(levels = levels(card_map),
           ordered = TRUE)
}

count_same <- function(v) {
  table(v) |>
    as.numeric() |>
    sort()
}

classify_hand_part_1 <- function(h) {
  counted_same <- count_same(h)
  case_when(
    identical(counted_same,  5) ~ 7,# five of a kind"
    identical(counted_same,  c(1, 4)) ~ 6, # four of a kind"
    identical(counted_same,  c(2, 3)) ~ 5, # full house"
    identical(counted_same,  c(1, 1, 3)) ~ 4, # three of a kind"
    identical(counted_same,  c(1, 2, 2)) ~ 3, # two pair
    identical(counted_same,  c(1, 1, 1, 2)) ~ 2, # one pair
    identical(counted_same,  c(1, 1, 1, 1, 1)) ~ 1 # high card
  )
}

rank_input <- function(df) {
  df |>
    arrange(classification,
            secondary_rank) |>
    mutate(ranking = 1:n())
}

hand_to_num <- function(h) {
  nums <- as.numeric(h)
  mults <- 13^(4:0)
  sum(nums * mults)
}

part_1 <- function(input) {
  parse_input(input,
              classifier = classify_hand_part_1,
              cards_vec = cards_1) |>
    rank_input() |>
    mutate(score = bid * ranking) |>
    pull(score) |>
    sum()
}

# ---- part 2 ----
classify_hand_part_2 <- function(h) {
  replacement <- table(h, exclude = "J") |>
    enframe() |> 
    mutate(name = factor(name, levels = levels(ref_cards))) |>
    arrange(desc(value), desc(name)) |>
    pull(name) |> 
    first() |>
    as.character()
  
  if (is.na(replacement)) {
    replacement <- "J"
  }
  
  h[which(h == "J")] <- replacement
  classify_hand_part_1(h)
}

part_2 <- function(input) {
  parse_input(input,
              classifier = classify_hand_part_2,
              cards_vec = cards_2) |>
    rank_input() |>
    mutate(score = bid * ranking) |>
    pull(score) |>
    sum()
}

# ---- evaluations ----
part_1(input) # 250453939
part_2(input) # 248652697

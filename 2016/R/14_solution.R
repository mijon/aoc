# ---- About ----
#
# More md5 hashes.
#
# I tried a few methods for this one. The first one is my third method. I
# realised that we don't need the specific hashes, we really just want to know
# which runs of triplets or quintuplets are present in the hash. So we can
# compress the hash down to a count vector of the 16 different possible
# character in the md5 hash, which also gives us a way to test triplets matching
# quintuplets: simply take the inner product of the two vectors.

library(tidyverse)
library(cli) # for md5 hash
INPUT_PATH <- "../input/14_input.txt"

# ---- input reading and parsing ----
input <- readr::read_lines(INPUT_PATH)

# ---- Working ----
# Given a salt, n, get the hash.
salt_hash <- function(n) hash_md5(paste0(input, n))

# We're doing string matching later, so we want to be able to generate things to
# match against. Since we're looking for triplets (e.g. "aaa") and quintuplets
# (e.g. "bbbbb"), we can generate them all now with a single function. (numbers
# first then letters as this is the same order as `sort` produces, useful
# later).
gen_reference <- function (n) c(strrep(0:9, n), strrep(letters[1:6], n))
quintuplets_reference <- gen_reference(5)
triplets_reference <- gen_reference(3)

# Given one or more hashes, we generate a vector of all the matches of
# `references`. The resulting vector is the same length as `references` and each
# element denotes how many matches of that `references` element were found
# across all the elements of `to_check`.
gen_count_vector <- function(to_check, references) {
  map(references, \(x) str_detect(to_check, x) |> as.numeric()) |>
    do.call(what = rbind, args = _) |> # makes it a matrix
    rowSums() # makes it a vector
}

# Because we're pre-computing and then searching inside the pre-computed block,
# we need to calculate a load of hashes, probably more than we need. Luckily
# this is relatively fast to do, so we're okay for now (it bites us in part 2).
loads_of_hashes <- salt_hash(1:50000)


find_keys <- function(loads_of_hashes) {
  
  # We make a big 16 x n matrix, with a column for each hash in
  # `loads_of_hashes`. Each column is the number of quintuplets matched in that
  # hash.
  quintuplets_matrix <- map(quintuplets_reference,
                            \(x) str_detect(loads_of_hashes, x) |>
                              as.numeric()) |>
    do.call(what = rbind, args = _) # this makes the list into a matrix
  
  # Similar as above, but for triplets, but then we need to only consider the
  # first. I skimmed over that part while reading the spec and spend far more
  # time getting the wrong answers than I really should have. So this code is a
  # little messier than it should be, but I'll keep it as it represents the
  # wasted time well.
  triplets_matrix <- loads_of_hashes |>
    str_extract(paste(triplets_reference, collapse = "|")) |>
    enframe() |>
    mutate(temp = 1) |>
    arrange(value) |>
    pivot_wider(names_from = value,
                values_from = temp,
                values_fill = list(temp = 0)) |>
    arrange(name) |> select(-c(`NA`, name)) |>
    as.matrix() |>
    t()

  
  # Running the checks is just sliding a 1000-column window across our
  # quintuplets_matrix and calculating the inner product with a triplets vector
  # as appropriate. `.progress` because it's slow.
  checks <- map(1:(50000 - 1000), \(i) sum(triplets_matrix[,i] * rowSums(quintuplets_matrix[,(i+1):(i+1000)])),
                .progress = TRUE)
  which(checks > 0)[64]
}


# part 2

# The process of stretching the keys takes a while, so make yourself a coffee
# and just relax. There must be a faster way, I guess a faster algorithm or some
# better round trip? I'm hashing them in a vectorised way, so it's not like I'm
# hashing a single hash 2016 times, then moving on to the next... I guess not
# doing as many would be better.
stretch_keys <- function(hashes, n) {
  cat("stretching keys")
  for (i in 1:n) {
    cat(i, "\n")
    hashes <- hash_md5(hashes)
  }
  hashes
  
}


part_1 <- function() {
  salt_hash(1:50000) |>
  find_keys()
}


part_2 <- function() {
  salt_hash(1:50000) |>
    stretch_keys(n = 2016) |>
  find_keys()
}


# ---- Results ----
part_1() # 18626
part_2() # 20092




# ---- Method 2 ----
#
# Method 2 involved the observation that we really don't care about the
# individual hashes at all, rather we care that *somewhere* in the next 1000
# hashes there is a match, since we don't need to specify exactly which of the
# next 1000 hashes has the match, we can compress the data structure
# representing the next 1000 hashes down to a 16 x 1 vector where each element
# is the count of the quintuplets in the next 1000 hashes.
#
# As we step through we can update our vector by subtracting a vector
# representing hash n+1 and adding a vector representing hash n+1001


# Because we're only looking at the *first* triplet that matches we need to do
# some extra work to remove any extra matches.
find_first_triplet <- function(hash) {
  idx <- which(str_extract(hash, paste0(triplets_reference, collapse = "|")) == triplets_reference)
  output <- rep(0,16)
  output[idx] <- 1
  output
}

method_2_part_1 <- function(input) {
  found_hashes <- c()
  cur_salt <- 1
  n_to_check <- 64
  
  hash_candidates <- map(1:1000, salt_hash)
  next_1000_hashes_quintuplets <- gen_count_vector(hash_candidates, quintuplets_reference)
  
  while(length(found_hashes) < n_to_check) {
    cur_hash <- salt_hash(cur_salt)
    end_hash <- salt_hash(cur_salt + 1000)
    
    # update the master vector counting the quintuplets of the next 1000 hashes.
    next_1000_hashes_quintuplets <- next_1000_hashes_quintuplets -
      gen_count_vector(cur_hash, quintuplets_reference) +
      gen_count_vector(end_hash, quintuplets_reference)
    
    cur_triplets <- find_first_triplet(cur_hash)
    
    if (sum(cur_triplets * next_1000_hashes_quintuplets) > 0) {
      found_hashes <- c(found_hashes, cur_salt)
    }
    
    cur_salt <- cur_salt + 1
  }
  found_hashes[[n_to_check]]
}

# This works, but is slower than the previous method.

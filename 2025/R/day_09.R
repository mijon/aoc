library(tidyverse)

input <- read_lines("../input/input_09.txt") |>
  str_split(",") |>
  map(as.numeric)

example_input <- "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3" |> str_split("\n") |> first() |>
  str_split(",") |> map(as.numeric)


calc_area <- function(p1, p2) {
  (abs(p1[1] - p2[1]) + 1) * (abs(p1[2] - p2[2]) + 1)
}

area_mat <- function(input, area_f = calc_area) {
  areas <- matrix(0, nrow = length(input),
                  ncol = length(input))

  for (row in 1:length(input)) {
    for (col in (row):length(input)) {
      areas[row, col] <- area_f(input[[row]], input[[col]])
    }
  }
  areas
}

part_1 <- function(input) {
  area_mat(input) |>
    max()
}


# ---- part 2 ----
# Sometimes, we are our most powerful computer Rather than trying to write code
# to solve this problem, I'll just draw some pictures and look at them. This is
# called "solving by inspection". :)

# First I want the input as a data.frame so I can use ggplot2 easily.
df <- read_csv("../input/input_09.txt",
         col_names = c("row", "col"))

# Let's look at the points
df |> mutate(n = 1:n()) |>
  ggplot(aes(x = col, y = row, colour = n)) +
  geom_point() +
  coord_equal() +
  geom_path()

# We see many points arranged in a rough circle with two points inside the
# circle. One appears to be on the centre line, while one is offset to one side.
# Immediately we see that one half of the circle seems to offer a larger area
# than the other. We will start there.

# Zoom in
df |> mutate(dist_from_centre = sqrt((row - 50000)^2 + (col - 50000)^2),
             n = 1:n()) |>
  filter(n <= 249) |> ggplot(aes(x = col, y = row, colour = n)) + geom_point() + coord_equal() + geom_path()

# We can get the locations of the two points in the middle by calculating the
# distance from the center of the circle and the sorting by that distance.
(centre_point <- arranged_points <- df |>
    mutate(dist_from_centre = sqrt((row - 50000)^2 + (col - 50000)^2),
           n = 1:n()) |>
    arrange(dist_from_centre) |>
    slice(1))

# We posit that the largest rectangle will have as a corner the point in the
# middle that's closest to the centre line.
# In my case it has a row value of 94693.
centre_point_row <- centre_point$row[[1]]

# We extend a line from this centre point out to see what the maximum width our
# rectangle could be. I.e. we want to find the furthest away column for which
# all closer points are above.
df |>
  mutate(n = 1:n()) |>
  filter(n <= 249) |>
  ggplot(aes(x = col, y = row, colour = n)) +
  geom_point() +
  coord_equal() +
  geom_path() +
  geom_hline(yintercept = centre_point_row,
             colour = "red")

# We can calculate that value as
(max_col <- df |> filter(row < centre_point_row) |>
  arrange(desc(row)) |>
  slice(1) |>
  pull(col))

# Now we drop another line down at this point and see there are some points at
# the bottom of the circle that look like decent candidates, just to the inside
# of our vertical line.
df |>
  mutate(n = 1:n()) |>
  filter(n <= 249) |>
  ggplot(aes(x = col, y = row, colour = n)) +
  geom_point() +
  coord_equal() +
  geom_path() +
  geom_hline(yintercept = centre_point_row,
             colour = "red") +
  geom_vline(xintercept = max_col,
             colour = "red")

# We can find that point with another filter:
(other_point <- df |> filter(row <= 50000, col <= max_col) |>
  arrange(desc(col), row) |>
  slice(1))


part_2 <- calc_area(c(centre_point$row[[1]], centre_point$col[[1]]),
                    c(other_point$row[[1]],  other_point$col[[1]]))


# --- result ----
part_1(input) # 4739623064
part_2 # 1654141440

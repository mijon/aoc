# ---- About ----
#
# This problem is a maze-solving problem, so since we wrote an a* solver
# earlier, why not use it for this problem. The first thing to notice is that
# the problem space is not actually a 4x4 grid (or rather that's not the *only*
# way to look at it). Since we're interested in the path taken to get to each
# point, starting at (1,1) is a different node in our problem space than being
# at (1,1) after having taken the path DU. So we need to handle that.
#
# Always one for over complicating, I am going to construct a class that gives
# us a nice API to work in this domain.

library(tidyverse)

source("a_star.R")

input <- read_file("../input/17_input.txt") |> str_trim()

# ---- Utils for checking doors ----
# Whether a door is open depends on the first four characters of the hash of the
# input concatenated with the path to get there, so we make a few util functions
# to handle that for us.
get_chr <- function(s, n) str_sub(s, start = n, end = n)
door_is_open <- function(c) c %in% c("b", "c", "d", "e", "f")

get_doors <- function(hash) {
  c(U = 1, D = 2, L = 3, R = 4) |>
    map_chr(\(n) get_chr(hash, n)) |>
    map_lgl(door_is_open)
}

# ---- maze node objects ----
#
# A maze node is the collection of a position and a path to get there. e.g.
# having started at (1,1) and gone DU, we'd have a node whose string
# representation could be `[1,1] DU`
new_maze_node <- function(grid_pos, path) {
  structure(
    list(
      grid_pos = grid_pos,
      path = path),
    class = "mazenode")
}

# Then we will need to look at our object, and rather than look at the list
# format, I'd prefer to look at something pithier.
format.mazenode <- function(mn) {
  paste0("[", rw(mn), ",", cl(mn), "] ", pth(mn))
}

print.mazenode <- function(mn) print(format(mn))

# We will often need to grab out parts of the object, so we make a few getters.
rw  <- function(mn) mn$grid_pos[1]
cl  <- function(mn) mn$grid_pos[2]
pth <- function(mn) mn$path

# Then from any one node we want to be able to generate a new node to the UDLR
# of that node.
mover <- function(row, col, path_chr) {
  function(mn) {
    new_maze_node(grid_pos = c(rw(mn) + row, cl(mn) + col),
                  path = paste0(pth(mn), path_chr))
  }
}

down  <- mover(+1,  0, "D")
up    <- mover(-1,  0, "U")
left  <- mover( 0, -1, "L")
right <- mover( 0, +1, "R")

# When we run the a* algorithm, we need to be able to check whether we're at the
# target node or not. We can't just use `identical`, as we (by definition) don't
# know the path we need to take to get to the target node in advance, so cannot
# construct the node accordingly. Hence we need a function that only checks the
# grid position.
same_pos <- function(nm1, nm2) rw(nm1) == rw(nm2) && cl(nm1) == cl(nm2)

# We also need a heuristic distance function, anything sensible will do here, so
# I will have the squared euclidean distance, one could also use Manhattan
# distance or something similar.
euc_dist_sq <- function(nm1, nm2) (rw(nm2) - rw(nm1))^2 + (cl(nm2) - cl(nm1))^2

# ---- funs specifically for the problem ----
# Now we set up the problem-specific functionality. As this task occurs on a 4x4
# grid, we don't want to allow movements outside of this grid, so we need a way
# to check that.
valid_node <- function(n) {
  all(
    rw(n) <= 4,
    rw(n) >= 1,
    cl(n) <= 4,
    cl(n) >= 1
  )
}

# Our `get_doors` function generates a Boolean vector with names in UDLR, and
# we'll want to be able to translate these over to functions. The easiest way I
# can think of is list slicing.
dirs_to_movements <- function(dirs) {
  dir_fns <- list(U = up, D = down, L = left, R = right)
  dir_fns[dirs]
}

# The neighbours of any node can be calculated by generating the hash, finding
# which doors are open, and then generating new node objects for the path that
# will take us to these new nodes.
gen_neighbours <- function(node) {
  hash <- gen_hash(node)
  potential_movements <- get_doors(hash) |> # Boolean vector depending on the door state
    keep(isTRUE) |> # only keep open doors
    names() |> # get the directions
    dirs_to_movements() # translate into movement functions
  
  map(potential_movements,
      \(m) m(node)) |> # make into nodes
    keep(valid_node)
}

gen_hash <- function(node) {
  cli::hash_md5(paste0(input, pth(node)))
}


# ---- part_1 ----

start_node <- new_maze_node(c(1,1), "")
target_node <- new_maze_node(c(4,4), "hopefully this string will never be needed")


part_1 <- function() {
  a_star(
    start_node, target_node,
    node_compare_fn = same_pos,
    heuristic_cost_fn = euc_dist_sq,
    get_neighbours = gen_neighbours,
    d_between_neighbours = const_d_between_neighbours
  )$path[[1]] |>
    pth()
}


# ---- part 2 ----
# For part 2, we need to find the longest path, which means we need to find
# *all* the paths to be sure. That's easy enough with a recursive function. I
# went with an accumulator to calculate the total at the end, rather than
# returning 1 at the end and adding the results up the call stack because the
# latter did not work.
#
# It's fine, but it's slower than I'd like:
part_2 <- function(start) {
  get_max <- function(node, acc) {
    if (same_pos(node, target_node)) {
      return(acc)
    }
    
    children <- gen_neighbours(node)
    
    # this is to avoid warnings when calling max. Technically it's not needed,
    # but it's nicer to handle these.
    if (length(children) == 0) {
      return(0)
    }
    
    cur_maxes <- c()
    
    # I wanted to do this with a map, but got loads of errors.
    for (c in children) {
      cur_maxes <- c(cur_maxes, get_max(c, acc + 1))
    }
    return(max(cur_maxes))
  }
  
  get_max(start, 0)
}


# This also makes the a* thing pointless: since we've generated all the paths,
# we could just pluck out the shortest...


# ---- Running ----
part_1() # RRRLDRDUDD, takes ~3.3ms
part_2(start_node) # 706, takes ~8s



# ---- Appendix: Getting all the paths out ----

part_2_with_paths <- function(start) {
  n <- 0
  paths <- list()
  
  get_max <- function(node, acc) {
    n <<- n + 1
    if (same_pos(node, target_node)) {
      paths <<- append(paths, list(node))
      return(acc)
    }
    
    children <- gen_neighbours(node)
    
    if (length(children) == 0) {
      return(0)
    }
    
    cur_maxes <- c()
    
    for (c in children) {
      cur_maxes <- c(cur_maxes, get_max(c, acc + 1))
    }
    return(max(cur_maxes))
  }
  
  get_max(start, 0)
  paths
}


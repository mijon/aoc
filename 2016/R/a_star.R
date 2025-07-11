
# ---- About ----
# The a* algorithm is a useful path-finding algorithm that's generally faster
# than a BFS or DFS search. It can be applied in many circumstances provided
# that you have a way to get the next candidate options from the current point.
# So it can work on grids or graphs. The benefit over basic DFS/BFS is the use
# of a priority queue: We apply some heuristic to approximate how close any
# candidate node is to the target node and always explore the closest first.
#
# This implementation makes extensive use of hashmaps which are very common in
# many languages, but not used that mich in R. The reason in picking these over
# lists is that the keys can be any arbitrary R object. This means that we can
# use nodes as keys regardless of what the nodes are, rather than being forced
# to use strings. This is useful for reconstructing the path, i.e. in the hash
# map where one node came from can be represented by a key-value pair of actual
# nodes.

# This function reconstructs the single path from start to finish given the
# hashmap `came_from` that holds all the links that have been explored.
reconstruct_path <- function(came_from, current) {
  total_path <- list(current)
  while (!isFALSE(gethash(came_from, key = current, nomatch = FALSE))) {
    current <- gethash(came_from, key = current)
    total_path <- append(total_path, values = list(current))
  }
  total_path
}

# TODO factor out
const_d_between_neighbours <- function(x, y) {
  1
}

# Not all items have `==` or `%in%` implemented, so we make a new function that
# does the same thing more generally.
elem <- function(item, l) {
  any(
    map_lgl(l, \(x) identical(x, item))
  )
}

#' R Implementation of a* Search Algorithm
#'
#' # I can't take credit for this, it's an R translation of the pseudocode on
#' the a* wikipedia article :)
#'
#' @param start Start node
#' @param end End node
#' @param heuristic_cost_fn node -> Int: returning the score estimate for some
#'   node. This is the key part of a*
#' @param get_neighbours node -> [node]: returning the list of nodes connected
#'   to a given node
#' @param d_between_neighbours node -> node -> int: returning the distance or
#'   cost between two adjacent nodes. In grid problems, just set this to a
#'   constant function.
#'
#' @returns **list** of two elements:
#'  - `path`: the shortest path found between `start` and `end`
#'  - `visited`: all visited nodes in this run (mostly used for debugging)
a_star <- function(start, end, 
                   heuristic_cost_fn, 
                   get_neighbours, 
                   d_between_neighbours 
                   ) {
  visited <- list() 
  frontier <- list(start)
  
  came_from <- hashtab()
  
  # for each node, n, g represents the cost from `start` to n. By definition,
  # the cost from `start` to itself is 0, so we can put that in at the start for
  # free.
  gScore <- hashtab()
  sethash(gScore, key = start, value = 0)
  
  # for each node n, f represents the *current* estimate for the best path from
  # `start` to `end` through n. At any point, f(n) = g(n) + h(n) where h(n) is
  # the heuristic cost from node n to `end`. Hence why we initialise this with
  # the `start` node (for which g = 0) with the hueristic value for `start`.
  fScore <- hashtab()
  sethash(fScore, key = start, value = heuristic_cost_fn(start, end))
  
  while (length(frontier) > 0) {
    # get the node in `frontier` with the lowest fScore value
    current <- get_best_candidate(frontier, fScore)
    
    if (identical(current, end)) {
      print("found goal, reconstructing...")
      return(list(path = reconstruct_path(came_from, current),
                  visited = visited))
    }
    
    # remove current from frontier
    # frontier <- frontier[frontier != current]
    visited <- append(visited, list(current))
    frontier <- discard(frontier, \(x) identical(x, current))
    
    # get the viable neighbours of current
    neighbours <- get_neighbours(current)
    for (neighbour in neighbours) {
      # if (identical(neighbour, c(6, 5))) browser()
      tentative_score <- gethash(gScore, key = current, nomatch = Inf) + d_between_neighbours(current, neighbour)
      if (tentative_score < gethash(gScore, key = neighbour, nomatch = Inf)) {
        # this path to neighbour is better than any previous one, so we record it
        sethash(came_from, key = neighbour, value = current)
        sethash(gScore, key = neighbour, value = tentative_score)
        sethash(fScore, key = neighbour, value = tentative_score + heuristic_cost_fn(neighbour, end))
        if (!elem(neighbour, frontier)) {
          frontier <- append(frontier, values = list(neighbour))
        }
      }
    }
    
  }
  return(FALSE)
}

# This is the priority feature, we need to get the node in the current frontier
# that has the best score in the heuristic.
get_best_candidate <- function(frontier, fScore) {
  scores <- map_dbl(frontier, \(x) gethash(fScore, x, nomatch = Inf))
  frontier[which.min(scores)][[1]]
}

deque <- function(values) {
  
  env <- new.env()
  assign("values", value = values)
  
  give <- function() {
    get("values", env)
  }
  
  pop_front <- function(n = 1) {
    values <- give()
    output <- head(values, n)
    assign("values", value = tail(values, -n), envir = env)
    output
  }
  
  pop_back <- function(n = 1) {
    values <- give()
    output <- tail(values, n)
    assign("values", value = head(values, -n), envir = env)
    rev(output)
  }
  
  push_front <- function(x) {
    new <- append(x, give())
    assign("values", value = new, envir = env)
  }
  
  push_back <- function(x) {
    new <- append(give(), x)
    assign("values", value = new, envir = env)
  }
  
  len <- function() {
    length(give())
  }
  
  output <- list(
    get = give,
    pop_front = pop_front,
    pop_back = pop_back,
    push_front = push_front,
    push_back = push_back,
    len = len
  )
  class(output) <- "deque"
  output
}

print.deque <- function(d) {
  print(d$get())
}
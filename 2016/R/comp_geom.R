on_segment <- function(p, q, r){
  Re(q) <= max(Re(p), Re(r)) &
    Re(q) >= min(Re(p), Re(r)) &
    Im(q) <= max(Im(p), Im(r)) &
    Im(q) >= min(Im(p), Im(r))
}

orientation <- function(p, q, r) {
  value <- (Im(q) - Im(p)) * (Re(r) - Re(q)) - (Re(q) - Re(p)) * (Im(r) - Im(q))
  sign(value)
}

do_intersect <- function(a, b, x, y) {
  o1 <- orientation(a, b, x)
  o2 <- orientation(a, b, y)
  o3 <- orientation(x, y, a)
  o4 <- orientation(x, y, b)
  
  if (o1 != o2 & o3 != o4) {
    return(TRUE)
  }
  
  if (o1 == 0 & on_segment(a, x, b)) {
    return(TRUE)
  }
  
  if (o2 == 0 & on_segment(a, y, b)) {
    return(TRUE)
  }
  
  if (o3 == 0 & on_segment(x, a, y)) {
    return(TRUE)
  }
  
  if (o4 == 0 & on_segment(x, b, y)) {
    return(TRUE)
  }
  
  FALSE
}


a <- 0 + 0i
b <- 10 + 0i
x <- 1 + 2i
y <- 1 - 2i

do_intersect(a, b, x, y)

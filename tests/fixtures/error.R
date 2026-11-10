f <- function(x) {
  y <- x + 1
  stop("boom: ", y)
}
g <- function() f(1)
g()
cat("not reached\n")

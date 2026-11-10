inner <- function(v) {
  v + 1
}
outer <- function(w) {
  w * 10
}
r <- outer(inner(1))
s <- outer(2)
cat("r:", r, "\n")

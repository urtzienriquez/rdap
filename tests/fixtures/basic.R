add <- function(a, b) {
  s <- a + b
  s * 2
}

compute <- function(xs) {
  total <- 0
  for (x in xs) {
    total <- total + add(x, 1)
  }
  total
}

v <- c(1, 2, 3)
res <- compute(v)
cat("result:", res, "\n")

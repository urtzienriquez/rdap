counter <- 0
bump <- function() {
  counter <<- counter + 1
  invisible(NULL)
}
v <- c(1, 2, 3)
for (k in 1:3) bump()
v[2] <- 20
cat("end\n")

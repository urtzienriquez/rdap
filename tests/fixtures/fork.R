work <- function(i) {
  cat("worker", i, "\n")
  i * 2
}
r <- parallel::mclapply(1:2, work, mc.cores = 2)
total <- sum(unlist(r))
cat("total:", total, "\n")

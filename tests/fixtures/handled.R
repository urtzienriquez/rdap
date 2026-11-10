risky <- function(x) {
  if (x > 1) stop("too big: ", x)
  x
}
r <- try(risky(5), silent = TRUE)
v <- tryCatch(risky(7), error = function(e) -1)
w <- tryCatch(risky(1), warning = function(w) 0)
cat("done", v, w, "\n")

l <- list(a = 1, b = 2)
for (i in 1:4) {
  l$a <- l$a + i
  l$b <- l$b * 2
}
cat("a:", l$a, "\n")

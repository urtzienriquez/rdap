x <- 1
tools::pskill(Sys.getpid(), 11L)
cat("not reached\n")

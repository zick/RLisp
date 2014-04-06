con = file(description="stdin", open="r")
cat('> ')
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  cat(paste(line, '\n'))
  cat('> ')
}
close(con)
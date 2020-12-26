library(stringr)
start <- Sys.time()
input <- "253149867"
sequence1 <- as.integer(unlist(str_split(input, "")))
sequence1 <- c(sequence1, 10:1000000)
lnklst <- c(sequence1[-1],sequence1[1])[order(sequence1)]
nseq <- length(sequence1)

makeMove <- function(current) {
  take1 <- lnklst[current]
  take2 <- lnklst[take1]
  take3 <- lnklst[take2]
  dest <- (current - 1:4 - 1) %% nseq + 1
  dest <- dest[which.min(dest %in% c(take1, take2, take3))]
  lnklst[current] <<- lnklst[take3]
  lnklst[take3] <<- lnklst[dest]
  lnklst[dest] <<- take1
}
current <- sequence1[1]
for(i in 1:10000000) {
  makeMove(current)
  current <- lnklst[current]
  print(i)
}
end <- Sys.time()
print(end - start)
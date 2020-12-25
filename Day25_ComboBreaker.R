library(readr)
library(stringr)
public_keys <- as.integer(scan("input25.txt", what = character(), sep = "\n"))

loopSize <- function(public, subject) {
  i <- 0
  value <- 1
  while (value != public) {
    value <- value * subject
    value <- value %% 20201227
    i <- i + 1
  }
  i
}

transform <- function(subject, loop_size) {
  value <- 1
  for (i in 1:loop_size) {
    value <- value * subject
    value <- value %% 20201227
  }
  value
}

loop_sizes <- c(loopSize(public_keys[1], 7), loopSize(public_keys[2], 7))
encryption_keys <- c(transform(public_keys[2], loop_sizes[1]), transform(public_keys[1], loop_sizes[2]))

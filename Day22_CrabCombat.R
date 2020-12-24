library(readr)
library(stringr)
f <- scan("input22.txt", what = character(), sep = "\n")
player1 <- as.integer(f[2:(which(f == "Player 2:")-1)])
player2 <- as.integer(f[(which(f == "Player 2:")+1):length(f)])
players <- list(player1, player2)

playRound <- function(p) {
  if (p[[1]][1] > p[[2]][1]) {
    p[[1]][length(p[[1]])+1] <- p[[1]][1]
    p[[1]][length(p[[1]])+1] <- p[[2]][1]
  } else {
    p[[2]][length(p[[2]])+1] <- p[[2]][1]
    p[[2]][length(p[[2]])+1] <- p[[1]][1]
  }
  p[[1]] <- p[[1]][-1]
  p[[2]] <- p[[2]][-1]
  p
}
while(length(players[[1]]) > 0 && length(players[[2]]) > 0) {
  players<-playRound(players)
}
print(sum(c(length(players[[1]]):1) * players[[1]]))

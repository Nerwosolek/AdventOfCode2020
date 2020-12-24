library(readr)
library(stringr)
f <- scan("input22.txt", what = character(), sep = "\n")
player1 <- as.integer(f[2:(which(f == "Player 2:")-1)])
player2 <- as.integer(f[(which(f == "Player 2:")+1):length(f)])
players <- list(player1, player2)

playersToString <- function(players) {
  paste("Player1",paste(players[[1]], collapse = "|"),
        "Player2",paste(players[[2]], collapse = "|"), sep=":")
}

playRound <- function(p) {
  c1 <- p[[1]][1]
  c2 <- p[[2]][1]
  if (c1 < length(p[[1]]) && c2 < length(p[[2]])) {
    subPlayers <- list(p[[1]][2:(c1+1)], p[[2]][2:(c2+1)])
    winner <- playGame(subPlayers)
    p[[winner]][length(p[[winner]])+1] <- p[[winner]][1]
    p[[winner]][length(p[[winner]])+1] <- p[[winner %% 2 + 1]][1]
  }
  else if (p[[1]][1] > p[[2]][1]) {
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

# play game
playGame <- function(players) {
  ht <- list()
  while(length(players[[1]]) > 0 && length(players[[2]]) > 0) {
    ht[[playersToString(players)]] <- TRUE
    players<-playRound(players)
    if (!is.null(ht[[playersToString(players)]])) {
      print(sum(c(length(players[[1]]):1) * players[[1]]))
      return(1)
    }
  }
  #ht[[playersToString(players)]] <- TRUE
  if (length(players[[1]]) == 0) {
    print(sum(c(length(players[[2]]):1) * players[[2]]))
    return(2)
  }
  print(sum(c(length(players[[1]]):1) * players[[1]]))
  return(1)
}

wins <- playGame(players)



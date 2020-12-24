library(readr)
library(stringr)
f <- scan("input20.txt", what = character(), sep = "\n")
tiles <- list()
for (i in 1:length(f)) {
  if (i %% 11 == 1) tileID <- str_extract(f[i],"[0-9]{4}")
  else {
    tiles[[tileID]] <- rbind(tiles[[tileID]],unlist(str_split(f[i],"")))
  }
}
sidesMatched <- list()
for (i in 1:length(tiles)) {
  sidesMatched[[names(tiles[i])]] <- 0
  for (n in c(1:length(tiles))[-i]) {
    for (side in list(tiles[[i]][1,], tiles[[i]][,1], tiles[[i]][10,], tiles[[i]][,10],
                      rev(tiles[[i]][1,]), rev(tiles[[i]][,1]), rev(tiles[[i]][10,]), 
                      rev(tiles[[i]][,10]))
        )
    {
        for (otherSide in list(tiles[[n]][1,], tiles[[n]][,1], tiles[[n]][10,], tiles[[n]][,10])){
          if (all(side == otherSide)) {
            sidesMatched[[names(tiles[i])]] <- sidesMatched[[names(tiles[i])]] + 1
            # browser()
          }
        }
    }
    
  }
}


library(readr)
library(stringr)
f <- scan("input24.txt", what = character(), sep = "\n")
tiles_traverse <- str_extract_all(f, "w|nw|sw|e|ne|se")
# white = TRUE
# black = FALSE
tiles <- matrix(TRUE, nrow = 236, ncol = 236)

direction <- function(code) {
  switch (code,
    "w" = c(-1,0),
    "nw" = c(0,-1),
    "ne" = c(1,-1),
    "e" = c(1,0),
    "se" = c(0,1),
    "sw" = c(-1,1)
  )
}

flip_and_save <- function(coords) {
  # white = TRUE
  # black = FALSE
  tiles[coords[1]+118,coords[2]+118] <<- !tiles[coords[1]+118,coords[2]+118]
}

for (line in tiles_traverse) {
  coords <- c(0,0)
  for (t in line) {
    coords <- coords + direction(t)
  }
  print(coords)
  flip_and_save(coords)
}

print(paste("blacks at start:",length(which(tiles == FALSE))))

false_neigh <- function(tiles, q, r) {
  c <- 0
  for (coord in list(c(-1,0),
  c(0,-1),
  c(1,-1),
  c(1,0),
  c(0,1),
  c(-1,1))) {
    if (!tiles[q+coord[1], r+coord[2]]) c <- c + 1
  }
  return (c)
}

flipAll <- function(tileMatrix) {
  tile_copy <- tileMatrix
  for (q in 2:235) {
    for (r in 2:235) {
      if (tileMatrix[q,r]) {
        neighCnt <- false_neigh(tileMatrix,q,r)
        if (neighCnt == 2) tile_copy[q,r] <- !tile_copy[q,r]
      }
      else {
        neighCnt <- false_neigh(tileMatrix,q,r)
        if (neighCnt == 0 || neighCnt > 2) tile_copy[q,r] <- !tile_copy[q,r]
      }
    }
  }
  tile_copy
}

for (i in 1:100) {
  tiles <- flipAll(tiles)
  print(paste("Day",i,"-",length(which(tiles == FALSE))))
}
print(paste("blacks after 100 days:",length(which(tiles == FALSE))))

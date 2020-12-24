library(readr)
library(stringr)
library(rlist)
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
corners <- as.double(names(list.search(sidesMatched, x ~ x == 2)))
print(corners,22)

insertImage <- function(big, small, row, col) {
  for(c in 1:10) {
    for (r in 1:10) {
      big[row * 10 - (10 - r), col * 10 - (10 - c)] <- small[r,c] 
    }
  }
  return (big)
}

flipHoriz <- function(tile) {
  if (nrow(tile) != 10 || ncol(tile) != 10) browser()
  for (i in 1:5) {
    temp <- tile[,(11-i)]
    tile[,(11-i)] <- tile[,i]
    tile[,i] <- temp
  }
  return (tile)
}

rotate <- function(tile) {
  temp <- NULL
  for (i in 1:10) {
    temp <- cbind(temp, rev(tile[i,]))  
  }
  return (temp)
}

checkLeft <- function(tile, rightSide)
{
  t <- tile
  for (i in 1:4) {
    side <- t[,1]
    if (all(side == rightSide)) {
        return (t)
    }
    f <- flipHoriz(t)
    side <- f[,1]
    if (all(side == rightSide)) {
      return (f)
    }
    if (i < 4) t <- rotate(t)
  }
  return(NULL)
}

checkTop <- function(tile, bottomSide)
{
  t <- tile
  for (i in 1:4) {
    side <- t[1,]
    if (all(side == bottomSide)) {
      return (t)
    }
    f <- flipHoriz(t)
    side <- f[1,]
    if (all(side == bottomSide)) {
      return (f)
    }
    if (i < 4) t <- rotate(t)
  }
  return(NULL)
}
image <- matrix(nrow = 120, ncol = 120, )
tilesToCheck <- as.vector(unlist(names(tiles)), mode = "character")
tilesToCheck <- tilesToCheck[-which(tilesToCheck == corners[1])]
for (rowInd in 1:12) {
  for (colInd in 1:12) {
    if (rowInd == 1 && colInd == 1) {
      image <- insertImage(image, tiles[[as.character(corners[1])]], 1, 1)
    }
    else {
      if (rowInd == 1) { # check left
        for (tileNbr in tilesToCheck) {
          matchingTile <- checkLeft(tiles[[tileNbr]], 
                                    image[1:10, ((colInd - 1) * 10)])
          if (!is.null(matchingTile)) {
            image <- insertImage(image, matchingTile, rowInd, colInd)
            tilesToCheck <- tilesToCheck[-which(tilesToCheck == tileNbr)]
            break
          }
        }
        
      } else { # check top
        for (tileNbr in tilesToCheck) {
          #if (rowInd == 3) browser()
          matchingTile <- checkTop(tiles[[tileNbr]], image[((rowInd - 1) * 10), 
                                                            (colInd * 10 - 9):(colInd * 10)])
          if (!is.null(matchingTile)) {
            image <- insertImage(image, matchingTile, rowInd, colInd)
            tilesToCheck <- tilesToCheck[-which(tilesToCheck == tileNbr)]
            break
          }
        }
      }
    }
  }
}

flipAllHoriz <- function(img) {
  #if (nrow(tile) != 10 || ncol(tile) != 10) browser()
  for (i in 1:(ncol(img)/2)) {
    temp <- img[,(ncol(img) + 1 - i)]
    img[,(ncol(img) + 1 - i)] <- img[,i]
    img[,i] <- temp
  }
  return (img)
}

rotateAll <- function(img) {
  temp <- NULL
  for (i in 1:ncol(img)) {
    temp <- cbind(temp, rev(img[i,]))  
  }
  return (temp)
}

image_no_borders <- image
dels <- c((1:12)*10,(1:12)*10 - 9)
image_no_borders <- image_no_borders[-dels,-dels]
#c(image_no_borders[1,19], image_no_borders[2,c(1,6,7,12,13,18,19,20)], image_no_borders[3,c(2,5,8,11,14)])
#c(image_no_borders[1,19], image_no_borders[2,c(1,6,7,12,13,18,19,20)], image_no_borders[3,c(2,5,8,11,14)])
for (i in 1:4) {
  cnt <- 0
  for (col in 1:77) {
    for (row in 1:94) {
      if (all(c(image_no_borders[row,(col-1)+19], 
                image_no_borders[row + 1,(col-1)+c(1,6,7,12,13,18,19,20)], 
                image_no_borders[row + 2,(col-1)+c(2,5,8,11,14,17)])=="#")) {
        cnt <- cnt + 1
        print(paste("Smok w",row,col,cnt))
      }
    }
  }
  image_no_borders <- rotateAll(image_no_borders)
}
image_no_borders <- flipAllHoriz(image_no_borders)
for (i in 1:4) {
  cnt <- 0
  for (col in 1:77) {
    for (row in 1:94) {
      if (all(c(image_no_borders[row,(col-1)+19], 
                image_no_borders[row + 1,(col-1)+c(1,6,7,12,13,18,19,20)], 
                image_no_borders[row + 2,(col-1)+c(2,5,8,11,14,17)])=="#")) {
        cnt <- cnt + 1
        print(paste("Smok w",row,col,cnt))
      }
    }
  }
  image_no_borders <- rotateAll(image_no_borders)
}
roughness <- length(which(image_no_borders == "#")) - 15 * 26
print(roughness)

library(readr)
f <- scan("input11.txt", what = character())
f <- strsplit(f, split="")
mf <- matrix(unlist(f), ncol = 95, byrow = TRUE)

findInDir <- function(floor, j, i, dirCol, dirRow) {
  targetCol <- j
  targetRow <- i
  res <- NULL
  repeat {
    targetCol <- targetCol + dirCol
    targetRow <- targetRow + dirRow
    if (targetCol < 1 || targetCol > ncol(floor) || targetRow < 1 || targetRow > nrow(floor)){
      break;  
    }
    else if(floor[targetRow, targetCol] != ".") {
      res <- floor[targetRow, targetCol]
      break
    }
  }
  res
}
visible <- function(floor, i, j) {
  vis <- vector()
  
  vis <- append(vis, findInDir(floor,j,i,0,-1)) #top
  vis <- append(vis, findInDir(floor,j,i,1,-1)) #right-top
  vis <- append(vis, findInDir(floor,j,i,1,0)) #right
  vis <- append(vis, findInDir(floor,j,i,1,1)) #right-bottom
  vis <- append(vis, findInDir(floor,j,i,0,1)) #bottom
  vis <- append(vis, findInDir(floor,j,i,-1,1)) #left-bottom
  vis <- append(vis, findInDir(floor,j,i,-1,0)) #left
  vis <- append(vis, findInDir(floor,j,i,-1,-1)) #left-top
  vis
}

calcRound <- function(floor, newFloor){
  changes <<- 0
  newFloor <- floor
  for (i in 1:nrow(floor)) {
    for (j in 1:ncol(floor)){
      if (floor[i,j] != ".") {
        cnt <- length(which(visible(floor, i, j) == "#"))
        if (cnt == 0) {
          newFloor[i,j] = "#"
          if(floor[i,j] == "L") changes <<- changes + 1
        }
        else if (cnt >= 5){
          newFloor[i,j] = "L"
          if(floor[i,j] == "#") changes <<- changes + 1
        }
        else {
          newFloor[i,j] = floor[i,j]
        }
      }
      else {
        newFloor[i,j] = "."
      }
    }
  }
  return(newFloor)
}
loopcnt <- 0
changes <- 0
repeat {
  mf <- calcRound(mf)
  loopcnt <- loopcnt + 1
  print(paste(loopcnt,"changes", changes))
  if (changes == 0) break
} 

occupied <- length(which(mf == "#"))
print(occupied)
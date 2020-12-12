library(readr)
f <- scan("input11.txt", what = character())
f <- strsplit(f, split="")

adjacent <- function(floor, i, j) {
  adj <- vector()
  if (i > 1) {
    adj <- append(adj, floor[[i-1]][j])
    if (j > 1) adj <- append(adj, floor[[i-1]][j-1])
    if (j < length(floor[[1]])) adj <- append(adj, floor[[i-1]][j+1])
  }
  if (i < length(floor)) {
    adj <- append(adj, floor[[i+1]][j])
    if (j > 1) adj <- append(adj, floor[[i+1]][j-1])
    if (j < length(floor[[1]])) adj <- append(adj, floor[[i+1]][j+1])
  }
  if (j > 1) adj <- append(adj, floor[[i]][j-1])
  if (j < length(floor[[1]])) adj <- append(adj, floor[[i]][j+1])
  return(adj)
}

calcRound <- function(floor, newFloor){
  changes <<- 0
  newFloor <- floor
  for (i in 1:length(floor)) {
    for (j in 1:length(floor[[1]])){
      if (floor[[i]][j] != ".") {
        cnt <- length(which(adjacent(floor, i, j) == "#"))
        if (cnt == 0) {
          newFloor[[i]][j] = "#"
          if(floor[[i]][j] == "L") changes <<- changes + 1
        }
        else if (cnt >= 4){
          newFloor[[i]][j] = "L"
          if(floor[[i]][j] == "#") changes <<- changes + 1
        }
        else {
          newFloor[[i]][j] = floor[[i]][j]
        }
      }
      else {
        newFloor[[i]][j] = "."
      }
    }
  }
  return(newFloor)
}
loopcnt <- 0
changes <- 0
repeat {
  f <- calcRound(f)
  loopcnt <- loopcnt + 1
  print(paste(loopcnt,"changes", changes))
  if (changes == 0) break
} 

occupied <- 0
for (row in f) {
  for (p in  row){
    if (p == "#") occupied <- occupied + 1
  }
}
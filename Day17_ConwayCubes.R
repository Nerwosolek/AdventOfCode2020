library(readr)
f <- scan(file = "input17.txt", what = character())
slice <- matrix(unlist(str_split(f,"")), nrow = 8, byrow = TRUE)
m3d = array('.', c(22,22,15))


active_neigh <- function(cube, x, y, z) {
 coords <- as.matrix(expand.grid(x=c(-1,0,1),y=c(-1,0,1),z=c(-1,0,1)))
 active <- 0
 if (cube[x,y,z] == "#") active <- -1
 for (i in 1:nrow(coords)) {
   if (cube[x+coords[i,"x"], y+coords[i,"y"], z+coords[i,"z"]] == "#") {
     active <- active + 1
   }
 }
 active
}

calcRound <- function(cube, width, length, height) {
  newCube <- cube
  for (x in ((22-width)/2+1):((22+width)/2)) {
    for (y in ((22-length)/2+1):((22+length)/2)) {
      for (z in ((15-height)/2+1):((15+height)/2)){
        cnt <- active_neigh(cube, x, y, z)
        if (cube[x,y,z] == "#" && (cnt < 2 || cnt > 3)) newCube[x,y,z] <- "."
        if (cube[x,y,z] == "." && cnt == 3) newCube[x,y,z] <- "#"
      }
    }
  }
  return(newCube)
}

for(x in 8:15) {
  for (y in 8:15) {
    m3d[x,y,8] = slice[x-7,y-7]
  }
}

for (rep in 1:6) {
  if (rep == 6)
    browser()
  m3d <- calcRound(m3d, 8 + rep * 2, 8 + rep * 2, 1 + rep * 2)
}

print(length(which(m3d == "#")))
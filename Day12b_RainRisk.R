library(readr)
f <- scan("input12.txt", what = character())
directions <- list(N = c(0,-1), S = c(0,1), E = c(1,0), W = c(-1,0))
L = matrix(c(0, 1, -1, 0), byrow = TRUE, nrow = 2)
R = matrix(c(0, -1, 1, 0), byrow = TRUE, nrow = 2)
rotations <- list(R90 = R, R180 = R %*% R, R270 = L, L90 = L, L180 = L %*% L, L270 = R)
applyCommand <- function(state, command) {
  operator <- substr(command,1,1)
  operand <- as.integer(substr(command,2,nchar(command)))
  if (operator %in% c("N","W","S","E")) {
    state[c("wayX","wayY")] <- state[c("wayX","wayY")] + directions[[operator]] * operand
  }
  else if (operator %in% c("R","L")) {
    state[c("wayX","wayY")] <- as.vector(rotations[[command]] %*% as.matrix(unname(state[c("wayX","wayY")])))
  } else {
    state[c("posX","posY")] <- state[c("posX","posY")] + state[c("wayX","wayY")] * operand
  }
  state
}
startPos = c(0,0)
wayPos = c(10, -1)
currentPos = startPos
currentState = c(currentPos, wayPos)
names(currentState) = c("posX", "posY", "wayX", "wayY")

for (cmd in f) {
  currentState <- applyCommand(currentState, cmd)
}
print(paste("Manhattan distance =",sum(abs(currentState[c("posX","posY")]))))

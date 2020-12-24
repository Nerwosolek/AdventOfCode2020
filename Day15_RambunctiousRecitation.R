lastNumber <- 0
lastNumberPosition <- 6
l <- vector(length = 30000000, mode = "integer")
l[18] <- 1
l[2] <- 2
l[4] <- 3
l[17] <- 4
l[20] <- 5
l[1] <- 6
start <- Sys.time()
while(lastNumberPosition < 30000000) {
  res <- l[lastNumber+1]
  if (res != 0) res <- (lastNumberPosition - res)
  l[lastNumber+1] <- lastNumberPosition
  lastNumber <- res
  lastNumberPosition <- lastNumberPosition + 1
}
print(lastNumber, 22)
end <- Sys.time()
print((end - start),5)
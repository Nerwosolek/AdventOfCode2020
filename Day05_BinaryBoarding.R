t  <- read.csv("input5.txt", stringsAsFactors = FALSE, header = FALSE)
t$V1 <- gsub("R|B","1",t$V1)
t$V1 <- gsub("L|F","0",t$V1)
t$V1 <- strtoi(t$V1, base = 2)
max(t$V1)
k <- t$V1[order(t$V1)]
k[which(diff(k) == 2)]+1
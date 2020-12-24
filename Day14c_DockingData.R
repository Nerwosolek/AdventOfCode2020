library(readr)
f = read_lines("input14.txt")
parseRow <- function(row) {
  pair <- str_split(row, "=")
  pair <- c(str_trim(pair[[1]][1]), str_trim(pair[[1]][2]))
  if (substr(pair[1],1,4) == "mask") {
    v <- c(pair[2])
    names(v) <- c("mask")
    return(v)
  }
  else {
    pair[1] = str_sub(str_extract(pair[1],"\\[[0-9]+\\]"),2,-2)
    names(pair) <- c("address","value")
    return(pair)
  }
}

mask <- function(value, mask) {
  temp <- rev(append(intToBits(as.integer(value)), c(0,0,0,0)))
  maskVec <- unlist(str_split(mask,""))
  temp[which(maskVec != "X")] = maskVec[which(maskVec != "X")]
  temp<-strtoi(str_c(temp[1:5],collapse = ""),base=2) * 2^31 + strtoi(str_c(temp[6:36],collapse = ""),base=2)
  return(temp)
}

floatingMask <- function(value, mask) {
  temp <- rev(append(intToBits(as.integer(value)), c(0,0,0,0)))
  maskVec <- unlist(str_split(mask,""))
  temp[which(maskVec == "1")] = maskVec[which(maskVec == "1")]
  temp[which(maskVec == "X")] = maskVec[which(maskVec == "X")]
  xCount <- length(maskVec[which(maskVec == 'X')])
  comb01 <- as.matrix(expand.grid(rep(list(c(1,0)),xCount)))
  combMaskedList <- rep(list(temp),2^xCount)
  for (i in 1:2^xCount) {
    combMaskedList[[i]][which(combMaskedList[[i]] == 'X')] = comb01[i,]
    combMaskedList[[i]] <- strtoi(str_c(combMaskedList[[i]][1:5],collapse = ""),base=2) * 2^31 
                         + strtoi(str_c(combMaskedList[[i]][6:36],collapse = ""),base=2)
  }
  return(combMaskedList) 
}
mem <- list()
for(entry in f) {
  row<-parseRow(entry)
  if (names(row)[1] == "mask") {
    currentMask <- row["mask"]
    #print(currentMask)
  } else {
    lapply(floatingMask(row["address"], currentMask), function(addr) {mem[[as.character(addr)]] <<- row["value"]})
  }
}
print(sum(as.double(unname(unlist(mem)))),22)


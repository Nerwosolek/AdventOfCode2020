library(readr)
f = read_lines("input14.txt")
df <- data.frame(stringsAsFactors = FALSE)

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
currentMask <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
mem <- list()
for(entry in f) {
  row<-parseRow(entry)
  if (names(row)[1] == "mask") {
    currentMask <- row["mask"]
    #print(currentMask)
  } else {
    mem[[row["address"]]] <- mask(row["value"], currentMask)
    #print(typeof(row))
    #print(row[2])
  }
}
#unname(unlist(mem))


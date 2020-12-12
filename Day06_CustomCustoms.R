t  <- scan("input6.txt", what = character(), multi.line = TRUE, blank.lines.skip = FALSE)
g = 1
df <- data.frame(answer=character(), group=integer(), stringsAsFactors=FALSE)
t <- str_split(t,"")
result <- list()
un <- list()
new_group = TRUE
for (v in t) {
  #print(v)
  if (length(v) == 0) {
    print("------------------")
    g = g + 1
    new_group = TRUE
    result[[length(result) + 1]] <- temp
    un[[length(un) + 1]] <- u
    temp = v
    u = v
  }
  else {
    print(sort(v))
    if (new_group) {
      temp = v
      u = v
      new_group = FALSE
    }
    temp = intersect(temp, v)
    cat(">>", sort(temp), "\n")
    u = union(u, v)
    #df <- rbind(df, c(answer=v, group=g), stringsAsFactors=FALSE)
  }
}

rr = unlist(lapply(result, length))
uu = unlist(lapply(un, length))
rr
sum(unlist(lapply(result, length)))
sum(unlist(lapply(un, length)))

#df <- unique(df)
#colnames(df)<- c("answer","group") 
#intersect(group_by(df, group)$n)
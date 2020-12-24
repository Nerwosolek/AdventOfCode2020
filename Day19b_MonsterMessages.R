library(readr)
library(stringr)
f <- scan("input19.txt", what = character(), sep = "\n")
rules <- f[1:135]
messages <- f[136:length(f)]
rule0 <- rules[str_detect(rules, "^0:")]
rule0 <- substr(rule0, 4, nchar(rule0))
rule0 <- as.integer(unlist(str_split(rule0," ")))
rules <- rules[-which(str_detect(rules, "^0:"))]
rules <- rules[order(as.integer(str_extract(rules, "^[0-9]+")))]
rules <- str_sub(rules,str_locate(rules, "^[0-9]+:\\s")[,2]+1)
rules[which(str_detect(rules,"[a,b]"))] <- str_extract(rules[str_detect(rules,"[a,b]")],"[a,b]")

isMatch <- function (rules, msg, ruleList) {
  if (rules[ruleList[1]] %in% c("a","b")) {
    if (substr(msg,1,1) != rules[ruleList[1]]) return(FALSE)
    else {
      msg <- substr(msg,2,nchar(msg))
      if (nchar(msg) == 0 && length(ruleList[-1]) == 0) return(TRUE)
      else if(nchar(msg) > 0 && length(ruleList[-1]) > 0) 
        return(isMatch(rules, msg, ruleList[-1]))
      else return(FALSE)
    }
  } else {
    rulesAlts <- str_split(str_trim(str_split(rules[ruleList[1]],"\\|")[[1]]), " ")
    for (i in 1:length(rulesAlts)) {
      rulesAlts[[i]] <- as.integer(rulesAlts[[i]])
    }
    if (length(rulesAlts) == 2) return(isMatch(rules, msg, c(rulesAlts[[1]], ruleList[-1])) || 
                                       isMatch(rules, msg, c(rulesAlts[[2]], ruleList[-1])))
    if (length(rulesAlts) == 1) return(isMatch(rules, msg, c(rulesAlts[[1]], ruleList[-1])))
  }
}

start <- Sys.time()
matches <- 0
for (m in messages) {
  if (isMatch(rules, m, rule0)) matches <- matches + 1
}
end <- Sys.time()
print(end - start)
# isMatch(rules, messages[1], rule0)



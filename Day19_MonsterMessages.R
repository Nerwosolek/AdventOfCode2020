library(readr)
library(stringr)
f <- scan("input19_no_loop.txt", what = character(), sep = "\n")
rules <- f[1:135]
messages <- f[136:length(f)]
rule0 <- rules[str_detect(rules, "^0:")]
rule0 <- substr(rule0, 4, nchar(rule0))
rules <- rules[-which(str_detect(rules, "^0:"))]
rules <- rules[order(as.integer(str_extract(rules, "^[0-9]+")))]
rules <- str_sub(rules,str_locate(rules, "^[0-9]+:\\s")[,2]+1)
rules[which(str_detect(rules,"[a,b]"))] <- str_extract(rules[str_detect(rules,"[a,b]")],"[a,b]")


#messages[1]
parse <- function(rules, rule, depth = 1, maxddepth = 1) {
  #if (depth > maxddepth && (rule == "11" || rule == "8")) return("X")
  #if (!is.character(rule) || is.na(rule)) browser()
  if (str_detect(rule,"^[a,b]$")) return(rule)
  else if (str_detect(rule, "^[0-9]+$")) {
    rule <- rules[as.integer(rule)]
    #if (!is.character(rule) || is.na(rule)) browser()
    return(parse(rules, rule, depth + 1, maxddepth))
  }
  temp_rule <- str_split(str_trim(str_split(rule,"\\|")[[1]]), " ")
  temp_vector <- vector()
  for (t in temp_rule) {
    #if (!is.character(t[1]) || is.na(t[1])) browser()
    #if (!is.character(t[2]) || is.na(t[2])) browser()
    if (length(t) == 3) {
      g <- expand.grid(parse(rules, t[1], depth + 1, maxddepth), parse(rules, t[2], depth + 1, maxddepth), parse(rules, t[3], depth + 1, maxddepth), stringsAsFactors = FALSE)
      temp_vector <- append(temp_vector, paste0(g$Var1, g$Var2, g$Var3))
    }
    else if (length(t) == 2) {
      g <- expand.grid(parse(rules, t[1], depth + 1, maxddepth), parse(rules, t[2], depth + 1, maxddepth), stringsAsFactors = FALSE)
      temp_vector <- append(temp_vector, paste0(g$Var1, g$Var2))
    }
    else {
      #if (!is.character(t[1]) || is.na(t[1])) browser()
      temp_vector <- append(temp_vector, parse(rules, t[1], depth + 1, maxddepth))
    }
  }
  return (temp_vector)
}

msgCnt <- 0
matchedMsg <- vector()
strs <- parse(rules, rule0)
for (message in messages) {
  if (any(message == strs)) {
    msgCnt <- msgCnt + 1
    matchedMsg <- append(matchedMsg, message)
  }
}
print(msgCnt)

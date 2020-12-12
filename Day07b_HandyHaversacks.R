library("stringr")
t  <- scan("input7.txt", what = character(), multi.line = FALSE, blank.lines.skip = TRUE,sep = "@")
find_inner_bags <- function(bags, lookup_vector) {
  res <- 0
  if (is.matrix(bags)) {
    for (i in 1:nrow(bags)) {
      m <- str_extract_all(str_extract_all(t[grep(paste0("^",bags[i,2]), t)], 
                                           "[1-9]([:blank:][a-z]+){3}|no other bags", simplify = TRUE), 
                           "[1-9]|no|([a-z]+[:blank:]){2}bag[s]?", simplify = TRUE)
      if (m[1,1] == "no") res <- res + as.integer(bags[i,1])
      else res <- res + as.integer(bags[i,1]) + as.integer(bags[i,1]) * find_inner_bags(m, lookup_vector)
    }
  } else {
    m <- str_extract_all(str_extract_all(t[grep(paste0("^",bags), t)], 
                                         "[1-9]([:blank:][a-z]+){3}|no other bags", simplify = TRUE), 
                         "[1-9]|no|([a-z]+[:blank:]){2}bag[s]?", simplify = TRUE)
    if (m[1,1] == "no") res
    else res <- res + find_inner_bags(m, lookup_vector)
  }
  res
}


find_inner_bags("clear green bags",t)

find_inner_bags("shiny gold bags",t)

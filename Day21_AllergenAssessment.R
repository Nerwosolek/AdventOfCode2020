library(readr)
library(stringr)
f <- scan("input21.txt", what = character(), sep = "\n")
food <- str_split(f, " ")
allergens <- str_sub(str_trim(str_extract(str_extract(f, "contains .+"), " .+")), 1, -2)
unique_allergens <- unique(unlist(str_split(allergens, ",\\s")))

for (i in 1:42) {
  food[[i]] <- food[[i]][-(which(food[[i]] == "(contains"):length(food[[i]]))]
}
foods_with_allergens <- list()
for (i in 1:length(unique_allergens)) {
  foods_with_allergens[unique_allergens[i]] <- 
    list(food[c(which(str_detect(allergens, paste0("\\s",unique_allergens[i],"|^",unique_allergens[i]))))])
}
possible <- vector()
for (a in foods_with_allergens) {
  inter <- a[[1]]
  for (i in 2:length(a)) {
    inter <- intersect(inter, a[[i]])
  }
  possible <- append(possible, inter)
}
possible <- unique(possible)
cntPossible <- 0
for (p in possible) {
  cntPossible <- cntPossible + length(which(unlist(food) == p))
}
cntNotPossible <- length(unlist(food)) - cntPossible

allergeticIngredients <- list()
for (n in names(foods_with_allergens)) {
  inter <- foods_with_allergens[[n]][[1]]
  for (i in 2:length(foods_with_allergens[[n]])) {
    inter <- intersect(inter, foods_with_allergens[[n]][[i]])
  }
  allergeticIngredients[[n]] <- inter
}

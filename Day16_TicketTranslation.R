library(readr)
library(stringr)
f = scan("input16.txt", what = character(), sep = "\n")
ranges <- unlist(str_extract_all(f[1:20], "[0-9]+-[0-9]+"))
nearby_tickets <- f[24:263]
all_fields_values <- as.integer(unlist(str_split(nearby_tickets,",")))
ranges_matrix <- matrix(as.integer(unlist(str_split(ranges,"-"))), ncol = 2, byrow = TRUE) 
all_fields_values >= ranges_matrix[,1]
error_rate <- 0
for (v in all_fields_values) {
  if (!any(v >= ranges_matrix[,1] & v <= ranges_matrix[,2])) error_rate <- error_rate + v
}

print(error_rate)

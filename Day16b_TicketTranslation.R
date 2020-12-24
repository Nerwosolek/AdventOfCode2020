library(readr)
library(stringr)
f = scan("input16.txt", what = character(), sep = "\n")
ranges <- unlist(str_extract_all(f[1:20], "[0-9]+-[0-9]+"))
#ranges_names <- str_remove(unlist(str_extract_all(f[1:20], "^.+:")), ":")
nearby_tickets <- f[24:263]
all_fields_values <- as.integer(unlist(str_split(nearby_tickets,",")))
ranges_matrix <- matrix(as.integer(unlist(str_split(ranges,"-"))), ncol = 2, byrow = TRUE) 
#names(ranges_matrix) <- ranges_names
#all_fields_values >= ranges_matrix[,1]
error_rate <- 0
del_indexes <- vector()
stay_indexes <- vector()
for (i in 1:length(all_fields_values)) {
  if (!any(all_fields_values[i] >= ranges_matrix[,1] & all_fields_values[i] <= ranges_matrix[,2])) {
    error_rate <- error_rate + all_fields_values[i]
    del_indexes <- append(del_indexes, ceiling(i / 20))
  }
}
nearby_valid_tickets <- str_split(nearby_tickets[-del_indexes],",")
ranges_matrix <- matrix(as.integer(unlist(str_split(ranges,"-"))), ncol = 4, byrow = TRUE) 
for (i in 1:20) ranges_matrix <- cbind(ranges_matrix, c(rep(0,20)))

nearby_valid_tickets <- lapply(nearby_valid_tickets, as.integer)
mValidTickets <- matrix(unlist(nearby_valid_tickets), ncol = 20, byrow = TRUE)
for (i in 1:nrow(ranges_matrix)) {
  for (t in 1:20) {
    if (all((mValidTickets[, t] >= ranges_matrix[i,1] & mValidTickets[, t] <= ranges_matrix[i,2]) |
            (mValidTickets[, t] >= ranges_matrix[i,3] & mValidTickets[, t] <= ranges_matrix[i,4]))) {
      ranges_matrix[i,4+t] <- t
    }
  }
}

sum(ranges_matrix[,5:24] == 0)




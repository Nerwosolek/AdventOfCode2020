t  <- scan("input7.txt", what = character(), multi.line = FALSE, blank.lines.skip = TRUE,sep = "@")
find_outer_bags <- function(bag_names, lookup_vector) {
  result_vector <- vector()
  for (bag_name in bag_names) {
    result_vector <- c(result_vector, str_extract(lookup_vector[grep(paste0("^.+",bag_name), lookup_vector)], "^[a-z]+[:blank:][a-z]+[:blank:]bag"))
  }
  result_vector
}
find_all_outer_bags <- function(starting_name, lookup_vector) {
  total_outer_bags <- vector()
  result_one_level <- find_outer_bags(starting_name, lookup_vector)
  while (length(result_one_level) > 0) {
    total_outer_bags <- union(total_outer_bags, unique(result_one_level))
    result_one_level <- find_outer_bags(unique(result_one_level), lookup_vector)
  }
  unique(total_outer_bags)
}

<<<<<<< HEAD
=======
#find_outer_bags("shiny gold bag", t)
#print("-----")
#find_outer_bags(find_outer_bags("shiny gold bag", t),t)
#unique(find_outer_bags(find_outer_bags("shiny gold bag", t),t))
>>>>>>> 8e44d05... 12 days ready for GitHub
find_all_outer_bags("shiny gold bag", t)
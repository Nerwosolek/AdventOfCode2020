library(readr)
timestamp = as.integer(read_lines("input13.txt", n_max = 1L))
buses = str_split(read_lines("input13.txt", skip = 1, n_max = 1L), ",")[[1]]
buses <- as.integer(buses[which(buses != 'x')])
result <- min(buses - timestamp %% buses) * buses[which.min(buses - timestamp %% buses)]
print(result)
schedule = str_split(read_lines("input13.txt", skip = 1, n_max = 1L), ",")[[1]]

rests <- which(schedule != 'x') - 1
dividers <- as.double(schedule[which(schedule != 'x')])
rests <- (dividers - rests) %% dividers

a <- as.double(1.0)
while (!all((((a * 863 - 791) %% dividers) == rests) == TRUE)) {
  a <- a + 1
  if (a %% 1000000 == 0) print(a)
}
print(paste(a, a * 863 - 791))
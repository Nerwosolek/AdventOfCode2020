library(readr)
f <- read_delim("input10.txt", delim =" ", col_types = "n", col_names = "Liczba")
f <- rbind(f, 0) 
f <- rbind(f, max(f$Liczba)+3) 
s <- sort(f$Liczba)
d <- diff(s)
length(d[which(d == 1)]) * length(d[which(d == 3)])

H <- new.env(hash = TRUE)
global_counter <- 0
cnt_next <- function(seq, start) {
  global_counter <<- global_counter + 1
  if (global_counter %% 1000 == 0) print(global_counter)
  sum <- 0
  if (start == length(seq)) return(1)
  if (!is.null(H[[as.character(start)]])) return(H[[as.character(start)]])
  if (start < length(seq) && seq[start+1]-seq[start] < 4) sum <- sum + cnt_next(seq, start+1)
  if (start < length(seq) - 1 && seq[start+2]-seq[start] < 4) sum <- sum + cnt_next(seq, start+2)
  if (start < length(seq) - 2 && seq[start+3]-seq[start] < 4) sum <- sum + cnt_next(seq, start+3)
  #print(sum)
  H[[as.character(start)]] <- sum
  return(sum)
}

res <- cnt_next(s, 1)
print(res, 22)


f <- read_delim("input9.txt", delim =" ", col_types = "n", col_names = "Liczba")
for(i in 26:nrow(f)) {
  if (sum(with(expand.grid(f[(i-25):(i-1),]$Liczba,
                   f[(i-25):(i-1),]$Liczba,
                   f[i,]$Liczba),
       Var1+Var2 == Var3 & Var1 != Var2)) == 0) {
    res <- f[i,]$Liczba
    break;
  }
}
for (i in 1:505) {
  for (k in (i + 1):506) {
    suma <- sum(f[i:k,]$Liczba)
    if (suma > res) break;
    if (suma == res) {
      res2 <- min(f[i:k,]$Liczba)+max(f[i:k,]$Liczba)
    }
  }
}
t  <- read.table("input.txt")
names(t) <- c("value")
for (i in c(1:198))
{
  #print(i)
  for (k in c((i+1):199))
  {
    #print(k)
    for (l in c((k+1):200))
    {
      if (t[i,] + t[k,] + t[l,] == 2020) {
        print (t[i,] * t[k,] * t[l,])
        print (t[i,])
        print (t[k,])
        print (t[l,])
      }    
    }
    
  }
}


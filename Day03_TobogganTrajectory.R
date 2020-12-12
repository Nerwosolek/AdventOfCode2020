library(tidyr)
library(dplyr)
t  <- read.csv("input3.txt", header = FALSE, stringsAsFactors = FALSE)
slopes_right = c(1,3,5,7,1)
slopes_down = c(1,1,1,1,2)
for (i in c(1:5))
{
  poz_x = 1
  tree_count = 0
  for (poz_y in c(seq(1,length(t[,1]),by=slopes_down[i])))
  {
    if (substr(t[poz_y,], poz_x, poz_x) == "#")
    {
      tree_count = tree_count + 1
    }
    poz_x <- (poz_x + slopes_right[i]-1) %% 31 + 1
  }
  print(paste(tree_count, slopes_right[i], slopes_down[i]))  
}

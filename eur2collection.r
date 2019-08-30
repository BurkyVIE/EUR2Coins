library(tidyverse)

read_lines(file = "eur2collection.txt") %>% 
  enframe(name = "Zeilennummer", value = "Sammlung") %>% 
  separate(Sammlung, into = c("ID", "Qualit�t"), sep = "-", convert = TRUE) %>% 
  left_join(coins %>% select(ID, Abbildung, M�nzzeichen),by = "ID") -> collection

library(tidyverse)

read_lines(file = "eur2collection.txt") %>% 
  enframe(name = "Zeilennummer", value = "Sammlung") %>% 
  separate(Sammlung, into = c("ID", "Qualität"), sep = "-", convert = TRUE) %>% 
  left_join(coins %>% select(ID, Abbildung, Münzzeichen),by = "ID") -> collection

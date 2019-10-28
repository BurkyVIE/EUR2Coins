library(tidyverse)

read_lines(file = "eur2collection.txt") %>% 
  enframe(name = "Zeilennummer", value = "Sammlung") %>% 
  separate(Sammlung, into = c("ID", "QualitÃ¤t"), sep = "-", convert = TRUE) %>%
  mutate(Box = (Zeilennummer - 1)  %/% 144 + 1,
         Tableau = (Zeilennummer - 1) %/% 24 %% 6 + 1,
         Spalte = (Zeilennummer - 1) %% 6 + 1,
         Zeile = (Zeilennummer - 1) %/% 6 %% 4 + 1,
         Ablage = paste0(Box, Tableau, Spalte, Zeile, "x", sprintf("%04d", Zeilennummer))) %>% 
  left_join(coins %>% select(ID, Abbildung, Münzzeichen),by = "ID") -> collection

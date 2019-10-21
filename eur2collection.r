library(tidyverse)

read_lines(file = "eur2collection.txt") %>% 
  enframe(name = "Zeilennummer", value = "Sammlung") %>% 
  separate(Sammlung, into = c("ID", "Qualität"), sep = "-", convert = TRUE) %>%
  mutate(Box = (Zeilennummer - 1)  %/% 144 + 1,
         Tableau = (Zeilennummer - 1) %/% 24 %% 6 + 1,
         Spalte = (Zeilennummer - 1) %% 6 + 1,
         Zeile = (Zeilennummer - 1) %/% 6 %% 4 + 1,
         Ablage = paste0(Box, "|", Tableau, "|", Spalte, "x", Zeile)) %>% 
  left_join(coins %>% select(ID, Abbildung, Münzzeichen),by = "ID") -> collection

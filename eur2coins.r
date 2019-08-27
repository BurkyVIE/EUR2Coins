library(tidyverse)
library(lubridate)

read_delim("eur2coins_celex.txt", delim ="|", locale = locale(encoding = "WINDOWS-1252")) %>% 
  mutate(Ausgabe = lubridate::ymd(Ausgabe),
         Münzart = factor(Münzart, levels = c("g", "u"), labels = c("Gedenkmünze", "Umlaufmünze")),
         Münzzeichen = str_split(Münzzeichen, pattern = ",")
         ) %>% 
  unnest() %>% 
  add_column(cs = 1) %>% 
  group_by(Land, Münzart, Prägejahr) %>% 
  mutate(ID = paste0(Prägejahr, Land, tolower(substr(Münzart, 1, 1)), cumsum(cs))) %>% 
  ungroup() %>% 
  mutate(Prägejahr = as.integer(Prägejahr),
         Land = toupper(Land)) %>% 
  select(Ausgabe, Münzart, Prägejahr, Land, Abbildung, Münzzeichen, Amtsblatt, ID) -> coins

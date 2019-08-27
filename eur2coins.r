library(tidyverse)
library(lubridate)

read_delim("eur2coins_celex.txt", delim ="|", locale = locale(encoding = "WINDOWS-1252")) %>% 
  mutate(Ausgabe = lubridate::ymd(Ausgabe),
         M�nzart = factor(M�nzart, levels = c("g", "u"), labels = c("Gedenkm�nze", "Umlaufm�nze")),
         M�nzzeichen = str_split(M�nzzeichen, pattern = ",")
         ) %>% 
  unnest() %>% 
  add_column(cs = 1) %>% 
  group_by(Land, M�nzart, Pr�gejahr) %>% 
  mutate(ID = paste0(Pr�gejahr, Land, tolower(substr(M�nzart, 1, 1)), cumsum(cs))) %>% 
  ungroup() %>% 
  mutate(Pr�gejahr = as.integer(Pr�gejahr),
         Land = toupper(Land)) %>% 
  select(Ausgabe, M�nzart, Pr�gejahr, Land, Abbildung, M�nzzeichen, Amtsblatt, ID) -> coins

library(tidyverse)

read_delim("eur2coins_celex.txt", delim ="|", locale = locale(encoding = "WINDOWS-1252"),
           col_types = cols(Amtsblatt = col_character(),
                            Land = col_character(),
                            Prägejahr = col_integer(),
                            Ausgabe = col_date(format = "%F"),
                            Münzart = col_character(),
                            Abbildung = col_character(),
                            Münzzeichen = col_character())) %>% 
  mutate(Münzart = factor(Münzart, levels = c("g", "u"), labels = c("Gedenkmünze", "Umlaufmünze")),
         Münzzeichen = str_split(Münzzeichen, pattern = ",")
         ) %>% 
  unnest() %>% 
  add_column(cs = 1) %>% 
  group_by(Land, Münzart, Prägejahr) %>% 
  mutate(ID = paste0(Prägejahr, Land, tolower(substr(Münzart, 1, 1)), cumsum(cs) %>% sprintf("%02d", .))) %>% 
  ungroup() %>% 
  mutate(Land = toupper(Land), 
         Münzzeichen = na_if(Münzzeichen, "_")) %>% 
  select(Ausgabe, Münzart, Prägejahr, Land, Abbildung, Münzzeichen, Amtsblatt, ID) -> coins

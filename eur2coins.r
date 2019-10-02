library(tidyverse)

read_delim("eur2coins_celex.txt", delim ="|", locale = locale(encoding = "UTF-8"),
           col_types = cols(Amtsblatt = col_character(),
                            Land = col_character(),
                            Pr�gejahr = col_integer(),
                            Ausgabe = col_date(format = "%F"),
                            M�nzart = col_character(),
                            Abbildung = col_character(),
                            M�nzzeichen = col_character())) %>% 
  mutate(M�nzart = factor(M�nzart, levels = c("g", "u"), labels = c("Gedenkm�nze", "Umlaufm�nze")),
         M�nzzeichen = str_split(M�nzzeichen, pattern = ",")) %>% 
  unnest(cols = c(M�nzzeichen)) %>%   # Erweitern um die M�nzzeichen
  add_column(cs = 1) %>% 
  group_by(Land, M�nzart, Pr�gejahr) %>% 
  mutate(ID = paste0(Pr�gejahr, Land, tolower(substr(M�nzart, 1, 1)), cumsum(cs) %>% sprintf("%02d", .))) %>% 
  ungroup() %>% 
  mutate(Land = toupper(Land), 
         M�nzzeichen = coalesce(M�nzzeichen, "")) %>% 
  select(Ausgabe, M�nzart, Pr�gejahr, Land, Abbildung, M�nzzeichen, Amtsblatt, ID) -> coins

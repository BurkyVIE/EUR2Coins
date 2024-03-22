# LIBRARIES ----
library(tidyverse)

# GLOBAL ----
## where is directory ----
directory <- ""

## import celex_*.txt ----
import_celex <- function(file) {
  read_delim(paste0(directory,file), delim ="|", locale = locale(encoding = "UTF-8"), lazy = FALSE,
             col_types = cols(Amtsblatt = col_character(),
                              Land = col_character(),
                              Prägejahr = col_integer(),
                              Ausgabe = col_date(format = "%F"),
                              Münzart = col_character(),
                              Abbildung = col_character(),
                              Münzzeichen = col_character()))
}

# IMPORT ----
## get list of files ----
filelist <- dir()
filelist <- filelist[startsWith(filelist, "eur2coins_celex")]

## do import
raw <- map_df(filelist, ~import_celex(.))

# TIDY ----
## do tidy ----
raw %>% 
  mutate(Münzart = factor(Münzart, levels = c("g", "u"), labels = c("Gedenkmünze", "Umlaufmünze")),
         Münzzeichen = str_split(Münzzeichen, pattern = ",")) %>% 
  unnest(Münzzeichen) %>% # Erweitern um die Münzzeichen
  add_column(cs = 1) %>% # Hilfsvariable für Durchnumerierung (ID)
  group_by(Land, Münzart, Prägejahr) %>% 
  mutate(ID = paste0(Prägejahr, Land, tolower(substr(Münzart, 1, 1)), cumsum(cs) %>% sprintf("%02d", .))) %>% 
  ungroup() %>% 
  mutate(Land = toupper(Land), 
         Münzzeichen = coalesce(Münzzeichen, "")) %>% 
  select(Ausgabe, Münzart, Prägejahr, Land, Abbildung, Münzzeichen, Amtsblatt, ID) -> coins

# CLEAN UP ----
##do clean up ----
rm(directory, import_celex, filelist, raw)

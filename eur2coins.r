# LIBRARIES ----
library(tidyverse)

# GLOBAL ----
## where is directory ----
directory <- paste0(getwd(), "/") # "getwd()" oder für die Weiterentwicklung (= Teilung der CELEX-Daten): "celex"

## import celex_*.txt ----
import_celex <- function(file) {
  read_delim(paste0(directory, file), delim ="|", locale = locale(encoding = "UTF-8"), lazy = FALSE, comment = "#",
             col_types = cols(Amtsblatt = col_character(),
                              Land = col_character(),
                              Prägejahr = col_character(),
                              Ausgabe = col_date(format = "%F"),
                              Münzart = col_character(),
                              Abbildung = col_character(),
                              Münzzeichen = col_character()))
}

# IMPORT ----
## get list of files ----
filelist <- dir(directory)
filelist <- filelist[startsWith(filelist, "eur2coins_celex")]

## do import
raw <- map_df(filelist, ~import_celex(.))

# TIDY ----
## do tidy ----
raw %>% 
  mutate(Prägejahr =str_split(Prägejahr, pattern = ",")) |> 
  unnest(Prägejahr) |>
  mutate(Prägejahr = as.integer(Prägejahr),
         Münzart = factor(Münzart, levels = c("g", "k"), labels = c("Gedenkmünze", "Kursmünze")),
         Münzzeichen = str_split(Münzzeichen, pattern = ",")) |> 
  unnest(Münzzeichen) |> # Erweitern um die Münzzeichen
  add_column(cs = 1) |> # Hilfsvariable für Durchnumerierung (ID)
  group_by(Land, Münzart, Prägejahr) |> 
  mutate(ID = paste0(Prägejahr, Land, tolower(substr(Münzart, 1, 1)), cumsum(cs) |> str_pad(2, pad = "0"))) |>
  ungroup() |> 
  mutate(Land = toupper(Land), 
         Münzzeichen = coalesce(Münzzeichen, "")) |> 
  select(Ausgabe, Münzart, Prägejahr, Land, Abbildung, Münzzeichen, Amtsblatt, ID) -> coins

# CLEAN UP ----
##do clean up ----
rm(directory, import_celex, filelist, raw)

# LIBRARIES ----
library(tidyverse)

# INITIALISIEREN ----
## Alle möglichen Bilder lt. 'celex' bzw 'coins'
base <- select(coins, Amtsblatt, Land, ID) |> 
  mutate(PicFile = paste0("cpic/",
                          str_sub(Land, 1, 1),
                          str_remove_all(Amtsblatt, "[\\D]"),
                          str_sub(Land, 2, 2),
                          ".jpg")) |> 
  nest(IDs = ID) |> 
  mutate(Exists = FALSE)

## Wenn 'cpic' schon existiert, bereits erfolgte Prüfungen übernehmen ----
if (exists("cpic")) {
  picfiles <- filter(cpic, Exists) |> pull(PicFile) |> unique()
  base <- mutate(base, Exists = PicFile %in% picfiles)
  rm(picfiles)
}
  
## Trenne Vorhandene von Nicht-Vorhandenen ----
keep <- filter(base, Exists)
check <- filter(base, !Exists)

## Prüfe auf neue Vorhandene ----
check <- mutate(check, Exists = map_lgl(PicFile, ~file.exists(.)))

## Füge wieder zusammen ----
cpic <- bind_rows(keep, check) |> 
  unnest(cols = IDs) |> 
  arrange(ID)

# AUFRÄUMEN ----
rm(base, keep, check)

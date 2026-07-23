# LIBRARIES ----
library(tidyverse)

# QUERY ----
cpic |> 
  # 1. Filtern und Zusammenführen
  filter(!Exists) |> 
  inner_join(
    filter(collection, !is.na(Zeilennummer)), 
    by = "ID"
  ) |> 
  
  # 2. Relevante Spalten extrahieren & transformieren
  transmute(
    Land,
    Art      = toupper(str_sub(ID, 7, 7)),
    Jahr     = as.integer(str_sub(ID, 1, 4)),
    Abbildung,
    Filename = str_extract(PicFile, "[A-Z]\\d*[A-Z]")
  ) |> 
  
  # 3. Duplikate entfernen & nach Gruppen sortieren
  distinct() |> 
  group_by(Land, Art, Abbildung, Filename) |> 
  
  # 4. Kleinste(s) Jahr je Gruppe wählen (robust gegen leere Tibbles)
  slice_min(order_by = Jahr, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  
  # 5. Spaltenanordnung & finale Sortierung
  relocate(Jahr, .after = Art) |> 
  arrange(Land, Jahr)

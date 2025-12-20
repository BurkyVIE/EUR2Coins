# LIBRARIES ----
library(tidyverse)

# QUERY ----
filter(cpic, !Exists) |>
  inner_join(filter(collection, !is.na(Zeilennummer)), by = "ID") |>
  transmute(Land,
            Art = toupper(str_sub(ID, 7, 7)),
            Jahr = as.integer(str_sub(ID, 1, 4)),
            Abbildung,
            Filename = str_extract(PicFile, "[A-Z]\\d*[A-Z]")) |>
  unique() |> 
  group_by(Land, Art, Abbildung, Filename) |> 
  summarise(Jahr = min(Jahr), .groups = "drop") |>
  relocate(Jahr, .after = Art) |>
  arrange(Land, Jahr) 

library(tidyverse)

select(coins, Amtsblatt, Land, ID) |>
  nest(IDs = ID) |>
  mutate(PicFile = paste0("cpic/",
                       str_sub(Land, 1, 1),
                       str_remove_all(Amtsblatt, "[\\D]"),
                       str_sub(Land, 2, 2),
                       ".jpg"),
         Exists = map_lgl(PicFile, ~file.exists(.))) |> 
  # filter(Exists) |> 
  unnest(cols = IDs) -> cpic

filter(cpic, !Exists) |>
  inner_join(filter(collection, !is.na(Zeilennummer)), by = "ID") |>
  transmute(Land,
            Jahr = as.integer(str_sub(ID, 1, 4)),
            Abbildung,
            Filename = str_extract(PicFile, "[A-Z]\\d*[A-Z]")) |>
  unique() |> 
  group_by(Land, Abbildung, Filename) |> 
  summarise(min_Jahr = min(Jahr)) |> 
  relocate(min_Jahr, .after = Land) |>
  arrange(Land, min_Jahr) 
  

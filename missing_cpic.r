filter(cpic, !Exists) |>
  inner_join(filter(collection, !is.na(Zeilennummer)), by = "ID") |>
  transmute(Land,
            Jahr = as.integer(str_sub(ID, 1, 4)),
            Abbildung,
            Filename = str_extract(PicFile, "[A-Z]\\d*[A-Z]")) |>
  unique() |>
  arrange(Land, Jahr)

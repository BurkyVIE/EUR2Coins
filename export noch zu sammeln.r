library(tidyverse)

df <- left_join(coins,
          collection %>% select(ID, Qualität, Ablage),
          by = 'ID') |>
  filter(is.na(Qualität)) |> # nur noch nicht gesammelte
  transmute(Bild = paste(Prägejahr, Land, paste0("(", str_sub(Münzart, 1, 1), ")"), str_sub(Abbildung, 1, 75)), Münzzeichen) |>
  group_by(Bild) |>
  summarise(Zeichen = paste(Münzzeichen, collapse = ", "))


knitr::kable(df, format = "pipe") |>
  writeClipboard()

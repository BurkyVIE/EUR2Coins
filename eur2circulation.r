library(tidyverse)

read_lines(file = "eur2coins_circulation.txt", lazy = FALSE) |>
  enframe(name = NULL, value = "Data") |>
  separate(Data, into = c("ID", "Auflage"), sep = "-", convert = TRUE) |> 
  mutate(Hfgkt = cut(Auflage, breaks = c(0, 5e4, 5e5, 5e6, 5e7, Inf), labels = 1:5)) |> # c("⇓", "⇘", "⇒", "⇗", "⇑") c("≤ 50k", "≤ 500k", "≤ 5m", "≤ 50m", "> 50m") c("v.l", "low", "med", "hgh", "v.h)
  arrange(ID) -> circulation

# Sortieren der Einträge
# read_lines(file = "eur2coins_circulation.txt", lazy = FALSE) |> sort() |> writeClipboard()

# Einträge die nicht in Coins sind
# anti_join(circulation, coins)

# Doppelte IDs
# which(duplicated(circulation$ID))
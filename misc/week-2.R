# Install if you don't have this package
# install.packages("tidyverse")

library(tidyverse)
library(readxl)

penguins <- read_csv("data-raw/penguins.csv")

read_csv("data-raw/2022-obtn-by-county.xlsx")

read_excel("data-raw/2019-obtn-by-county.xlsx")

# Working directories -----------------------------------------------------

penguins_mean_bill_length <-
  penguins |>
  filter(island == "Biscoe") |>
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

# Native pipe vs tidyverse pipe -------------------------------------------

penguins %>%
  count(species, sex)

# Parentheses -------------------------------------------------------------

penguins |>
  select(-species)

penguins |>
  select(-(bill_length_mm:body_mass_g))

read_csv("data-raw/penguins_data.csv", na = c("-999", "-999.0"))

# select() issues ---------------------------------------------------------

penguins |>
  select(-island:year)

penguins |>
  select(c(-1, island:bill_length_mm))

# Does not remove the "species" variable but this does:

penguins |>
  select(island:year)

# NA values ---------------------------------------------------------------

read_csv("data-raw/penguins_data.csv") |>
  view()


# NA values ---------------------------------------------------------------

penguins <- read_csv(file = "data-raw/penguins.csv")

penguins |>
  mutate(not_actually_na = "NA") |>
  mutate(actually_na = na_if(not_actually_na, "NA")) |>
  mutate(really_not_na = replace_na(actually_na, "NA"))


# Rounding ----------------------------------------------------------------

penguins |>
  filter(island == "Biscoe") |>
  drop_na(body_mass_g, sex) |>
  group_by(sex) |>
  summarize(mean_body_mass = mean(body_mass_g)) |>
  mutate(mean_body_mass = round(mean_body_mass, digits = 0))

# Viewing your dataset ----------------------------------------------------

penguins |>
  filter(species == "Adelie") |>
  view()

penguins |>
  print(n = Inf)

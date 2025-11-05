# Function Arguments ------------------------------------------------------

library(tidyverse)

penguins <- read_csv("data-raw/penguins.csv")

ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm,
    y = bill_depth_mm
  )
) +
  geom_point()

ggplot(
  penguins,
  aes(
    bill_length_mm,
    bill_depth_mm
  )
) +
  geom_point()

# Functions as Recipes ----------------------------------------------------

# https://joyfoodsunshine.com/the-most-amazing-chocolate-chip-cookies/#wprm-recipe-container-8678

cookie_ingredients <- read_csv("data-raw/cookie-ingredients.csv") |>
  rename(amount_type = quantity_type)

calculate_cookie_ingredients <- function(number_of_cookies) {}

calculate_cookie_ingredients(number_of_cookies = 100)

# Show in Excel ----------------------------------------------------------

library(tidyverse)
library(fs)

penguins <- read_csv("https://data.rfortherestofus.com/penguins-2007.csv")

show_in_excel_penguins <- function() {
  write_csv(
    x = penguins,
    file = "my-data.csv",
    na = ""
  )

  file_show(path = "my-data.csv")
}

show_in_excel_penguins()

show_in_excel <- function(data) {}

# ACS Data ---------------------------------------------------------------

library(tidycensus)
library(janitor)

race_ethnicity_data <-
  get_acs(
    geography = "state",
    variables = c(
      "B03002_003",
      "B03002_004",
      "B03002_005",
      "B03002_006",
      "B03002_007",
      "B03002_008",
      "B03002_009",
      "B03002_012"
    )
  )

race_ethnicity_data

# Basic function

get_acs_race_ethnicity <- function() {
  race_ethnicity_data <-
    get_acs(
      geography = "state",
      variables = c(
        "B03002_003",
        "B03002_004",
        "B03002_005",
        "B03002_006",
        "B03002_007",
        "B03002_008",
        "B03002_009",
        "B03002_012"
      )
    )

  race_ethnicity_data
}

get_acs_race_ethnicity()

# Change variable value text

get_acs_race_ethnicity <- function() {
  race_ethnicity_data <-
    get_acs(
      geography = "state",
      variables = c(
        "White" = "B03002_003",
        "Black/African American" = "B03002_004",
        "American Indian/Alaska Native" = "B03002_005",
        "Asian" = "B03002_006",
        "Native Hawaiian/Pacific Islander" = "B03002_007",
        "Other race" = "B03002_008",
        "Multi-Race" = "B03002_009",
        "Hispanic/Latino" = "B03002_012"
      )
    )

  race_ethnicity_data
}

get_acs_race_ethnicity()

# Add argument for clean_variable_name

# Show ... to add arguments to existing function

library(tidyverse)
library(readxl)

# Tidy Data Rule #1: Every Column is a Variable

billboard

# Tidy Data Rule #3: Every Cell is a Single Value

addresses <-
  read_csv("data-raw/addresses.csv")


# Tidy Data Rule #2: Every Row is an Observation

survey_data <-
  read_csv("data-raw/survey_data.csv")

demographics

favorite_parts

# Survey Monkey data

read_xlsx("data-raw/survey-monkey-data.xlsx")

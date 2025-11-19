#  Recreating this: https://show.rfor.us/cditiy

library(tidyverse)
library(ggchicklet)
library(scales)
library(patchwork)

cbem <- read_csv("https://rin3fall2025.rfortherestofus.com/data-raw/cbem.csv")

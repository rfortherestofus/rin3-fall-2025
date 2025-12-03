library(tidyverse)
library(palmerpenguins)

# Theme -------------------------------------------------------------------

theme_dk <- function() {
  theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_text(
        color = "grey60",
        size = 18
      )
    )
}


# Plots -------------------------------------------------------------------

ggplot(
  data = penguins,
  aes(
    x = bill_length_mm,
    y = bill_depth_mm
  )
) +
  geom_point()

ggplot(
  data = penguins,
  aes(
    x = bill_length_mm,
    y = bill_depth_mm,
    color = island
  )
) +
  geom_point() +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(
      color = "grey60",
      size = 18
    )
  )

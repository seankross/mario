library(dplyr)
library(tibble)
library(ggplot2)

Formaldehyde %>%
  as_tibble() %>%
  ggplot(aes(carb, optden)) +
    geom_point() +
    theme_minimal()

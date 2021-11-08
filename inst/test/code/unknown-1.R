library(dplyr)

Formaldehyde %>%
  as_tibble() %>%
  slice(1:2)

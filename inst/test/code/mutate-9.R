library(dplyr)

Formaldehyde %>%
  mutate(carb + optden, `carb + optden` / carb)

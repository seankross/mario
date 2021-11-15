library(dplyr)

Formaldehyde %>%
  mutate(Sum = carb + optden, carb = NULL)

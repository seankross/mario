library(dplyr)

Formaldehyde %>%
  mutate(Sum = carb + optden, carb = carb * 2)

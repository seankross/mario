library(dplyr)

Formaldehyde %>%
  mutate(Carb2 = carb, DoubleCarb2 = Carb2 * 2)

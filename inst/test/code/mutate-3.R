library(dplyr)

Formaldehyde %>%
  mutate(DoubleCarb = carb * 2, TinyCarb = DoubleCarb / 200)

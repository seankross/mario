library(dplyr)

Formaldehyde %>%
  mutate(DoubleCarb = carb * 2, HalfOptden = optden / 2)

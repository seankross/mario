# The output trace for this could be improved
library(dplyr)

Formaldehyde %>%
  mutate(carb + optden, `carb + optden` / carb)

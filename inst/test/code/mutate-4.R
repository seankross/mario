library(dplyr)

Formaldehyde %>%
  mutate(Two = 2, Last = Two + carb + 100)

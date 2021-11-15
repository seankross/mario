library(dplyr)

Formaldehyde %>%
  mutate(Sum = carb + optden, Sum * 100)

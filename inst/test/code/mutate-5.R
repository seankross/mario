library(dplyr)

Formaldehyde %>%
  mutate(Sum = carb + optden, DoubleSum = Sum * 2, Two = 2, 1 + 3)

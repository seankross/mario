library(dplyr)

Formaldehyde %>%
  arrange(desc(carb)) %>%
  slice(1:3) %>%
  mutate(Sum = cyl + mpg) %>%
  select(optden)

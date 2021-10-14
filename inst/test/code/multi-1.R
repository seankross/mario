library(dplyr)

Formaldehyde %>%
  filter(optden < 0.5) %>%
  arrange(desc(carb)) %>%
  slice(1)

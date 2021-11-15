library(dplyr)

Formaldehyde %>%
  mutate(`Carb + Optden` = carb + optden, Sum = `Carb + Optden`)# %>% parse_pipeline() %>% mario:::load_vars()

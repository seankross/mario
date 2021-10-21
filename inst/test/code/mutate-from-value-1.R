library(dplyr)

Formaldehyde %>%
  mutate(X = 1, Y = "Two") %>%
  mutate(Z = paste(Y, "Three"))

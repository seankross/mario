library(dplyr)

mtcars %>%
  slice(1:10) %>%
  slice(5:10) %>%
  slice(c(1, 3))

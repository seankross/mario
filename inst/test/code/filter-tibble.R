library(dplyr)
library(tibble)

tribble(~A, 1, 2, 3, 4) %>%
  filter(A >= 3)

library(dplyr)
library(tibble)

t <- tribble(
  ~X, 4, 3, 2, 1
)

t %>%
  arrange(X)

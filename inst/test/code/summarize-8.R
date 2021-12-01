library(dplyr)

mt <- mtcars %>%
  select(mpg, cyl, hp) %>%
  group_by(cyl) %>%
  slice(1:3)

mt %>%
  summarise(Q = quantile(mpg, c(0.01, 0.02, 0.98, 0.99)))

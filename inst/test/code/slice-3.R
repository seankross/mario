library(dplyr)

mt <- mtcars %>%
  select(mpg, cyl, hp) %>%
  group_by(cyl) %>%
  slice(1:2) %>%
  ungroup()

mt %>%
  slice(1:3)

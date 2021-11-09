library(dplyr)

mt <- mtcars %>%
  as_tibble() %>%
  select(mpg, cyl, hp) %>%
  group_by(cyl) %>%
  slice(1:2) %>%
  ungroup() %>%
  group_by(cyl)

mt %>%
  select(mpg)

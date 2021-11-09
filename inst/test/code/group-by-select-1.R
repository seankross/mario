library(dplyr)

mt <- mtcars %>%
  as_tibble() %>%
  select(mpg, cyl, hp, wt) %>%
  group_by(cyl) %>%
  slice(1:2) %>%
  ungroup() %>%
  group_by(cyl)

mt %>%
  select(wt, cyl)

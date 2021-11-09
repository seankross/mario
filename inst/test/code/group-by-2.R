library(dplyr)

mt <- mtcars %>%
  as_tibble() %>%
  select(mpg, cyl, hp) %>%
  group_by(cyl) %>%
  slice(1:2) %>%
  ungroup() %>%
  mutate(Airbags = c(2, 1, 0, 2, 0, 1))

mt %>%
  group_by(cyl, Airbags)

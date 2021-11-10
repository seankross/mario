library(dplyr)

mt <- mtcars %>%
  as_tibble() %>%
  select(mpg, cyl, hp) %>%
  group_by(cyl) %>%
  slice(1:2)

mt %>%
  mutate(cyl = "A")

library(dplyr)

mt <- mtcars %>%
  select(mpg, cyl, hp) %>%
  group_by(cyl) %>%
  slice(1:3)

mt %>%
  summarise(mpg + hp)

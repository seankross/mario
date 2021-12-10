library(dplyr)

mt <- mtcars %>%
  select(mpg, cyl) %>%
  slice(1)

mt %>%
  group_by(cyl)
